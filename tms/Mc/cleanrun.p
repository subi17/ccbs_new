/* ---------------------------------------------------------------------------
  MODULE .......: CLEANRUN.P
  FUNCTION .....: UI for billing cleaning run 
  APPLICATION ..: TMS
  AUTHOR .......: aam (from nnlamu3)
  CREATED ......: 11.12.02
  MODIFIED .....: 12.09.03/aam brand
                  22.06.04/aam killed subscriptions
                  21.09.05/aam  invoice nbr with fGetInvNum(),
                                don't update invoice nbr here
                  28.11.06/aam new logic for fGetInvNum()              
  VERSION ......: M15 
  -------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{finvnum.i}
{billrund.i NEW}

def var invDte     as date format "99-99-99"     no-undo init today.
def var lasno      as char format "x(12)"        no-undo.
def var ok         as log  format "Yes/No"       no-undo.
def var InvGroup   like InvGroup.InvGroup        no-undo.
def var b-acc      as lo                         no-undo.
def var ciperiod   as i                          no-undo.
def var i          as int  format "zzzzzzz9"     no-undo.

DEF VAR asno1      AS INT  FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR asno2      AS INT  FORMAT ">>>>>>>9" NO-UNDO.
def var atpvm1     as date format "99-99-99" no-undo.
def var atpvm2     as date format "99-99-99" no-undo.
def var mininv     like InvGroup.mininv   no-undo.
def var upmth      like InvGroup.UnbilledLimit    no-undo.
def var kysy_rajat as log                    no-undo.

def var unknown    as i   no-undo.
def var not-inv    as de  no-undo.
def var mark       as lo  no-undo format "Yes/No" init true.
def var lowvalue   as lo  no-undo format "Yes/No".
def var defcurr    as c   no-undo.
def var extratime  as i   no-undo.    
DEF VAR lAmt       AS DE  NO-UNDO.
DEF VAR lVatAmt    AS DE  NO-UNDO. 
DEF VAR lQty       AS I   NO-UNDO. 
DEF VAR ldCredLoss AS DEC NO-UNDO.
DEF VAR liMinDays  AS INT NO-UNDO INIT 90. 
DEF VAR llKilled   AS LOG NO-UNDO. 
DEF VAR lcPrefix   AS CHAR NO-UNDO.

DEF BUFFER bEventCust FOR Customer.

{tmsparam.i oh-tuasno  RETURN}. unknown  = tmsparam.IntVal.

/* default values from cparam */
defcurr  = fCParamC("DefCurrency").

/* check that default currency exists */
find first currency where
           currency.Currency = defcurr
no-lock no-error.
if not avail currency OR defcurr = ? OR defcurr = "" then do:
   message 
      "Default currency is not defined. Continue anyway ? " update ok
   view-as alert-box message.
   if not ok then return.
end.

def var pHandle   as handle no-undo.
run lamupers persistent set pHandle.

form
   skip(17)
with
   overlay title color value(ctc)
   " " + ynimi + " BILLING CLEANING RUN " + string(pvm,"99-99-99") + " "
   color value(cfc) width 80
   frame taka.

form
   InvGroup label  " Invoice group .........."
           help "Invoice group's code"
   InvGroup.IGName no-label format  "x(16)"    skip
   asno1   label  " Customer number ........"
           help "Customers FROM number"
   "-"
   asno2   no-label help "Customers TO number"  skip
   atpvm1  label " Time period ............"
           help "Earliest date of calls"
   "-"
   atpvm2  no-label help "Latest date of calls" skip
   invDte  label " Invoice date ..........."
           help "Date of invoicing for all invoices"
                                                skip
  extratime label " Extra days ............." FORMAT "z9"
           help "Push dueday forward with this many extra days on every bill"
                                                SKIP
  ciperiod label " Contract Inv Period ...." format "999999"
  help "Latest period where unbilled contract items are billed now"     
                                                skip
  mark    label " Approve automatically .."
          help "Approve all invoices automatically (Yes/No) ?" 
                                                skip(1)    
  ldCredLoss 
     label " Book to credit loss ...."
     help "Book invoices below this limit to credit loss"
     format ">>>>9.99"
     SKIP
  liMinDays   
     label " Days from last billing ."
     help "Minimum days from last billed period's end to this period's end"
     SKIP
  llKilled
     label " Killed Subscriptions ..."
     help "Take only customers with (K)illed subscriptions or (A)ll"
     format "Killed/All"
     SKIP

with title color value(ctc) " CRITERIA FOR CREATING INVOICES " side-labels
   color value(cfc) row 2 centered overlay frame rajat.

form
    " Consecutive invoice number: " lasno  no-label           skip
    " Minimum invoicing amount .: " mininv no-label           skip
with
   title color value (ctc) " INVOICE GROUP DATA " color value(cfc)
   overlay centered row 15 frame lasno.

cfc = "sel". run ufcolor. ccc = cfc.
view frame taka. pause 0 no-message.

cfc = "lis". run ufcolor.
ehto = 9. run ufkey.

ASSIGN
atpvm2 = date(month(today),1,year(today)) - 1
atpvm1 = date(month(atpvm2),1,year(atpvm2))
ciperiod = year(today) * 100 + month(today)
llKilled = TRUE.

IF asno1 = 0 THEN 
ASSIGN
   asno1  = unknown + 1
   asno2  = 99999999.

ELSE DO:
   FIND Customer WHERE Customer.CustNum = asno1 NO-LOCK NO-ERROR.
   FIND InvGroup WHERE 
        InvGroup.Brand    = gcBrand AND
        InvGroup.InvGroup = Customer.InvGroup NO-LOCK NO-ERROR.
   IF NOT AVAIL InvGroup THEN DO:
      MESSAGE
      "Default invoice group code is not defined "
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   ELSE IF InvGroup.Banned = TRUE THEN DO:
      MESSAGE
      "This customer belongs to invoice group '" InvGroup.InvGroup "' as "
       InvGroup.IGName SKIP
      "and this invoice group is normally not invoiced at all (banned). "                  SKIP(2)
      "Do you still want to create invoice for customer"      skip
      Customer.CustNum Customer.CustName " ?"

      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
      IF ok = FALSE THEN RETURN.

   END.

   ASSIGN 
   asno2 = asno1
   InvGroup = Customer.InvGroup.
END.

view frame lasno.

display lasno with frame lasno.
pause 0 no-message.

kysy_rajat = true.

pause 0.

toimi:
   repeat with frame valinta on endkey undo toimi, return:
      if kysy_rajat then do:
         /* We ask the limits */
         ehto = 9. run ufkey.
         update
            InvGroup
            asno1 asno2   validate(input asno2  >= input asno1,
                          "Impossible Customer number limitation!")
            atpvm1 atpvm2 validate(input atpvm2 >= input atpvm1,
                          "Impossible Time period limitation!")
         invDte extratime
         ciperiod
         mark
         ldCredLoss
         liMinDays
         llKilled
         with frame rajat editing :
            readkey. nap = keylabel(lastkey).
            if lookup(nap,poisnap) > 0 then do:
               hide message no-pause.

               if frame-field = "InvGroup" then do:
                  assign frame rajat InvGroup.
                  if InvGroup = "" then do:
                     hide frame rajat no-pause.
                     hide frame lasno no-pause.
                     hide frame taka  no-pause.
                     return.
                  end.

                  find InvGroup where 
                       InvGroup.Brand    = gcBrand AND
                       InvGroup.InvGroup = InvGroup
                  no-lock no-error.
                  if not avail InvGroup then do:
                     bell.  message "Unknown invoice group !".
                     next.
                  end.
                  disp InvGroup.IGName with frame rajat.
                  pause 0.
 
                  /* consecutive invoice number */
                  lasno  = fGetInvNum(InvGroup.InvGroup,
                                      1,
                                      INPUT  INPUT InvDte,
                                      OUTPUT lcPrefix).
                 
                  assign
                     /* shall we update accounts */
                     b-acc  = InvGroup.UpdCustBal
                     /* minimum invoicing amt */
                     mininv = InvGroup.MinInv
                     /* unpaid month limit */
                     liMinDays = InvGroup.UnbilledLimit.
                  if lasno = "" then do:
                     message "NOTICE: Invoice number is ZERO !".
                     pause no-message.
                     next.
                  end.
                  else
                     disp lasno mininv with frame lasno.

                  DISPLAY liMinDays WITH FRAME rajat.  
               end.

               ELSE IF FRAME-FIELD = "asno1" THEN ASSIGN asno1.

               ELSE IF FRAME-FIELD = "asno2" THEN DO:
                  if input asno2 = 0 then do:
                     asno2 = input asno1.
                     disp asno1 @ asno2 with frame rajat.
                     assign 
                        asno1
                        asno2.
                  end.
                  if asno2 < asno1 then do:
                     message 
                        "Impossible customer number limitation"
                     view-as alert-box error.
                     next.
                  end.   
               END.

               ELSE IF FRAME-FIELD = "invDte" THEN DO:
                  IF INPUT invDte = ? THEN DO:
                     MESSAGE
                     "Invoice date is mandatory !"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  IF INPUT invDte > TODAY THEN MESSAGE 
                     "You should NOT use an invoice date" SKIP
                     "other than today !"
                  VIEW-AS ALERT-BOX WARNING.
               END.

               ELSE IF FRAME-FIELD = "ciperiod" THEN DO:
                  RUN uperch(INPUT FRAME rajat ciperiod,output i).
                  IF i > 0 THEN NEXT.

               END.
            end.
            apply lastkey.
         end.

         if input atpvm1 = ?  then atpvm1 = 01/01/1900.
         if input atpvm2 = ?  then atpvm2 = 12/31/9999.
         if input asno2  = "" then asno2  = 99999999.

         kysy_rajat = false.
      end.

      assign ufk = 0 ufk[1] = 132 ufk[2] = 0
                     ufk[4] = 0 ufk[5] = 795
                     ufk[8] = 8 ehto = 0.
      run ufkey.
      if toimi = 1 then do:
         kysy_rajat = true.
         next toimi.
      end.

      if toimi = 5 then do:

         /* reject if lanro is ZERO */
         if lasno = "" then do:
            message "NOTICE: Invoice number is ZERO !".
            pause no-message.
            next toimi.
         end.

         leave toimi.

      end.

      if toimi = 8 then do:
         hide message no-pause.
         hide frame rajat no-pause.
         hide frame taka no-pause.
         return.
      end.

   end. /* toimi */

hide frame lasno no-pause.

/* We make it thru all the calls, what we wanted to handle */
message "Sorting customers and calls ...".
XCUST:
for each Customer   no-lock  where
         Customer.Brand     = gcBrand  AND
         Customer.CustNum  >= asno1    and
         Customer.CustNum  <= asno2    and
         Customer.CustNum  >  unknown  and
         Customer.InvGroup  = InvGroup,

   first InvGroup  no-lock where
         InvGroup.Brand    = gcBrand AND
         InvGroup.InvGroup = Customer.InvGroup.

   /* only customers with killed subscriptions */
   IF llKilled THEN DO:
      
      /* if there is even one active mobsub -> new billable material will
         come and there is no need to clean events now */
      IF CAN-FIND(FIRST MobSub OF Customer) THEN NEXT.

      /* check also possible sub-customers */      
      ok = TRUE.
      FOR EACH bEventCust NO-LOCK WHERE
               bEventCust.InvCust = Customer.CustNum:
          IF CAN-FIND(FIRST MobSub OF bEventCust) THEN DO:
             ok = FALSE.
             LEAVE.
          END.
      END.
      IF NOT ok THEN NEXT. 
    
   END.
    
   put screen row 23 col 1                              
      string(Customer.CustNum) + " " + Customer.CustName + fill(" ",20).

   CREATE ttInvCust.
   ASSIGN 
      ttInvCust.CustNr  = Customer.CustNum
      ttInvCust.MinInv  = InvGroup.MinInv
      ttInvCust.CallQty = 0
      /* if only killed taken -> no need to check min. limit */  
      ttInvCust.LowVal  = llKilled.

end.


FIND FIRST ttInvCust NO-LOCK NO-ERROR.
IF NOT AVAIL ttInvCust THEN DO:
   MESSAGE 
      "No invoices can be created using given criteria !"
   VIEW-AS ALERT-BOX MESSAGE.
   RETURN.
END.

run pCleanRun in pHandle(invDte,
                         atpvm1,
                         atpvm2,
                         ciperiod,
                         extratime,
                         ldCredLoss,
                         liMinDays).

ok = true.

/* If automatic approving is not allowed */
if not mark then message "Do You want to approve invoicing (Y/N) ?" update ok.

run pGetAmt in pHandle (output lQty,
                        output lAmt,
                        output lVatAmt).

/* approve invoice run */
if ok AND lQty > 0 then DO: 
    run pUpdInvGroup in pHandle.
    message lQty 
            "invoices were created." skip
            "Total amount excluding VAT was" lAmt skip
            "Total payable amount, including VAT and reductions was" lVatAmt
    view-as alert-box
    title " Billing statistics " .
END.

/* remove all created invoices */
else if lQty NE 0 then run pCancel in pHandle.
/* no new invoices */
else message "No invoice(s) were created !" view-as alert-box message.

/* clear persistent procedures */
RUN pCleanMem IN pHandle.

IF VALID-HANDLE(pHandle) THEN DELETE PROCEDURE pHandle.

hide message no-pause.
put screen row 22 col 60 "               ".
hide frame rajat no-pause.
hide frame log no-pause.
hide frame taka no-pause.
