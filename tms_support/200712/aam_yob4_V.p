/* ---------------------------------------------------------------------------
  MODULE .......: NNLAMU3.P
  FUNCTION .....: Laskujen muodostus Niin kiinteästä kuin fixedistä
  APPLICATION ..: NN
  AUTHOR .......: PT/TT
  CREATED ......: 06.11.96
  MODIFIED .....: 27.03.01
                  13.07.01 kl: bIncVAT (from PriceList.InclVAT)
                  20.07.01 jp: mobile AND miniminvoice VALUE
                  09.08.01 jp: browsing old mob Calls
                  27.03.02 ht: menu-selectable, without InvRunLog logic
                  02.04.02 lp - calculate invoices amount (llkm)
                              - same MESSAGE FOR monitoring program's work
                  08.04.02 aam lamt already includes Qty of invoices ->
                               llkm NOT needed 
                  09.04.02 lp INPUT PARAMETER CustNum1
                  15.04.02 aam: get also the total VALUE from pGetAmt AND
                                show billing statistics
                  26.04.02 kl:  pUpdInvGroup instead of pUpdInvSeq
                  04.09.02/aam  billrund.i
                  10.09.02/aam  check period 
                  09.10.02/jp   Banned Check for invgroup
                  22.10.02/aam  delete persistent procedure
                  11.03.03/tk   tokencheck
                  11.09.03/aam  brand
                  13.11.03/aam  log beginning and finish times
                  04.06.04/aam  contract period by default previous month
                  08.04.05/aam  InvCode added as a limiting factor
                  21.09.05/aam  invoice nbr with fGetInvNum(),
                                don't update invoice nbr here
                  21.09.05/jp   rerate and double              
                  29.09.05/aam  counter for customer qty,
                                creation duration
                  28.11.06/aam  new logic for fGetInvNum()              
                  16.04.07/aam  new parameters to pCreateInv
                  07.05.07/mvi  check that no test invoices exist
                  07.09.07/aam  use new invtype index for 99 check
   Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/testpaa.i} 
katun = "ari".

{Func/tmsparam.i2}
{Inv/billrund.i NEW}
{Func/faccper.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}
{Func/finvnum.i}
{Func/timestamp.i}

IF lcRight NE "RW" THEN DO:
   MESSAGE " You cannot create invoices ! " VIEW-AS ALERT-BOX.
   RETURN.
END.

DEF var CustNum1 LIKE Customer.CustNum.

def var invDte     as Date format "99-99-99"     NO-UNDO init TODAY.
def var lcInvNum   as char format "x(12)"        NO-UNDO.
def var ok         as log  format "Yes/No"       NO-UNDO.
DEF VAR InvGroup    LIKE InvGroup.InvGroup       NO-UNDO.
DEF VAR b-acc      AS lo                         NO-UNDO.
DEF VAR ciperiod   AS i                          NO-UNDO.
def var i          as int  format "zzzzzzz9"     NO-UNDO.

def var atpvm1     as Date format "99-99-99" NO-UNDO.
def var atpvm2     as Date format "99-99-99" NO-UNDO.
/*def var CustNum1      as int  format "zzzzzzz9" NO-UNDO. */
def var CustNum2      as int  format "zzzzzzz9" NO-UNDO.
DEF VAR mininv     LIKE InvGroup.MinInvAmt   NO-UNDO.
DEF VAR upmth      LIKE InvGroup.UnbilledLimit    NO-UNDO.
DEF VAR kysy_rajat AS LOG                    NO-UNDO.

DEF VAR unknown    AS i   NO-UNDO.
DEF VAR not-inv    AS DE  NO-UNDO.
def var mark       as lo  no-undo format "Yes/No" init TRUE.
def var lowvalue   as lo  no-undo format "Yes/No".
DEF VAR defcurr    AS c   NO-UNDO.
DEF VAR extratime  AS i   NO-UNDO.    
DEF VAR discProd   AS C   NO-UNDO.
DEF VAR lAmt       AS DE  NO-UNDO.
DEF VAR lVatAmt    AS DE  NO-UNDO. 
DEF VAR lQty       AS I   NO-UNDO. 
DEF VAR liInvCode  AS INT NO-UNDO.
DEF VAR llRerate   AS LOG NO-UNDO.
DEF VAR llDouble   AS LOG NO-UNDO.
DEF VAR ldBegTime  AS DEC NO-UNDO.
DEF VAR ldEndTime  AS DEC NO-UNDO. 
DEF VAR liCustQty  AS INT NO-UNDO.
DEF VAR liDurDays  AS INT NO-UNDO.
DEF VAR liDurTime  AS INT NO-UNDO.
DEF VAR lcPrefix   AS CHAR NO-UNDO.
DEF VAR lcMessage  AS CHAR NO-UNDO.
DEF VAR lcBillRun  AS CHAR NO-UNDO.

DEF STREAM sTimeLog.

{Func/tmsparam.i oh-tuasno  RETURN}. unknown = TMSParam.IntVal.

/* Check that no Test Invoices exist in TMS,
   Tests must be deleted before real invoicing can be done.  */
FIND FIRST Invoice NO-LOCK WHERE 
           Invoice.Brand   = gcBrand AND
           Invoice.InvType = 99 
           NO-ERROR.
IF AVAIL Invoice THEN DO:
   MESSAGE "Test invoices must be deleted " SKIP
      "before normal invoices can be created." 
      VIEW-AS ALERT-BOX.
   RETURN.
END.

/* default values from TMSParam */
ASSIGN
   defcurr  = fCParamC("DefCurrency")
   discProd = fCParamC("DiscProd").

/* check that default Currency exists */
FIND FIRST Currency where
           Currency.Currency = defcurr
no-lock no-error.
if not avail Currency OR defcurr = ? OR defcurr = "" THEN DO:
   MESSAGE 
      "Default Currency is not defined. Continue anyway ? " UPDATE ok
   VIEW-AS ALERT-BOX MESSAGE.
   IF NOT ok THEN RETURN.
END.

DEF VAR pHandle   AS handle NO-UNDO.
RUN /apps/snet/200712/lamupers_rebill.p persistent set pHandle.

PUT SCREEN ROW 22 COL 1 FILL(" ",60).
PUT SCREEN ROW 23 COL 1 FILL(" ",60).

form
   skip(17)
WITH
   OVERLAY TITLE COLOR value(ctc)
   " " + ynimi + " INVOICING, PHASE 1 " + string(pvm,"99-99-99") + " "
   COLOR value(cfc) width 80 ROW 1
   FRAME taka.

form
   InvGroup label  " Invoice group .........."
           help "Invoice group's code"
   InvGroup.IGName no-label format  "x(16)"    SKIP
   liInvCode  label  " Invoice Run Code ......."
           help "Invoice run code, 0=ALL"
           FORMAT ">9" SKIP 
   CustNum1   label  " Customer number ........"
           help "Customers FROM number"
   "-"
   CustNum2   no-label help "Customers TO number"  SKIP
   atpvm1  label " Time Period ............"
           help "Earliest Date of calls"
   "-"
   atpvm2  no-label help "Latest Date of calls" SKIP
   invDte  label " Invoice Date ..........."
           help "Date of invoicing for all invoices"
                                                SKIP
  extratime label " Extra days ............." FORMAT "z9"
           help "Push dueday forward with this many extra days on every bill"
                                                SKIP
  ciperiod label " Contract Inv Period ...." format "999999"
  help "Latest Period where Unbilled contract items are Billed now"     SKIP 
  llRerate label " Rerate" FORMAT "Yes/No"  
  HELP "Re-Rate Calls Before Invoicing" 
  llDouble label "Double" FORMAT "Yes/No"     
  HELP "Remove Double Calls Before Invoicing"                SKIP
   mark    label " Approve automatically .."
           help "Approve all invoices automatically (Yes/No) ?" 
                                                SKIP    
   lowvalue label " Low value invoices ....."
           help "Create invoices that are below the minimum invoicing amount"
with title color value(ctc) " CRITERIA FOR CREATING INVOICES " side-labels
   COLOR value(cfc) ROW 2 centered OVERLAY FRAME rajat.

form
    " Consecutive invoice number: " lcInvNum  NO-LABEL           SKIP
    " Minimum invoicing amount .: " mininv NO-LABEL           SKIP
    " Oldest unpaid Calls ......: " upmth  NO-LABEL  
    lowvalue format "(created)/(not created)" NO-LABEL
WITH
   title color value (ctc) " INVOICE GROUP DATA " COLOR value(cfc)
   OVERLAY centered ROW 14 FRAME lCustNum.

cfc = "sel". RUN Syst/ufcolor. ccc = cfc.
view FRAME taka. PAUSE 0 no-message.

cfc = "lis". RUN Syst/ufcolor.
ehto = 9. RUN Syst/ufkey.

ASSIGN
atpvm2 = date(month(TODAY),1,year(TODAY)) - 1
atpvm1 = date(month(atpvm2),1,year(atpvm2))
ciperiod = IF MONTH(TODAY) = 1
           THEN (YEAR(TODAY) - 1) * 100 + 12
           ELSE year(TODAY) * 100 + month(TODAY) - 1
llRerate = TRUE llDouble = TRUE.

IF CustNum1 = 0 THEN 
ASSIGN
   CustNum1  = unknown + 1
   CustNum2  = 99999999.

ELSE DO:
   FIND Customer WHERE Customer.CustNum = CustNum1 NO-LOCK NO-ERROR.
   FIND InvGroup OF Customer NO-LOCK NO-ERROR.
   IF NOT AVAIL InvGroup THEN DO:
      MESSAGE
      "Default invoice group code is not defined "
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE IF invgroup.banned = TRUE THEN DO:
      MESSAGE
      "This customer belongs to invoice group '" invgroup.invgroup "' as " 
                                                 InvGroup.igname   SKIP
      "and this invoice group is normally not invoiced at all (banned). "     
                                                                         SKIP(2)
      "Do you still want to create invoice for customer"                 SKIP
      Customer.CustNum Customer.CustName " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
      IF ok = FALSE THEN RETURN.

   END.


   ASSIGN 
   CustNum2 = CustNum1
   InvGroup = Customer.InvGroup.
END.

view FRAME lCustNum.

DISPLAY lcInvNum WITH FRAME lCustNum.
PAUSE 0 no-message.

kysy_rajat = TRUE.

PAUSE 0.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, RETURN:
      IF kysy_rajat THEN DO:
         /* We ask the limits */
         ehto = 9. RUN Syst/ufkey.
         UPDATE
            InvGroup
            liInvCode
            CustNum1 CustNum2   validate(INPUT CustNum2  >= INPUT CustNum1,
                          "Impossible Customer number limitation!")
            atpvm1 atpvm2 validate(INPUT atpvm2 >= INPUT atpvm1,
                          "Impossible Time Period limitation!")
         invDte extratime
         ciperiod
         llRerate llDouble 
         mark
         lowvalue
         WITH FRAME rajat EDITING :
            READKEY. nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE no-pause.

               if frame-field = "InvGroup" THEN DO:
                  ASSIGN FRAME rajat InvGroup.
                  if InvGroup = "" THEN DO:
                     HIDE FRAME rajat no-pause.
                     HIDE FRAME lCustNum no-pause.
                     HIDE FRAME taka  no-pause.
                     RETURN.
                  END.

                  FIND InvGroup where 
                       InvGroup.Brand    = gcBrand AND
                       InvGroup.InvGroup = InvGroup
                  no-lock no-error.
                  IF NOT AVAIL InvGroup THEN DO:
                     bell.  message "Unknown invoice group !".
                     NEXT.
                  END.
                  DISP InvGroup.IGName WITH FRAME rajat.
                  PAUSE 0.

                  /* consecutive invoice number */
                  lcInvNum  = fGetInvNum(InvGroup.InvGroup,
                                         1,
                                         INPUT  INPUT InvDte,
                                         OUTPUT lcPrefix).
                                         
                  ASSIGN 
                     /* shall we UPDATE accounts */
                     b-acc  = InvGroup.UpdCustBal 
                     /* MINIMUM invoicing Qty */
                     mininv = InvGroup.MinInvAmt
                     /* unpaid MONTH Limit */
                     upmth  = InvGroup.UnbilledLimit.
                  IF lcInvNum = "" THEN DO:
                     message "NOTICE: Invoice number is ZERO !".
                     PAUSE no-message.
                     NEXT.
                  END.
                  ELSE
                     DISP lcInvNum mininv upmth lowvalue WITH FRAME lCustNum.
               END.

               ELSE IF FRAME-FIELD = "CustNum1" THEN ASSIGN CustNum1.

               ELSE IF FRAME-FIELD = "CustNum2" THEN DO:
                  IF INPUT CustNum2 = 0 THEN DO:
                     CustNum2 = INPUT CustNum1.
                     DISP CustNum1 @ CustNum2 WITH FRAME rajat.
                     ASSIGN 
                        CustNum1
                        CustNum2.
                  END.
                  IF CustNum2 < CustNum1 THEN DO:
                     MESSAGE 
                        "Impossible customer number limitation"
                     VIEW-AS ALERT-BOX error.
                     NEXT.
                  END.   
               END.

               ELSE IF FRAME-FIELD = "invDte" THEN DO:
                  IF INPUT invDte > TODAY THEN MESSAGE 
                     "You should NOT use an invoice date" SKIP
                     "other than today !"
                  VIEW-AS ALERT-BOX WARNING.
               END.

               ELSE IF FRAME-FIELD = "ciperiod" THEN DO:
                  RUN Syst/uperch(INPUT FRAME rajat ciperiod,output i).
                  IF i > 0 THEN NEXT.

               END.
            END.
            APPLY LASTKEY.
         END.

         DISP lowvalue WITH FRAME lCustNum.

         IF INPUT atpvm1 = ?  THEN atpvm1 = 01/01/1900.
         IF INPUT atpvm2 = ?  THEN atpvm2 = 12/31/9999.
         if input CustNum2  = "" THEN CustNum2  = 9999999.

         kysy_rajat = FALSE.
      END.

      ASSIGN ufk = 0 ufk[1] = 132 ufk[2] = 0
                     ufk[4] = 0 ufk[5] = 795
                     ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN DO:
         kysy_rajat = TRUE.
         NEXT toimi.
      END.

      IF toimi = 5 THEN DO:

         /* check period */
         IF fPeriodLocked(InvDte,TRUE) THEN NEXT toimi.

         /* derive invoicing Period from invoice's Date 
         Period = year(invDte) * 100 + month(invDte). */

         /* reject IF InvNum is ZERO */
         IF lcInvNum = "" THEN DO:
            message "NOTICE: Invoice number is ZERO !".
            PAUSE no-message.
            NEXT toimi.
         END.

         LEAVE toimi.

      END.

      IF toimi = 8 THEN DO:
         HIDE MESSAGE no-pause.
         HIDE FRAME rajat no-pause.
         HIDE FRAME taka no-pause.
         RETURN.
      END.

   END. /* toimi */

HIDE FRAME lCustNum no-pause.

OUTPUT STREAM sTimeLog TO /tmp/invrun_dur.log append.
PUT STREAM sTimeLog UNFORMATTED
   "Customer based (lamu3) started  (brand " gcBrand 
   " group " InvGroup ") " 
   STRING(TODAY,"99.99.9999") " "
   STRING(TIME,"hh:mm:ss")
   SKIP.
OUTPUT STREAM sTimeLog CLOSE.

ASSIGN ldBegTime = fMakeTS()
       liCustQty = 0.

/* We make it THRU ALL the Calls, what we wanted TO handle */
PAUSE 0.
message "Sorting customers and Calls ...".

def stream sread.

def var lcline as char no-undo.
def var licust as int  no-undo.

def temp-table ttcust no-undo
   field custnum as int
   index custnum custnum.
   
/*   
input stream sread from /apps/snet/200712/rebill_0711_I.log.

repeat:

   import stream sread unformatted lcline.
   licust = integer(entry(2,lcline,chr(9)))  no-error.
   
   if error-status:error then next.
   
   if can-find(first ttcust where ttcust.custnum = licust) then next.
   
   create ttcust.
   ttcust.custnum = licust.
end.

input stream sread close.

input stream sread from /apps/snet/200712/rebill_0711.log.

repeat:

   import stream sread unformatted lcline.
   licust = integer(entry(1,lcline,chr(9))) no-error.
   
   if error-status:error then next.
   
   if can-find(first ttcust where ttcust.custnum = licust) then next.
   
   create ttcust.
   ttcust.custnum = licust.
end.

input stream sread close.
   
input stream sread from /tmp/unbilled_invseq_1007_I.log.

repeat:

   import stream sread unformatted lcline.
   licust = integer(entry(2,lcline,chr(9))) no-error.
   
   if error-status:error then next.
   
   if can-find(first ttcust where ttcust.custnum = licust) then next.
   
   create ttcust.
   ttcust.custnum = licust.
end.

input stream sread close.
*/


XCUST:
for each ttcust,
   first Customer   no-lock  where
         Customer.Brand     = gcBrand     AND 
         Customer.CustNum   = ttcust.custnum,
   FIRST InvGroup OF Customer no-lock:

   if customer.invgroup ne "vat1" then next. 
   
   IF liInvCode > 0 AND Customer.InvCOde NE liInvCode THEN NEXT.
   
   IF liCustQty < 10 OR liCustQty MOD 100 = 0 THEN 
   PUT SCREEN ROW 23 col 1 
      STRING(liCustQty) + " " + 
      string(Customer.CustNum) + " " + Customer.CustName + fill(" ",20).

   CREATE ttInvCust.
   ASSIGN 
      ttInvCust.CustNr  = Customer.CustNum
      ttInvCust.MinInv  = InvGroup.MinInvAmt
      ttInvCust.InvRec  = 0
      ttInvCust.LowVal  = lowvalue
      liCustQty         = liCustQty + 1.

END.

PUT SCREEN ROW 23 COL 1 FILL(" ",60).

FIND FIRST ttInvCust NO-LOCK NO-ERROR.
IF NOT AVAIL ttInvCust THEN DO:
   MESSAGE 
      "No invoices can be created using given criteria !"
   VIEW-AS ALERT-BOX MESSAGE.
   RETURN.
END.

message licustqty "customers were picked"
view-as alert-box.

RUN pCreateInv in pHandle(invDte,
                          atpvm1,
                          atpvm2,
                          ciperiod,
                          extratime,
                          llRerate,
                          llDouble,
                          liCustQty,
                          1,
                          "").

HIDE MESSAGE NO-PAUSE.
PAUSE 0.

ldEndTime = fMakeTS().

/* duration */
liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghfunc1,
                             ldBegTime,
                             ldEndTime,
                             OUTPUT liDurTime).
                        
RUN pGetAmt in pHandle (OUTPUT lQty,
                        OUTPUT lAmt,
                        OUTPUT lVatAmt).

OUTPUT STREAM sTimeLog TO /tmp/invrun_dur.log append.
PUT STREAM sTimeLog UNFORMATTED
   "Customer based (lamu3) finished (brand " gcBrand 
   " group " InvGroup "): " 
   fTS2HMS(ldEndTime)
   "|Dur: " 
      (IF liDurDays > 0 
       THEN STRING(liDurDays) + " days and"
       ELSE "")
      STRING(liDurTime,"hh:mm:ss") 
   "|Qty: " lQty
   "|Amt VAT0: " lAmt
   "|Amt With VAT: " lVatAmt
   SKIP.
OUTPUT STREAM sTimeLog CLOSE.


ok = TRUE.
/* IF automatic approving is NOT allowed */
if not mark then message "Do You want to approve invoicing (Y/N) ?" UPDATE ok.


/* approve invoice RUN */
IF ok AND lQty > 0 THEN DO: 
    RUN pUpdInvGroup in pHandle.

    RUN pGetBillRunID IN pHandle (OUTPUT lcBillRun).
    
    lcMessage = 
       STRING(lQty) +  
       " invoices were created." + CHR(10) + 
       "Duration for creation was " +
        (IF liDurDays > 0 
         THEN STRING(liDurDays) + " days and"
         ELSE "") +
       STRING(liDurTime,"hh:mm:ss") + CHR(10) + 
       "Total amount excluding VAT was " + 
          TRIM(STRING(lAmt,"->>>>>>>>9.99"))  + CHR(10) +
       "Total payable amount, including VAT and reductions was " + 
          TRIM(STRING(lVatAmt,"->>>>>>>>9.99")).

    DO FOR ActionLog TRANS:
       CREATE ActionLog.
       ASSIGN 
          ActionLog.Brand        = gcBrand   
          ActionLog.TableName    = "Invoice"  
          ActionLog.KeyValue     = lcBillRun 
          ActionLog.ActionID     = "BillRun"
          ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                   MONTH(TODAY)
          ActionLog.ActionDec    = lQty
          ActionLog.ActionChar   = lcMessage
          ActionLog.ActionStatus = 0.
          ActionLog.ActionTS     = fMakeTS().
    END.
    
    MESSAGE lcMessage
    VIEW-AS ALERT-BOX
    title " Billing statistics " .
END.

/* remove ALL created invoices */
ELSE IF lQty NE 0 THEN RUN pCancel in pHandle.
/* no NEW invoices */
else message "No invoice(s) were created !" VIEW-AS ALERT-BOX MESSAGE.

/* clear persistent procedures */
RUN pCleanMem IN pHandle.

IF VALID-HANDLE(pHandle) THEN DELETE PROCEDURE pHandle.

HIDE MESSAGE no-pause.
put screen row 22 col 60 "               ".
HIDE FRAME rajat no-pause.
HIDE FRAME LOG no-pause.
HIDE FRAME taka no-pause.

