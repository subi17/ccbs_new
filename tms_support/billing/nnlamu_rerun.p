{Syst/testpaa.i}
katun = "ari".
{Func/tmsparam2.i}
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
def var CustNum2      as int  format "zzzzzzz9" NO-UNDO.
DEF VAR mininv     LIKE InvGroup.MinInvAmt   NO-UNDO.
DEF VAR upmth      LIKE InvGroup.UnbilledLimit    NO-UNDO.
DEF VAR kysy_rajat AS LOG                    NO-UNDO.

DEF VAR unknown    AS i   NO-UNDO.
DEF VAR not-inv    AS DE  NO-UNDO.
def var mark       as lo  no-undo format "Yes/No" init TRUE.
def var lowvalue   as lo  no-undo format "Yes/No".
DEF VAR defcurr    AS c   NO-UNDO.
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
DEF VAR ldtDueDate AS DATE NO-UNDO.
DEF VAR lcFile AS CHAR NO-UNDO. 

DEF STREAM sTimeLog.

{Func/tmsparam.i oh-tuasno  RETURN}. unknown = TMSParam.IntVal.

/* Check that no Test Invoices exist in TMS,
   Tests must be deleted before real invoicing can be done.  */
FIND FIRST Invoice NO-LOCK WHERE 
           Invoice.Brand   = gcBrand AND
           Invoice.InvType = 99 
           NO-ERROR.
/*
IF AVAIL Invoice THEN DO:
   MESSAGE "Test invoices must be deleted " SKIP
      "before normal invoices can be created." 
      VIEW-AS ALERT-BOX.
   RETURN.
END.
*/
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
RUN Inv/lamupers persistent set pHandle.

PUT SCREEN ROW 22 COL 1 FILL(" ",60).
PUT SCREEN ROW 23 COL 1 FILL(" ",60).

form
   skip(17)
WITH
   OVERLAY TITLE COLOR value(ctc)
   " " + ynimi + " REGENERATION " + string(pvm,"99-99-99") + " "
   COLOR value(cfc) width 80 ROW 1
   FRAME taka.

form
   skip(1)
   lcFile format "X(60)" label "Customer File" 
      help "File containing customer numbers" skip(1)
   atpvm1  label " Time Period ............"
           help "Earliest Date of calls"
   "-"
   atpvm2  no-label help "Latest Date of calls" SKIP
   invDte  label " Invoice Date ..........."
           help "Date of invoicing for all invoices"
                                                SKIP
  ldtDueDate LABEL " Due Date ..............." FORMAT "99-99-99"
           help "Due date, overrides normal logic"
                                                SKIP
  ciperiod label " Contract Inv Period ...." format "999999"
     help "Latest Period where Unbilled contract items are Billed now"     SKIP 
  lowvalue label " Low value invoices ....."
           help "Create invoices that are below the minimum invoicing amount"
with title color value(ctc) " CRITERIA FOR CREATING INVOICES " side-labels
   COLOR value(cfc) ROW 2 centered OVERLAY FRAME rajat.

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

PAUSE 0 no-message.

kysy_rajat = TRUE.

PAUSE 0.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, RETURN:
      IF kysy_rajat THEN DO:
         /* We ask the limits */
         ehto = 9. RUN Syst/ufkey.
         UPDATE
            lcFile
            atpvm1 atpvm2 validate(INPUT atpvm2 >= INPUT atpvm1,
                          "Impossible Time Period limitation!")
         invDte ldtDueDate
         ciperiod
         lowvalue
         WITH FRAME rajat EDITING :
            READKEY. nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE no-pause.

               IF FRAME-FIELD = "invDte" THEN DO:
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

         IF INPUT atpvm1 = ?  THEN atpvm1 = 01/01/1900.
         IF INPUT atpvm2 = ?  THEN atpvm2 = 12/31/9999.

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

         IF ldtDueDate NE ? THEN DO:
            IF ldtDueDate < invDte THEN DO:
               MESSAGE "Due date cannot be earlier than invoice date"
               VIEW-AS ALERT-BOX ERROR.
               NEXT toimi.
            END.
            
            IF ldtDueDate > invDte + 60 THEN DO:
               MESSAGE "Check the due date"
               VIEW-AS ALERT-BOX INFORMATION.
               NEXT toimi.
            END.
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
input stream sread from value(lcFile).

def var lcline as char no-undo.
def var licust as int no-undo.
def var lldo as log no-undo.

repeat:

   import stream sread lcline.
   licust = int(lcline) no-error.
   if error-status:error or licust = 0 then next.

   FOR first Customer no-lock where
         Customer.CustNum  = licust,
   FIRST InvGroup OF Customer no-lock:

      if can-find(first invoice use-index custnum where
                        invoice.brand = "1" and
                        invoice.custnum = customer.custnum and
                        invoice.invtype = 1 and
                        invoice.invdate = invDte)
      then next. 
     
      if can-find(first ttinvcust where ttinvcust.custnr = licust) then next.
   
      IF liCustQty < 10 OR liCustQty MOD 100 = 0 THEN 
      PUT SCREEN ROW 23 col 1 
         STRING(liCustQty) + " " + 
         string(Customer.CustNum) + " " + Customer.CustName + fill(" ",20).

      CREATE ttInvCust.
      ASSIGN 
         ttInvCust.CustNr  = Customer.CustNum
         ttInvCust.MinInv  = InvGroup.MinInvAmt
         ttInvCust.CallQty = 0
         ttInvCust.LowVal  = lowvalue
         liCustQty         = liCustQty + 1.

   END.

end.

input stream sread close.

PUT SCREEN ROW 23 COL 1 FILL(" ",60).

FIND FIRST ttInvCust NO-LOCK NO-ERROR.
IF NOT AVAIL ttInvCust THEN DO:
   MESSAGE 
      "No customers available"
   VIEW-AS ALERT-BOX MESSAGE.
   RETURN.
END.

lldo = false.
message licustqty "customers were found from the file." skip
        "Continue with the invoice creation?"
view-as alert-box question
buttons yes-no
set lldo.
if not lldo then return.

/* Calculate bundle first month fee */
FOR EACH ttInvCust:

   RUN Mm/bundle_first_month_fee.p(atpvm1,
                                atpvm2,
                                ttInvCust.CustNr,
                                0,
                                0,
                                "",
                                OUTPUT i).
 
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      MESSAGE "Calculation for bundle first month fee failed for customer "
              ttInvCust.CustNr SKIP
              RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   /* If customer has DSS active then calculate Bundle fee */
   /* based on the DSS total consumption                   */
   RUN Mm/dss_bundle_first_month_fee.p(atpvm1,
                                    atpvm2,
                                    ttInvCust.CustNr,
                                    0,
                                    0,
                                    "",
                                    OUTPUT i).
 
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      MESSAGE "Calculation for bundle first month fee failed for DSS customer "
              ttInvCust.CustNr SKIP
              RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
END.

RUN pCreateInv in pHandle(invDte,
                          atpvm1,
                          atpvm2,
                          ciperiod,
                          ldtDueDate,
                          llRerate,
                          llDouble,
                          liCustQty,
                          1,
                          "",
                          FALSE).

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

    RUN pGetBillRunID IN pHandle (OUTPUT lcBillRun).

    RUN pUpdInvGroup in pHandle.

    lcMessage = 
       STRING(lQty) +  
       " invoices were created." + CHR(10) + 
       "Duration for creation was " +
        (IF liDurDays > 0 
         THEN STRING(liDurDays) + " days and "
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
          ActionLog.ActionStatus = 2.
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

