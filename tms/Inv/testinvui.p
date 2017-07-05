/* ---------------------------------------------------------------------------
  MODULE .......: TESTINVUI.P (based on nnlamu3.p)
  FUNCTION .....: Create Test Invoices
  APPLICATION ..: TMS
  AUTHOR .......: mvi 
  CREATED ......: 07.03.07
  MODIFIED .....: 
  Version ......: XFERA
  TODO .........: process logging
  -------------------------------------------------------------------------- */
DISABLE TRIGGERS FOR LOAD OF FixedFee.
DISABLE TRIGGERS FOR LOAD OF SingleFee.

{Func/log.i}
fsetLogFileName("/scratch/print/testinv/testinvui_" + 
   STRING(YEAR(TODAY),"9999") + 
   STRING(MONTH(TODAY),"99") +
   STRING(DAY(TODAY),"99") + 
   REPLACE(STRING(TIME,"hh:mm:ss"),":","")
   + ".log").
fSetLogEntryTypes(fGetValidLogEntryTypes()).
fSetGlobalLoggingLevel(1).
fClearLog().

{Syst/commali.i}
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

DEF INPUT PARAMETER CustNum1 LIKE Customer.CustNum. 

def var invDte     as Date format "99-99-99"     NO-UNDO init TODAY.
def var lcInvNum   as char format "x(12)"        NO-UNDO.
def var ok         as log  format "Yes/No"       NO-UNDO.
DEF VAR InvGroup    LIKE InvGroup.InvGroup       NO-UNDO.
DEF VAR b-acc      AS lo                         NO-UNDO.
DEF VAR ciperiod   AS i                          NO-UNDO.
def var i          as int  format "zzzzzzz9"     NO-UNDO.
def var atpvm1     as Date format "99-99-99" NO-UNDO.
def var atpvm2     as Date format "99-99-99" NO-UNDO.
def var CustNum2      as int  format "zzzzzzzz9" NO-UNDO.
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
DEF VAR useFile    AS LOGICAL NO-UNDO FORMAT "YES/NO" INIT FALSE. 
DEF VAR llCreateXML AS LOG  NO-UNDO.
DEF VAR lcBillRun   AS CHAR NO-UNDO.

DEF STREAM sTimeLog.

{Func/tmsparam.i oh-tuasno  RETURN}. unknown = TMSParam.IntVal.

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
RUN Inv/lamupers.p persistent set pHandle.

MESSAGE "Running in Test mode" VIEW-AS ALERT-BOX.

PUT SCREEN ROW 22 COL 1 FILL(" ",60).
PUT SCREEN ROW 23 COL 1 FILL(" ",60).

form
   skip(17)
WITH
   OVERLAY TITLE COLOR value(ctc)
   " " + ynimi + " TEST INVOICING, PHASE 1 " + string(pvm,"99-99-99") + " "
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
           help "Customers FROM number" FORMAT "zzzzzzzz9" 
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
           SKIP
   useFile label " Use only Customer List.." FORMAT "Yes/No"
           help "Form only invoices matching the MSISDN list" SKIP
   llCreateXML LABEL " Create XMLs ............" FORMAT "Yes/No"
           HELP "Create XMLs for PDF generation"
           
with title color value(ctc) " CRITERIA FOR CREATING TEST INVOICES " side-labels
   COLOR value(cfc) ROW 2 centered OVERLAY FRAME rajat.

form
    " Consecutive invoice number: " lcInvNum  NO-LABEL           SKIP
    " Minimum invoicing amount .: " mininv NO-LABEL           SKIP
WITH
   title color value (ctc) " INVOICE GROUP DATA " COLOR value(cfc)
   OVERLAY centered ROW 16 FRAME lCustNum.

cfc = "sel". RUN Syst/ufcolor.p. ccc = cfc.
view FRAME taka. PAUSE 0 no-message.

cfc = "lis". RUN Syst/ufcolor.p.
ehto = 9. RUN Syst/ufkey.p.

ASSIGN
atpvm2 = date(month(TODAY),1,year(TODAY)) - 1
atpvm1 = date(month(atpvm2),1,year(atpvm2))
ciperiod = IF MONTH(TODAY) = 1
           THEN (YEAR(TODAY) - 1) * 100 + 12
           ELSE year(TODAY) * 100 + month(TODAY) - 1
llRerate = TRUE llDouble = TRUE
llCreateXML = TRUE.

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
         ehto = 9. RUN Syst/ufkey.p.
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
         useFile
         llCreateXML
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
                     DISP lcInvNum mininv  WITH FRAME lCustNum.
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
                  RUN Syst/uperch.p(INPUT FRAME rajat ciperiod,output i).
                  IF i > 0 THEN NEXT.

               END.
            END.
            APPLY LASTKEY.
         END.

         IF INPUT atpvm1 = ?  THEN atpvm1 = 01/01/1900.
         IF INPUT atpvm2 = ?  THEN atpvm2 = 12/31/9999.
         if input CustNum2  = "" THEN CustNum2  = 9999999.

         kysy_rajat = FALSE.
      END.

      ASSIGN ufk = 0 ufk[1] = 132 ufk[2] = 0
                     ufk[4] = 0 ufk[5] = 795
                     ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN DO:
         kysy_rajat = TRUE.
         NEXT toimi.
      END.

      IF toimi = 5 THEN DO:

         /* check period */
         IF fPeriodLocked(InvDte,TRUE) THEN NEXT toimi.

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
fLog("Customer based (lamu3) started  (brand " + gcBrand + ")",katun).

ASSIGN ldBegTime = fMakeTS()
       liCustQty = 0.

/* We make it THRU ALL the Calls, what we wanted TO handle */
PAUSE 0.


IF useFile THEN DO:

   DEF VAR lcTestInvInputFile AS CHAR NO-UNDO.
   DEF VAR lcCustNum          AS CHAR NO-UNDO.
   DEF VAR liCustNum          AS INT  NO-UNDO.

   DEF STREAM sInput.

   DEFINE TEMP-TABLE ttAcceptedCustNums NO-UNDO
     FIELD CustNum AS INT
     FIELD Found   AS LOGICAL INIT FALSE 
     INDEX CustNum CustNum.
    
   lcTestInvInputFile = fCParamC("TestInvCustNumFile").
   IF lcTestInvInputFile = "" OR lcTestInvInputFile = ? THEN
      lcTestInvInputFile = "/scratch/print/testinv/test_custnums.txt".
      
   IF SEARCH(lcTestInvInputFile) <> ? THEN DO:
      INPUT STREAM sInput FROM VALUE(lcTestInvInputFile).
      
      REPEAT:
         IMPORT STREAM sInput UNFORMATTED lcCustNum.
         IF lcCustNum = "" THEN NEXT.

         liCustNum = INT(lcCustNum) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT.

         IF CAN-FIND(FIRST ttAcceptedCustNums WHERE
                           ttAcceptedCustNums.CustNum= liCustNum) THEN NEXT.

         CREATE ttAcceptedCustNums.
                ttAcceptedCustNums.CustNum = liCustNum.
      END. /* REPEAT:*/
        
      INPUT STREAM sInput CLOSE.

   END. /* IF SEARCH(lcTestInvInputFile) <> ? THEN DO: */
   ELSE DO:
      fLog("Problem with CUSTOMER list file (not found?)",katun).

   END.
   MESSAGE "Sorting customers and Calls, using predefined CUSTOMER list ".

   FOR EACH ttAcceptedCustNums,
      FIRST Customer NO-LOCK WHERE 
            Customer.CustNum = ttAcceptedCustNums.CustNum,
      FIRST InvGroup OF Customer NO-LOCK:

         CREATE ttInvCust.
         ASSIGN
            ttInvCust.CustNr  = Customer.CustNum
            ttInvCust.MinInv  = InvGroup.MinInvAmt
            ttInvCust.CallQty = 0
            ttInvCust.LowVal  = NO
            liCustQty         = liCustQty + 1.
   
         /* mark found, so we can log error cases */
         ttAcceptedCustNums.found = TRUE.
   END. /* FOR EACH ttAcceptedCustNums */
   EMPTY TEMP-TABLE ttAcceptedCustNums.

   PAUSE 0.
END.
ELSE DO:
   message "Sorting customers and Calls ...".

   XCUST:
   FOR EACH Customer   no-lock  where
            Customer.Brand     = gcBrand     AND 
            Customer.CustNum  >= CustNum1    AND
            Customer.CustNum  <= CustNum2    AND
            Customer.CustNum  >  unknown     AND
            Customer.InvGroup  = InvGroup,
    
      FIRST InvGroup OF Customer no-lock:

      IF liInvCode > 0 AND Customer.InvCOde NE liInvCode THEN NEXT.
      
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
END.

PUT SCREEN ROW 23 COL 1 FILL(" ",60).

/* calculate bundle first month fees */
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

FIND FIRST ttInvCust NO-LOCK NO-ERROR.
IF NOT AVAIL ttInvCust THEN DO:
   MESSAGE 
      "No invoices can be created using given criteria !"
   VIEW-AS ALERT-BOX MESSAGE.
   RETURN.
END.
RUN pCreateTestInv in pHandle("",
                              invDte,
                              ?,
                              atpvm1,
                              atpvm2,
                              ciperiod,
                              extratime,
                              llRerate,
                              llDouble,
                              liCustQty,
                              "").


HIDE MESSAGE NO-PAUSE.
PAUSE 0.

ldEndTime = fMakeTS().


fLog("Invoice Testing Customer based (testinvui/lamu3) " +
   "finished (brand " + gcBrand + ") ",katun).

ok = TRUE.


/* duration */
liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghfunc1,
                             ldBegTime,
                             ldEndTime,
                             OUTPUT liDurTime).
                        
/* IF automatic approving is NOT allowed */
if not mark then message "Do You want to approve invoicing (Y/N) ?" UPDATE ok.

RUN pGetAmt in pHandle (OUTPUT lQty,
                        OUTPUT lAmt,
                        OUTPUT lVatAmt).

/* approve invoice RUN */
IF ok AND lQty > 0 THEN DO: 
    
    RUN pGenerateReport IN pHandle.

    RUN pGetBillRunID IN pHandle (OUTPUT lcBillRun).
    
    /* clears temptables */
    RUN pUpdInvGroup in pHandle.

    /* create xmls for pdfs */
    IF llCreateXML THEN 
       RUN Inv/invoice_xml_testbill.p(invDte,
                                lcBillRun).

    MESSAGE lQty 
            "invoices were created." SKIP
            "Duration for creation was" 
            (IF liDurDays > 0 
             THEN STRING(liDurDays) + " days and"
             ELSE "")
            STRING(liDurTime,"hh:mm:ss") SKIP
            "Total amount excluding VAT was" lAmt SKIP
            "Total payable amount, including VAT and reductions was" lVatAmt
            SKIP
            "Billing run ID is" lcBillRun
    VIEW-AS ALERT-BOX
    title " Billing statistics " .
END.

/* remove ALL created invoices */
ELSE IF lQty NE 0 THEN RUN pCancel in pHandle.
/* no NEW invoices */
else message "No invoice(s) were created !" VIEW-AS ALERT-BOX MESSAGE.

fCloseLog().
/* clear persistent procedures */
RUN pCleanMem IN pHandle.

IF VALID-HANDLE(pHandle) THEN DELETE PROCEDURE pHandle.

HIDE MESSAGE no-pause.
put screen row 22 col 60 "               ".
HIDE FRAME rajat no-pause.
HIDE FRAME LOG no-pause.
HIDE FRAME taka no-pause.

