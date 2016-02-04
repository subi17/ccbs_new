/* ----------------------------------------------------------------------------
  MODULE .......: NNCIMU.P
  FUNCTION .....: Creating credit invoices
  APPLICATION ..: NN
  AUTHOR .......: PT & KL
  CREATED ......: 11-12-97
  changePVM ....: 14-12-97 pt PAUSE no-message luonnin jalkeen
                  26-03-98 kl own number list FOR credit invoices in 
                              nncoinv
                  15.04.98 pt "suor" <- 0
                  27.05.98 kl just layout
                  12.10.98 kl transact limits added  AND changed from default
                  13.10.98 pt f2: notes f3: memo
                  27.10.98 kl credited invoice's State into 9 + actions
                  17.11.98 kl CREATE userlog Event, hInvNum into Invoice
                  26.11.98 pt store # of original inv. into memo[1] WITH
                              explanation text (nnteks # 50)
                  01.04.99 kl InvRowId sequence
                  14.06.99 kl WInvDisp updating
                  19.07.99 kl Customer.Deposit[2] / Invoice.OverPaym
                  20.07.99 kl Customer.LatestInv
                  30.07.99 kl credit also Qty of Calls & minutes
                  24.08.99 pt set time stamps onto NEW hlasku record, 
                              Invoice.OverPaym is SUBTRACTED from customer's
                              Balance (when an invoice WITH OverPaym is 
                              credited)
                  24.08.99 kl hlasku.ClaimQty = 0
                  25.10.99 kl functions in ASSIGN
                  04.11.99 kl Eventlog functions
                  18.11.99 pt inv Date must NOT be < TODAY
                  17.12.99 kl State initially FALSE
                  20.12.99 kl loop FOR finding AVAILABLE Credit invoice number
                  07.02.99 kl check AccNum number FOR Credit ROW from BillCode
                  16.02.00 kl special handling FOR hlasku.OverPaym
                  02.03.00 kl question FOR unmarking changed
                  06.03.00 kl inv-no into InvNum (applhelp knows it)
                  21.03.00 jp overpayment
                  10.05.00 kl customer Balance updating fixed
                  05.12.00 kl FIN added TO credit invoice Date limitation
                  19.12.00 pt set hlasku.InvSeq = 0 IF it was < 0
                  05.02.01 kl use TEMP-TABLE, no more FORMAT errors ...
                  12.02.01 kl updating cinv.CurrAmt was missing
                  02.11.01 kl send invoice number TO nnpcst
                  07.01.02/aam nnpcst must get the credited invoice nbr,
                               NOT the credit nbr 
                  25.01.02/aam credited invoice cannot be paid,
                               get the invoice nbr FOR Credit invoice just 
                               before creation,
                               SlsAccNum from original line (NOT from BillCode),
                               mark the credited AND the Credit invoice AS paid
                  17.04.02/aam ALL into same TRANSACTION (also nnpcst AND 
                               credpaym) -> ALL is undone when cancelled, 
                               write logs in the END after everything is done 
                  16.05.02/aam partial crediting 
                  17.05.02/tk  RUN Mc/memo
                  28.05.02/aam userlog removed 
                  03.06.02/aam katun added to Invoice.Memo
                  07.06.02/aam use Invoice.OverPaym for overpayment,
                               change the sign also for cinv.VATAmt
                  18.06.02/aam don't update InvNum with F1,
                               new button F7 (cancel)
                  26.07.02/tk  eventlog functions persistent
                  10.09.02/aam check period
                  26.09.02/aam customer balances in table CustBal and CustCount
                  01.10.02/aam use fInvoiceAmt()
                  17.10.02/aam mark InvType 5 for credit invoice,
                               do not change printstate to 9,
                               allow change of row amounts in partial crediting
                  04.11.02/jr  Eventlog
                  12.11.02/jr  "Invoice" => "invoice" in memo
                  13.11.02/jr  Replaced old memo with new one
                  24.01.03/aam allow crediting of partly paid invoices
                  12.03.03/aam set cinv.ddState as zero
                  26.03.03/aam check user's crediting limit 
                  27.03.03/aam if dd-invoice is credited before it has been
                               sent to bank -> cancel dd-data from orig.invoice
                  11.06.03/aam message if dd-invoice is credited too late
                  15.09.03/aam brand
                  13.10.03/aam invoice can be given in si-recid2,
                               don't change invoicetype for type 3 
                  04.12.03/aam default due date = invoice date            
                  29.12.03/aam WInvDisp = true for partially credited
                  06.02.04 jp  CustNum for memo
                  18.08.04/aam set cinv.PrintState as 0
                  04.02.05/aam nnpcst.i
                  10.03.05/aam update InvGroup.CrInvNum only if possible
                  20.09.05/aam invoice number with fGetInvNum()
                  16.12.05/aam copy invasub and invccn to credit invoice
                  04.01.06/aam partial credit and event release not allowed
                               for inv.types 3&4
                  16.06.06/aam ClaimState instead of ClaimQty
                  28.11.06/aam new logic for fGetInvNum()
                  30.11.06/aam new logic for InvType 
                  02.01.07/aam own invoice type for creditings of each type
                  12.01.07/aam invgroup from original invoice -> nbr sequence 
                  19.01.07/mvi "credit invoice" term now called "credit note"
                  06.03.07/aam fLocalNextExtID
                  29.08.07/aam logic through request handling (creditnote.p),
                               token check for event release,
                               reason codes 
  Version ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/faccper.i}
{Func/fbankday.i}
{Ar/nnpcst.i}
{Func/fcreditreq.i}
{Func/cparam2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Payment'}
{Func/fuserright.i}
{Func/fcreditvalid.i}
{Syst/tmsconst.i}

def var ok         as lo  format "Yes/No"     NO-UNDO.
def var State      as lo  format "Yes/No"     NO-UNDO.
DEF VAR liInvNum   LIKE   Invoice.InvNum      NO-UNDO FORMAT ">>>>>>>>>".
DEF VAR hInvDate   LIKE   Invoice.InvDate     NO-UNDO.
DEF VAR hDueDate   LIKE   Invoice.DueDate     NO-UNDO.

DEF VAR xCrAmt     AS DE  NO-UNDO. 
DEF VAR xPrev      AS i   NO-UNDO. 
DEF VAR xPartial   AS lo  NO-UNDO. 
DEF VAR llNewInv   AS LO  NO-UNDO INIT TRUE.
DEF VAR ldRowAmt   AS DE  NO-UNDO. 
DEF VAR llAllowRel AS LO  NO-UNDO. 
DEF VAR liCount    AS INT NO-UNDO. 
DEF VAR lii        AS INT NO-UNDO. 

DEF VAR ldCreditLimit   AS DEC  NO-UNDO.
DEF VAR liCalled        AS INT  NO-UNDO. 
DEF VAR lcCustName      AS CHAR NO-UNDO.
DEF VAR ldActStamp      AS DEC  NO-UNDO. 
DEF VAR lcError         AS CHAR NO-UNDO. 
DEF VAR lcCanRelease    AS CHAR NO-UNDO.
DEF VAR lcReasonGrp     AS CHAR NO-UNDO.
DEF VAR lcReasonGrpDesc AS CHAR NO-UNDO. 
DEF VAR lcReason        AS CHAR NO-UNDO.
DEF VAR lcReasonDesc    AS CHAR NO-UNDO.
DEF VAR liReturn        AS INT  NO-UNDO.
DEF VAR lcReturnDesc    AS CHAR NO-UNDO.
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR liDDCancel      AS INT  NO-UNDO.
DEF VAR ldActTime       AS DEC  NO-UNDO.
DEF VAR liActTime       AS INT  NO-UNDO.
DEF VAR lcExtInvID      AS CHAR NO-UNDO.
DEF VAR lcCheckError    AS CHAR NO-UNDO.
DEF VAR lcMode          AS CHAR NO-UNDO.
DEF VAR lcReasonNote    AS CHAR NO-UNDO.

form
  "Create a credit note from an existing invoice." AT 9 SKIP(1)
  "        Original Invoice ....:" lcExtInvID FORMAT "X(12)" NO-LABEL
                                   HELP "Debit invoice number"  
                                   "(" SPACE(0) 
                                   liInvNum NO-LABEL 
                                   SPACE(0) ")" SKIP
  "          - Date ............:" Invoice.InvDate    NO-LABEL   SKIP
  "          - Total Amount ....:" Invoice.InvAmt NO-LABEL   
                                   format "->>,>>>,>>>.99"     SKIP
  "        Customer ............:" Customer.CustNum   NO-LABEL   
                                   FORMAT ">>>>>>>9"
                                   lcCustName FORMAT "X(30)"  NO-LABEL   SKIP
  "        Invoicing Group......:" Customer.InvGroup  NO-LABEL 
                                   InvGroup.IGName FORMAT "X(30)" no-label  
                                   skip(1)
  "          - Date ............:" hInvDate  format "99-99-99"
                                   no-label help "Date of new Credit note" 
                                   SKIP
  "          - Due Date ........:" hDueDate format "99-99-99"
                                   no-label help "Due Date of new Credit note" 
                                   SKIP
  "          - Credit Amount ...:" xCrAmt NO-LABEL 
                                   format "->>,>>>,>>>.99"  SKIP
  "          - Reason Category .:" lcReasonGrp NO-LABEL
                                   lcReasonGrpDesc NO-LABEL FORMAT "X(30)" 
                                   SKIP
  "          - Reason Code......:" lcReason NO-LABEL
                                   lcReasonDesc NO-LABEL FORMAT "X(30)" SKIP
  "          - Reason Note......:" lcReasonNote NO-LABEL FORMAT "X(30)" SKIP
  "          - Return Of Payment:" liReturn NO-LABEL
                                   FORMAT "9"
                    HELP "0=no returns, 1=to advance payment, 2=refund"
                                  VALIDATE(INPUT liReturn <= 2,
                                           "Valid values are 0-2") SPACE(8)
                                   lcReturnDesc NO-LABEL FORMAT "X(35)"
                                   skip(1)
  "        Release Events ......:" State    NO-LABEL
                    help "Change the state of credited events into unbilled"
                                   skip
WITH
   OVERLAY width 80 ROW 1 side-labels COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + "   CREDIT NOTES        " + string(pvm,"99-99-99") + " "
   FRAME rajat.

form
   skip(1)
   "   This invoice has most likely been credited already"    SKIP
   "   with invoice number:" Invoice.CrInvNum ". If You are SURE" SKIP
   "   You want to create a new credit note answer YES,"   SKIP
   "   else answer NO."                                       skip(1)
   "   Continue creating credit note .:" ok
   skip(1)
WITH NO-LABEL width 60 OVERLAY centered ROW 5
   title " ARE YOU SURE ?" FRAME frm.


FUNCTION fCreditAmt RETURNS LOGICAL.

   IF can-find(FIRST wMarked) THEN DO:

      ASSIGN xCrAmt     = 0
             llAllowRel = TRUE.

      FOR EACH wMarked, 
      FIRST InvRow no-lock where
         recid(InvRow) = wMarked.Line:

         ldRowAmt = -1 * wMarked.Amt.

         /* IF VATAmt is excluded THEN it must be added TO line amounts */ 
         IF Invoice.VATIncl = FALSE THEN ASSIGN 
           ldRowAmt = ROUND(ldRowAmt * (1 + InvRow.VATPerc / 100),2). 

         xCrAmt = xCrAmt + ldRowAmt.

         /* if row amount has been changed then release of events is
            not allowed */
         IF wMarked.Amt NE InvRow.Amt THEN ASSIGN
            llAllowRel = FALSE.
      END. 

      FOR EACH wMarked WHERE 
               wMarked.Line < 0 AND
               wMarked.Amt NE 0:

         IF llAllowRel THEN       
         CASE wMarked.Line:
         /* Interest amount */
         WHEN -1 THEN DO:
                    IF wMarked.Amt NE Invoice.InterestAmt
                    THEN llAllowRel = FALSE. 
                 END.
         /* overpayment */
         WHEN -2 THEN DO:
                    IF wMarked.Amt NE Invoice.OverPaym
                    THEN llAllowRel = FALSE. 
                 END.
         /* advance payment */
         WHEN -3 THEN DO:
                    IF wMarked.Amt NE Invoice.AdvPaym
                    THEN llAllowRel = FALSE.
                 END.
         END CASE.

         xCrAmt = xCrAmt - wMarked.Amt.
      END.

   END.
   ELSE ASSIGN xCrAmt = -1 * Invoice.InvAmt.

   IF State AND llAllowRel = FALSE THEN DO:
      MESSAGE 
      "Crediting amounts are different than on the original invoice." SKIP
      "Calls / Contract fees will not be released for rebilling."
      VIEW-AS ALERT-BOX
      INFORMATION.

      State = FALSE.
   END.

   DISPLAY xCrAmt State WITH FRAME rajat.  

END FUNCTION.

FUNCTION fDispReasonDesc RETURNS LOGIC
   (icReason AS CHAR):

   IF icReason > "" THEN 
      lcReasonDesc = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "CreditNote",
                                      "Reason",
                                      icReason).
   ELSE lcReasonDesc = "".
   
   DISP lcReasonDesc WITH FRAME rajat.
   
END FUNCTION.

FUNCTION fDispReturnDesc RETURNS LOGIC
   (iiReturn AS INT):

   CASE iiReturn:
   WHEN 1 THEN lcReturnDesc = "Move to advance payment balance".
   WHEN 2 THEN lcReturnDesc = "Refund to customer".
   OTHERWISE   lcReturnDesc = "No returns".
   END CASE. 
   
   DISP lcReturnDesc WITH FRAME rajat.
   
END FUNCTION.

FUNCTION fDispReasonGrpDesc RETURNS LOGIC
   (icGrp AS CHAR):

   IF icGrp > "" THEN 
      lcReasonGrpDesc = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                         "CreditNote",
                                         "ReasonGrp",
                                         icGrp).
   ELSE lcReasonGrpDesc = "".
   
   DISP lcReasonGrpDesc WITH FRAME rajat.
    
END FUNCTION.

cfc = "sel".  RUN Syst/ufcolor.

/* crediting limit from user */
FIND FIRST TMSUser NO-LOCK WHERE
           TMSUser.UserCode = katun NO-ERROR.
IF AVAILABLE TMSUser THEN 
      ldCreditLimit =  fUserLimitAmt(TMSUser.UserCode,
                                     {&CREDIT_NOTE_LIMIT_TYPE}) .
 
/* release right */
IF SetTMSUser(katun) 
THEN lcCanRelease = getTMSRight("SYST").
ELSE lcCanRelease = "".
 
liCalled = 0. 
IF si-recid2 NE ? THEN DO:
   FIND Invoice NO-LOCK WHERE RECID(Invoice) = si-recid2 NO-ERROR.
   si-recid2 = ?.

   IF NOT AVAILABLE Invoice THEN DO:
      MESSAGE "Unknown invoice"
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.
   
   liCalled = Invoice.InvNum.
END.

ASSIGN 
   /* nbr of cancellation days for dd payments */
   liDDCancel = fCParamI("DDCancelDays")
   /* activation time for requests */
   ldActTime  = fCParamDE("CreditNoteActTime").
   
IF liDDCancel = ? THEN liDDCancel = 0.
IF ldActTime = ?  THEN ldActTime  = 19.

liActTime = TRUNCATE(ldActTime,0) * 3600 +
            (ldActTime - TRUNCATE(ldActTime,0)) * 6000.

rajat:
repeat WITH FRAME rajat:


   IF llNewInv THEN DO:
      ASSIGN State       = FALSE
             llAllowRel  = TRUE    /* allow release of events */
             xPrev       = liInvNum
             liReturn    = 0
             lcReason    = ""
             lcExtInvID  = "".

      IF liCalled = 0 THEN DO:
         ehto = 9. RUN Syst/ufkey.

         UPDATE lcExtInvID validate(lcExtInvID = "" OR 
                          can-find(FIRST Invoice where 
                                         Invoice.Brand  = gcBrand AND
                                         Invoice.ExtInvID = input lcExtInvID),
                          "Unknown invoice !").

         IF lcExtInvID = "" THEN LEAVE rajat.

         FIND Invoice WHERE
              Invoice.Brand    = gcBrand AND
              Invoice.ExtInvID = lcExtInvID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Invoice THEN NEXT.
         
         liInvNum = Invoice.InvNum.
               
      END.

      ELSE DO:
         ASSIGN liInvNum = liCalled.
      END. 

      DISPLAY liInvNum WITH FRAME rajat.
      IF liInvNum = 0 THEN LEAVE rajat.

      /* NEW invoice selected */
      IF xPrev NE liInvNum THEN 
         EMPTY TEMP-TABLE wMarked. 
   END.

   FIND Invoice where Invoice.InvNum = liInvNum no-lock.
   IF Invoice.Brand NE gcBrand THEN DO:
      MESSAGE "Invoice belongs to brand" Invoice.Brand
      VIEW-AS ALERT-BOX
      ERROR.
      IF liCalled > 0 THEN LEAVE rajat.
      NEXT.
   END.

   lcExtInvID = Invoice.ExtInvID.
   DISPLAY lcExtInvID WITH FRAME rajat.
   
   IF llNewInv THEN DO:
   
      /* check request */
      lcCheckError = fCheckCreditNoteRequest(Invoice.CustNum,
                                        Invoice.InvNum).
      IF lcCheckError > "" THEN DO:
          MESSAGE lcCheckError  VIEW-AS ALERT-BOX INFORMATION.
         IF liCalled > 0 THEN LEAVE rajat.
         NEXT.
      END.

      /* check invoice */
      lcCheckError = fCheckInvoice(BUFFER Invoice, "", "", OUTPUT lcMode).
      IF lcCheckError > "" THEN DO:
          MESSAGE lcCheckError  VIEW-AS ALERT-BOX INFORMATION.
         IF liCalled > 0 THEN LEAVE rajat.
         NEXT.
      END.

      /* check crediting limit */
      IF Invoice.InvAmt > ldCreditLimit THEN DO:
         MESSAGE "Invoice amount is" Invoice.InvAmt
                 ", but crediting limit has been set to" ldCreditLimit
                 "."  SKIP
                 "Function not allowed."
         VIEW-AS ALERT-BOX INFORMATION.

         IF liCalled > 0 THEN LEAVE rajat.
         NEXT rajat.
      END.

      DISP Invoice.InvDate Invoice.InvAmt.

      ASSIGN
      hInvDate = TODAY + INTEGER(liActTime < TIME + 60) 
      hDueDate = hInvDate.
   END.

   FIND Customer of Invoice no-lock.
   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                  BUFFER Customer).
   DISP Customer.CustNum lcCustName Customer.InvGroup.

   IF Invoice.InvType >= 3 AND Invoice.InvType <= 4 THEN llAllowRel = FALSE.
   
   DO FOR InvGroup:
      FIND FIRST InvGroup WHERE
                 InvGroup.Brand    = gcBrand AND
                 InvGroup.InvGroup = Customer.InvGroup 
      no-lock no-error.
      DISPLAY InvGroup.IGName.  
   END.

   /* DISPLAY credited amount */
   fCreditAmt(). 

   fDispReturnDesc(liReturn).
   
   DISPLAY 
      hInvDate
      hDueDate
      WITH FRAME rajat.
      
   UPDATE
      lcReasonGrp
      lcReason
      lcReasonNote
      State    WHEN llAllowRel AND lcCanRelease = "RW" AND 
                    Invoice.InvType <= 1
   EDITING:
      READKEY.

      /* UNDO AND LEAVE WITH F4 cause it says so in the menu */
      if keylabel(lastkey) = "F4" THEN UNDO rajat, LEAVE rajat.

      ELSE IF KEYLABEL(LASTKEY) = "F9" AND 
         LOOKUP(FRAME-FIELD,"lcReason,lcReasonGrp") > 0
      THEN DO:

         IF FRAME-FIELD = "lcReasonGrp" THEN DO:           
            RUN Help/h-tmscodes("CreditNote", /* TableName */
                           "ReasonGrp",     /* FieldName */
                           "AR",
                           OUTPUT lcCode).

            IF lcCode ne "" AND lcCode NE ? THEN DO:
               DISPLAY lcCode @ lcReasonGrp WITH FRAME rajat.   
               fDispReasonGrpDesc(lcCode).
            END.   
         END.

         ELSE IF FRAME-FIELD = "lcReason" THEN DO:           
            RUN Help/h-tmscodes("CreditNote", /* TableName */
                           "Reason",     /* FieldName */
                           INPUT INPUT FRAME rajat lcReasonGrp,
                           OUTPUT lcCode).

            IF lcCode ne "" AND lcCode NE ? THEN DO:
               DISPLAY lcCode @ lcReason WITH FRAME rajat.   
               fDispReasonDesc(lcCode).
            END.   
         END.
                   
         ehto = 9.
         RUN Syst/ufkey.
         NEXT. 
      END.

      ELSE IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:

         IF FRAME-FIELD = "hInvDate" THEN DO:

            IF INPUT hInvDate < TODAY THEN DO:
               MESSAGE 
                  "You can not use a credit note date prior to today"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
         END.

         ELSE IF FRAME-FIELD = "lcReasonGrp" THEN DO:
            fDispReasonGrpDesc(INPUT INPUT lcReasonGrp).
           
            IF lcReasonGrpDesc = "" THEN DO:
               MESSAGE "Unknown reason category"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            FIND TMSCodes WHERE
                 TMSCodes.TableName = "CreditNote" AND
                 TMSCodes.FieldName = "ReasonGrp"  AND
                 TMSCodes.CodeValue = INPUT lcReasonGrp NO-LOCK NO-ERROR.
            IF AVAILABLE TMSCodes AND 
               LOOKUP(STRING(Invoice.InvType),TMSCodes.ConfigValue) = 0
            THEN DO:
               MESSAGE "Category is not allowed for this invoice type"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
                
         END.
         
         ELSE IF FRAME-FIELD = "lcReason" THEN DO:
            fDispReasonDesc(INPUT INPUT lcReason).
           
            IF lcReasonDesc = "" THEN DO:
               MESSAGE "Unknown reason code"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            FIND TMSCodes WHERE
                 TMSCodes.TableName = "CreditNote" AND
                 TMSCodes.FieldName = "Reason"     AND
                 TMSCodes.CodeValue = INPUT lcReason NO-LOCK NO-ERROR.
            IF AVAILABLE TMSCodes AND 
               TMSCodes.CodeGroup NE INPUT lcReasonGrp THEN DO:
               MESSAGE "Reason belongs to another category"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
            
         END.
        
      END.

      APPLY LASTKEY.
   END.

   hDueDate = hInvDate.
   DISP hDueDate WITH FRAME rajat.
   
   assign llNewInv = true. 

   toimi:
   repeat WITH FRAME rajat:
      ASSIGN ufk = 0 ehto = 0 
      ufk[1] = 91  ufk[2] = 927  ufk[3] = 0 ufk[4] = 1807
      ufk[5] = 908 ufk[7] = 1721 ufk[8] = 8.
      
      /* no partial crediting for special invoices */
      IF Invoice.InvType >= 3 AND Invoice.InvType <= 4 THEN ufk[4] = 0.
      
      /* yoigo special */
      ufk[4] = 0.
      
      RUN Syst/ufkey.

      IF toimi = 8 THEN LEAVE rajat.

      ELSE IF toimi = 1 THEN DO:
         llNewInv = false.
         NEXT  rajat.
      END.

      ELSE IF toimi = 2 THEN DO TRANS: /* memo */
         FIND Invoice WHERE Invoice.InvNum = liInvNum
         NO-LOCK NO-ERROR.
         RUN Mc/memo(INPUT Invoice.CustNum,
                  INPUT "invoice",
                  INPUT STRING(Invoice.InvNum),
                  INPUT "Invoice number").
         NEXT.
      END.

      /* cancel */
      else if toimi = 7 then do:
         liInvNum = 0.
         clear frame rajat no-pause.
         IF liCalled > 0 THEN LEAVE rajat.
         next rajat. 
      end. 

      /* partial crediting; mark invoice lines TO be credited */
      ELSE IF toimi = 4 THEN DO:
         RUN Ar/partcred(liInvNum,
                      input-output table wMarked).
         fCreditAmt(). 
      END.

      ELSE IF toimi = 5 THEN DO:

         /* credited amount can't be more than invoice amount */
         IF abs(xCrAmt) GT abs(Invoice.InvAmt) THEN DO:
            message "Invoice amount is" Invoice.InvAmt ". Credited amount"
                    "should not be more than that. Crediting is cancelled."
            VIEW-AS ALERT-BOX
            error.
            NEXT toimi.
         END.

         /* check period */
         IF fPeriodLocked(hInvDate,TRUE) THEN NEXT toimi. 

         IF liReturn = 2 AND LENGTH(Customer.BankAcc) < 24 THEN DO:
            MESSAGE "Customer hasn't got a valid bank account for a refund."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
       
         /* check that cdrs exist if they are to be released */
         IF State AND 
            CAN-FIND(FIRST InvRow OF Invoice WHERE InvRow.RowType = 2)
         THEN DO:
            FOR EACH SubInvoice OF Invoice NO-LOCK:

               IF NOT CAN-FIND(FIRST InvRow WHERE
                      InvRow.Invnum = SubInvoice.Invnum AND
                      InvRow.SubInvNum = SubInvoice.SubInvNum AND
                      InvRow.RowType = 2) THEN NEXT.

               IF NOT CAN-FIND(FIRST MobCDR WHERE 
                                     MobCDR.InvCust = Invoice.CustNum AND
                                     MobCDR.InvSeq  = SubInvoice.InvSeq)
               THEN DO:
                  MESSAGE "Events of this invoice are not found. Release is"
                          "therefore not possible."
                  VIEW-AS ALERT-BOX INFORMATION.
                  NEXT toimi. 
               END.   
            END.
         END.
         
         ok = FALSE.
         message "Create a request for credit note?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         SET OK.

         IF ok THEN LEAVE toimi.

      END.
   END. /* toimi */

   /* Ask FOR unmarking the Billed rows */
   IF NOT State AND llAllowRel AND 
      lcCanRelease = "RW" AND Invoice.InvType <= 1
   THEN DO:
      MESSAGE 
         "Do You want to keep the Calls and CONTRACT FEES marked as Billed ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.

      IF NOT ok THEN State = TRUE.
   END.

   /* move to next day if activation time has passed */
   IF liActTime < TIME AND hInvDate = TODAY THEN hInvDate = hInvDate + 1.
   
   /* requests will be run in the evening of given date */
   ldActStamp = fMake2DT(hInvDate,
                         liActTime).

   /* make a request */
   lii = fFullCreditNoteRequest(Invoice.Custnum,
                                Invoice.InvNum,
                                "",
                                "",
                                lcReason,     /* Crediting Reason Code */
                                lcReasonNote, /* Crediting Reason Note */
                                liReturn,
                                State,
                                ldActStamp,
                                FALSE,
                                "",
                                lcMode,
                                OUTPUT lcError).

   IF lii > 0 THEN DO:
      MESSAGE "Request ID for credit note is" lii SKIP
              "It will be activated on" 
              fTS2Hms(ldActStamp)
      VIEW-AS ALERT-BOX 
      TITLE " REQUEST CREATED ".
   END.
   
   ELSE
      MESSAGE "Creation of request failed:" SKIP
              lcError
      VIEW-AS ALERT-BOX ERROR.
                          

   CLEAR FRAME rajat no-pause.
   liInvNum = 0.

   IF liCalled > 0 THEN LEAVE rajat.

END. /* rajat */

HIDE FRAME rajat no-pause.
HIDE MESSAGE.


