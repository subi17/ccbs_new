/* ----------------------------------------------------------------------------
  MODULE .......: NNDEPO.P
  FUNCTION .....: Save Deposits            
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED  .....: 07.05.99 pt
  MODIFIED .....: 13.10.99 kl invoice number can be used
                  24.01.00 jp Payment.AccType
                  25.01.00 jp TMSParams & overpayments
                  21.11.01/aam smaller scope FOR TMSParam -> LOCK is released
                               after the payment is saved 
                  15.01.02/aam longer FORMAT FOR cust nbr
                  19.02.02/aam mark "AD" AS PaymSrc 
                  07.03.02 lp  added AdvPaym 
                  10.04.02/ht  ImportStamp added
                  03.05.02/aam use PaymVouch (fvoucher.i)
                  30.05.02/tk  event logging
                  03.06.02/aam booking type "bank" added,
                               CHOOSE booking type FOR both debet AND credit,
                               OPLog also from deposits 
                  07.06.02/aam VAT-handling FOR AdvPaym
                  24.09.02/jr  invno => invnum so can use F9
                  25.09.02/aam customer balances in table CustBal
                  13.11.02/jr  Changed memo and added Eventlog for it
                  29.11.02 lp  added FORMAT for paName and daName
                  11.04.03/aam added interest, credit loss and 
                               claiming cost handling
                  15.09.03/aam brand
                  07.01.04/aam VatUsage
                  06.02.04 jp  custnum for memo
                  29.03.04/aam refund
                  26.01.05/aam eventlog of OpLog moved inside the function
                  21.08.06/aam check acc.period lock,
                               credit loss removed
                  12.01.07/aam Cli to fCustBal
                  09.03.07/aam ExtVoucher
                  01.10.07/aam request for payment
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{fcustbal.i}
{cparam2.i}
{faccper.i}
{fpaymentreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

   DEFINE VARIABLE lhOPLog AS HANDLE NO-UNDO.
   lhOPLog = BUFFER OPLog:HANDLE.
   RUN StarEventInitialize(lhOPLog).

END.


DEF BUFFER bsuor FOR Payment.

DEF VAR pdate     AS DA NO-UNDO FORMAT "99.99.99".
DEF VAR CustNum   AS I  NO-UNDO FORMAT "zzzzzz9".
DEF VAR amtPaid   AS DE NO-UNDO FORMAT "->,>>>,>>9.99".
DEF VAR dAcct     LIKE Account.AccNum NO-UNDO.
DEF VAR cAcct     LIKE Account.AccNum NO-UNDO.
DEF VAR paName    LIKE Account.AccName NO-UNDO.
DEF VAR daName    LIKE Account.AccName NO-UNDO.
DEF VAR dType     AS C NO-UNDO.
DEF VAR cType     AS C NO-UNDO. 
DEF VAR i         AS I NO-UNDO.
DEF VAR invnum    LIKE Invoice.InvNum NO-UNDO.
DEF VAR cAcType   LIKE Account.AccType NO-UNDO.
DEF VAR dAcType   LIKE Account.AccType NO-UNDO.
DEF VAR ConseNo   AS INT  NO-UNDO.
DEF VAR liAccInd  AS INT  NO-UNDO. 

DEF VAR ResDep    AS INT  NO-UNDO.
DEF VAR OvePay    AS INT  NO-UNDO.
DEF VAR AdvPay    AS INT  NO-UNDO.
DEF VAR BankAcc   AS INT  NO-UNDO. 
DEF VAR IntAcc    AS INT  NO-UNDO.
DEF VAR CollAcc   AS INT  NO-UNDO.
DEF VAR CLossAcc  AS INT  NO-UNDO. 
DEF VAR liVatAcc  AS INT  NO-UNDO. 

DEF VAR liOpType  AS INT  NO-UNDO. 
DEF VAR ldeOpAmt  AS DEC  NO-UNDO.
DEF VAR llAccType AS LOG  NO-UNDO. 
DEF VAR lcTypes   AS CHAR NO-UNDO.
DEF VAR lcDispTyp AS CHAR NO-UNDO EXTENT 2.
DEF VAR ldeApVat  AS DEC  NO-UNDO.     
DEF VAR llRefund  AS LOG  NO-UNDO. 
DEF VAR lcCLI     AS CHAR NO-UNDO. 
DEF VAR ldCustDP  AS DEC  NO-UNDO.
DEF VAR ldCustAP  AS DEC  NO-UNDO.
DEF VAR ldCustOP  AS DEC  NO-UNDO. 
DEF VAR lcPref    AS CHAR NO-UNDO.

DEF VAR lcResult   AS CHAR NO-UNDO.
DEF VAR liPaymType AS INT  NO-UNDO.
DEF VAR lcPosting  AS CHAR NO-UNDO.
DEF VAR lcAccount  AS CHAR NO-UNDO.

ASSIGN lcDispTyp[1] = "(D)eposit,(O)verPayment,(A)dvance Payment,(B)ank" 
       lcDispTyp[2] = "(I)nterest,(C)laiming Cost"
       lcTypes      = lcDispTyp[1] + "," + lcDispTyp[2].          

FORM
   SKIP(2)
"   Customer no. ...........:" CustNum FORMAT ">>>>>>>9"
        Customer.CustName                                 SKIP
"   - Recent Deposit .......:" ldCustDP FORMAT "->>>>>>>9.99" SKIP
"   - Recent Over Payment ..:" ldCustOP FORMAT "->>>>>>>9.99" SKIP
"   - Recent Advance Payment:" ldCustAP FORMAT "->>>>>>>9.99" SKIP(1)
"   Invoice number .........:" invnum
    HELP "Book payment on this invoice, can be left empty"  SKIP
"   Date of Payment ........:" pdate             
    HELP "Booking day (Accounting day)"    SKIP
"   Amount Paid ............:" amtPaid                   
    HELP "Amount of payment"             SKIP
"   Debet Account ..........:" 
    dType 
      HELP "D,O,A,B,I,C"
      validate(INDEX("DOABIC",dType) GT 0,
               "Valid choices are D,O,A,B,I and C")
    dAcct
       HELP "Account no. - in bookkeeping - WHERE money was received" 
    paName  FORMAT "x(25)"                        
    SKIP
"   Credit Account .........:" 
    cType 
      HELP "D,O,A,B,I,C"
      validate(INDEX("DOABIC",cType) GT 0,
               "Valid choices are D,O,A,B,I and C")
    cAcct
       HELP "No. of a Bookkeeping (debt) Account"
    daName  FORMAT "x(25)"                                                
    SKIP
"   Refund .................:"
    llRefund 
       HELP "Is this a refund payment"
       FORMAT "Yes/No"
    SKIP(1)
"   Types: " 
    lcDispTyp[1] AT 13 FORMAT "X(60)" SKIP
    lcDispTyp[2] AT 13 FORMAT "X(60)" 
    SKIP(1)

WITH
   OVERLAY WIDTH 80 TITLE " " + ynimi + " ADV.PAYMENTS & DEPOSITS " + 
   STRING(pvm,"99-99-99") + " " CENTERED NO-LABELS FRAME MAIN.

FUNCTION fChooseType RETURNS INTEGER
   (iType AS CHAR). 

    IF INDEX("DOABIC",iType) = 0 THEN RETURN 0. 

    CASE iType:
    WHEN "D" THEN RETURN ResDep.
    WHEN "O" THEN RETURN OvePay.
    WHEN "A" THEN RETURN AdvPay.
    WHEN "B" THEN RETURN BankAcc.
    WHEN "I" THEN RETURN IntAcc.
    WHEN "C" THEN RETURN CollAcc.
    END CASE.

END FUNCTION.

FUNCTION fCheckAcc RETURNS LOGICAL
    (iType AS CHAR,
     iAcc  AS INT).

    IF iAcc = 0 THEN RETURN TRUE. 

    FIND Account WHERE 
         Account.Brand  = gcBrand AND
         Account.AccNum = iAcc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Account THEN DO:
        MESSAGE "Unknown Account" iAcc
        VIEW-AS ALERT-BOX.
        RETURN FALSE.
    END.

    ASSIGN llAccType = TRUE. 

    CASE iType:
    WHEN "D" THEN IF Account.AccType NE 7  THEN llAccType = FALSE.
    WHEN "O" THEN IF Account.AccType NE 6  THEN llAccType = FALSE.
    WHEN "A" THEN IF Account.AccType NE 19 THEN llAccType = FALSE.
    WHEN "B" THEN IF Account.AccType NE 4  THEN llAccType = FALSE.
    WHEN "I" THEN IF Account.AccType NE 14 THEN llAccType = FALSE.
    WHEN "C" THEN IF Account.AccType NE 11 THEN llAccType = FALSE.
    END CASE.

    IF llAccType = FALSE THEN DO:
        MESSAGE "Type" ENTRY(INDEX("DOABIC",iType),lcTypes) 
                "was chosen, but Account" iAcc 
                "is not that type of an Account"
        VIEW-AS ALERT-BOX.
        RETURN FALSE. 
    END.     
    ELSE RETURN TRUE. 

END FUNCTION.

FUNCTION fCreateOPLog RETURNS LOGICAL
    (iType AS INT,
     iAmt  AS DEC).

   /* creating OPLog */
   IF iAmt NE 0 THEN DO:
      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = Customer.CustNum
         OPLog.EventDate = Payment.PaymDate
         OPLog.UserCode  = katun
         OPLog.EventType = iType 
         OPLog.InvNum    = Payment.InvNum
         OPLog.Voucher   = Payment.Voucher
         OPLog.Amt       = iAmt. 

      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOPLog).
   END.       
   
END FUNCTION.

ASSIGN ResDep   = fCParamI("ResDepositsAcc")
       OvePay   = fCParamI("OverPayAcc")
       AdvPay   = fCParamI("AdvPaymAcc")
       BankAcc  = fCParamI("BankAcc")
       IntAcc   = fCParamI("OTIntAcc")
       CollAcc  = fCParamI("ClaimCostAcc")
       CLossAcc = fCParamI("CreditLossAcc").

PAUSE 0.
MAIN:
REPEAT TRANS WITH FRAME MAIN:

   IF toimi NE 1 THEN DO:
      CLEAR FRAME MAIN NO-PAUSE.
      ASSIGN 
      CustNum  = 0 
      amtPaid  = 0 
      dAcct    = 0 
      cAcct    = 0 
      pDate    = TODAY
      dType    = "B"
      cType    = ""
      lcCLI    = ""
      llRefund = FALSE.
   END.

   PAUSE 0.
   DISPLAY lcDispTyp WITH FRAME Main.

   ehto = 9. RUN ufkey.
   UPDATE
      CustNum 
      invnum 
      pdate 
      amtPaid 
      dType 
      dAcct 
      cType
      cAcct 
      llRefund
   WITH FRAME MAIN EDITING:
      READKEY.
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME MAIN:
         HIDE MESSAGE.

         IF FRAME-FIELD = "CustNum" THEN DO:
            IF INPUT CustNum = 0 THEN UNDO MAIN, LEAVE MAIN.
            FIND FIRST Customer WHERE 
                       Customer.Brand   = gcBrand AND
                       Customer.CustNum = INPUT CustNum 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL Customer THEN DO:
               BELL.
               MESSAGE "Unknown Customer !".
               NEXT.
            END.  
            DISP Customer.CustName.
         END.

         ELSE IF FRAME-field = "invnum" THEN DO:
            IF INPUT invnum NE 0 THEN DO:
               FIND FIRST Invoice WHERE 
                          Invoice.Brand  = gcBrand AND
                          Invoice.InvNum = INPUT invnum
               NO-LOCK NO-ERROR.
               IF NOT AVAIL Invoice THEN DO:
                  BELL.
                  MESSAGE "Invoice does not exist !".
                  NEXT.
               END.  
               ELSE IF Invoice.CustNum NE Customer.CustNum  THEN DO:
                  BELL.
                  MESSAGE "Invoice is for customer " + 
                          STRING(Invoice.CustNum) + "!".
                  NEXT-PROMPT invnum.
                  NEXT.
               END.
            END.
            
            lcCLI = "Total".
 
            /* Does this customer have some unused balance */
            ASSIGN ldCustDP = fGetCustBal(Customer.CustNum,lcCLI,"DP")
                   ldCustOP = fGetCustBal(Customer.CustNum,lcCLI,"OP")
                   ldCustAP = fGetCustBal(Customer.CustNum,lcCLI,"AP").

            DISP ldCustDP ldCustOP ldCustAP. 
         END.

         ELSE IF FRAME-FIELD = "dType" THEN DO:
            DISPLAY fChooseType(INPUT INPUT dType) @ dAcct.
            /* suggest a refund */
            IF INPUT dType = "B" AND INPUT AmtPaid < 0 
            THEN DISPLAY TRUE @ llRefund.
         END. 

         ELSE IF FRAME-FIELD = "cType" THEN DO:
            DISPLAY fChooseType(INPUT INPUT cType) ;& cAcct. 
            /* suggest a refund */
            IF INPUT cType = "B" AND INPUT AmtPaid > 0 
            THEN DISPLAY TRUE @ llRefund.
         END. 

         ELSE IF FRAME-field = "amtPaid" THEN DO:
            IF INPUT Amtpaid = 0 THEN DO:
               NEXT-PROMPT CustNum.
               NEXT.
            END.
         END.      

         ELSE IF FRAME-field = "cAcct" THEN DO:
            IF fCheckAcc(INPUT INPUT cType,
                         INPUT INPUT cAcct) = FALSE
            THEN DO:
                NEXT-PROMPT cAcct.
                NEXT. 
            END. 

            IF INPUT cAcct = 0 THEN DO:
                DISPLAY "" ;& daName.
            END.
            ELSE IF AVAILABLE Account THEN DO:
               DISP Account.AccName @ daName.
               cAcType = Account.AccType.
            END. 
         END.   

         ELSE IF FRAME-field = "dAcct" THEN DO:
            IF fCheckAcc(INPUT INPUT dType,
                         INPUT INPUT dAcct) = FALSE
            THEN DO:
                NEXT-PROMPT dAcct.
                NEXT. 
            END. 

            IF INPUT dAcct = 0 THEN DO:
                DISPLAY "" ;& daName.
            END.
            ELSE IF AVAILABLE Account THEN DO:
               DISP Account.AccName @ paName.
               dAcType = Account.AccType.
            END.
         END.   

      END.   
      APPLY LASTKEY.
   END.   
TASK:
   REPEAT WITH FRAME MAIN:
      ASSIGN
      ehto = 1 ufk = 0 
      ufk[1] = 7 ufk[2] = 0 /* 992 */ ufk[4] = 806 
      ufk[5] = 15 ufk[6] = 12 ufk[7] = 185       ufk[8] = 8.
      IF amtPaid = 0 OR dAcct = 0 OR cAcct = 0
      THEN ASSIGN ufk[4] = 0 ufk[5] = 0 ufk[7] = 0. 
      RUN ufkey.       
      IF toimi = 1 THEN NEXT MAIN.

      IF toimi = 5 THEN DO:
         IF dType = cType THEN DO:
            MESSAGE "Same type can not be used for both debet and credit"
                    "booking."
            VIEW-AS ALERT-BOX.
            NEXT.
         END.
         IF cAcct = 0 OR dAcct = 0 THEN DO:
            MESSAGE "Both debet and credit Account must be defined."
            VIEW-AS ALERT-BOX.
            NEXT. 
         END. 

         IF fPeriodLocked(pDate,TRUE) THEN NEXT.
         
         LEAVE TASK.
      END.

      IF toimi = 6 THEN UNDO MAIN, NEXT MAIN.
      IF toimi = 7 THEN RUN commontt(CustNum). /*nnastt(CustNum).*/
      IF toimi = 8 THEN LEAVE MAIN.

      IF toimi = 4 THEN DO:
         RUN memo(INPUT Customer.Custnum,
                  INPUT "Customer",
                  INPUT STRING(Customer.CustNum),
                  INPUT "Customer number").
         NEXT.

      END.
   END. 

   /* refund */
   IF llRefund 
   THEN liPaymType = 6.
   ELSE liPaymType = 0.

   ASSIGN 
      lcPosting = ""
      lcAccount = STRING(dAcct) + ":" + STRING(cAcct).
      
   i = fPaymentWithPostingsRequest(Customer.CustNum,
                                   liPaymType,
                                   "AD",
                                   pdate,
                                   Invnum,
                                   amtPaid,
                                   lcPosting,
                                   lcAccount,
                                   "",                    /* memo */
                                   ?,                     /* handling time */
                                   0,                     /* control */
                                   0,                     /* interest */
                                   "",                    /* creator */
                                   OUTPUT lcResult).
                                  
   IF i > 0 THEN 
      MESSAGE "Request ID for payment is" i
      VIEW-AS ALERT-BOX 
      TITLE " Request Created ".
   ELSE 
      MESSAGE "Request creation failed:" SKIP
              lcResult
      VIEW-AS ALERT-BOX ERROR.
 
END.

HIDE FRAME MAIN NO-PAUSE.
HIDE MESSAGE.

