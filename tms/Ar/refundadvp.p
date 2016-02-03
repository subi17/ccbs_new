/* ----------------------------------------------------------------------------
  MODULI .......: REFUNDADVP.P
  TEHTAVA ......: Refund customer's advance payment balance
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 05.11.07
  MUUTOSPVM ....: 
  VERSIO .......: 
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fcustbal.i}
{Func/fbankdata.i}
{Func/fmanualpaymreq.i}
{Func/fuserright.i}

DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
DEF INPUT PARAMETER icCLI     AS CHAR NO-UNDO.

DEF VAR llOk        AS LOG  NO-UNDO. 
DEF VAR ldAmt       AS DEC  NO-UNDO.
DEF VAR ldAPAmt     AS DEC  NO-UNDO. 
DEF VAR ldtDate     AS DATE NO-UNDO. 
DEF VAR liTime      AS INT  NO-UNDO.
DEF VAR lcMemo      AS CHAR NO-UNDO. 
DEF VAR lcBal       AS CHAR NO-UNDO. 
DEF VAR llUfkey     AS LOG  NO-UNDO. 
DEF VAR liRequest   AS INT  NO-UNDO.
DEF VAR ldActStamp  AS DEC  NO-UNDO.
DEF VAR lcResult    AS CHAR NO-UNDO.
DEF VAR lcBankAcc   AS CHAR NO-UNDO.
DEF VAR lcCustName  AS CHAR NO-UNDO.

FORM
   Customer.CustNum COLON 14
      LABEL "Customer"
      lcCustName NO-LABEL FORMAT "X(40)"
      SKIP
   icCLI COLON 14
      LABEL "MSISDN"
      FORMAT "X(15)"
      SKIP
   lcBankAcc COLON 14
      LABEL "Bank Account"
      FORMAT "X(30)" 
      SKIP(1)
   ldAPAmt COLON 25
      LABEL "Advance Payment Balance" 
      FORMAT "->,>>>,>>9.99" 
      SKIP(1)
   ldAmt COLON 25
      LABEL "Refund Amount" 
      HELP "Amount to be refunded"
      FORMAT ">,>>>,>>9.99"
      SKIP(1)
   lcMemo COLON 6 
      LABEL "Memo"
      HELP "Memo to be written on payment"
      VIEW-AS EDITOR
      SIZE 50 BY 3
   WITH TITLE " REFUND CUSTOMER'S ADVANCE PAYMENT BALANCE " SIDE-LABELS
   ROW 3 CENTERED OVERLAY FRAME fCrit.

FIND Customer WHERE 
     Customer.Brand   = gcBrand AND
     Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Unknown customer" iiCustNum
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.

IF fTokenRights(katun,"SYST") = "" THEN DO:
   MESSAGE "Function not allowed"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.


/* get current balance */
ldAPAmt = fGetCustBal(Customer.CustNum,icCLI,"AP").

IF ldAPAmt <= 0 THEN DO:
   MESSAGE "Function not allowed"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

lcBankAcc = Customer.BankAcc.
IF LENGTH(lcBankAcc) > 10 THEN 
   lcBankAcc = SUBSTRING(lcBankAcc,5,4) + " " + 
               SUBSTRING(lcBankAcc,9,4) + " " +
               SUBSTRING(lcBankAcc,13,2) + " " +
               SUBSTRING(lcBankAcc,15). 

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
                              
PAUSE 0.
DISP Customer.CustNum
     lcCustName 
     icCLI
     lcBankAcc
     ldAPAmt
WITH FRAME fCrit. 

llUfkey = FALSE.
       
toimi:
repeat WITH FRAME fCrit ON ENDKEY UNDO toimi, NEXT toimi:

   PAUSE 0.
   DISPLAY ldAmt
           lcMemo
           WITH FRAME fCrit.
 
   IF llUfkey THEN DO:
      ASSIGN
      ufk    = 0
      ufk[1] = 7  
      ufk[5] = 1734 
      ufk[8] = 8
      ehto   = 0.
      RUN ufkey.
   END.
   ELSE ASSIGN 
      llUfkey = TRUE
      toimi   = 1.

   IF toimi = 1 THEN DO:
   
      ASSIGN ehto = 9.
      RUN ufkey.

      REPEAT ON ENDKEY UNDO, LEAVE:
      
          UPDATE 
          ldAmt
          lcMemo
          WITH FRAME fCrit EDITING:
          
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME fCrit:
             
                PAUSE 0.
             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.   
   
   ELSE IF toimi = 5 THEN DO:
      
      IF ldAmt = 0 THEN DO:
         MESSAGE "Amount has not been given"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      IF ldAmt > ldApAmt THEN DO:
         MESSAGE "Refunded amount cannot be more than current balance"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      /* customer must have a (valid) bank account */
      IF LENGTH(Customer.BankAcc) < 24 OR 
         NOT fCheckBankAcc(Customer.BankAcc) THEN DO:
         MESSAGE "Customer hasn't got a valid bank account"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      ehto = 5. 
      RUN ufkey.
 
      /* 15 mins from now */
      ASSIGN 
         ldtDate = TODAY
         liTime  = TIME + (15 * 60).
         
      IF liTime > 86399 THEN ASSIGN
         ldtDate = ldtDate + 1
         liTime  = liTime - 86400.
         
      ldActStamp = fMake2Dt(ldtDate,liTime).
      
      liRequest = 
         fManualPaymentRequest(Customer.CustNum,
                               icCLI,              /* cli */
                               0,                  /* invoice */
                               1,                  /* type */
                               ldAmt,              /* amount */
                               TODAY,              /* paym. date */
                               lcMemo,             /* memo */
                               ldActStamp,         /* handling time */
                               "",                 /* creator */
                               OUTPUT lcResult).

      IF liRequest = 0 THEN 
         MESSAGE "Payment request could not be created;" SKIP
                 lcResult
         VIEW-AS ALERT-BOX ERROR.

      ELSE 
         MESSAGE "Request ID for manual payment is" liRequest SKIP
                 "It will be activated on" 
                 fTS2Hms(ldActStamp)
         VIEW-AS ALERT-BOX 
         TITLE " REQUEST CREATED ".
           
      LEAVE toimi.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE toimi.
   END.
      
END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.

 
