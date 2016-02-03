/* ----------------------------------------------------------------------------
  MODULI .......: REFUBAL.P
  TEHTAVA ......: Refund customer's balances
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 30.03.04
  MUUTOSPVM ....: 04.09.07/aam changes to refupaym parameters
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fcustbal.i}

DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.

DEF VAR llOk        AS LOG  NO-UNDO. 
DEF VAR liAccNum    AS INT  NO-UNDO.
DEF VAR ldAmt       AS DEC  NO-UNDO.
DEF VAR ldOPAmt     AS DEC  NO-UNDO.
DEF VAR ldDPAmt     AS DEC  NO-UNDO.
DEF VAR ldAPAmt     AS DEC  NO-UNDO. 
DEF VAR ldtDate     AS DATE NO-UNDO. 
DEF VAR lcMemo      AS CHAR NO-UNDO. 
DEF VAR lcBal       AS CHAR NO-UNDO. 
DEF VAR llUfkey     AS LOG  NO-UNDO. 
DEF VAR liVoucher   AS INT  NO-UNDO.


FORM
   Customer.CustNum AT 2 NO-LABEL 
      Customer.CustName NO-LABEL 
   SKIP(1)
   "Current balances:" AT 2 SKIP
   ldOPAmt
      LABEL "Overpayment ..." AT 5 
      FORMAT "->,>>>,>>9.99" SKIP
   ldDPAmt 
      LABEL "Deposit ......." AT 5
      FORMAT "->,>>>,>>9.99" SKIP
   ldAPAmt 
      LABEL "Advance payment" AT 5    
      FORMAT "->,>>>,>>9.99" SKIP   
      SKIP(1)
   lcBal AT 2 
      LABEL "Refunded balance" 
      HELP "O = overpayment, D = deposit, A = adv.payment"
      FORMAT "X"
      VALIDATE(LOOKUP(INPUT lcBal,"o,d,a") > 0,
               "Valid choices are O, D and A")
      SKIP
   ldAmt AT 2
      LABEL "Amount ........."
      HELP "Amount to be refunded"
      FORMAT ">,>>>,>>9.99"
      SKIP
   liAccNum AT 2
      LABEL "Account ........"
      HELP "Account nbr for bank posting"
      FORMAT ">>>>>9"
      Account.AccName NO-LABEL 
      SKIP
   lcMemo AT 2 
      LABEL "Memo text ......"
      HELP "Memo to be written on payment"
      VIEW-AS EDITOR
      SIZE 50 BY 3
   WITH TITLE " REFUND CUSTOMER'S BALANCE " SIDE-LABELS
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

/* get current balances */
ASSIGN ldDPAmt = fGetCustBal(Customer.CustNum,"TOTAL","DP")
       ldOPAmt = fGetCustBal(Customer.CustNum,"TOTAL","OP")
       ldAPAmt = fGetCustBal(Customer.CustNum,"TOTAL","AP").

PAUSE 0.
DISP Customer.CustNum
     Customer.CustName 
     ldDPAmt ldOPAmt ldAPAmt
WITH FRAME fCrit. 

ASSIGN nap      = "1"
       llUfkey  = FALSE
       liAccNum = fCParamI("BankAcc").
       
/* default balance to be used */
IF ldDPAmt > 0 THEN ASSIGN lcBal   = "D"
                           ldAmt   = ldDPAmt.
ELSE IF ldAPAmt > 0 THEN ASSIGN lcBal = "A"
                                ldAmt = ldAPAmt.
ELSE IF ldOPAmt > 0 THEN ASSIGN lcBal = "O"
                                ldAmt = ldOPAmt.
       
toimi:
repeat WITH FRAME fCrit ON ENDKEY UNDO toimi, NEXT toimi:

   FIND Account WHERE 
        Account.Brand  = gcBrand AND
        Account.AccNum = liAccNum NO-LOCK NO-ERROR.

   PAUSE 0.
   DISPLAY lcBal 
           ldAmt
           liAccNum
           Account.AccName WHEN AVAILABLE Account
           lcMemo
           WITH FRAME fCrit.
 
   IF llUfkey THEN DO:
      ASSIGN
      ufk[1]= 132  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
      ufk[5]= 1734 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
      ehto = 3.
      RUN ufkey.p.

      READKEY.
      nap = keylabel(LASTKEY).
   END.
   ELSE llUfkey = TRUE.

   if lookup(nap,"1,f1") > 0 THEN DO:
   
      ASSIGN ehto = 9.
      RUN ufkey.p.

      REPEAT ON ENDKEY UNDO, LEAVE:
      
          UPDATE 
          lcBal
          ldAmt
          liAccNum
          lcMemo
          WITH FRAME fCrit EDITING:
          
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME fCrit:
             
                PAUSE 0.

                IF FRAME-FIELD = "lcBal" THEN DO:
                   IF INPUT lcBal NE lcBal THEN DO:
                      CASE INPUT lcBal:
                      WHEN "O" THEN ldAmt = ldOPAmt.
                      WHEN "D" THEN ldAmt = ldDPAmt.
                      WHEN "A" THEN ldAmt = ldAPAmt.
                      END CASE.
                      DISPLAY ldAmt WITH FRAME fCrit.
                   END.
                END.
                
                ELSE IF FRAME-FIELD = "liAccNum" THEN DO:
                   FIND Account WHERE
                        Account.Brand  = gcBrand AND
                        Account.AccNum = INPUT liAccNum NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE Account THEN DO:
                      BELL.
                      MESSAGE "Unknown account".
                      NEXT.
                   END.
                   IF Account.AccType NE 4 THEN DO:
                      BELL.
                      MESSAGE "Type of account is not 4 (payment)".
                      NEXT.
                   END.
                   DISPLAY Account.AccName WITH FRAME fCrit.
                END.    

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.   
   
   else if lookup(nap,"5,f5") > 0 THEN DO:
      
      IF ldAmt = 0 THEN DO:
         MESSAGE "Amount has not been given"
         VIEW-AS ALERT-BOX
         ERROR.
         NEXT.
      END.

      ehto = 5. 
      RUN ufkey.
 
      RUN refupaym (iiCustNum,
                    "",
                    0,
                    lcBal + "P",
                    ldAmt,
                    TODAY,
                    liAccNum,
                    lcMemo,
                    OUTPUT liVoucher).
                      
      IF liVoucher = 0 OR RETURN-VALUE > "" THEN 
      MESSAGE "Payment could not be created;" SKIP
              RETURN-VALUE
      VIEW-AS ALERT-BOX
      ERROR.
      ELSE 
      MESSAGE "Payment was made with voucher nbr" liVoucher
      VIEW-AS ALERT-BOX
      TITLE " Done ".
           
      LEAVE toimi.
   END.

   else if lookup(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.
      
END. /* toimi */

HIDE MESSAGE no-pause.
HIDE FRAME fCrit no-pause.

 
