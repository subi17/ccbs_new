/* ------------------------------------------------------
  MODULE .......: PBANKREP
  FUNCTION .....: List payments' bank accounts
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 08.11.02
  MODIFIED .....: 12.09.03/aam brand
                  21.04.05/aam skip payment type 6  
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i}


DEF INPUT  PARAMETER icInvGrp    AS CHAR  NO-UNDO. 
DEF INPUT  PARAMETER idtDate1    AS DATE  NO-UNDO.
DEF INPUT  PARAMETER idtDate2    AS DATE  NO-UNDO.

DEF VAR lcLine1      AS CHAR NO-UNDO FORMAT "x(78)".
DEF VAR lcLine2      LIKE lcLine1.
DEF VAR lcLine3      LIKE lcLine1.
DEF VAR lcLine4      LIKE lcLine1.
DEF VAR liPage       AS INT  NO-UNDO.
DEF VAR liLine       AS INT  NO-UNDO.
DEF VAR liWidth      AS INT  NO-UNDO INIT 78.

DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR lcGrpHeader  AS CHAR NO-UNDO. 
DEF VAR liQty        AS INT  NO-UNDO.
DEF VAR liFound      AS INT  NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO. 
DEF VAR ldtPaid      AS DATE NO-UNDO. 
DEF VAR ldAmt        AS DEC  NO-UNDO. 

DEF TEMP-TABLE ttPaid NO-UNDO
   FIELD InvGrp      AS CHAR
   FIELD BankAcc     AS CHAR 
   FIELD PaymSrc     AS CHAR
   FIELD PaidAmt     AS DEC 
   INDEX InvGrp InvGrp BankAcc PaymSrc.

ASSIGN 
    lcLine1   = FILL("=",liWidth)
    lcLine2   = FILL("=",liWidth)
    lcLine3   = FILL("-",liWidth)
    lcLine4   = FILL("-",liWidth).

form header
   lcLine1 AT 1 SKIP
   ynimi  AT 1 FORMAT "x(30)" 
      "PAYMENTS BY BANK ACCOUNT" AT 35
      "Page" AT 69
      liPage FORMAT "ZZZZ9" SKIP
   lcGrpHeader AT 1 FORMAT "X(30)"
      lcDateHeader AT 35 FORMAT "X(30)"
      pvm FORMAT "99.99.99" AT 71 SKIP
   lcLine2 AT 1 SKIP

   "Bank Account"    AT 7
   "Payment Source"  AT 28
   "Amount"          TO 57
   SKIP

   lcLine3 AT 1 SKIP
   WITH WIDTH 78 NO-LABEL NO-BOX FRAME fPageHead.

FORM "Paym. found:" AT 2 liFound FORMAT ">>,>>>,>>9"
WITH NO-LABELS OVERLAY ROW 14 CENTERED TITLE " Collecting payments "
   FRAME fQty.


FUNCTION fChkPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF liLine + iAddLine >= skayt1 THEN DO:

        IF liPage > 0 THEN DO:
           {Syst/uprfeed.i liLine}
        END.

        liPage = liPage + 1.
        VIEW STREAM tul FRAME fPageHead.
        liLine = 6.

        RETURN TRUE.
    END.    

    ELSE RETURN FALSE.
END.

ASSIGN lcDateHeader = STRING(idtDate1,"99.99.99") + " - " +
                      STRING(idtDate2,"99.99.99")
       lcGrpHeader  = "Inv.Group: ".


IF icInvGrp NE "" THEN lcGrpHeader = lcGrpHeader + icInvGrp.
ELSE lcGrpHeader = lcGrpHeader + "ALL".

PAUSE 0.
VIEW FRAME fQty. 

/* separate loop -> info can be shown to user */
FOR EACH Payment NO-LOCK WHERE
         Payment.Brand       = gcBrand      AND
         Payment.AccDate    >= idtDate1     AND
         Payment.AccDate    <= idtDate2     AND
         Payment.BankAcc  NE ""             AND
         Payment.PaymType NE 6              AND
         NOT Payment.PaymSrc BEGINS "MP"    AND
         Payment.PaymSrc NE "AD",
FIRST Customer OF Payment NO-LOCK WHERE
      (IF icInvGrp NE ""
       THEN Customer.InvGroup = icInvGrp
       ELSE TRUE):

   ASSIGN liFound = liFound + 1.

   IF liFound MOD 1000 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liFound WITH FRAME fQty.
   END.

   FIND FIRST ttPaid WHERE
              ttPaid.InvGrp  = Customer.InvGroup  AND
              ttPaid.BankAcc = Payment.BankAcc AND
              ttPaid.PaymSrc = Payment.PaymSrc NO-ERROR.
   IF NOT AVAILABLE ttPaid THEN DO:
      CREATE ttPaid.
      ASSIGN ttPaid.InvGrp  = Customer.InvGroup
             ttPaid.BankAcc = Payment.BankAcc
             ttPaid.PaymSrc = Payment.PaymSrc.
   END. 

   ASSIGN ttPaid.PaidAmt  = ttPaid.PaidAmt + Payment.PaymAmt.
END.

PAUSE 0.
DISPLAY liFound WITH FRAME fQty.


fChkPage(99).

FOR EACH ttPaid
BREAK 
BY ttPaid.InvGrp 
BY ttPaid.BankAcc
BY ttPaid.PaymSrc:


   IF FIRST-OF(ttPaid.InvGrp) THEN DO:

      fChkPage(2). 

      FIND InvGroup WHERE 
           InvGroup.Brand    = gcBrand AND
           InvGroup.InvGroup = ttPaid.InvGrp 
      NO-LOCK NO-ERROR.

      PUT STREAM tul UNFORMATTED
         ttPaid.InvGrp AT 1 
         SPACE(1) 
         (IF AVAILABLE InvGroup
          THEN InvGroup.IgName
          ELSE "Unknown") 
         SKIP.

      liLine = liLine + 1.
   END.


   fChkPage(0).

   PUT STREAM tul         
      ttPaid.BankAcc       AT 7   FORMAT "X(20)"
      ttPaid.PaymSrc       AT 28  FORMAT "X(8)"
      ttPaid.PaidAmt       TO 57  FORMAT "->>>>>>>>>9.99"
      SKIP.

   liLine = liLine + 1.

   ACCUMULATE ttPaid.PaidAmt   (TOTAL BY ttPaid.BankAcc
                                      BY ttPaid.InvGrp)
              ttPaid.PaidAmt   (COUNT BY ttPaid.BankAcc).


   IF LAST-OF(ttPaid.BankAcc) THEN DO:

      /* subtotal only if more than 1 lines */
      IF (ACCUM COUNT BY ttPaid.BankAcc ttPaid.PaidAmt) > 1 
      THEN DO:

         fChkPage(1).

         PUT STREAM tul UNFORMATTED
            FILL("-",51)  AT 7 SKIP
            ttPaid.BankAcc AT 7
            SPACE(1)
           "total".

         PUT STREAM tul
            (ACCUM TOTAL BY ttPaid.BankAcc ttPaid.PaidAmt)
               TO 57 FORMAT "->>>>>>>>>9.99"
            SKIP.

         liLine = liLine + 2.
      END.

      fChkPage(0).
      PUT STREAM tul SKIP(1).
      liLine = liLine + 1.

   END.

   IF LAST-OF(ttPaid.InvGrp) THEN DO:
      fChkPage(2).

      PUT STREAM tul UNFORMATTED
         FILL("-",57)  AT 1 SKIP
         ttPaid.InvGrp AT 1
         SPACE(1)
         "total".

      PUT STREAM tul
         (ACCUM TOTAL BY ttPaid.InvGrp ttPaid.PaidAmt)
            TO 57 FORMAT "->>>>>>>>>9.99"
          SKIP(1).

      liLine = liLine + 3.
   END. 


   IF LAST(ttPaid.InvGrp) THEN DO:

      fChkPage(1).

      PUT STREAM tul UNFORMATTED
         FILL("=",57) AT 1 SKIP
         "GRAND TOTAL" AT 1.

      PUT STREAM tul
         (ACCUM TOTAL ttPaid.PaidAmt)
            TO 57 FORMAT "->>>>>>>>>9.99"
         SKIP.

      liLine = liLine + 2.
   END. 

END.

{Syst/uprfeed.i liLine}

HIDE FRAME fQty NO-PAUSE. 
