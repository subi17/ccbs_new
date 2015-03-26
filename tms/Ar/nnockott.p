/* -----------------------------------------------------------------------
  MODULE .......: NNOCKOTT.P
  FUNCTION .....: Reads in an OCR payment file,  AND converts
                  it into a file nnkosu.tmp
  APPLICATION ..: NN
  AUTHOR .......: 
  CREATED ......: 30.01.2002/aam from nnocko
  changePVM ....: 
                  09.08.02/aam transfer/delete of file moved to nnkosu.p,
                               use temp-table parameter instead of ascii-file
                  08.01.02/aam new logic for handling T10-records,
                               better forming of memo-texts

  VERSION ......: M15

 ---------------------------------------------------------------------------- */

{commali.i}
{excel.i}
{date.i} 
{cparam2.i}
{bankrej.i}
/* temp-table */
{paymfile.i}
{farplog.i}
{freadref.i}

DEF INPUT  PARAMETER  ocr-file   AS c NO-UNDO.
DEF INPUT  PARAMETER  ocr-acct   AS i NO-UNDO.
DEF INPUT  PARAMETER  LogPrefix  AS c NO-UNDO.
DEF OUTPUT PARAMETER  TABLE FOR ttPayment.
DEF OUTPUT PARAMETER  rc         AS i NO-UNDO.


 /*********************************************
 * RETURN Code (rc) values:                   *
 * rc = -1: payment file was read earlier     *
 * rc =  0: payment file was NOT found        *
 * rc >  0: number of payments read/converted *
 *********************************************/

DEF STREAM ocr.

DEF VAR x           AS c    NO-UNDO.
DEF VAR xtyp        AS c    NO-UNDO.
DEF VAR yy          AS INT  NO-UNDO.
DEF VAR xdate       AS c    NO-UNDO.
DEF VAR ok          AS lo   NO-UNDO.
DEF VAR debug       AS lo   NO-UNDO.
DEF VAR dd          AS i    NO-UNDO.
DEF VAR mm          AS i    NO-UNDO.
DEF VAR yyy         AS i    NO-UNDO.
DEF VAR chkdate     AS DA   NO-UNDO.

def var x_writeDate as da format "99.99.9999" NO-UNDO.
def var x_payDate   as da format "99.99.9999" NO-UNDO.

def var x_RefNo     as c  format "x(25)"        NO-UNDO.
def var x_paidamt   as de format "z,zzz,zz9.99" NO-UNDO.
def var x_wdate     as c  format "x(6)"   NO-UNDO.
def var x_pdate     as c  format "x(6)"   NO-UNDO.
def var InvNo       as i  format "zzzzzzz9" NO-UNDO.
DEF VAR liCustNum   AS I  NO-UNDO. 
DEF VAR i           AS i  NO-UNDO.

DEF VAR LIi         AS i  NO-UNDO.
DEF VAR xSign       AS c  NO-UNDO.
DEF VAR xArchive    AS c  NO-UNDO.
DEF VAR xCust       AS c  NO-UNDO.
DEF VAR xInterest   AS DE NO-UNDO.
DEF VAR xOrigin     AS c  NO-UNDO.
DEF VAR xBankAcc    AS c  NO-UNDO. 
DEF VAR xCurrency   AS c  NO-UNDO.
DEF VAR xNbr        AS c  NO-UNDO.
DEF VAR xPeriod     AS c  NO-UNDO. 
DEF VAR xPType      AS c  NO-UNDO.
DEF VAR xBType      AS c  NO-UNDO. 
DEF VAR xReceipt    AS c  NO-UNDO. 
DEF VAR xLevel      AS c  NO-UNDO. 
DEF VAR xMemo       AS c  NO-UNDO. 
DEF VAR xMemType    AS c  NO-UNDO.

DEF VAR xDetail     AS LOG  NO-UNDO. 
DEF VAR xLength     AS INT  NO-UNDO.
DEF VAR llFound     AS LOG  NO-UNDO. 
DEF VAR ldPaid      AS DEC  NO-UNDO.
DEF VAR lcMemo      AS CHAR NO-UNDO. 
DEF VAR lcMemMod    AS CHAR NO-UNDO. 
DEF VAR lcRefSrc    AS CHAR NO-UNDO. 

FUNCTION fExpLine RETURNS LOGICAL.

         CREATE ttPayment.
         ASSIGN 
            ttPayment.AccDate   = x_writeDate        /* bookkeeping DAY     */
            ttPayment.PaymDate   = x_PayDate          /* Payment DAY         */
            ttPayment.RefNum     = x_RefNo            /* reference No.       */
            ttPayment.Inv       = InvNo              /* Invoice No.         */
            ttPayment.CustNum   = liCustNum          /* Customer nbr        */
            ttPayment.AmtPaid   = ldPaid             /* payment             */
            ttPayment.ocr-acct  = ocr-acct           /* account             */
            ttPayment.ArchiveId = xArchive           /* archive id          */
            ttPayment.Origin    = xOrigin            /* Code FOR Origin     */
            ttPayment.FName     = ocr-file           /* filename            */
            ttPayment.Interest  = xInterest          /* interest sum        */
            ttPayment.CustName  = xCust              /* customer name       */
            ttPayment.BankAcc   = xBankAcc           /* bank account 
                                                         (where paid) */
            ttPayment.Memo      = lcMemo             /* info                */
            .            

         ASSIGN xDetail = FALSE
                llFound = FALSE
                lcMemo  = ""
                ldPaid  = 0
                rc      = rc + 1.

END FUNCTION.

rc = 0.

DO:

   IF search(ocr-file) = ? THEN RETURN.

   INPUT STREAM ocr from value(ocr-file).


   RECORD:
   repeat:
      IMPORT STREAM ocr UNFORMATTED x.

      ASSIGN xtyp = substr(x,1,3).

      if xtyp = "T00" THEN DO:
         /* is previous line NOT yet exported */
         IF llFound THEN DO:
            fExpLine().
         END.

         ASSIGN xCurrency = substr(x,97,3)
                xBankAcc  = substr(x,10,14)
                xNbr      = substr(x,24,3)
                xPeriod   = substr(x,27,12)
                xDetail   = FALSE
                llFound   = FALSE.
      END.

      ELSE IF xtyp = "T10" THEN DO:      

         /* base record or detail record */
         xLevel = substring(x,188,1).
         IF xLevel = "" THEN xLevel = "0". 

         /* if base record was rejected -> reject detail records also */
         IF xLevel > "0" AND NOT llFound THEN NEXT.

         /* is previous line not yet exported */
         IF xLevel = "0" AND llFound 
         THEN DO:
            fExpLine().
         END.

         ASSIGN
         xOrigin   = "UR" 
         xPType    = substring(x,49,1)
         xBType    = substring(x,50,3)
         xMemo     = RIGHT-TRIM(substring(x,53,35))
         xSign     = substring(x,88,1)
         x_paidamt = decimal(substring(x,89,18)) / 100
         xReceipt  = substring(x,107,1).

         /* some checks only for base records */
         IF xLevel = "0" THEN DO:

            /* no negative sums */
            IF xSign = "-" THEN NEXT.

            /* certain bank accounts are rejected */
            IF LOOKUP(xBankAcc,xRejBanks) GT 0 THEN NEXT.

            /* take only payments AND their withdrawals */
            IF LOOKUP(xPType,"1,3") = 0 THEN NEXT. 
         END. 

         /* only payments of certain type */
         IF LOOKUP(xBType,"700,701,703,704,710") = 0 THEN NEXT. 

         /* negative sum, a withdrawal */
         if xSign = "-" THEN ASSIGN 
            x_paidamt = x_paidamt * -1.

         /* convert FIM sums TO EUR */
         IF xCurrency = "FIM" THEN ASSIGN
            x_paidamt = round(x_paidamt / 5.94573,2).


         /* these only from base record */
         IF xLevel = "0" THEN DO:
            ASSIGN 
            x_wdate  = substr(x,31,6)
            yy       = integer(substr(x_wdate,1,2)).
            IF yy < 80 THEN yy = 2000 + yy.  ELSE yy = 1900 + yy.
            x_writeDate = date(integer(substr(x_wdate,3,2)) ,
                               integer(substr(x_wdate,5,2)) ,
                               yy).

            ASSIGN
            x_pdate  = substr(x,43,6)
            yy       = integer(substr(x_pdate,1,2)).
            IF yy < 80 THEN yy = 2000 + yy.  ELSE yy = 1900 + yy.
            x_payDate = date(integer(substr(x_pdate,3,2)) ,
                             integer(substr(x_pdate,5,2)) ,
                             yy).

            ASSIGN 
            xArchive  = substring(x,13,18)
            x_RefNo   = substring(x,160,20)
            xCust     = substring(x,109,35).

            DO LIi = 1 TO length(x_RefNo):
               if substring(x_RefNo,LIi,1) ne "0" THEN LEAVE.
            END.

            x_RefNo = substring(x_RefNo,LIi).

            lcRefSrc = fReadRefNum(x_Refno,
                                   OUTPUT liCustNum,
                                   OUTPUT Invno).
            /* IF lcRefSrc > "" THEN xOrigin = "T" + lcRefSrc. */
                        
            ASSIGN llFound = TRUE
                   ldPaid  = x_paidamt
                   lcMemo  = xMemo.

         END. 

         ELSE DO:
            ASSIGN xDetail = TRUE
                   lcMemo  = lcMemo + 
                             (IF lcMemo > "" 
                              THEN IF SUBSTRING(lcMemo,LENGTH(lcMemo),1) NE "."
                                   THEN ". "
                                   ELSE " " 
                              ELSE "") +
                             xMemo + 
                             (IF xMemo > ""  THEN " " ELSE "")  +
                             STRING(x_paidamt). 
         END.            

      END.

      /* details */
      ELSE IF xTyp = "T11" THEN DO:
         ASSIGN xLength  = integer(substring(x,4,3))
                xMemType = substring(x,7,2).
         /* take only messages */
         IF LOOKUP(xMemType,"00,02,04,06") GT 0 AND llFound THEN DO:
            xMemo  = RIGHT-TRIM(substring(x,9,MAX(1,xLength - 8))).

            REPEAT:
               /* remove extra spaces inside the text */
               lcMemMod = REPLACE(xMemo,FILL(" ",4)," ").
               IF lcMemMod = xMemo THEN LEAVE.
               xMemo = lcMemMod.
            END. 

            lcMemo = lcMemo + 
                     (IF lcMemo NE "" THEN ", " ELSE "") +
                     xMemo.
         END.
      END.

      ELSE DO:
        IF llFound THEN DO:
            fExpLine().
        END.
      END.

   END.

   IF llFound THEN DO:
      fExpLine().
   END.

   INPUT  STREAM ocr CLOSE.

END.


