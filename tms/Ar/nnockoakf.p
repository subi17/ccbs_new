/* -----------------------------------------------------------------------
  MODULE .......: NNOCKOAKF.P
  FUNCTION .....: Read in an Aktiv Kapital payment file
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 23.02.05 from nnocko
  changePVM ....: 21.12.05/aam add sums to Interest, ClaimCost and Amt in   
                               case there are several lines to one invoice
  VERSION ......: M15

 ---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i} 
/* temp-table */
{Ar/paymfile.i}
{Func/farplog.i}
{Func/cparam2.i}
{Func/finvbal.i}
{Func/freadref.i}

DEF INPUT  PARAMETER  ocr-file   AS c NO-UNDO.
DEF INPUT  PARAMETER  ocr-acct   AS i NO-UNDO.
DEF INPUT  PARAMETER  LogPrefix  AS c NO-UNDO.
DEF OUTPUT PARAMETER  TABLE FOR ttPayment.
DEF OUTPUT PARAMETER  rc         AS i NO-UNDO.

 /*********************************************
 * RETURN Code (rc) values:                   *
 * rc = -1: payment file was read earlier     *
~ * rc =  0: payment file was NOT found        *
 * rc >  0: number of payments read/converted *
 *********************************************/

DEF STREAM ocr.

DEF VAR x           AS c    NO-UNDO.
DEF VAR xtyp        AS c    NO-UNDO.
DEF VAR yy          AS INT  NO-UNDO.
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
def var liInvNum     as i  format "zzzzzzz9" NO-UNDO.
DEF VAR liCustNum   AS I  NO-UNDO. 
DEF VAR i           AS i  NO-UNDO.

DEF VAR paybal      AS DE NO-UNDO.
DEF VAR LIi         AS i  NO-UNDO.
DEF VAR xSign       AS c  NO-UNDO.
DEF VAR xArchive    AS c  NO-UNDO.
DEF VAR xDDRet      AS c  NO-UNDO.
DEF VAR xCust       AS c  NO-UNDO.
DEF VAR xInterest   AS DE NO-UNDO.
DEF VAR xOrigin     AS c  NO-UNDO.
DEF VAR xBankAcc    AS c  NO-UNDO. 
DEF VAR xCurrency   AS c  NO-UNDO.
                             
DEF VAR llBooked     AS LOG  NO-UNDO. 
DEF VAR ldMinAmt     AS DEC  NO-UNDO.
DEF VAR ldMaxAmt     AS DEC  NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO. 
DEF VAR lcRefSrc     AS CHAR NO-UNDO. 
DEF VAR lcType       AS CHAR NO-UNDO. 
DEF VAR llCreate     AS LOG  NO-UNDO. 

DEF BUFFER bPayment FOR ttPayment.

DEF TEMP-TABLE ttTarget NO-UNDO
   FIELD InvNum AS INT
   FIELD InvBal AS DEC
   INDEX InvNum InvNum. 
   
ASSIGN rc        = 0.

IF search(ocr-file) = ? THEN RETURN.

INPUT STREAM ocr from value(ocr-file).

RECORD:
repeat:
      IMPORT STREAM ocr UNFORMATTED x.

      ASSIGN xtyp = substr(x,1,1).

      if xtyp = "0" THEN DO:
          ASSIGN xCurrency = substr(x,23,1).
      END.

      ELSE IF xtyp = "3" THEN DO:   

         ASSIGN
         xBankAcc = substr(x,2,14)
         x_wdate  = substr(x,16,6)
         yy       = integer(substr(x_wdate,1,2))
         xDDRet   = substr(x,90,1)
         xOrigin  = "AK".

         IF yy < 80 THEN yy = 2000 + yy.  ELSE yy = 1900 + yy.

         x_writeDate = date(
                    integer(substr(x_wdate,3,2)) ,
                    integer(substr(x_wdate,5,2)) ,
                    yy).

         ASSIGN
         x_pdate  = substr(x,22,6)
         yy       = integer(substr(x_pdate,1,2)).

         IF yy < 80 THEN yy = 2000 + yy.  ELSE yy = 1900 + yy.

         x_payDate = date(
                    integer(substr(x_pdate,3,2)) ,
                    integer(substr(x_pdate,5,2)) ,
                    yy).

         ASSIGN 
         xArchive  = substring(x,28,16)
         x_RefNo   = substring(x,44,20)
         xCust     = substring(x,64,12).

         DO LIi = 1 TO LENGTH(x_RefNo):
            if substring(x_RefNo,LIi,1) ne "0" THEN LEAVE.
         END.

         x_RefNo = substring(x_RefNo,LIi).
         
         /* reference is always invoice nbr */
         IF LENGTH(x_Refno) > 9 
         THEN liInvNum = INTEGER(SUBSTRING(x_Refno,LENGTH(x_Refno) - 8,8)).
         ELSE liInvNum  = INTEGER(SUBSTRING(x_Refno,1,LENGTH(x_RefNo) - 1)).
         
         ASSIGN x_paidamt = deci(substr(x,78,10)) / 100
                xSign     = substring(x,88,1)
                
                /* row type, 
                   P = payment
                   K = interest
                   H = claiming cost
                */
                lcType    = SUBSTRING(x,89,1).

         /* negative sum, a withdrawal */
         if xSign = "1" THEN ASSIGN 
            x_paidamt = x_paidamt * -1.

         llCreate = TRUE.
         
         /* join interest and claiming cost with actual payment */
         IF LOOKUP(lcType,"K,H") > 0 THEN DO:
            FIND FIRST ttPayment WHERE 
                       ttPayment.Inv = liInvNum NO-ERROR.
            IF AVAILABLE ttPayment THEN llCreate = FALSE.
         END.
   
         IF llCreate THEN DO:
            CREATE ttPayment.
            ASSIGN 
            ttPayment.AccDate   = x_writeDate        /* bookkeeping DAY     */
            ttPayment.PaymDate  = x_PayDate          /* Payment DAY         */
            ttPayment.RefNum    = x_RefNo            /* reference No.       */
            ttPayment.Inv       = liInvNum            /* Invoice No.         */
            ttPayment.CustNum   = liCustNum          /* customer            */
            ttPayment.ocr-acct  = ocr-acct           /* account             */
            ttPayment.ArchiveId = xArchive           /* archive id          */
            ttPayment.Origin    = xOrigin            /* Code FOR Origin     */
            ttPayment.FName     = ocr-file           /* filename            */
            ttPayment.CustName  = xCust              /* customer name       */
            ttPayment.BankAcc   = xBankAcc           /* bank account 
                                                         (where paid) */
            rc = rc + 1.

            FIND FIRST Invoice WHERE 
                       Invoice.Brand  = gcBrand AND
                       Invoice.InvNum = ttPayment.Inv 
            NO-LOCK NO-ERROR.
            IF AVAIL Invoice AND Invoice.CrInvNum = 0 
            THEN ttPayment.CustNum = Invoice.CustNum.
         END.

         /* type of payment */
         CASE lcType:
         WHEN "K" THEN ttPayment.Interest  = ttPayment.Interest +
                                             x_paidamt.  /* interest      */
         WHEN "H" THEN ttPayment.ClaimCost = ttPayment.ClaimCost +
                                             x_paidamt.  /* claiming cost */
         OTHERWISE     ttPayment.AmtPaid   = ttPayment.AmtPaid + 
                                             x_paidamt.  /* payment       */
         END CASE. 

      END.
END.

INPUT  STREAM ocr CLOSE.

