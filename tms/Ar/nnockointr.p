/* -----------------------------------------------------------------------
  MODULE .......: NNOCKOINTR.P
  FUNCTION .....: Reads in an Intrum's OCR payment PaymFile,  AND converts
                  it into a PaymFile nnkosu.tmp
  APPLICATION ..: NN
  AUTHOR .......: 
  CREATED ......: 11.02.2002/ht  from nnocko...
  changePVM ....: 15.03.2002/aam FIND any invoice from customer IF
                                 nothing was found using Invoice.DueDate,
                                 more logic TO getting customer nbr
                  22.03.2002/ht  testing customer CMT developed more
                  22.04.2002/aam check from PaymLog IF PaymFile has been 
                                 Processed
                  09.08.02/aam transfer/delete of PaymFile moved to nnkosu.p,
                               use temp-table parameter instead of ascii-file,
                               possibility to bypass arplog-check
                  30.09.02/aam skip credit invoices                             
  Version ......: M15
 ---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i} 
/* temp-table */
{Ar/paymfile.i}
{Func/farplog.i}

DEF INPUT  PARAMETER  ocr-file   AS c NO-UNDO.
DEF INPUT  PARAMETER  ocr-acct   AS i NO-UNDO.
DEF INPUT  PARAMETER  LogPrefix  AS c NO-UNDO.
DEF OUTPUT PARAMETER  TABLE FOR ttPayment.
DEF OUTPUT PARAMETER  rc         AS i NO-UNDO.

 /*********************************************
 * RETURN Code (rc) values:                   *
 * rc = -1: payment PaymFile was read earlier     *
 * rc =  0: payment PaymFile was NOT found        *
 * rc >  0: CMT of payments read/converted *
 *********************************************/

DEF STREAM ocr.

DEF VAR x           AS c    NO-UNDO.
DEF VAR xTyp        AS c    NO-UNDO.
DEF VAR yy          AS INT  NO-UNDO.
DEF VAR xDate       AS c    NO-UNDO.
DEF VAR dd          AS i    NO-UNDO.
DEF VAR mm          AS i    NO-UNDO.
DEF VAR yyy         AS i    NO-UNDO.
DEF VAR chkdate     AS DA   NO-UNDO.

def var x_writeDate as da format "99.99.9999" NO-UNDO.
def var x_payDate   as da format "99.99.9999" NO-UNDO.

def var x_RefNum     as c  format "x(25)"        NO-UNDO.
def var x_paidamt   as de format "z,zzz,zz9.99" NO-UNDO.
def var x_wdate     as c  format "x(6)"   NO-UNDO.
def var x_pdate     as c  format "x(6)"   NO-UNDO.
def var InvNum       as i  format "zzzzzzz9" NO-UNDO.
DEF VAR DateLimit   AS DA NO-UNDO.
DEF VAR i           AS i  NO-UNDO.

DEF VAR xArchive    AS c  NO-UNDO.
DEF VAR xCust       AS c  NO-UNDO.
DEF VAR xInterest   AS DE NO-UNDO.
DEF VAR xOrigin     AS c  NO-UNDO.
DEF VAR xBankAcc    AS c  NO-UNDO. 
DEF VAR xCurrency   AS c  NO-UNDO.
DEF VAR xNbr        AS c  NO-UNDO.
DEF VAR xmemo       AS c  NO-UNDO. 
DEF VAR xCustCost   AS DE NO-UNDO. 
DEF VAR xPreArchive AS c  NO-UNDO.
DEF VAR llNew       AS lo NO-UNDO.
DEF VAR lCustNr     AS i  NO-UNDO.
DEF VAR paybal      AS DE NO-UNDO.
/* chk sums */
DEF VAR xTotSum0    AS DE NO-UNDO. /* original in ocr-file */
DEF VAR xTotSum1    AS DE NO-UNDO. /* found payments       */
DEF VAR xTotSum2    AS DE NO-UNDO. /* written in tmp-file  */

DEF VAR ok1         AS lo NO-UNDO.
DEF VAR ok2         AS lo NO-UNDO.

DEF VAR xGetCust    AS CH NO-UNDO. 

DEFINE TEMP-TABLE t_ocr
    FIELD   InvNum     AS I       /* invoice no.               */
    FIELD   x_paidamt AS DE      /* payment                   */
    FIELD   xArchive  AS C       /* archive id                */
    FIELD   xInterest AS DE      /* Interest sum              */
    FIELD   xCustCost AS DE      /* customer's costs          */
    FIELD   xCust     AS C       /* customer BankOffice             */
    FIELD   xBankAcc  AS C       /* Bank AccNum (where CommPaid) */
    FIELD   xmemo     AS C       /* info                      */
    FIELD   xCustNum  AS I.      /* customer                  */

FUNCTION fClearInvNo   RETURNS INTEGER
  (INPUT p AS CHAR).

  IF p = "" THEN RETURN 0.
  REPEAT:
     IF INDEX("1234567890",SUBSTR(p,1,1)) = 0 THEN
        p = SUBSTR(p,2).
     ELSE LEAVE.
  END.
  RETURN INTEGER(p).

END FUNCTION.

{Func/cparam.i DateLimit RETURN} DateLimit = TMSParam.DateVal.

rc = 0.

IF search(ocr-file) = ? THEN RETURN.

ELSE DO:

   INPUT STREAM ocr from value(ocr-file).

   /* sniff the PaymFile creation & bookkeeping dates */
   repeat:
      IMPORT STREAM ocr x.

      ASSIGN xtyp = substr(x,1,2).
      IF xtyp = "00" THEN DO:
         IF substr(x,1,8) = "00INTRUM" THEN ASSIGN
            ok1   = TRUE
            xDate = SUBSTR(x,9,6).
         ELSE 
            ok1   = FALSE.
      END.
      ELSE IF xtyp = "90" THEN 
            ok2   = TRUE.
   END.

   INPUT STREAM ocr CLOSE.

   IF NOT ok1 THEN DO:
      MESSAGE "This is not Intrum's OCR-file!"
      VIEW-AS ALERT-BOX TITLE " REMARK ! ".
      RETURN.
   END.
   IF NOT ok2 THEN DO:
      /* Summary record is missing OR line feed is missing, adds line feed */
      INPUT STREAM ocr CLOSE.
      OUTPUT STREAM ocr TO VALUE(ocr-file) APPEND.
      PUT STREAM ocr UNFORMATTED my-nl.
      OUTPUT STREAM ocr CLOSE.
   END.

   ASSIGN
      dd  = INT(SUBSTR(xDate,5,2))
      mm  = INT(SUBSTR(xDate,3,2))
      yyy = INT(SUBSTR(xDate,1,2))
      yyy = fAddCent(yyy)

      chkdate     = DATE(mm,dd,yyy)
      x_writeDate = chkdate
      x_PayDate   = chkdate
      xOrigin     = "IN"
      lcDouble    = "". 

   /* check log */
   IF NOT fCheckArplog(ocr-file,
                       LogPrefix,
                       katun)
   THEN RETURN. 

   /* open the ocr PaymFile again */
   INPUT  STREAM ocr from value(ocr-file).   

   RECORD:
   repeat:
      IMPORT STREAM ocr UNFORMATTED x.

      xTyp = substr(x,1,2).
      IF xTyp = "00" THEN NEXT.

      /* eof record */
      IF xTyp = "90" THEN DO:
         xTotSum0 = decimal(substring(x,69,12)) / 100. 
         LEAVE.
      END.

      /* no negative sums */
      IF decimal(substring(x,77,12)) < 0 THEN NEXT.

      ASSIGN 
         xmemo     = substr(x,9,20)     /* customer's reference */
         InvNum    = fClearInvNo(substr(x,29,20))
         x_Refnum  = substr(x,29,20)
         xArchive  = substr(x,49,8)     /* Intrum's reference   */     
         xCust     = substr(x,57,20)
         xBankAcc  = substr(x,89,14)
         xGetCust  = RIGHT-TRIM(xmemo).

         /* Two various customer CMT cases are possible */

         xGetCust  = IF NUM-ENTRIES(xmemo) GT 1 
                     THEN ENTRY(1,xmemo)
                     ELSE IF LENGTH(xGetCust) GT 1
                          THEN substring(xGetCust,1,LENGTH(xGetCust))
                          ELSE "".

         IF NOT CAN-FIND
         (FIRST Customer WHERE Customer.CustNum = INT(xGetCust)) 
         THEN DO:

            xGetCust  = IF LENGTH(xGetCust) GT 1
                        THEN substring(xGetCust,1,LENGTH(xGetCust) - 1)
                        ELSE "".

            IF NOT CAN-FIND
            (FIRST Customer WHERE Customer.CustNum = INT(xGetCust) AND
                     CAPS(SUBSTR(Customer.CustName,1,20)) = CAPS(xCust))
            THEN xGetCust = "".

         END.

         ASSIGN
            lCustNr = IF LENGTH(xGetCust) LE 8
                      THEN INT(xGetCust)
                      ELSE 0.

      IF      xTyp = "10" THEN ASSIGN
         x_paidamt = decimal(substring(x,77,12)) / 100
         xInterest = 0
         xCustCost = 0.
      ELSE IF xTyp = "20" THEN ASSIGN
         x_paidamt = 0
         xInterest = decimal(substring(x,77,12)) / 100
         xCustCost = 0.
      ELSE IF xTyp = "40" THEN ASSIGN
         x_paidamt = 0
         xInterest = 0
         xCustCost = decimal(substring(x,77,12)) / 100.

      xTotSum1     = xTotSum1 + x_paidamt + xInterest + xCustCost.

      IF InvNum NE 0 THEN DO:
         CREATE t_ocr.
         llNew = TRUE.
      END.
      ELSE IF xArchive NE xPreArchive THEN DO:

         /* Finding invoice CMT WITH customer CMT */

         FindUnPaid:
            FOR EACH Invoice NO-LOCK WHERE
                     Invoice.CustNum = lCustNr AND
                     Invoice.InvAmt  > 0       AND 
                     Invoice.DueDate < x_PayDate
                  BY Invoice.DueDate:

               RUN invbal (Invoice.InvNum,OUTPUT paybal).
               IF paybal <= 0 THEN NEXT.

               InvNum = Invoice.InvNum.
               LEAVE FindUnpaid.
            END.
         /* IF still without invoice CMT */
         IF InvNum = 0 THEN DO:
            FIND LAST Invoice WHERE
                      Invoice.CustNum = lCustNr AND
                      Invoice.InvAmt > 0        AND 
                      Invoice.DueDate < x_PayDate NO-LOCK NO-ERROR.

            IF AVAIL Invoice THEN InvNum = Invoice.InvNum.
         END.

         /* any invoice will DO */
         IF InvNum = 0 THEN DO:
            FIND LAST Invoice WHERE
                      Invoice.CustNum = lCustNr     AND
                      Invoice.InvAmt  > 0           AND 
                      Invoice.InvDate GE 01/01/2002
                      NO-LOCK NO-ERROR.

            IF AVAIL Invoice THEN InvNum = Invoice.InvNum.
         END.

         CREATE t_ocr.
         llNew = TRUE.
      END.
      ELSE llNew = FALSE.

      IF llNew THEN
         ASSIGN 
            t_ocr.InvNum     = InvNum
            t_ocr.xCustNum  = lCustNr
            t_ocr.xmemo     = xmemo
            t_ocr.xArchive  = xArchive
            t_ocr.xCust     = xCust
            t_ocr.xBankAcc  = xBankAcc.

      ASSIGN 
         t_ocr.x_paidamt = t_ocr.x_paidamt + x_paidamt
         t_ocr.xInterest = t_ocr.xInterest + xInterest
         t_ocr.xCustCost = t_ocr.xCustCost + xCustCost
         xPreArchive     = substr(x,49,8).
   END.

   INPUT  STREAM ocr CLOSE.

   FOR EACH t_ocr:

      CREATE ttPayment.
      ASSIGN 
            ttPayment.AccDate   = x_writeDate        /* bookkeeping DAY     */
            ttPayment.PaymDate  = x_PayDate          /* Payment DAY         */
            ttPayment.RefNum    = x_RefNum            /* reference No.       */
            ttPayment.Inv       = t_ocr.InvNum        /* Invoice No.         */
            ttPayment.AmtPaid   = t_ocr.x_paidamt    /* payment             */
            ttPayment.ocr-acct  = ocr-acct           /* AccNum             */
            ttPayment.ArchiveId = t_ocr.xArchive     /* archive id          */
            ttPayment.Origin    = xOrigin            /* Code FOR Origin     */
            ttPayment.FName     = ocr-file           /* FileName            */
            ttPayment.Interest  = t_ocr.xInterest    /* Interest sum        */
            ttPayment.CustName  = t_ocr.xCust        /* customer BankOffice       */
            ttPayment.BankAcc   = t_ocr.xBankAcc     /* Bank AccNum 
                                                         (where CommPaid) */
            ttPayment.Memo      = t_ocr.xmemo        /* info                */
            ttPayment.ClaimCost = t_ocr.xCustCost    /* customer's costs    */
            ttPayment.CustNum    = t_ocr.xCustNum     /* customer nbr */
         .            

      ASSIGN
      xTotSum2 = xTotSum2 + t_ocr.x_paidamt + t_ocr.xInterest + t_ocr.xCustCost
      rc       = rc + 1.
   END.

   IF xTotSum1 NE xTotSum0 OR xTotSum2 NE xTotSum0 THEN
   MESSAGE "Error in reading OCR-file !"                           SKIP
           "Summary record was missing or check sums did't match." SKIP
   VIEW-AS ALERT-BOX TITLE " REMARK ! ".

END.

