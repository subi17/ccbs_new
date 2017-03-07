/* -----------------------------------------------------------------------
  MODULE .......: NNOCKO.P
  FUNCTION .....: Reads in a payment file and converts it into temp-table
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 29.05.97 pt
  changePVM ....: 27.09.01/jr
                  29.10.01/aam move the processed file into a different
                               directory instead of deleting it,
                               use import "unformatted",
                               amount must be divided WITH 100,
                               EXPORT also ocr-acct
                  08.11.01/aam negative sums were NOT handled
                  21.11.01/aam archive id added 
                  27.11.01/aam direct debit events
                  20.12.01/aam customer name AND interest added
                  04.01.02/aam special reference nbr 20000913,
                               take inv. nbr from the END of reference nbr,
                               source of payment added 
                  07.01.02/aam save bank account TO memo,
                               check currency from 0-line,
                               DATE added TO backup file name,
                               check that the backup file doesn't exist
                  09.08.02/aam transfer/delete of file moved to nnkosu.p,
                               use temp-table parameter instead of ascii-file
                  30.09.02/aam skip credit/credited invoices 
                  26.02.03/aam DDError 
                  20.05.03/aam use finvbal
                  22.05.03/aam special reference nbr beginning with "1111"
                  09.02.04/aam RefNumForm,
                               find unpaid invoices with amount range
                  21.06.04/aam better logic for finding target invoices
                  20.09.04/aam target also to credited invoices and
                               credit loss invoices 
                  03.11.04/aam use fCalcInvBal              
                  01.04.05/aam handle credit loss invoices like unpaid ones
                  13.04.05/aam check payment archive already here 
                  20.04.05/aam if invoice with exact match isn't found then
                               just use unpaid from oldest 
                  04.05.05/aam mark the original order of rows (POrder)
                  01.11.05/aam read all lines into temp-table before targeting
                               to invoices -> reduce corrections from 
                               errorneus payments before posting,
                               if negative payment remains after that then
                               direct it to unregistered payments
                  22.03.06/aam payment plans             
                  26.04.06/aam improvement to targeting logic (rl)
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

DEF STREAM sPaym.
DEF STREAM sLog.

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
DEF VAR liOrder      AS INT  NO-UNDO. 
DEF VAR liPPlanID    AS INT  NO-UNDO. 
DEF VAR llFound      AS LOG  NO-UNDO. 

DEF BUFFER bPayment FOR ttPayment.

DEF TEMP-TABLE ttTarget NO-UNDO
   FIELD InvNum AS INT
   FIELD InvBal AS DEC
   INDEX InvNum InvNum. 
   
ASSIGN rc        = 0.

IF search(ocr-file) = ? THEN RETURN.

INPUT STREAM sPaym from value(ocr-file).

RECORD:
repeat:
 
      IMPORT STREAM sPaym UNFORMATTED x.

      ASSIGN xtyp = substr(x,1,1).

      if xtyp = "0" THEN DO:
          ASSIGN xCurrency = substr(x,23,1).
      END.

      ELSE IF xtyp = "3" OR
         /* direct debit */
         xtyp = "5" 
      THEN DO:   

         ASSIGN
         xBankAcc = substr(x,2,14)
         x_wdate  = substr(x,16,6)
         yy       = integer(substr(x_wdate,1,2))
         xDDRet   = substr(x,90,1)
         xOrigin  = IF xTyp = "5" 
                    THEN "DD"
                    ELSE "RF".

         /* was direct debit succesful */
         if xTyp = "5" THEN DO:
            IF xDDRet ne "0" and xDDRet ne "" THEN DO:
               FOR FIRST TMSCodes NO-LOCK WHERE
                         TMSCodes.TableName = "Payment" AND
                         TMSCodes.FieldName = "DDError" AND
                         TMSCodes.CodeValue = xDDRet:
                  xDDRet = TMSCodes.CodeName.           
               END.
            END.
            IF xDDRet = "0" THEN xDDRet = "". 
         END.
         ELSE xDDRet = "".

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

         lcRefSrc = fReadRefNum(x_Refno,
                                OUTPUT liCustNum,
                                OUTPUT liInvNum).

         IF lcRefSrc > "" THEN xOrigin = "R" + lcRefSrc.            

         ASSIGN                            
            x_paidamt = deci(substr(x,78,10)) / 100
            xSign     = substring(x,88,1).

         /* convert FIM sums TO EUR */
         IF xCurrency = "" or xCurrency = "0" THEN ASSIGN
            x_paidamt = round(x_paidamt / 5.94573,2).

         /* negative sum, a withdrawal */
         if xSign = "1" THEN ASSIGN 
            x_paidamt = x_paidamt * -1.

         CREATE ttPayment.
         ASSIGN 
            ttPayment.AccDate   = x_writeDate        /* bookkeeping DAY     */
            ttPayment.PaymDate  = x_PayDate          /* Payment DAY         */
            ttPayment.RefNum    = x_RefNo            /* reference No.       */
            ttPayment.Inv       = liInvNum            /* Invoice No.         */
            ttPayment.CustNum   = liCustNum          /* customer            */
            ttPayment.AmtPaid   = x_paidamt          /* payment             */
            ttPayment.ocr-acct  = ocr-acct           /* account             */
            ttPayment.ArchiveId = xArchive           /* archive id          */
            ttPayment.Origin    = xOrigin            /* Code FOR Origin     */
            ttPayment.FName     = ocr-file           /* filename            */
            ttPayment.Interest  = xInterest          /* interest sum        */
            ttPayment.CustName  = xCust              /* customer name       */
            ttPayment.BankAcc   = xBankAcc           /* bank account 
                                                         (where paid) */
            ttPayment.DDError   = xDDRet             /* direct debit error  */ 
            ttPayment.ErrorTxt  = "" 
            liOrder             = liOrder + 1
            ttPayment.POrder    = liOrder.            

         rc = rc + 1.

         /* is payment already Booked */
         IF xDDRet = "" THEN 
         FOR FIRST Payment NO-LOCK USE-INDEX PaymArc WHERE
                   Payment.Brand   = gcBrand              AND
                   Payment.PaymArc = ttPayment.ArchiveId  AND
                   Payment.AccDate = ttPayment.AccDate:
            /* corrections may have the same archive id */
            IF (Payment.PaymAmt > 0 AND ttPayment.AmtPaid > 0) OR
               (Payment.PaymAmt < 0 AND ttPayment.AmtPaid < 0)
            THEN 
            ttPayment.ErrorTxt = "Already booked, voucher " + 
                                 STRING(Payment.Voucher).
         END.

      END.
      
END.

INPUT STREAM sPaym CLOSE.


/* check correction payments; are there corresponding positive payments that
   can be deleted along with the negative one */
FOR EACH ttPayment WHERE
         ttPayment.AmtPaid < 0,
   FIRST bPayment WHERE
         bPayment.CustNum = ttPayment.CustNum AND
         bPayment.RefNum  = ttPayment.RefNum  AND
         bPayment.AmtPaid = -1 * ttPayment.AmtPaid:
         
   OUTPUT STREAM sLog TO /apps/tms/snet/rf_correction.log append.
   PUT STREAM sLog UNFORMATTED
      TODAY                CHR(9)
      ttPayment.CustNum    CHR(9)
      ttPayment.RefNum     CHR(9)
      ttPayment.ArchiveId  CHR(9)
      ttPayment.AccDate    SKIP.
   OUTPUT STREAM sLog CLOSE.
   
   DELETE bPayment.
   DELETE ttPayment.
   
END. 

FOR EACH ttPayment WHERE 
         ttPayment.AmtPaid > 0:

   /* find unpaid invoices of customer if reference nbr didn't 
      contain invoice nbr */
   IF (ttPayment.Inv = 0 OR ttPayment.Origin = "RL")  AND 
      ttPayment.CustNum  > 0     AND    
      ttPayment.Double   = FALSE AND
      ttPayment.ErrorTxt = ""    AND
      ttPayment.DDError  = ""    AND       /* error in dd */
      LOOKUP(ttPayment.Origin,"RV,RA,RS,RG") = 0 /* adv.payment */
   THEN DO:

      llBooked = FALSE.

      /* is payment already booked */
      for first Payment no-lock use-index paymarc where
                Payment.Brand    = gcBrand              AND
                Payment.PaymArc  = ttPayment.ArchiveId  AND
                Payment.AccDate  = ttPayment.AccDate:
     
         /* corrections may have the same archive id */
         IF (Payment.PaymAmt > 0 AND ttPayment.AmtPaid > 0) OR
            (Payment.PaymAmt < 0 AND ttPayment.AmtPaid < 0)
         THEN ASSIGN ttPayment.Inv = Payment.InvNum
                     llBooked      = TRUE. 
      end. 

      /* check if there are unpaid invoices, if not then just
         add customer's deposit balance (or if there aren't 
         enough unpaid invoices) 
      */
      IF NOT llBooked THEN DO:
              
         IF ttPayment.Origin = "RL" THEN ASSIGN 
            liPPlanID      = ttPayment.Inv
            ttPayment.Inv = 0.
         ELSE liPPlanID = 0.
            
         FindUnpaid:
         DO liCount = 1 TO 3:

            
            /* first try to find with exact amount, then all unpaid */
            CASE liCount:
            WHEN 1 OR WHEN 2 THEN ASSIGN 
               ldMinAmt = MAX(0.01,ttPayment.AmtPaid) 
               ldMaxAmt = ldMinAmt.
            WHEN 3 THEN ASSIGN 
               ldMinAmt = 0.01
               ldMaxAmt = 99999999.
            END CASE.
                                    
            /* payment plan  */
            IF ttPayment.Origin = "RL" AND liCount = 1 THEN ASSIGN 
               ldMinAmt = 0.01
               ldMaxAmt = 99999999.             

            FOR EACH Invoice NO-LOCK USE-INDEX CustNum WHERE
                     Invoice.Brand    = gcBrand           AND
                     Invoice.CustNum  = ttPayment.CustNum AND
                     Invoice.InvAmt   > 0                 AND
                     Invoice.PaymState NE 2               AND
                     Invoice.InvType   NE 3               AND
                     Invoice.InvType   NE 4               AND
                     Invoice.PrintState > 0 
            BY Invoice.DueDate:

               /* paid with a payment plan reference; if no invoices belonging 
                  to that plan are found during the first round (licount=1) 
                  then try to find any unpaid invoice from customer */
               IF ttPayment.Origin = "RL" AND liCount = 1 THEN DO:
                  llFound = FALSE.
                  
                  FOR FIRST PaymPlan NO-LOCK WHERE
                            PaymPlan.PPlanID = liPPlanID,
                      FIRST PPInv OF PaymPlan NO-LOCK WHERE
                            PPInv.InvNum = Invoice.InvNum:
                     llFound = TRUE.
                  END.
                  
                  IF NOT llFound THEN NEXT. 
               END.
                
               /* else if paid with a normal reference, and invoice belongs
                  to a payment plan -> skip at 1 round */
               ELSE IF liCount = 1 AND Invoice.PaymState = 4 THEN DO:
                  llFound = TRUE.
                  
                  FOR EACH PPInv NO-LOCK WHERE
                           PPInv.InvNum = Invoice.InvNum,
                     FIRST PaymPlan OF PPInv NO-LOCK WHERE
                           PaymPlan.PPStatus = 3:

                     /* types 1 and 2 use normal reference */
                     IF PaymPlan.PPType = 3 THEN llFound = FALSE.
                  END.
                  
                  IF NOT llFound THEN NEXT. 
               END.
               
               /* already handled in this run */
               FIND FIRST ttTarget WHERE 
                          ttTarget.InvNum = Invoice.InvNum NO-ERROR.
               IF AVAILABLE ttTarget 
               THEN paybal = ttTarget.InvBal. 
               ELSE paybal = fCalcInvBal(BUFFER Invoice, 
                                         TODAY + 1000,
                                         /* disregard possible ePaym */
                                         FALSE) +
                             /* credit loss is treated like unpaid */    
                             fCredLossPaid(BUFFER Invoice,
                                           TODAY + 1000,
                                           /* not used */       
                                           OUTPUT lii).
 
               /* always try to unbook credit loss first, even if
                  intention was to pay another invoice 
                  -> disregard amount limits with credit loss invoices */
               IF Invoice.PaymState NE 3 AND
                  (paybal < ldMinAmt OR
                   paybal > ldMaxAmt)
               THEN NEXT.

               IF paybal <= 0 THEN NEXT. 
                     
               CREATE bPayment.
               BUFFER-COPY ttPayment TO bPayment.
               ASSIGN bPayment.AmtPaid  = MIN(paybal,ttPayment.AmtPaid)
                      bPayment.Inv      = Invoice.InvNum
                      bPayment.Double   = TRUE
                      ttPayment.AmtPaid = ttPayment.AmtPaid - 
                                          bPayment.AmtPaid.
   
               IF NOT AVAILABLE ttTarget THEN DO:
                  CREATE ttTarget.
                  ttTarget.InvNum = Invoice.InvNum.
               END.
               ttTarget.InvBal = paybal - bPayment.AmtPaid. 
                     
               /* all were booked to invoices */
               IF ttPayment.AmtPaid = 0 THEN DO:
                  DELETE ttPayment.
                  LEAVE FindUnpaid.
               END.
                     
            END.
                     
         END.
               
         /* if not enough of unpaid invoices were found to cover 
            the entire payment -> overpayment */
         IF AVAILABLE ttPayment   AND 
            ttPayment.AmtPaid > 0 AND
            ttPayment.Inv = 0 
         THEN DO:
                  
            FOR FIRST ttTarget,
                FIRST bPayment WHERE
                      bPayment.Inv       = ttTarget.InvNum     AND
                      bPayment.ArchiveID = ttPayment.ArchiveID:
                            
               bPayment.AmtPaid = bPayment.AmtPaid + ttPayment.AmtPaid.
               DELETE ttPayment.
            
               LEAVE.
            END.
                  
            /* if still no luck leave invoice nbr as 0 -> adv.payment */ 
         END.
               
      END.
   END. 

   /* invoice number was included in reference number */
   ELSE IF ttPayment.Inv > 0 THEN DO:
   
       FIND FIRST Invoice WHERE 
                  Invoice.Brand  = gcBrand AND
                  Invoice.InvNum = ttPayment.Inv 
       NO-LOCK NO-ERROR.
       IF AVAIL Invoice AND Invoice.CrInvNum = 0 
       THEN ttPayment.CustNum = Invoice.CustNum.
            
   END.
END.

