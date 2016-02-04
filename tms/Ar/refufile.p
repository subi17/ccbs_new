/* ----------------------------------------------------------------------
  MODULE .......: refufile
  TASK .........: Create a payment file from refund payments
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 30.03.04
  CHANGED ......: 01.09.04/aam accept only Sampo
                  03.11.04/aam nordea has different end part in header
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Ar/refufile.i}
{Func/fbankdata.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/timestamp.i}
{Func/refcode.i}

DEF INPUT-OUTPUT PARAMETER TABLE FOR ttPaym.

DEF INPUT  PARAMETER icBankAcc AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idtDate   AS DATE NO-UNDO.
DEF OUTPUT PARAMETER oiDone    AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER ocInfo   AS CHAR NO-UNDO. 

DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO. 
DEF VAR lcCompanyID AS CHAR NO-UNDO. 
DEF VAR liBatch     AS INT  NO-UNDO. 
DEF VAR liCnt       AS INT  NO-UNDO. 
DEF VAR ldTime      AS DEC  NO-UNDO.
DEF VAR lcPaymInfo  AS CHAR NO-UNDO. 
DEF VAR ldTotal     AS DEC  NO-UNDO. 
DEF VAR lcFileExt   AS CHAR NO-UNDO INIT ".txt".
DEF VAR lcBankName  AS CHAR NO-UNDO. 
DEF VAR lcNewLine   AS CHAR NO-UNDO. 

DEF STREAM sFile.

FUNCTION fDateConv RETURNS CHARACTER
   (idtDate AS DATE).

   RETURN STRING(YEAR(idtDate) MOD 100,"99")  +
          STRING(MONTH(idtDate),"99") +
          STRING(DAY(idtDate),"99").
END.

IF idtDate = ? THEN idtDate = TODAY.

FIND FIRST Company NO-LOCK.
lcCompanyID = REPLACE(Company.CompanyID,"-","").
DO liCnt = 1 TO LENGTH(lcCompanyID):
   IF LOOKUP(SUBSTRING(lcCompanyID,liCnt,1),"0,1,2,3,4,5,6,7,8,9") > 0
   THEN DO:
      lcCompanyID = SUBSTRING(lcCompanyID,liCnt).
      LEAVE.
   END.
END.
lcCompanyID = FILL("0",9 - LENGTH(lcCompanyID)) + lcCompanyID.

IF NOT icBankAcc BEGINS "8" THEN DO:
   ocInfo = "Refund is possible only through Sampo".
   RETURN.
END.

/* check company's bank account */
icBankAcc = fBankAcc2Data(icBankAcc).
IF NOT fCheckBankAcc(icBankAcc) THEN DO:
   ocInfo = "Company's bank account is invalid".
   RETURN.
END.    

CASE SUBSTRING(icBankAcc,5,1):
WHEN "1" OR
WHEN "2" THEN lcBankName = "nordea".
WHEN "3" THEN DO:
            CASE SUBSTRING(icBankAcc,6,1):
            WHEN "1" THEN lcBankName = "handels".
            WHEN "2" THEN lcBankName = "mandatum".
            WHEN "3" THEN lcBankName = "seb".
            WHEN "4" THEN lcBankName = "danske".
            WHEN "6" THEN lcBankName = "tapiola".
            OTHERWISE lcBankName = "".
            END CASE.
         END.
WHEN "4" THEN lcBankName = "sp".
WHEN "5" THEN lcBankName = "oko".
WHEN "8" THEN lcBankName = "sampo".
OTHERWISE lcBankName = "".
END CASE.

ASSIGN lcFile     = fCParamC("RefundFile") + "_" + lcBankName
       lcTransDir = fCParamC("RefundDir")
       lcNewLine  = CHR(13) + CHR(10).
       
IF lcFile = "" OR lcFile = ? OR
   lcTransDir = "" OR lcTransDir = ?
THEN DO:
   ocInfo = "File definitions incomplete".
   RETURN.
END.

FOR EACH ttPaym:

   FIND Payment WHERE Payment.Voucher = ttPaym.Voucher EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Payment OR Payment.Brand NE gcBrand THEN DO:
      ocInfo = "Payment " + STRING(ttPaym.Voucher) + " missing".
      RETURN.
   END.

   FIND Customer OF Payment NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      ocInfo = "Customer " + STRING(Payment.CustNum) + " missing".
      RETURN.
   END. 

   /* customers must have a bank account; first check from dd-authorization */
   RUN Ar/nnsvte (Customer.CustNum,
               TODAY, 
               OUTPUT ttPaym.CustBank).
   IF ttPaym.CustBank = "" THEN ttPaym.CustBank = Customer.BankAcc.

   IF ttPaym.CustBank = "" THEN DO:
      ocInfo = "Customer " + STRING(ttPaym.CustNum) + " has no bank account".
      RETURN.
   END. 
   
   Payment.BankAcc = ttPaym.CustBank.
   
   /* convert account into data format and validate it */
   ttPaym.CustBank = fBankAcc2Data(ttPaym.CustBank).
   IF NOT fCheckBankAcc(ttPaym.CustBank) THEN DO:
      ocInfo = "Customer " + STRING(ttPaym.CustNum) + 
                " has an invalid bank account".
      RETURN.
   END. 
     
   IF ttPaym.Amt < 0 THEN DO:
      ocInfo = "Negative sums are not allowed".
      RETURN.
   END. 
   
END.

ASSIGN liBatch = NEXT-VALUE(PaymVouch)
       lcFile  = lcFile + STRING(liBatch) + lcFileExt
       ocInfo  = "File creation was interrupted (" + lcFile + ")".
       ldTime  = fMakeTS().


OUTPUT STREAM sFile TO VALUE(lcFile).

/* header */
PUT STREAM sFile UNFORMATTED
   "LM03"
   "00"
   STRING(icBankAcc,"x(14)")
   STRING(lcCompanyId,"X(9)")
   fDateConv(TODAY)
   STRING(ENTRY(1,STRING(TIME,"HH:MM"),":"),"99")
   STRING(ENTRY(2,STRING(TIME,"HH:MM"),":"),"99")
   STRING(SUBSTRING(icBankAcc,1,1),"X(2)")
   fDateConv(idtDate)
   STRING(Company.CompName,"X(35)")
   STRING(STRING(liBatch),"X(35)")
   SPACE(17)
   "1".
   
/* nordea */
IF LOOKUP(SUBSTRING(icBankAcc,1,1),"1,2") > 0
THEN PUT STREAM sFile UNFORMATTED
   SPACE(88)
   "0"
   STRING(lcCompanyId,"X(10)")
   SPACE(66)
   lcNewLine.
                  
/* general */
ELSE PUT STREAM sFile UNFORMATTED
   SPACE(65)
   SPACE(100)
   lcNewLine.
   
/* payments */
FOR EACH ttPaym,
   FIRST Payment EXCLUSIVE-LOCK WHERE
         Payment.Voucher = ttPaym.Voucher,
   FIRST Customer OF Payment NO-LOCK:

   lcPaymInfo = fTeksti(505,Customer.Language).
   
   PUT STREAM sFile UNFORMATTED
   "LM03"
   "10"
   STRING(icBankAcc,"x(14)")
   STRING(Customer.CustName,"x(30)")
   STRING(Customer.Address,"x(20)")
   STRING(Customer.ZipCode + " " + Customer.PostOffice,"x(20)")
   STRING(ttPaym.CustBank,"x(14)")
   SPACE(3)
   "5"        
   STRING(lcPaymInfo,"X(70)")
   SPACE(2)
   fDateConv(idtDate)
   STRING(ttPaym.Amt * 100,"999999999999")
   SPACE(17)
   STRING(STRING(ttPaym.Voucher),"x(20)")
   SPACE(20)
   SPACE(5)
   SPACE(40)
   lcNewLine.

   ASSIGN Payment.ExpStamp = ldTime
          Payment.ExpUser  = katun
          Payment.PaymFile = STRING(liBatch)
          oiDone           = oiDone + 1
          ldTotal          = ldTotal + ttPaym.Amt.
    
   DELETE ttPaym.
         
END.   
   
/* tailer */
PUT STREAM sFile UNFORMATTED
   "LM03"
   "90"
   STRING(icBankAcc,"x(14)")
   STRING(lcCompanyID,"999999999")
   fDateConv(TODAY)
   STRING(oiDone,"999999")
   STRING(ldTotal * 100,"9999999999999")
   SPACE(25)
   STRING(STRING(liBatch),"X(35)")
   SPACE(17)
   SPACE(69)
   SPACE(100)
   lcNewLine.
   
OUTPUT STREAM sFile CLOSE.

ocInfo = "Batch nbr is " + STRING(liBatch).

/* move the new file to the actual transfer directory */
IF lcTransDir > "" THEN DO:
   IF NOT fTransDir(lcFile,
                    lcFileExt,
                    lcTransDir)
   THEN ocInfo = "File could not be moved to transfer directory (" +
                  lcTransDir + ")".
END.
   
   
   
   
