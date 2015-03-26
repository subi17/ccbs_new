{commali.i}
{timestamp.i}
{msisdn.i}
{forderstamp.i}
{orderfunc.i}

DEF INPUT  PARAMETER icFileType  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO.

DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR liCustNum   AS INT  NO-UNDO.
DEF VAR lcCustID    AS CHAR NO-UNDO.
DEF VAR lcCLI       AS CHAR NO-UNDO.
DEF VAR lcICC       AS CHAR NO-UNDO.
DEF VAR lcCLIType   AS CHAR NO-UNDO.
DEF VAR lcBEvent1   AS CHAR NO-UNDO.
DEF VAR lcBEvent2   AS CHAR NO-UNDO.
DEF VAR lcBEvent3   AS CHAR NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO INIT "|".
DEF VAR lcCLIRange  AS CHAR NO-UNDO EXTENT 2.
DEF VAR lcICCRange  AS CHAR NO-UNDO EXTENT 2. 
DEF VAR ldCurrent   AS DEC  NO-UNDO.
DEF VAR lcSource    AS CHAR NO-UNDO.
DEF VAR lcChannel   AS CHAR NO-UNDO.
DEF VAR lcSalesman  AS CHAR NO-UNDO.
DEF VAR lcOrderer   AS CHAR NO-UNDO INIT "Yoigo".
DEF VAR lcError     AS CHAR NO-UNDO.
DEF VAR lcTax       AS CHAR NO-UNDO.
DEF VAR lcHeader    AS CHAR NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcMemo      AS CHAR NO-UNDO.
DEF VAR lcTypeDesc  AS CHAR NO-UNDO.
DEF VAR lcSimStock  AS CHAR NO-UNDO.
DEF VAR lcCLIStock  AS CHAR NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.


FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):
   
   IF lcError > "" THEN RETURN FALSE. 
   
   PUT STREAM sLog UNFORMATTED
      lcLine 
      lcSep
      "ERROR:" 
      icMessage
      SKIP.
   
   oiErrors = oiErrors + 1.

   lcError = icMessage.
     
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

CASE icFileType:

WHEN "gift" THEN ASSIGN 
   lcSource   = "Script"
   lcChannel  = "YOIGO"
   lcSalesman = "GIFT"
   lcTypeDesc = "GIFT"
   lcSimStock = "GIFT"
   lcCLIStock = "GIFT".

WHEN "preact" THEN ASSIGN
   lcSource   = "Script"
   lcChannel  = "PRE-ACT"
   lcSalesman = "YOIGO"
   lcTypeDesc = "PRE-ACTIVE"
   lcSimStock = "PREACTIV"
   lcCLIStock = "PREACTIVATED".
   
OTHERWISE RETURN "ERROR:Unknown file type".
END CASE. 

ASSIGN 
   lcCLIRange[2] = "9999999999"
   lcICCRange[2] = FILL("9",20).

INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(icLogFile) APPEND.

ldCurrent = fMakeTS().

PUT STREAM sLog UNFORMATTED
   "File: " icFile
   SKIP
   "Started: " 
   fTS2HMS(ldCurrent)
   SKIP.

ASSIGN
   liCnt       = R-INDEX(icFile,"/")
   lcPlainFile = icFile.
   
IF liCnt > 1 THEN 
   lcPlainFile = SUBSTRING(lcPlainFile,liCnt + 1).


REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   ASSIGN
      oiRead  = oiRead + 1
      lcError = "".
   
   ASSIGN 
      liCustNum = INTEGER(ENTRY(1,lcLine,lcSep))
      lcCustID  = ENTRY(2,lcLine,lcSep)
      lcCLI     = ENTRY(3,lcLine,lcSep)
      lcICC     = ENTRY(4,lcLine,lcSep)
      lcCLIType = ENTRY(5,lcLine,lcSep)
      lcBEvent1 = ENTRY(6,lcLine,lcSep)
      lcBEvent2 = ENTRY(7,lcLine,lcSep)
      lcBEvent3 = ENTRY(8,lcLine,lcSep)
      lcTax     = ENTRY(9,lcLine,lcSep) 
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Invalid data format").
      NEXT.
   END.
  
   FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      fError("Customer number not found").
   END.

   IF Customer.OrgID NE lcCustID THEN DO:
      fError("Customer ID not found").
   END.

   IF CAN-FIND(MobSub WHERE MobSub.CLI = lcCLI) THEN DO:
      fError("Mobsub already in system").
   END.

   FOR FIRST Order NO-LOCK WHERE 
             Order.CLI = lcCLI AND
             LOOKUP(Order.StatusCode,"2,7") = 0:
      fError("Order already exists").
   END.
 
   FIND CLIType WHERE
        CLIType.Brand   = gcBrand AND
        CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CLIType THEN DO:
      fError("CLIType not found").
   END.
   
   FIND FIRST MSISDN WHERE 
              MSISDN.Brand = gcBrand AND 
              MSISDN.CLI = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MSISDN THEN DO:
      fError("No MSISDN/ICC found").
   END.
   
   ELSE DO:
      IF MSISDN.StatusCode NE 1 THEN DO:
         fError("MSISDN in wrong status").
      END.
      
      ELSE IF MSISDN.Pos NE lcCLIStock THEN DO:
         fError("MSISDN in wrong stock").
      END.
   END.
   
   FIND Sim WHERE Sim.ICC = lcICC NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Sim THEN DO:
      fError("No MSISDN/ICC found").
   END.
   
   ELSE DO:
      IF Sim.SimStat NE 1 THEN DO:
         fError("ICC in wrong status").
      END.

      ELSE IF Sim.Stock NE lcSimStock THEN DO:
         fError("ICC in wrong stock").
      END.
   END.

   IF lcBEvent1 > "" THEN DO:
   
      FIND FeeModel WHERE
           FeeModel.Brand = gcBrand AND
           FeeModel.FeeModel = lcBEvent1 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FeeModel THEN DO:
         fError("Billing event not found").
      END.
   END.
   
   IF lcError > "" 
   THEN ASSIGN
      lcHeader = lcTypeDesc + " Order Error"
      lcMemo   = lcError.
   ELSE DO:
      lcHeader = lcTypeDesc + " Order OK".
      
      IF icFileType = "gift" 
      THEN lcMemo = "Christmas gift".
      ELSE lcMemo = "Pre-activated".
      
      lcMemo = lcMemo +  
               " subscription created via script in order " +
               "file " + lcPlainFile + "." + CHR(10).
               
      IF icFileType = "gift" 
      THEN lcMemo = lcMemo + "Suscripción regalo de Navidad".
      ELSE lcMemo = lcMemo + "Suscripción de tarjeta SIM preactivada".
      
      lcMemo = lcMemo +       
               " generada por script en fichero de pedido " + 
               lcPlainFile + ".".
   END.
   
   CREATE Order.
   ASSIGN
      Order.Brand           = gcBrand
      Order.OrderId         = NEXT-VALUE(OrderId)
      Order.MsSeq           = NEXT-VALUE(MobSub)
      Order.CLI             = lcCLI
      Order.ICC             = lcICC
      Order.CLIType         = lcCLIType
      Order.PayType         = (CLIType.PayType = 2)
      Order.CustNum         = liCustNum
      Order.Salesman        = lcSalesman
      Order.Source          = lcSource
      Order.OrderChannel    = lcChannel
      Order.Orderer         = lcOrderer
      Order.CrStamp         = ldCurrent
      Order.InvCustRole     = 1
      Order.UserRole        = 1
      Order.FeeModel        = lcBEvent1.

      /* Remove the hardcoding and call the function to update the statuscode and mark the timestamp */
      IF lcError > "" THEN
         fSetOrderStatus(Order.OrderId,"2").
      ELSE fSetOrderStatus(Order.OrderId,"1").

      fMarkOrderStamp(Order.OrderID,"Change",0.0).
        
    CREATE OrderCustomer.
    ASSIGN 
       OrderCustomer.Brand        = gcBrand     
       OrderCustomer.OrderID      = Order.OrderID
       OrderCustomer.RowType      = 1
       OrderCustomer.CustNum      = liCustNum
       OrderCustomer.FirstName    = Customer.FirstName
       OrderCustomer.SurName1     = Customer.CustName
       OrderCustomer.SurName2     = Customer.SurName2
       OrderCustomer.Company      = Customer.CompanyName
       OrderCustomer.Address      = Customer.Address
       OrderCustomer.Street       = Customer.Address 
       OrderCustomer.ZipCode      = Customer.ZipCode
       OrderCustomer.PostOffice   = Customer.PostOffice
       OrderCustomer.Country      = Customer.Country

       OrderCustomer.CustTitle    = Customer.HonTitle
       OrderCustomer.EMail        = Customer.EMail
       OrderCustomer.BankCode     = Customer.BankAcc
       OrderCustomer.Category     = Customer.Category
       OrderCustomer.Region       = Customer.Region
       OrderCustomer.Nationality  = Customer.Nationality
       OrderCustomer.Language     = STRING(Customer.Language)
       OrderCustomer.FixedNumber  = Customer.Phone
       OrderCustomer.MobileNumber = Customer.SMSNumber
       OrderCustomer.CustIDType   = Customer.CustIDType
       OrderCustomer.CustID       = Customer.OrgId
       OrderCustomer.BirthDay     = Customer.BirthDay
       OrderCustomer.Sex          = Customer.Sex

       /* marketing values */
       OrderCustomer.OperSMSMarketing   = Customer.DirMarkSMS
       OrderCustomer.OperEMailMarketing = Customer.DirMarkEmail
       OrderCustomer.OperPostMarketing  = Customer.DirMarkPOST
       OrderCustomer.OutSMSMarketing    = Customer.OutMarkSMS
       OrderCustomer.OutEMailMarketing  = Customer.OutMarkEmail
       OrderCustomer.OutPostMarketing   = Customer.OutMarkPOST.
       
    /* payment on delivery */
    CREATE OrderPayment.
    ASSIGN
       OrderPayment.Brand   = gcBrand
       OrderPayment.OrderId = Order.OrderId
       OrderPayment.Method  = 1. 


    CREATE Memo.
    ASSIGN 
       Memo.Brand     = gcBrand
       Memo.HostTable = "Order"
       Memo.KeyValue  = STRING(Order.OrderID)
       Memo.CustNum   = Order.CustNum
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun 
       Memo.MemoTitle = lcHeader
       Memo.MemoText  = lcMemo
       Memo.CreStamp  = ldCurrent.

   /* reserve msisdn for this order */
   IF Order.StatusCode = "1" THEN DO: 
      fMakeMsidnHistory(RECID(MSISDN)).
      ASSIGN 
         MSISDN.StatusCode = 2
         MSISDN.OrderID    = Order.OrderID
         MSISDN.MsSeq      = Order.MsSeq.
   END.
   
END.


PUT STREAM sLog UNFORMATTED
   "Ended: " 
   STRING(TODAY,"99.99.9999") " " STRING(TIME,"hh:mm:ss")
   SKIP
   "Read: "
   oiRead 
   " Succesful: "
   oiRead - oiErrors
   " Errors: " 
   oiErrors
   SKIP.
   
   
