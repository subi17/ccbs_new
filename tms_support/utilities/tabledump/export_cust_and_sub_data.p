/*
   The program will export (dump) data for customers whose numbers are
   in the file specified in variable gcInputFile.
   
   The file can contain either individual customers
   or range of customers (for example 466-556). There can be multiple lines
   and in one line multiple entries delimited with a comma character.

   The dump files contains a scrambled data for the customer and
   ordercustomer (rowtype 1).
*/

DEFINE VARIABLE gcInputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDumpDirectory   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcMaleListFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFemaleListFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSurNameListFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcTenant          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llActiveSubs      AS LOGICAL NO-UNDO.

DEFINE VARIABLE liCollectMax AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCollected AS INTEGER NO-UNDO. 

ASSIGN
   gcTenant          = "masmovil"
   gcInputFile       = gcTenant + "_custlist.txt"
   gcDumpDirectory   = gcTenant + "_dump_data"
   gcMaleListFile    = "malenamelist.txt" 
   gcFemaleListFile  = "femalenamelist.txt" 
   gcSurNameListFile = "surnamelist.txt" 
   liCollectMax    = 10
   llActiveSubs = true.

/* No need to touch lines below... */

FILE-INFO:FILE-NAME = gcDumpDirectory.
IF FILE-INFO:FILE-TYPE = ? OR INDEX(FILE-INFO:FILE-TYPE,"D") = 0
THEN DO:
   MESSAGE "Invalid dump directory (not available or not a directory)"
   VIEW-AS ALERT-BOX.
   RETURN.
END. 

DEFINE STREAM instr.
DEFINE STREAM outstr.
DEFINE STREAM outstrdyn.
DEFINE STREAM outstrc.
DEFINE STREAM outstro.

DEFINE VARIABLE giMaleCount    AS INTEGER NO-UNDO.
DEFINE VARIABLE giFemaleCount  AS INTEGER NO-UNDO.
DEFINE VARIABLE giSurNameCount AS INTEGER NO-UNDO.
DEFINE VARIABLE ghQuery        AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
   INDEX CustNum IS PRIMARY UNIQUE CustNum.

DEFINE TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer
   INDEX OrderID IS PRIMARY Brand OrderID.

DEFINE TEMP-TABLE ttScramble NO-UNDO
   FIELD ScrambleType AS CHARACTER
   FIELD ScrambleId   AS INTEGER
   FIELD ScrambleName AS CHARACTER
   INDEX ScrambleId IS PRIMARY UNIQUE ScrambleType ScrambleId
   INDEX ScrambleName IS UNIQUE ScrambleType ScrambleName.


FUNCTION fGetScrambleName RETURNS CHARACTER
   ( icType AS CHARACTER ):

   DEFINE VARIABLE lii AS INTEGER NO-UNDO.
   
   CASE icType:
      WHEN "MALE"
      THEN lii = giMaleCount.
      WHEN "FEMALE"
      THEN lii = giFemaleCount.
      WHEN "SURNAME"
      THEN lii = giSurNameCount.
      OTHERWISE RETURN "".      
   END CASE.
   
   lii = RANDOM(1,lii).
   
   FOR ttScramble WHERE
      ttScramble.ScrambleType = icType AND
      ttScramble.ScrambleId   = lii:
      
      RETURN ttScramble.ScrambleName.  
   END.
   
   RETURN "".

END FUNCTION.


FUNCTION fRandomEmail RETURNS CHARACTER:

   DEFINE VARIABLE lii AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcc AS CHARACTER NO-UNDO.
   
   DO lii = 1 TO 8:
      lcc = lcc + SUBSTRING("abcdefghijklmnopqrstuvwxyz",RANDOM(1,25),1).
   END.
   
   RETURN lcc + "@" + "testnoaddr.com".

END FUNCTION.


FUNCTION fGetRandomCustomerID RETURNS CHARACTER
   ( icType AS CHARACTER ):
   
   DEFINE VARIABLE lcCustomerID AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lii          AS INTEGER   NO-UNDO. 

   CASE icType:
      WHEN "nif" THEN DO:
         
         DO lii = 1 TO 8:
            lcCustomerID = lcCustomerID + 
                           substring("1234567890",RANDOM(1,10),1).
         END.
         
         lcCustomerID = lcCustomerID + 
            substring("TRWAGMYFPDXBNJZSQVHLCKET",
            int(lcCustomerID) MOD 23 + 1,1).
      END.
      WHEN "nie" THEN DO:
         
         DO lii = 1 TO 7:
            lcCustomerID = lcCustomerID + 
                           substring("1234567890",RANDOM(1,10),1).
         END.
         lcCustomerID = "X" + lcCustomerID +
            substring("TRWAGMYFPDXBNJZSQVHLCKET",
            int(lcCustomerID) MOD 23 + 1,1).
      END. 
      WHEN "passport" THEN DO:
         
         DO lii = 1 TO 9:
            lcCustomerID = lcCustomerID + 
                           substring("1234567890",RANDOM(1,10),1).
         END.
      END. 
      WHEN "cif" THEN DO:
         
         lcCustomerID = SUBSTRING("ABCDEFGHIJKLMNOPQRSTUVWXY",RANDOM(1,25),1).
         DO lii = 1 TO 8:
            lcCustomerID = lcCustomerID + 
                           substring("1234567890",RANDOM(1,10),1).
         END.
      END. 
      OTHERWISE DO:
         lcCustomerID = "UNKNOWN".
      END.
   END.

   RETURN lcCustomerID.

END.


FUNCTION fGetRandomAccountID RETURNS CHARACTER:

   DEFINE VARIABLE lii AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcAccount AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liDigit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE multiplier AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liSum AS INTEGER NO-UNDO. 
   DEFINE VARIABLE account_chk AS INTEGER NO-UNDO. 

   DO lii = 1 TO 10:
      CASE lii:
         WHEN 1 THEN multiplier = 1.
         WHEN 2 THEN multiplier = 2.
         WHEN 3 THEN multiplier = 4.
         WHEN 4 THEN multiplier = 8.
         WHEN 5 THEN multiplier = 5.
         WHEN 6 THEN multiplier = 10.
         WHEN 7 THEN multiplier = 9.
         WHEN 8 THEN multiplier = 7.
         WHEN 9 THEN multiplier = 3.
         WHEN 10 THEN multiplier = 6.
      END.
      ASSIGN
         liDigit = RANDOM(0,9) 
         lcAccount = lcAccount + string(liDigit)
         liSum = liSum + (liDigit * multiplier).
   END.

   account_chk = liSum MOD 11.
   IF account_chk > 1 THEN account_chk = 11 - account_chk.

   RETURN (STRING(account_chk) + lcAccount).

END.


FUNCTION fCreatettCustomer RETURNS LOGICAL
   ( iiCustNum AS INTEGER ):

   DEFINE VARIABLE liSubs AS INTEGER NO-UNDO. 

   FIND ttCustomer WHERE ttCustomer.CustNum = iiCustNum NO-ERROR.

   IF AVAILABLE ttCustomer
   THEN RETURN TRUE.

   FIND Customer NO-LOCK WHERE Customer.CustNum = iiCustNum NO-ERROR.

   IF NOT AVAILABLE Customer OR Customer.Brand NE "1"
   THEN RETURN TRUE. 

   IF llActiveSubs AND 
      NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE
                         MobSub.custnum = iiCustNum) THEN RETURN TRUE.


   FOR EACH mobsub NO-LOCK where
            mobsub.custnum = iiCustNum :
      liSubs = liSubs + 1.
   end.

   /* too heavy to dump */
   IF liSubs > 5 THEN RETURN TRUE.

   IF liCollected >= liCollectMax THEN RETURN TRUE.
   liCollected = liCollected + 1.

   DEFINE VARIABLE lcBankAcct   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOcBankAcct AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrigID     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFirstName  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustName   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSurName2   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEmail      AS CHARACTER NO-UNDO.

   ASSIGN
      lcBankAcct  = SUBSTRING(Customer.BankAcct,1,9) + fGetRandomAccountID() WHEN Customer.BankAcct > ""
      lcOrigId    = fGetRandomCustomerID(Customer.CustIdType)
      lcFirstName = fGetScrambleName(IF LOOKUP(Customer.HonTitle,"Sr.,Mr.,Sr,Mr") > 0
                                     THEN "MALE" ELSE "FEMALE")
      lcCustName  = fGetScrambleName("SURNAME")
      lcSurName2  = "DEV " + gcTenant.
      lcEmail     = fRandomEmail()
      .

   IF lcOrigID = "UNKNOWN"
   THEN RETURN TRUE.

   CREATE ttCustomer.
   BUFFER-COPY Customer
      EXCEPT
         BankAcct
         Orgid 
         Firstname
         Custname
         Surname2
         Email
      TO ttCustomer
      ASSIGN
         ttCustomer.BankAcct  = lcBankAcct
         ttCustomer.Orgid     = lcOrigID
         ttCustomer.Firstname = lcFirstName
         ttCustomer.Custname  = lcCustName
         ttCustomer.Surname2  = lcSurName2
         ttCustomer.Email     = lcEmail
         .

   FOR EACH OrderCustomer NO-LOCK WHERE
      OrderCustomer.CustNum = Customer.CustNum AND
      OrderCustomer.RowType = 1:

      ASSIGN
         lcOcBankAcct = ""
         lcOcBankAcct = SUBSTRING(Ordercustomer.Bank,1,9) + fGetRandomAccountID() WHEN Ordercustomer.Bank > ""
         .
   
      CREATE ttOrderCustomer.
      BUFFER-COPY OrderCustomer
         EXCEPT
            CustIdtype
            Custid
            Firstname
            Surname1
            Surname2
            Bank
            Email
         TO ttOrderCustomer
         ASSIGN
            ttOrderCustomer.CustIdtype = Customer.CustIdType
            ttOrderCustomer.Custid     = lcOrigID
            ttOrderCustomer.Firstname  = lcFirstName
            ttOrderCustomer.Surname1   = lcCustName
            ttOrderCustomer.Surname2   = lcSurName2
            ttOrderCustomer.Bank       = lcOcBankAcct
            ttOrderCustomer.Email      = lcEmail
            .            
   END.

   RETURN FALSE.

END.


FUNCTION fLoadScrambleData RETURNS INTEGER
   ( icType AS CHARACTER,
     icFile AS CHARACTER ):

   DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liId   AS INTEGER   NO-UNDO.
   
   INPUT STREAM instr FROM VALUE(icFile).   
   
   REPEAT:
      
      IMPORT STREAM instr UNFORMATTED lcLine.
   
      FIND ttScramble WHERE
         ttScramble.ScrambleType = icType AND
         ttScramble.ScrambleName = lcLine
      NO-ERROR.
      
      IF AVAILABLE ttScramble
      THEN NEXT.
      
      CREATE ttScramble.
      
      ASSIGN
         liId = liId + 1
         ttScramble.ScrambleType = icType
         ttScramble.ScrambleName = lcLine          
         ttScramble.ScrambleId   = liId
         .

   END.

   RETURN liId.

   FINALLY:   
      INPUT STREAM instr CLOSE.
   END FINALLY.

END FUNCTION.


PROCEDURE pPopulatettCustomer:
   
   INPUT STREAM instr FROM VALUE(gcInputFile).
   
   DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lii AS INTEGER NO-UNDO.
   DEFINE VARIABLE lij AS INTEGER NO-UNDO.
   DEFINE VARIABLE liFrom AS INTEGER NO-UNDO.
   DEFINE VARIABLE liTo   AS INTEGER NO-UNDO.
     
   REPEAT:

      IMPORT STREAM instr UNFORMATTED lcLine.
      DISPLAY "Processing line:" lcLine.

      DO lii = 1 TO NUM-ENTRIES(lcLine):
         IF NUM-ENTRIES(ENTRY(lii,lcLine),"-") > 1
         THEN ASSIGN
                  liFrom = INTEGER(ENTRY(1,ENTRY(lii,lcLine),"-"))
                  liTo   = INTEGER(ENTRY(2,ENTRY(lii,lcLine),"-"))
                  .
         ELSE ASSIGN
                  liFrom = INTEGER(ENTRY(lii,lcLine))
                  liTo   = liFrom
                  . 
   
         DO lij = liFrom TO liTo:
            fCreatettCustomer(lij).
         END.
      END.

   END.

   liCollected = 0.
   
   FINALLY:   
      INPUT STREAM instr CLOSE.
   END FINALLY.
   
END PROCEDURE.


FUNCTION fDynExport RETURNS CHARACTER
   ( INPUT ihBufHandle  AS HANDLE ):

   DEFINE VARIABLE lhFld     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE liCnt     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liExtnt   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcArray   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResult  AS CHARACTER NO-UNDO.

   IF ihBufHandle:TYPE <> "BUFFER"
   THEN RETURN ?.

   DO liCnt = 1 TO ihBufHandle:NUM-FIELDS:

      ASSIGN lhFld = ihBufHandle:BUFFER-FIELD(liCnt).

      IF lhFld:DATA-TYPE = "clob" OR lhFld:DATA-TYPE = "blob"
      THEN NEXT.

      IF lhFld:EXTENT = 0
      THEN DO:
         IF lhFld:BUFFER-VALUE = ?
         THEN lcTmp = "?".
         ELSE
         CASE lhFld:DATA-TYPE:
            WHEN "character"
            THEN lcTmp = QUOTER(lhFld:BUFFER-VALUE).
            WHEN "raw"
            THEN lcTmp = '"' + STRING(lhFld:BUFFER-VALUE) + '"'.
            WHEN "datetime" OR WHEN "datetime-tz"
            THEN lcTmp = STRING(YEAR(lhFld:BUFFER-VALUE),"9999") + "-" +
                        STRING(MONTH(lhFld:BUFFER-VALUE),"99")  + "-" +
                        STRING(DAY(lhFld:BUFFER-VALUE),"99")    + "T" +
                        SUBSTRING(STRING(lhFld:BUFFER-VALUE),12).
            OTHERWISE lcTmp = STRING(lhFld:BUFFER-VALUE).
         END CASE.

         lcResult = lcResult + lcTmp + " ".
      END.
      ELSE DO:
         lcArray = "".
         DO liExtnt = 1 TO lhFld:EXTENT:
            IF lhFld:BUFFER-VALUE(liExtnt) = ?
            THEN lcTmp = "?".
            ELSE
            CASE lhFld:DATA-TYPE:
               WHEN "character"
               THEN lcTmp = QUOTER(lhFld:BUFFER-VALUE(liExtnt)).
               WHEN "raw"
               THEN lcTmp = '"' + STRING(lhFld:BUFFER-VALUE(liExtnt)) + '"'.
               WHEN "datetime" OR WHEN "datetime-tz"
               THEN lcTmp = STRING(YEAR(lhFld:BUFFER-VALUE(liExtnt)),"9999") + "-" +
                           STRING(MONTH(lhFld:BUFFER-VALUE(liExtnt)),"99")  + "-" +
                           STRING(DAY(lhFld:BUFFER-VALUE(liExtnt)),"99")    + "T" +
                           SUBSTRING(STRING(lhFld:BUFFER-VALUE(liExtnt)),12).
               OTHERWISE lcTmp = STRING(lhFld:BUFFER-VALUE(liExtnt)).
            END CASE.
           lcArray = lcArray + lcTmp + " ".
         END.
         lcResult = lcResult + RIGHT-TRIM(lcArray," ") + " ".
      END.
   END.

   RETURN RIGHT-TRIM(lcResult," ").

END FUNCTION.


FUNCTION fExportQuery RETURNS LOGICAL
   ( ihBufferObject AS HANDLE,
     icWhereClause  AS CHARACTER ):

   DEFINE VARIABLE llOK           AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcDumpFileName AS CHARACTER NO-UNDO.

   lcDumpFileName = LC(ihBufferObject:TABLE) + ".d".
   
   IF lcDumpFileName BEGINS "tt"
   THEN lcDumpFileName = SUBSTRING(lcDumpFileName,3).

   OUTPUT STREAM outstrdyn TO VALUE(gcDumpDirectory + "/" + 
                                    lcDumpFileName) APPEND.

   ghQuery:SET-BUFFERS(ihBufferObject).
   ghQuery:QUERY-PREPARE("FOR EACH " + ihBufferObject:TABLE + " NO-LOCK WHERE " + icWhereClause).
   ghQuery:QUERY-OPEN().
   
   DO WHILE TRUE:
   
      llOK = ghQuery:GET-NEXT(NO-LOCK).
   
      /* Query handle is invalid, no more records, or query is not open */
      IF llOK = ? OR NOT llOK
      THEN LEAVE.
   
      PUT STREAM outstrdyn UNFORMATTED fDynExport(ihBufferObject) SKIP.

   END.

   RETURN FALSE.

   FINALLY:
      ghQuery:QUERY-CLOSE().
      OUTPUT STREAM outstrdyn CLOSE.
   END.

END FUNCTION.


PROCEDURE pExportCustomersAndDataRelated:
   
   CREATE QUERY ghQuery.
   
   OUTPUT STREAM outstrc TO VALUE(gcDumpDirectory + "/customer.d") APPEND.
   
   DEFINE VARIABLE lcWhere           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcWhereWithBrand  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaDateSt         AS DATE      NO-UNDO.
   
   IF MONTH(TODAY) = 1
   THEN ldaDatest = DATE(12,1,YEAR(TODAY) - 1).
   ELSE ldaDateSt = DATE(MONTH(TODAY) - 1, 1, YEAR(TODAY)).
   
   FOR EACH ttCustomer:

      liCollected = liCollected + 1.
      
      PAUSE 0.
      DISPLAY "Processing customer: " ttCustomer.CustNum liCollected "/" liCollectMax WITH 1 DOWN ROW 1.

      FIND FIRST MobSub NO-LOCK WHERE
         MobSub.Brand = "1" AND
         MobSub.CustNum = ttCustomer.CustNum
      NO-ERROR.
      
      IF NOT AVAILABLE MobSub
      THEN NEXT.

      EXPORT STREAM outstrc ttCustomer.

      lcWhere = SUBSTITUTE("CustNum = &1", ttCustomer.CustNum).
      fExportQuery(BUFFER BillTarget:HANDLE, lcWhere).
      fExportQuery(BUFFER Limit:HANDLE, lcWhere).
      fExportQuery(BUFFER SingleFee:HANDLE, lcWhere).
      fExportQuery(BUFFER CustomerReport:HANDLE, lcWhere).
      fExportQuery(BUFFER CustBal:HANDLE, lcWhere).
      fExportQuery(BUFFER MServicelPool:HANDLE, lcWhere).

      lcWhere = SUBSTITUTE("InvCust = &1 AND DateSt >= &2", ttCustomer.CustNum, ldaDateSt).
      fExportQuery(BUFFER MobCDR:HANDLE, lcWhere).
      
      OUTPUT STREAM outstr TO VALUE(gcDumpDirectory + "/invoice.d") APPEND.
      FOR EACH Invoice NO-LOCK WHERE
         Invoice.brand    = "1"                AND
         Invoice.custnum  = ttCustomer.CustNum AND
         Invoice.invdate >= 6/1/2016:

         IF Invoice.Invtype NE 1 AND
            Invoice.Invtype NE 5
         THEN NEXT.

         EXPORT STREAM outstr Invoice.

         lcWhere = SUBSTITUTE("InvNum = &1", Invoice.InvNum).
         fExportQuery(BUFFER SubInvoice:HANDLE, lcWhere).
         fExportQuery(BUFFER InvRow:HANDLE, lcWhere).
         fExportQuery(BUFFER InvASub:HANDLE, lcWhere).
         fExportQuery(BUFFER Payment:HANDLE, lcWhere).
         fExportQuery(BUFFER InvRowCounter:HANDLE, lcWhere).
      END.
      OUTPUT STREAM outstr CLOSE.

      OUTPUT STREAM outstr TO VALUE(gcDumpDirectory + "/invoicetargetgroup.d") APPEND.
      FOR EACH InvoiceTargetGroup NO-LOCK WHERE
         InvoiceTargetGroup.CustNum = ttCustomer.CustNum:

         EXPORT STREAM outstr invoicetargetgroup.

         lcWhere = SUBSTITUTE("ITGroupID = &1", InvoiceTargetGroup.ITGroupID).
         
         fExportQuery(BUFFER InvoiceTarget:HANDLE, lcWhere).
      END.
      OUTPUT STREAM outstr CLOSE.

      lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND HostTable = 'Customer' AND KeyValue = '&3'", ttCustomer.CustNum).
      fExportQuery(BUFFER PIndicator:HANDLE, lcWhereWithBrand).

      OUTPUT STREAM outstr TO VALUE(gcDumpDirectory + "/mobsub.d") APPEND.

      FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand = "1" AND
         MobSub.CustNum = ttCustomer.CustNum:

         EXPORT STREAM outstr MobSub.

         lcWhere = "&1 = '&2'".
         fExportQuery(BUFFER SIM:HANDLE, SUBSTITUTE(lcWhere,"ICC",MobSub.ICC)).
         fExportQuery(BUFFER IMSI:HANDLE, SUBSTITUTE(lcWhere,"IMSI",MobSub.IMSI)).
         lcWhere = SUBSTITUTE("CLI = '&1'", MobSub.CLI).
         fExportQuery(BUFFER MSISDN:HANDLE, lcWhere).
         fExportQuery(BUFFER CallAlarm:HANDLE, lcWhere).
         
         ASSIGN
            lcWhere = SUBSTITUTE("MsSeq = &1", MobSub.MsSeq)
            lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND MsSeq = &1", MobSub.MsSeq)
            .         
         
         fExportQuery(BUFFER SubSer:HANDLE, lcWhere).
         fExportQuery(BUFFER MsRequest:HANDLE, lcWhere).
         fExportQuery(BUFFER FaTime:HANDLE, lcWhereWithBrand).
         fExportQuery(BUFFER MServiceLimit:HANDLE, lcWhere).
         fExportQuery(BUFFER MServicelPool:HANDLE, lcWhere).
         fExportQuery(BUFFER MSBalance:HANDLE, lcWhere).
         fExportQuery(BUFFER MinConsumption:HANDLE, lcWhere).
         fExportQuery(BUFFER SaldoCounter:HANDLE, lcWhere).
         fExportQuery(BUFFER ServiceLCounter:HANDLE, lcWhere).
         fExportQuery(BUFFER SubsTerminal:HANDLE, lcWhere).
         fExportQuery(BUFFER PrepaidRequest:HANDLE, lcWhereWithBrand).
         fExportQuery(BUFFER SlCounterItem:HANDLE, lcWhere).
         fExportQuery(BUFFER MSOwner:HANDLE, lcWhere).
         fExportQuery(BUFFER DCCLI:HANDLE, lcWhere).
         fExportQuery(BUFFER DCCounter:HANDLE, lcWhere).
         fExportQuery(BUFFER Segmentation:HANDLE, lcWhere).
         fExportQuery(BUFFER TMCounter:HANDLE, lcWhere).
         fExportQuery(BUFFER SoLog:HANDLE, lcWhere).
         fExportQuery(BUFFER Limit:HANDLE, lcWhere).
         fExportQuery(BUFFER InvSeq:HANDLE, lcWhere).

         lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND HostTable = 'MobSub' AND KeyValue = '&1'", MobSub.MsSeq).         
         fExportQuery(BUFFER Memo:HANDLE, lcWhereWithBrand).
         fExportQuery(BUFFER PIndicator:HANDLE, lcWhereWithBrand).
         fExportQuery(BUFFER FixedFee:HANDLE, lcWhereWithBrand).

         FOR EACH FixedFee NO-LOCK WHERE
             FixedFee.Brand     = "1"      AND
             FixedFee.HostTable = "Mobsub" AND
             FixedFee.KeyValue  = STRING(Mobsub.MSSeq):

            lcWhere = SUBSTITUTE("FFNum = &1", FixedFee.FFNum).
            fExportQuery(BUFFER FFItem:HANDLE, lcWhere).

         END.

         OUTPUT STREAM outstro TO VALUE(gcDumpDirectory + "/order.d") APPEND.

         FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = MobSub.MsSeq:

            EXPORT STREAM outstro Order.

            lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND OrderID = &1", Order.OrderID).
            fExportQuery(BUFFER OrderAccessory:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER OrderAction:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER OrderPayment:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER OrderTimeStamp:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER OrderTopUp:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER OrderFusion:HANDLE, lcWhereWithBrand).
            fExportQuery(BUFFER CoTarg:HANDLE, lcWhereWithBrand).

            lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND OrderID = &1", Order.OrderID).
            fExportQuery(BUFFER ttOrderCustomer:HANDLE, lcWhereWithBrand).

            lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND OrderID = &1 AND RowType = 1 AND CustNum NE &2", Order.OrderID, ttCustomer.CustNum).
            fExportQuery(BUFFER OrderCustomer:HANDLE, lcWhereWithBrand).

            lcWhereWithBrand = SUBSTITUTE("Brand = '1' AND OrderID = &1 AND RowType > 1", Order.OrderID).
            fExportQuery(BUFFER OrderCustomer:HANDLE, lcWhereWithBrand).

            lcWhere = SUBSTITUTE("OrderID = &1", Order.OrderID).
            fExportQuery(BUFFER MNPProcess:HANDLE, lcWhere).

      
            FOR EACH MNPProcess NO-LOCK WHERE
               MNPProcess.OrderID = Order.OrderId :
               
               lcWhere = SUBSTITUTE("MNPSeq = &1", MNPProcess.MNPSeq).
               fExportQuery(BUFFER MNPSub:HANDLE, lcWhere).
               fExportQuery(BUFFER MNPDetails:HANDLE, lcWhere).
               fExportQuery(BUFFER MNPOperation:HANDLE, lcWhere).               
               
            END.
      
            IF Order.Invnum > 0
            THEN DO:
               lcWhere = SUBSTITUTE("InvNum = &1", Order.InvNum).
               fExportQuery(BUFFER Invoice:HANDLE, lcWhere).
               fExportQuery(BUFFER SubInvoice:HANDLE, lcWhere).
               fExportQuery(BUFFER InvRow:HANDLE, lcWhere).
               fExportQuery(BUFFER InvASub:HANDLE, lcWhere).
               fExportQuery(BUFFER Payment:HANDLE, lcWhere).
               fExportQuery(BUFFER InvRowCounter:HANDLE, lcWhere).
            END.
         END.
         
         OUTPUT STREAM outstro CLOSE.

      END.
      
      OUTPUT STREAM outstr CLOSE.
   END.
   
   FINALLY:
      IF VALID-HANDLE(ghQuery)
      THEN DO:
         ghQuery:QUERY-CLOSE().
         DELETE OBJECT ghQuery.
      END.
      
      OUTPUT STREAM outstrc CLOSE.

   END FINALLY.
   
END PROCEDURE.

ASSIGN
   giMaleCount    = fLoadScrambleData("MALE", gcMaleListFile)
   giFemaleCount  = fLoadScrambleData("FEMALE", gcFemaleListFile)
   giSurNameCount = fLoadScrambleData("SURNAME", gcSurNameListFile)
   .

RUN pPopulatettCustomer.

RUN pExportCustomersAndDataRelated.
