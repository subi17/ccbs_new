/* read_printhouse_zipcode.p    16.03.11/aam
*/

{commali.i}
{cparam2.i}
{ftransdir.i}
{timestamp.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPrintHouseConf AS HANDLE NO-UNDO.
   lhPrintHouseConf = BUFFER PrintHouseConf:HANDLE.
   RUN StarEventInitialize(lhPrintHouseConf).
END.


DEF INPUT  PARAMETER icFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiDone   AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors AS INT  NO-UNDO.

DEF VAR lcZipCode   AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcReadLine  AS CHAR NO-UNDO. 
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR liCheck     AS INT  NO-UNDO.

DEF TEMP-TABLE ttUpdated NO-UNDO
   FIELD ZipCode AS CHAR
   INDEX ZipCode ZipCode.
   
DEF BUFFER bUpdateConf FOR PrintHouseConf.

DEF STREAM sRead.
DEF STREAM sLog.

FORM 
   oiDone   AT 2 FORMAT ">>>>>>>>9" LABEL "Rows Read" SKIP
   oiErrors AT 2 FORMAT ">>>>>>>>9" LABEL "Errors .." SKIP
   WITH OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " IMPORT " FRAME fQty.


FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcReadLine  ";"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   oiErrors = oiErrors + 1.
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

ASSIGN
   lcLogFile  = fCParamC("PHouseZipCodeLog")
   lcTransDir = fCParamC("PHouseZipCodeArc").

IF lcLogFile = ? OR lcLogFile = "" THEN 
   lcLogFile = "/tmp/read_printhouse_zipcode_#DATE.log".
   
ASSIGN 
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999"))
   lcLogFile = REPLACE(lcLogFile,"#TIME",
                       REPLACE(STRING(TIME,"hh:mm:ss"),":","")).
                                        

/* file without the dir */
lcPlainFile = icFile.
IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
   lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
 

INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   icFile  " "
   STRING(TODAY,"99.99.99") " "
   STRING(TIME,"hh:mm:ss") SKIP.

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.


REPEAT:

   lcReadLine = "".
    
   IMPORT STREAM sRead UNFORMATTED lcReadLine.

   IF lcReadLine = "" THEN NEXT. 
   
   lcZipCode = ENTRY(1,lcReadLine,";").
   IF lcZipCode = "" THEN NEXT. 
      
   liCheck = INTEGER(lcZipCode) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Invalid format").
      NEXT.
   END.

   FIND FIRST PrintHouseConf WHERE
              PrintHouseConf.Brand     = gcBrand AND
              PrintHouseConf.Report    = "Invoice" AND
              PrintHouseConf.TableName = "Customer" AND
              PrintHouseConf.FieldName = "ZipCode" AND
              PrintHouseConf.KeyValue  = lcZipCode AND
              PrintHouseConf.ToDate   >= TODAY AND
              PrintHouseConf.FromDate <= TODAY NO-LOCK NO-ERROR.
   IF AVAILABLE PrintHouseConf THEN DO:
      IF PrintHouseConf.ToDate < 12/31/2049 THEN DO:
         FIND CURRENT PrintHouseConf EXCLUSIVE-LOCK.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrintHouseConf).
         PrintHouseConf.ToDate = 12/31/2049.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrintHouseConf).
      END.   
   END.
   
   ELSE DO:
      CREATE PrintHouseConf.
      ASSIGN 
         PrintHouseConf.Brand     = gcBrand 
         PrintHouseConf.Report    = "Invoice" 
         PrintHouseConf.TableName = "Customer"
         PrintHouseConf.FieldName = "ZipCode" 
         PrintHouseConf.KeyValue  = lcZipCode
         PrintHouseConf.FromDate  = TODAY
         PrintHouseConf.ToDate    = 12/31/2049
         PrintHouseConf.PrintHouse = "INDRA".
   
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPrintHouseConf).
   END.
   
   CREATE ttUpdated.
   ttUpdated.ZipCode = lcZipCode.
   fLogLine("OK"). 

   oiDone = oiDone + 1.
   
   IF NOT SESSION:BATCH AND 
      (oiDone < 100 OR oiDone MOD 100 = 0) THEN DO:
      PAUSE 0.
      DISP oiDone oiErrors WITH FRAME fQty.
   END.
END.

/* file contains all codes that should be used from here onwards, so those
   that were not included in the file are ended */
IF CAN-FIND(FIRST ttUpdated) THEN 
FOR EACH bUpdateConf NO-LOCK WHERE
         bUpdateConf.Brand     = gcBrand AND
         bUpdateConf.Report    = "Invoice" AND
         bUpdateConf.TableName = "Customer" AND
         bUpdateConf.FieldName = "ZipCode" AND
         bUpdateConf.ToDate   > TODAY AND
         bUpdateConf.FromDate < TODAY:
        
   IF CAN-FIND(FIRST ttUpdated WHERE 
                  ttUpdated.ZipCode = bUpdateConf.KeyValue) THEN NEXT.
                  
   FIND FIRST PrintHouseConf WHERE 
      RECID(PrintHouseConf) = RECID(bUpdateConf) EXCLUSIVE-LOCK.
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrintHouseConf).
   PrintHouseConf.ToDate = TODAY - 1.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrintHouseConf).
END.

fCleanEventObjects().

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "PrinHouseConf"  
      ActionLog.KeyValue     = lcPlainFile
      ActionLog.ActionID     = "ZIPCODE_PH"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = oiDone
      ActionLog.ActionChar   = STRING(oiDone) + 
                               " zip codes were updated"
      ActionLog.ActionStatus = 3
      ActionLog.UserCode     = katun
      ActionLog.FromDate     = TODAY
      ActionLog.ToDate       = TODAY.
      ActionLog.ActionTS     = fMakeTS().
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLog CLOSE.

/* move to archive */
IF oiDone > 0 AND lcTransDir > "" THEN DO:   
   fTransDir(icFile,
             "",
             lcTransDir).
END.
 
