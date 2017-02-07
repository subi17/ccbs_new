/* ----------------------------------------------------------------------
  MODULE .......: msrequest_dump.p
  TASK .........: Create a dump file for requests
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 23.02.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR ldaModified  AS DATE     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lhTable      AS HANDLE   NO-UNDO.
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcEventKey   AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO. 
DEF VAR ldFromStamp  AS DEC      NO-UNDO.
DEF VAR ldtLastDump  AS DATETIME NO-UNDO.

DEF TEMP-TABLE ttStatus NO-UNDO
   FIELD ReqStatus AS INT.

DEF STREAM sFile.


/***  Main start ***/

RUN pInitialize. 

RUN pDumpRequests.

RUN pFinalize. 

/***  Main end ***/


PROCEDURE pInitialize:
   
   lcNumeric = SESSION:NUMERIC-FORMAT.

   FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile THEN DO:
      lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
      
      IF DumpFile.DecimalPoint = "." 
      THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
      ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
   END.
   ELSE DO:
      ASSIGN 
         lcDelimiter = CHR(9)
         SESSION:NUMERIC-FORMAT = "AMERICAN".
   END.

   FOR EACH DFField OF DumpFile NO-LOCK WHERE
            DFField.ToDate   >= TODAY AND
            DFField.FromDate <= TODAY
   BY DFField.OrderNbr:
      lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                     DFField.DFField.
   END.
         
   fSplitTS(idLastDump,
            OUTPUT ldaModified,
            OUTPUT liCnt).

   ldtLastDump = fTimeStamp2DateTime(idLastDump).

   ASSIGN
      lhTable     = BUFFER MsRequest:HANDLE
      lcKeyFields = fEventKeyFields(lhTable).

   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "MsRequest" AND
            TMSCodes.FieldName = "ReqStatus" AND
            TMSCodes.InUse     > 0:
      CREATE ttStatus.
      ttStatus.ReqStatus = INTEGER(TMSCodes.CodeValue) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DELETE ttStatus.
   END.

   IF icDumpMode = "Full" THEN 
      ldFromStamp = fMake2Dt(TODAY - 65,0).
   ELSE ldFromStamp = fMake2Dt(TODAY - 30,0).
   
   OUTPUT STREAM sFile TO VALUE(icFile).

END PROCEDURE.

PROCEDURE pDumpToFile:

   IF icDumpMode = "modified" THEN DO:
      IF NOT fWasRecordModified(lhTable,
                                icEventSource,
                                icEventFields,
                                idLastDump,
                                ldaModified,
                                ldtLastDump,
                                "")
      THEN RETURN.
   END.

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      ASSIGN
         lcField = ENTRY(liCnt,lcDumpFields)
         lcValue = "".
   
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#Memo" THEN DO:
            FOR EACH Memo NO-LOCK WHERE
                     Memo.Brand = gcBrand AND
                     Memo.HostTable = "MsRequest" AND
                     Memo.KeyValue  = STRING(MsRequest.MsRequest)
            BY Memo.CreStamp:
               lcValue = lcValue + (IF lcValue > "" THEN "  " ELSE "") +
                         Memo.MemoText.
            END.             
            /* YOT-2952 */
            lcValue = REPLACE(lcValue,CHR(10)," "). 
         END.
         END CASE.
      END.
   
      ELSE DO:

         lhField = lhTable:BUFFER-FIELD(lcField).
   
         IF lhField:DATA-TYPE = "DECIMAL" THEN DO:
            IF (INDEX(lhField:NAME,"stamp") > 0 OR 
                lhField:NAME = "ReqDParam1") AND
               lhField:BUFFER-VALUE >= 0 
            THEN lcValue = STRING(lhField:BUFFER-VALUE,"99999999.99999").
            ELSE lcValue = TRIM(STRING(lhField:BUFFER-VALUE,
                                "->>>,>>>,>>9.9<")).
         END.       
         ELSE lcValue = lhField:BUFFER-VALUE.

         /* YOT-2952 */
         IF lhField:DATA-TYPE EQ "CHARACTER" THEN lcValue = REPLACE(lcValue,CHR(10)," ").
      END.
      
      PUT STREAM sFile UNFORMATTED
         lcValue.
   
      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED
            lcDelimiter.
   END.

   PUT STREAM sFile UNFORMATTED
      SKIP.

   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0. 
      DISP oiEvents LABEL "Requests" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.

PROCEDURE pDumpRequests:

   MsRequestLoop:
   FOR EACH ttStatus NO-LOCK:
      /* YOT-4874 Add to dump also old requests where new updatestamp */
      FOR EACH MsRequest NO-LOCK USE-INDEX UpdateStamp WHERE
               MsRequest.Brand     = gcBrand AND
               MsRequest.ReqStatus = ttStatus.ReqStatus AND
               MsRequest.UpdateStamp >= idLastDump AND
               MsRequest.ActStamp < ldFromStamp
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            olInterrupted = TRUE.
            LEAVE.
         END.
         
         RUN pDumpToFile.
       
      END.

      FOR EACH MsRequest NO-LOCK USE-INDEX ReqStatus WHERE
               MsRequest.Brand     = gcBrand AND
               MsRequest.ReqStatus = ttStatus.ReqStatus AND
               MsRequest.ActStamp >= ldFromStamp
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            olInterrupted = TRUE.
            LEAVE.
         END.
         
         RUN pDumpToFile.
       
      END.
   END.

   IF NOT SESSION:BATCH THEN 
      HIDE FRAME fQty NO-PAUSE.

END PROCEDURE.

PROCEDURE pFinalize:
 
   OUTPUT STREAM sFile CLOSE.

   SESSION:NUMERIC-FORMAT = lcNumeric.
   
END PROCEDURE.


