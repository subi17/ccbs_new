/* ----------------------------------------------------------------------
  MODULE .......: PrepaidRequest_dump.p
  TASK .........: Create a dump file for PrepaidRequests
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 03.07.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}

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
DEF VAR lcCreator    AS CHAR     NO-UNDO.
DEF VAR ldCreated    AS DEC      NO-UNDO.
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcEventKey   AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.
DEF VAR ldValue      AS DEC      NO-UNDO.
DEF VAR ldFromStamp  AS DEC      NO-UNDO.
DEF VAR ldCurrent    AS DEC      NO-UNDO.

DEF STREAM sFile.

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

ASSIGN
   lhTable     = BUFFER PrepaidRequest:HANDLE
   lcKeyFields = fEventKeyFields(lhTable)
   ldCurrent   = fMakeTS().

IF icDumpMode = "Modified" THEN ldFromStamp = idLastDump.
ELSE ldFromStamp = 20060101.

OUTPUT STREAM sFile TO VALUE(icFile).


PrepaidRequestLoop:
FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "PrepaidRequest" AND
         TMSCodes.FieldName = "Source",
    EACH PrepaidRequest NO-LOCK WHERE
         PrepaidRequest.Brand   = gcBrand AND
         PrepaidRequest.Source  = TMSCodes.CodeValue AND
         PrePaidRequest.TSRequest > ldFromStamp AND
         PrePaidRequest.TSRequest <= ldCurrent
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         END CASE.
      END.
      
      ELSE DO:
         lhField = lhTable:BUFFER-FIELD(lcField).
      
         IF lhField:DATA-TYPE = "DECIMAL" THEN DO:

            ldValue = lhField:BUFFER-VALUE.
            
            CASE lcField:
            WHEN "TopupAmt" OR 
            WHEN "VatAmt" THEN 
               lcValue = TRIM(STRING(ldValue / 100,"->>>,>>>,>>9.9<")). 
            WHEN "TSRequest" OR 
            WHEN "TSResponse" THEN 
               lcValue = STRING(ldValue,"99999999.99999").
            OTHERWISE 
               lcValue = TRIM(STRING(lhField:BUFFER-VALUE,"->>>,>>>,>>9.9<")).
            END CASE.    
         END.   
         ELSE lcValue = lhField:BUFFER-VALUE.
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
      DISP oiEvents LABEL "PrepaidRequests" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


