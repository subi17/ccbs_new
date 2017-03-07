/* ----------------------------------------------------------------------
  MODULE .......: MSISDN_dump.p
  TASK .........: Create a dump file for free MSISDNs (all)
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 15.11.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric     AS CHAR     NO-UNDO.
DEF VAR lcDelimiter   AS CHAR     NO-UNDO.
DEF VAR ldaModified   AS DATE     NO-UNDO.
DEF VAR liCnt         AS INT      NO-UNDO.
DEF VAR lhTable       AS HANDLE   NO-UNDO.
DEF VAR lcDumpFields  AS CHAR     NO-UNDO.
DEF VAR lcValue       AS CHAR     NO-UNDO.
DEF VAR lhField       AS HANDLE   NO-UNDO.
DEF VAR lcField       AS CHAR     NO-UNDO.
DEF VAR ldCurrent     AS DEC      NO-UNDO.
DEF VAR lhTemp        AS HANDLE   NO-UNDO.

DEF TEMP-TABLE ttRank NO-UNDO
   FIELD Rank AS INT
   FIELD RankName AS CHAR
   INDEX Rank Rank.

DEF TEMP-TABLE ttStatus NO-UNDO
   FIELD StatusCode AS INT
   INDEX StatusCode StatusCode.
   
DEF TEMP-TABLE ttCLI NO-UNDO
   FIELD CLI AS CHAR
   FIELD StatusCode AS INT
   FIELD Rank AS INT
   FIELD Pos AS CHAR
   FIELD ValidTo AS DEC
   INDEX CLI CLI.
   
DEF STREAM sFile.


ASSIGN
   lcNumeric = SESSION:NUMERIC-FORMAT
   ldCurrent = fMakeTS().

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
   lhTable = BUFFER MSISDN:HANDLE
   lhTemp  = BUFFER ttCLI:HANDLE.

FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "MSISDNNumber" AND
         TMSCodes.FieldName = "Rank" AND
         TMSCodes.InUse > 0:
                           
   CREATE ttRank.
   ASSIGN ttRank.Rank     = INTEGER(TMSCodes.CodeValue)
          ttRank.RankName = TMSCodes.CodeName NO-ERROR.
          
   IF ERROR-STATUS:ERROR THEN DELETE ttRank.
END.

OUTPUT STREAM sFile TO VALUE(icFile).


IF icDumpMode = "full" THEN        
MSISDNFullLoop:
FOR EACH MSISDN NO-LOCK USE-INDEX CLI WHERE
         MSISDN.Brand = gcBrand AND
         MSISDN.ValidTo >= fMake2DT(TODAY,0)
BREAK BY MSISDN.CLI
      BY MSISDN.ValidFrom DESC
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   IF FIRST-OF(MSISDN.CLI) THEN 
      RUN pWrite2File (lhTable).
END.
      
ELSE DO:

   /* dump all free (available), and from others only changed ones */
   
   CREATE ttStatus.
   ttStatus.StatusCode = {&MSISDN_ST_UNKNOWN}.
   CREATE ttStatus.
   ttStatus.StatusCode = {&MSISDN_ST_AVAILABLE}.

   MSISDNFreeLoop:
   FOR EACH ttStatus,
       EACH MSISDN NO-LOCK USE-INDEX StatusCode WHERE
            MSISDN.Brand = gcBrand AND
            MSISDN.StatusCode = ttStatus.StatusCode AND
            MSISDN.ValidTo   >= fMake2Dt(ldaModified,0)
   BREAK BY MSISDN.CLI
         BY MSISDN.ValidFrom DESC
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF FIRST-OF(MSISDN.CLI) THEN DO:
         CREATE ttCLI.
         BUFFER-COPY MSISDN TO ttCLI.
      END.
   END.

   IF NOT olInterrupted THEN    
   MSISDNOthersLoop:
   FOR EACH MsStat NO-LOCK WHERE
            NOT CAN-FIND(FIRST ttStatus WHERE 
                               ttStatus.StatusCode = MsStat.StatusCode),
       EACH MSISDN NO-LOCK USE-INDEX ActionDate WHERE
            MSISDN.Brand = gcBrand AND
            MSISDN.StatusCode = MSStat.StatusCode AND
            MSISDN.ActionDate = ldaModified 
   BREAK BY MSISDN.CLI
         BY MSISDN.ValidFrom DESC
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF FIRST-OF(MSISDN.CLI) THEN DO:
         FIND FIRST ttCLI WHERE
                    ttCLI.CLI = MSISDN.CLI NO-ERROR.
         IF AVAILABLE ttCLI AND ttCLI.ValidTo > MSISDN.ValidTo THEN NEXT.
         
         IF NOT AVAILABLE ttCLI THEN CREATE ttCLI.
         BUFFER-COPY MSISDN TO ttCLI.
      END.
   END.
            
   IF NOT olInterrupted THEN 
   FOR EACH ttCLI:
      RUN pWrite2File(lhTemp).
   END.
      
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE pWrite2File:

   DEF INPUT PARAMETER ihTable AS HANDLE NO-UNDO.
   
   FIND FIRST MSISDNNumber WHERE MSISDNNumber.CLI = ihTable::CLI 
      NO-LOCK NO-ERROR.
      
   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#RankName" THEN DO:
            lcValue = "".
            IF AVAILABLE MSISDNNumber THEN DO:
               FIND FIRST ttRank WHERE ttRank.Rank = MSISDNNumber.Rank 
                  NO-ERROR.
               IF AVAILABLE ttRank THEN lcValue = ttRank.RankName.
            END.    
         END.
         OTHERWISE lcValue = "".
         END CASE.
      END.
      
      ELSE DO:
         lhField = ihTable:BUFFER-FIELD(lcField) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN DO:
            IF lhField:DATA-TYPE = "DECIMAL" THEN
               lcValue = TRIM(STRING(lhField:BUFFER-VALUE,"->>>,>>>,>>9.9<")).
            ELSE lcValue = lhField:BUFFER-VALUE.
         END.
         ELSE lcValue = "".
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

   IF NOT SESSION:BATCH AND oiEvents MOD 1000 = 0 THEN DO:
      PAUSE 0. 
      DISP oiEvents LABEL "MSISDNs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.                       


