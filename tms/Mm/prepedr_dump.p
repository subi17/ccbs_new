/* ----------------------------------------------------------------------
  MODULE .......: prepedr_dump.p
  TASK .........: Prepaid EDR dump for Track
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.02.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/detailvalue.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaFromDate   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate     AS DATE NO-UNDO.
DEF INPUT  PARAMETER ilAppend      AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric   AS CHAR NO-UNDO.
DEF VAR lcDelimiter AS CHAR NO-UNDO.
DEF VAR lcCsvVersion AS CHAR NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR liTime AS INT NO-UNDO. 

DEFINE TEMP-TABLE ttCSVPos NO-UNDO
   FIELD SourceFileName AS INT
   FIELD RunningIndex AS INT
   FIELD ExtRunIndex AS INT
   FIELD HandlingTime AS INT
   FIELD ServiceFeeAmount AS INT
   FIELD ServiceFeeType AS INT
   FIELD OldServiceClass AS INT
   FIELD ServFeeExpDateAfter AS INT
   FIELD Subscriberfee AS INT.

/* at the moment there's only one possible EDR format */
lcCsvVersion = "0101YD".
CREATE ttCSVPos.
ASSIGN
   ttCSVPos.SourceFileName   = fGetPosition(lcCsvVersion,"Source File Name")
   ttCSVPos.RunningIndex     = fGetPosition(lcCsvVersion,"Running index")
   ttCSVPos.ExtRunIndex = fGetPosition(lcCsvVersion,"External Running Index")
   ttCSVPos.HandlingTime     = fGetPosition(lcCsvVersion,"Handling time")
   ttCSVPos.ServiceFeeAmount = fGetPosition(lcCsvVersion,"Service Fee Amount")
   ttCSVPos.ServiceFeeType   = fGetPosition(lcCsvVersion,"Service Fee Type")
   ttCSVPos.ServFeeExpDateAfter = fGetPosition(lcCsvVersion,
                                      "Service fee exp date after")
   ttCSVPos.OldServiceClass = fGetPosition(lcCsvVersion,"Old Service Class").

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

IF ilAppend THEN 
   OUTPUT STREAM sFile TO VALUE(icFile) APPEND.
ELSE OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH PrepEDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepEDR.ReadDate >= idaFromDate AND
         PrepEDR.ReadDate <= idaToDate
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.
   
   FIND FIRST edrdtl WHERE
              edrdtl.datest = PrepEDR.DateSt AND
              edrdtl.dtlseq = PrepEDR.DtlSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL edrdtl THEN NEXT.
   
   fSplitts(PrepEDR.ReadInTS, OUTPUT ldaDate, OUTPUT liTime).
   
   PUT STREAM sFile UNFORMATTED
      ENTRY(ttCSVPos.SourceFileName,edrdtl.detail,"|") lcDelimiter
      ENTRY(ttCSVPos.RunningIndex,edrdtl.detail,"|") lcDelimiter
      ENTRY(ttCSVPos.ExtRunIndex,edrdtl.detail,"|") lcDelimiter
      ENTRY(ttCSVPos.HandlingTime,edrdtl.detail,"|") lcDelimiter
      PrepEDR.Datest                       lcDelimiter
      STRING(PrepEDR.TimeStart,"HH:MM:SS") lcDelimiter
      ENTRY(ttCSVPos.ServiceFeeAmount,edrdtl.detail,"|") lcDelimiter
      PrepEDR.SuccessCode                  lcDelimiter
      ENTRY(ttCSVPos.OldServiceClass,edrdtl.detail,"|") lcDelimiter
      PrepEDR.NewSC                        lcDelimiter
      PrepEDR.ServFeeExpDateBefore         lcDelimiter
      ENTRY(ttCSVPos.ServFeeExpDateAfter,edrdtl.detail,"|") lcDelimiter
      PrepEDR.BalanceAfter                 lcDelimiter
      PrepEDR.CLI                          lcDelimiter
      ENTRY(ttCSVPos.ServiceFeeType, edrdtl.detail,"|") lcDelimiter
      PrepEDR.ErrorCode                    lcDelimiter
      ldaDate                              lcDelimiter
      STRING(liTime,"HH:MM:SS")            lcDelimiter
      PrepEDR.CLIType                      SKIP.
   
   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "EDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

EMPTY TEMP-TABLE ttCSVPos.
SESSION:NUMERIC-FORMAT = lcNumeric.
