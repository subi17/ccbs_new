/* ----------------------------------------------------------------------
  MODULE .......: edr_error_rerate.p
  TASK .........: Daily cron to re-handle EDRs with error 1001/1002
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.12.13
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/detailseq.i}
{Func/cparam2.i}
{Rate/edr_reader.i}

DEF VAR lcLogFolder AS CHAR NO-UNDO. 

lcLogFolder = fCParam("EDR","RerateLogDir").
IF lcLogFolder EQ ? OR lcLogFolder EQ "" THEN RETURN.

DEF BUFFER bPrepEDR FOR PrepEDR.
DEF VAR lcLogFile AS CHAR NO-UNDO. 

DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 

DEF STREAM sout.
lcLogFile = lcLogFolder + "edr_rehandle_" + 
   STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + ".log".

FILE-INFO:FILE-NAME = lcLogFile.
IF FILE-INFO:FILE-TYPE EQ ? THEN DO:
   OUTPUT STREAM sout TO VALUE(lcLogFile) APPEND.
   PUT STREAM sout UNFORMATTED 
    "RehandleTS;ReadInTS;MSISDN;Date;DtlSeq;ErrorCode;MsSeq;Custnum;CLIType;"  
    "ErrorCode_after;MsSeq_after;Custnum_after;CLIType_after" SKIP.
END.
ELSE OUTPUT STREAM sout TO VALUE(lcLogFile) APPEND.

FOR EACH PrepEDR NO-LOCK WHERE
         PrepEDR.DateSt = TODAY AND
        (PrepEDR.ErrorCode = 1001 OR
         PrepEDR.ErrorCode = 1002 OR
         PrepEDR.ErrorCode = 5004 OR
         PrepEDR.ErrorCode = 5006),
   FIRST EDRDtl NO-LOCK WHERE
         EDRDtl.DateSt = PrepEDR.DateSt AND
         EDRDtl.DtlSeq = PrepEDR.DtlSeq:

   EMPTY TEMP-TABLE ttEDR.

   i = i + 1.

   BUFFER-COPY PrepEDR TO ttEDR.
   ASSIGN
      ttEDR.ErrorCode = 0
      ttEDR.MsSeq = 0 
      ttEDR.Custnum = 0
      ttEDR.CLIType = ""
      ttEDR.ServiceFeeType = ENTRY(ttCSVPos.ServiceFeeType,EDRDtl.Detail,"|").

   RUN pAnalyzeEDR(BUFFER ttEDR).

   IF ttEDR.ErrorCode EQ PrepEDR.ErrorCode THEN NEXT.
   
   j = j + 1.
   
   PUT STREAM sout UNFORMATTED
      fts2hms(fMakeTS()) ";"
      fts2hms(PrepEDR.ReadInTS) ";"
      PrepEDR.CLI ";"
      PrepEDR.DateSt ";"
      PrepEDR.DtlSeq ";"
      PrepEDR.ErrorCode ";"
      PrepEDR.MsSeq ";"
      PrepEDR.Custnum ";"
      PrepEDR.CLIType ";"
      ttEDR.ErrorCode ";"
      ttEDR.MsSeq ";"
      ttEDR.Custnum ";"
      ttEDR.CLIType SKIP.

   FIND bPrepEDR EXCLUSIVE-LOCK WHERE
       ROWID(bPrepEDR) EQ ROWID(PrepEDR).
   BUFFER-COPY ttEDR USING MsSeq ErrorCode Custnum CLIType TO bPrepEDR.
   bPrepEDR.ReadInTS = fMakeTS(). /* for HPD dump */
   RELEASE bPrepEDR.

   IF ttEDR.ErrorCode EQ 0 THEN RUN pHandleEDR(BUFFER ttEDR).
END.

OUTPUT STREAM sout CLOSE.

IF NOT SESSION:BATCH THEN DO:
   MESSAGE i "EDRs were re-handled, with " j "changes" VIEW-AS ALERT-BOX.
END.
