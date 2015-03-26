DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.

DEF VAR lcLogFile    AS CHARACTER NO-UNDO.
DEF VAR lcSpoolDir   AS CHARACTER NO-UNDO.
DEF VAR lcOutDir     AS CHARACTER NO-UNDO.
DEF VAR lcDel        AS CHARACTER NO-UNDO INIT "|".
DEF VAR ldtTimeStamp AS DATETIME  NO-UNDO.
DEF VAR liEvents     AS INTEGER   NO-UNDO.
DEF VAR liReqStatus  AS INT NO-UNDO. 
DEF VAR ldeFrom      AS DEC NO-UNDO. 
DEF VAR ldeTo        AS DEC NO-UNDO. 
DEF VAR lcReqSource  AS CHAR NO-UNDO.

{commpaa.i}
katun = "Cron".
gcbrand = "1".
{cparam2.i}
{timestamp.i}
{ftransdir.i}
{tmsconst.i}

IF idaFromDate EQ ? OR
   idaToDate EQ ? OR
   idaFromDate > idaToDate THEN DO:

   IF NOT SESSION:BATCH THEN
      MESSAGE "Incorrect input date values:"
         idaFromDate idaToDate VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir")
       ldeFrom    = fMake2Dt(idaFromDate,0)
       ldeTo      = fMake2Dt(idaToDate + 1,0).

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/msrequest_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

DO liReqStatus = 0 TO 100:
   
   ldtTimeStamp = DATETIME(TODAY,MTIME).

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand = gcBrand AND
            MsRequest.ReqStatus = liReqStatus AND
            MsRequest.ActStamp >= ldeFrom AND
            MsRequest.ActStamp < ldeTo:
      
      IF LOOKUP(STRING(MsRequest.ReqType),{&REQTYPES_HPD}) EQ 0 THEN NEXT.

      IF MsRequest.ReqSource EQ "" THEN DO:
         IF MsRequest.UserCode BEGINS "VISTA_" THEN
            lcReqSource = {&REQUEST_SOURCE_NEWTON}.
         ELSE lcReqSource = {&REQUEST_SOURCE_MANUAL_TMS}.
      END.
      ELSE lcReqSource = MsRequest.ReqSource.
      
      IF LOOKUP(lcReqSource,{&REQUEST_SOURCES_MANUAL}) = 0 AND
         MsRequest.ReqType NE 46 THEN NEXT.
      
      liEvents = liEvents + 1.

      IF NOT SESSION:BATCH AND liEvents MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP liEvents MsRequest.ReqType
              liReqStatus MsRequest.Actstamp LABEL "MsRequest" 
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */
                  
      PUT STREAM slog UNFORMATTED
          "MsRequest"                            lcDel
          "CREATE"                               lcDel
          fNotNull(STRING(RECID(MsRequest)))     lcDel
          fNotNull(STRING(MsRequest.MsRequest))  lcDel
          fNotNull(STRING(ldtTimeStamp))         lcDel

          fNotNull(STRING(MsRequest.MsRequest))  lcDel 
          fNotNull(STRING(Msrequest.MsSeq))      lcDel +
          fNotNull(MsRequest.CLI)                lcDel +
          fNotNull(STRING(MsRequest.Custnum)) + lcDel +
          fNotNull(STRING(MsRequest.ReqType)) + lcDel +
          fNotNull(STRING(MsRequest.ReqStatus)) + lcDel +
          fNotNull(MsRequest.UserCode) + lcDel +
          fNotNull(lcReqSource) + lcDel +
          fNotNull(STRING(MsRequest.CreStamp)) + lcDel +
          fNotNull(STRING(MsRequest.ActStamp)) + lcDel +
          fNotNull(STRING(MsRequest.DoneStamp)) + lcDel +
          fNotNull(MsRequest.ReqCParam1) + lcDel +
          fNotNull(MsRequest.ReqCParam2) + lcDel +
          fNotNull(MsRequest.ReqCParam3) + lcDel +
          fNotNull(MsRequest.ReqCParam4) + lcDel +
          fNotNull(MsRequest.ReqCParam5) + lcDel +
          fNotNull(STRING(MsRequest.ReqIParam1)) + lcDel +
          fNotNull(STRING(MsRequest.ReqIParam2)) + lcDel +
          fNotNull(STRING(MsRequest.ReqIParam3)) + lcDel +
          fNotNull(STRING(MsRequest.ReqIParam4)) + lcDel +
          fNotNull(STRING(MsRequest.ReqDParam1)) SKIP.
   END.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
