DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTariffBundle AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldCLIType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldtariffBundle AS CHARACTER NO-UNDO.

{commpaa.i}
katun = "Cron".
gcbrand = "1".
{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/mobsub_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

FUNCTION fDateToString RETURNS CHAR (INPUT idaDate AS DATE):
   IF idaDate EQ ? THEN RETURN "".
   RETURN STRING(idaDate).
END.

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK,
   FIRST Customer WHERE
         Customer.CustNum = MobSub.CustNum NO-LOCK:

   ASSIGN liEvents = liEvents + 1
          lcOldCLIType = ""
          lcOldtariffBundle = "".

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MobSub"
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   CASE MobSub.TariffBundle:
      WHEN "CONTDATA" THEN lcTariffBundle = "CONTRD1".
      WHEN "CONTD2"   THEN lcTariffBundle = "CONTRD2".
      WHEN "CONTD3"   THEN lcTariffBundle = "CONTRD3".
      WHEN "CONTD4"   THEN lcTariffBundle = "CONTRD4".
      OTHERWISE lcTariffBundle = MobSub.TariffBundle.
   END CASE.
               
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.MsSeq = MobSub.MsSeq USE-INDEX MsSeq:

      IF MsOwner.CLIType NE MobSub.CLIType OR
         MsOwner.TariffBundle NE MobSub.TariffBundle THEN DO:
         ASSIGN lcOldCLIType = MsOwner.CLIType
                lcOldtariffBundle = MsOwner.TariffBundle.
         LEAVE.
      END.
   END.

   PUT STREAM slog UNFORMATTED
       "MobSub"                               lcDel
       "CREATE"                               lcDel
       fNotNull(STRING(RECID(MobSub)))        lcDel
       fNotNull(STRING(MobSub.MsSeq))         lcDel
       fNotNull(STRING(ldtTimeStamp))         lcDel
       fNotNull(STRING(MobSub.MsSeq))         lcDel
       fNotNull(STRING(MobSub.CustNum))       lcDel
       fNotNull(MobSub.CLI)                   lcDel
       fNotNull(MobSub.CLIType)               lcDel
       fNotNull(lcTariffBundle)               lcDel
       fNotNull(STRING(MobSub.PayType))       lcDel
       fNotNull(STRING(MobSub.ActivationTS))  lcDel
       fNotNull(STRING(MobSub.MultiSimID))    lcDel
       fNotNull(STRING(MobSub.MultiSimType))  lcDel
       fNotNull(MobSub.IMSI)                  lcDel
       fNotNull(MobSub.BarrCode)              lcDel
       fDateToString(MobSub.TariffActDate)    lcDel
       fNotNull(lcOldCLIType)                 lcDel
       fNotNull(lcOldtariffBundle)            lcDel
       STRING(MobSub.TariffActTS)             SKIP.         
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

