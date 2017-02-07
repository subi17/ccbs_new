/* ----------------------------------------------------------------------
  MODULE .......: migration_request_sender.p
  TASK .........: Program collects dump file that contains migration requests
                  that will be sent to NodoCentral.
                  Information is read from orders that are waiting for 
                  migration (ORDER_STATUS_MIGRATION_PENDING).
                  Program also changes related ordrer statuses to 
                  ORDER_STATUS_MIGRATION_ONGOING when it has created an entry
                  to outgoing file. 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  VERSION.......:
  CREATED ......: 25.1.17
  CHANGED ......:
  ------------------------------------------------------------------------*/
{tmsconst.i}
{commpaa.i}
{cparam2.i}
{timestamp.i}
{ftransdir.i}
gcBrand = "1".

DEF STREAM sOut.
DEF STREAM sLog.

DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcOutFile AS CHAR NO-UNDO.
DEF VAR liMigrationOn AS INT NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcOutDir AS CHAR NO-UNDO.
DEF VAR lcRow AS CHAR NO-UNDO. 
DEF VAR lcDelim AS CHAR NO-UNDO INIT "|".
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For generating file and log file names*/
DEF BUFFER Order FOR Order.

liMigrationOn = fCParamI("MigrationOn").
IF liMigrationOn EQ 0 THEN QUIT.

/*Set directory handling parameters*/
lcLogDir = fCParam("MB_Migration", "MigrationLogDir").
IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

lcSpoolDir = fCParam("MB_Migration", "MigrationSpoolDir").
IF lcSpoolDir EQ "" OR lcSpoolDir EQ ? THEN lcSpoolDir = "/tmp/".

lcOutDir = fCParam("MB_Migration", "MigrationOutDir").
IF lcOutDir EQ "" OR lcOutDir EQ ? THEN lcOutDir = "/tmp/".

/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcOutFile = lcSpoolDir + "MM_MIGRATION_LIST_" + lcTimePart + ".txt".
lcLogFile = lcLogDir + "MM_MIGRATION_LIST_" + lcTimePart + ".log".

message "alkaa" VIEW-AS ALERT-BOX.

OUTPUT STREAM sOut TO VALUE(lcOutFile) APPEND.
OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED 
   "Migration file building starts " + fTS2HMS(fMakeTS()) SKIP.

/*Data collection*/
FOR EACH Order EXCLUSIVE-LOCK WHERE
         Order.Brand EQ gcBrand AND
         Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING}: 
         message "jiihaa" VIEW-AS ALERT-BOX.
   lcRow = Order.CLI.
   PUT STREAM sLog UNFORMATTED lcRow SKIP.
   PUT STREAM sOut UNFORMATTED lcRow SKIP.

   Order.StatusCode = {&ORDER_STATUS_MIGRATION_ONGOING}.

END.

PUT STREAM sLog UNFORMATTED 
   "Migration file building ends " + fTS2HMS(fMakeTS()) SKIP.

fMove2TransDir(lcOutFile, ".txt", lcOutDir).

