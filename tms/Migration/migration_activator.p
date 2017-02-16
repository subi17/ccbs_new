/* ----------------------------------------------------------------------
  MODULE .......: migration_activator.p
  TASK .........: Program activates orders that waiting for activation
                  and activation time is reached.
                  Handled status is (ORDER_STATUS_MIGRATION_WAITING_ACTIVATION).
                  Orders are moved to DELIVERED by this program.
                  Timing plan:
                  Orders will have timestamp 02:00 for activation.
                  This program is run once/day after this timestamp.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  VERSION.......:
  CREATED ......: 16.2.17
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
DEF VAR liMigrationOn AS INT NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcDelim AS CHAR NO-UNDO INIT "|".
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For generating file and log file names*/
DEF BUFFER Order FOR Order.

liMigrationOn = fCParamI("MigrationOn").
IF liMigrationOn EQ 0 THEN QUIT.

/*Set directory handling parameters*/
lcLogDir = fCParam("MB_Migration", "MigrationLogDir").
IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcLogFile = lcLogDir + "MM_MIGRATION_LIST_" + lcTimePart + ".log".


OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED 
   "Migration file building starts " + fTS2HMS(fMakeTS()) SKIP.

/*Data collection*/
FOR EACH Order EXCLUSIVE-LOCK WHERE
         Order.Brand EQ gcBrand AND
         Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING}: 
   PUT STREAM sOut UNFORMATTED "" SKIP.

   Order.StatusCode = {&ORDER_STATUS_MIGRATION_ONGOING}.

END.

PUT STREAM sLog UNFORMATTED 
   "Migration file building ends " + fTS2HMS(fMakeTS()) SKIP.


