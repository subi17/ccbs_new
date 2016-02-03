/* ----------------------------------------------------------------------
  MODULE .......: errorcdr_stat_dump.p
  TASK .........: run errorcdr statistic through dump routine
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 30.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric  AS CHAR NO-UNDO.
DEF VAR ldaCDRDate AS DATE NO-UNDO EXTENT 2.
DEF VAR lcError    AS CHAR NO-UNDO.


lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.

ASSIGN
   ldaCDRDate[1] = DATE(12,1,2006)
   ldaCDRDate[2] = TODAY.

RUN errorcdr_stat (ldaCDRDate[1],
                   ldaCDRDate[2],
                   icFile,
                   "",     /* trans dir, dumpfile_run takes care of this */
                   OUTPUT oiEvents,
                   OUTPUT lcError).
 
SESSION:NUMERIC-FORMAT = lcNumeric.


