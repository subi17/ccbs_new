/* ----------------------------------------------------------------------
  MODULE .......: edrhistory_dump_run.p
  TASK .........: Dumps rerated call changes from yesterday. 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 07.11.12
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/lib/eventlog.i}
{Func/direct_dbconnect.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

fInitializeConnectTables("MobCDR","").

RUN pDirectConnect2Dbs(gcBrand,
                       "",
                       TODAY - 1,
                       TODAY - 1).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("edrhistory_dump",RETURN-VALUE).
   RETURN.
END.

RUN edrhistory_dump.p(
   iiDumpID,
   icFile,
   icDumpMode,
   idLastDump,
   icEventSource,
   icEventFields,
   OUTPUT oiEvents,
   OUTPUT olInterrupted).
