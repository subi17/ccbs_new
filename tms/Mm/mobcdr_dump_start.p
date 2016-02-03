/* ----------------------------------------------------------------------
  MODULE .......: mobcdr_dump_start.p
  TASK .........: Create a dump file for cdrs
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 04.01.11
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/direct_dbconnect.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR ldaOldDb    AS DATE NO-UNDO.

ASSIGN 
   ldaFromDate = TODAY - 1
   ldaToDate   = TODAY - 1.


RUN pStartDump(ldaToDate,FALSE).
IF RETURN-VALUE BEGINS "ERROR" OR olInterrupted THEN 
   RETURN RETURN-VALUE.

/* do two runs if db has been renewed on the event day -> some tickets have
   been saved to old db on the 1. day */
ldaOldDb = ?.
FOR FIRST ttDB WHERE 
          ttDb.ConnName = "" AND
          ttDb.TableName = "MobCDR",
    FIRST DBConfig NO-LOCK WHERE
          DBConfig.DBConfigId = ttDb.DbConfigId:
  IF DBConfig.FromDate = ldaToDate THEN ldaOldDb = DbConfig.FromDate - 1.
END.
      
IF ldaOldDb NE ? THEN DO:
   RUN pStartDump(ldaOldDb,TRUE).
   IF RETURN-VALUE BEGINS "ERROR" THEN
      RETURN RETURN-VALUE.
END.
 
 
PROCEDURE pStartDump:
 
   DEF INPUT PARAMETER idaConnectDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER ilAppend       AS LOG  NO-UNDO.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("MobCDR,McdrDtl2","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          idaConnectDate,
                          idaConnectDate).

   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.
 
   RUN mobcdr_dump.p (icDumpID,
                      icFile,
                      icDumpMode,
                      idLastDump,
                      icEventSource,
                      icEventFields,
                      ldaFromDate,
                      ldaToDate,
                      ilAppend,
                      OUTPUT oiEvents,
                      OUTPUT olInterrupted).
END PROCEDURE.
                   
                  
