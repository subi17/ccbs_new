/* ----------------------------------------------------------------------
  module .......: errorcdr_dump_start.p
  task .........: create a dump file for error cdrs
  application ..: tms
  author .......: vikas
  created ......: 28.02.11
  version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{direct_dbconnect.i}

def input  parameter icdumpid      as int  no-undo.
def input  parameter icfile        as char no-undo.
def input  parameter icdumpmode    as char no-undo.
def input  parameter idlastdump    as dec  no-undo.
def input  parameter iceventsource as char no-undo.
def input  parameter iceventfields as char no-undo.
def output parameter oievents      as int  no-undo.
def output parameter olinterrupted as log  no-undo.

DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR ldaOldDb    AS DATE NO-UNDO.
DEF VAR lcNumeric   AS CHAR NO-UNDO.

ASSIGN 
   ldaFromDate = TODAY - 1
   ldaToDate   = TODAY - 1.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icdumpid NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.

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
  IF DBConfig.FromDate = ldaToDate THEN ldaOldDb = (DbConfig.FromDate - 1).
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
   fInitializeConnectTables("MobCDR,McdrDtl2,ErrorCDR","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          idaConnectDate,
                          idaConnectDate).

   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
 
   run errorcdr_dump.p(icDumpID,
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

SESSION:NUMERIC-FORMAT = lcNumeric.

