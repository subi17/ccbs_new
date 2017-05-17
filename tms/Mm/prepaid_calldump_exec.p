/* -----------------------------------------------
  MODULE .......: Error_calldump_execP
  FUNCTION .....: RUN modules AT 01.0errorcalls dump
  APPLICATION ..: NN
  AUTHOR .......: kl
  CREATED ......: xx.xx.xx
  MODIFIED .....: 17.05.17  Version ......: 
------------------------------------------------------ */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}
{Func/direct_dbconnect.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

fELog("DAILY","ErrorCallDumpStarted").

RUN pCallDump ("MobCDR,ErrorCDR",
               "Mm/calldump_prepaid.p",
               icFile,
               TODAY - 1).
 
fELog("DAILY","ErrorCallDumpStopped").

QUIT.

PROCEDURE pCallDump:

   DEF INPUT PARAMETER icTableNames AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icModule     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icFilename   AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idaRunDate   AS DATE NO-UNDO.

   DEF VAR ldaOldDb AS DATE NO-UNDO.
  
   
   EMPTY TEMP-TABLE ttDB.

   /* connect to correct cdr dbs before starting the dump modules */
   fInitializeConnectTables(icTableNames,"").
   RUN pDirectConnect2Dbs(gcBrand,
                          "", 
                          idaRunDate,
                          idaRunDate).
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fELog("DAILY/" + icModule,RETURN-VALUE).
      RETURN.
   END.
   ELSE RUN VALUE(icModule) (icfilename).

   /* do two runs if db has been renewed on the event day -> some tickets have
      been saved to old db on the 1. day */
   ldaOldDb = ?.
   FOR FIRST ttDB WHERE 
             ttDb.ConnName = "",
       FIRST DBConfig NO-LOCK WHERE
             DBConfig.DBConfigId = ttDb.DbConfigId:
      IF DBConfig.FromDate = idaRunDate THEN ldaOldDb = DbConfig.FromDate - 1.
   END.
      
   IF ldaOldDb NE ? THEN DO:
      fInitializeConnectTables(icTableNames,"").
      RUN pDirectConnect2Dbs(gcBrand,
                             "", 
                             ldaOldDb,
                             ldaOldDb).
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fELog("DAILY/" + icModule + "/old",RETURN-VALUE).
         RETURN.
      END.
      ELSE RUN VALUE(icModule) (icfilename).
   END. 
   
END PROCEDURE.


