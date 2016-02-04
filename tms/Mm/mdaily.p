/* -----------------------------------------------
  MODULE .......: DAILY.P
  FUNCTION .....: RUN modules AT 01.00 every night
  APPLICATION ..: NN
  AUTHOR .......: kl
  CREATED ......: xx.xx.xx
  MODIFIED .....: 27.05.04 tk dumpall
                  05.02.07 jl calldump
                  12.03.07 jl servicedump
                  14.03.07 kl eventlogdump
                  15.03.07 kl icc_msisdn_rep
                  19.03.07 kl invoicedump on Sundays
                  29.03.07 kl ppcomprep on Sundays
                  11.04.07 jp highusagedel
                  15.05.07 vk makefailurerep.p
                  09.07.07 vk killerrcalls.p
                  13.07.07 vk makefailurerep.p will be run on every Sunday
                  24.07.07 kl highuseagedel not needed so removed
                  13.08.07 pa invoicedump called everyday except Sunday
                  
  Version ......: M15
------------------------------------------------------ */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Func/lib/eventlog.i}
{Func/direct_dbconnect.i}

/* weekday related processes */
CASE WEEKDAY(TODAY):

   /* Sunday */
   WHEN 1 THEN DO:
      
      fELog("DAILY","PrePaidReportStarted").
      RUN Mc/ppcomprep.
      fELog("DAILY","PrePaidReportStopped").
   END.
   
END.

fELog("DAILY","CallDumpStarted").

RUN pCallDump ("MobCDR",
               "calldump.p",
               TODAY - 1).

RUN pCallDump ("PrepCDR",
               "calldump_prepaid.p",
               TODAY - 1).
                
RUN pCallDump ("MobCDR,ErrorCDR",
               "error_calldump.p",
               TODAY - 1).
 
fELog("DAILY","CallDumpStopped").

QUIT.


PROCEDURE pCallDump:

   DEF INPUT PARAMETER icTableNames AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icModule     AS CHAR NO-UNDO.
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
   ELSE RUN VALUE(icModule).

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
      ELSE RUN VALUE(icModule).
   END. 
   
END PROCEDURE.


