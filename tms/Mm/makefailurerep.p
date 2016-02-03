{Syst/commpaa.i}
gcBrand = "1".
katun = "cron".

{Func/lib/eventlog.i}
{Func/direct_dbconnect.i}

DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR ldaRunFrom  AS DATE NO-UNDO EXTENT 2.
DEF VAR ldaRunTo    AS DATE NO-UNDO EXTENT 2.
DEF VAR liState     AS INT  NO-UNDO.
DEF VAR liRun       AS INT  NO-UNDO.

fELog("WEEKLY","makefailurerepStarted").

/* previous week from sunday to saturday */
ASSIGN
   ldaFromDate   = TODAY - 6 - WEEKDAY(TODAY)
   ldaToDate     = TODAY - WEEKDAY(TODAY)
   ldaRunFrom[1] = ldaFromDate
   ldaRunTo[1]   = ldaToDate.
   
/* do two runs if dbs have been renewed during the week
   note; by default all dbs have been renewed at the same time, so check
   only first */
RUN pGetDBPeriods(gcBrand,
                  "MobCDR",
                  ldaFromDate,
                  ldaToDate,
                  OUTPUT ldaRunFrom[1],
                  OUTPUT ldaRunTo[1],
                  OUTPUT ldaRunFrom[2],
                  OUTPUT ldaRunTo[2]).

 
DO liRun = 1 TO 2:

   IF ldaRunFrom[liRun] = ? THEN NEXT.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("MobCDR,McdrDtl2,ErrorCDR","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",  
                          ldaRunTo[liRun],
                          ldaRunTo[liRun]).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fELog("WEEKLY","makefailurerep:" + RETURN-VALUE).
      QUIT.
   END.

   RUN errorcdr_dump.p (ldaRunFrom[liRun],
                        ldaRunTo[liRun],
                        ldaFromDate,
                        ldaToDate).
END.                        
                     
fELog("WEEKLY","makefailurerepStopped").

