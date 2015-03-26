/* monthlysubcount.p    04.01.11/aam 
*/

{commpaa.i}
katun = "cron".
gcBrand = "1".

{direct_dbconnect.i}
{eventlog.i}

DEF VAR ldaPrevPeriod AS DATE NO-UNDO.


fELog("MONTHLYSUBCOUNT","Started").

/* connect to correct cdr dbs */
fInitializeConnectTables("PrepCDR","").

ldaPrevPeriod = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.

RUN pDirectConnect2Dbs(gcBrand,
                       "",
                       ldaPrevPeriod,
                       ldaPrevPeriod).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("MONTHLYSUBCOUNT",RETURN-VALUE).
   QUIT.
END.

RUN monthlysubcount_report.p (TODAY).

fELog("MONTHLYSUBCOUNT","Finished").

