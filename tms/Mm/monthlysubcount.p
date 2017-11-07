/* monthlysubcount.p    04.01.11/aam 
*/

{Syst/commpaa.i}
Syst.Var:katun = "cron".
Syst.Var:gcBrand = "1".

{Func/direct_dbconnect.i}
{Syst/eventlog.i}

DEF VAR ldaPrevPeriod AS DATE NO-UNDO.


fELog("MONTHLYSUBCOUNT","Started").

/* connect to correct cdr dbs */
fInitializeConnectTables("PrepCDR","").

ldaPrevPeriod = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.

RUN pDirectConnect2Dbs(Syst.Var:gcBrand,
                       "",
                       ldaPrevPeriod,
                       ldaPrevPeriod).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("MONTHLYSUBCOUNT",RETURN-VALUE).
   QUIT.
END.

RUN Mm/monthlysubcount_report.p (TODAY).

fELog("MONTHLYSUBCOUNT","Finished").

