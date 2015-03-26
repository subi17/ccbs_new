/* tapfilerun.p    04.01.11/aam 
*/

{commpaa.i}
katun = "cron".
gcBrand = "1".

{direct_dbconnect.i}
{eventlog.i}

DEF VAR ldaEventDate AS DATE NO-UNDO.


fELog("TAPFILE","Started").

/* connect to correct cdr dbs */
fInitializeConnectTables("RoamCDR","").

ldaEventDate = TODAY - 1.

RUN pDirectConnect2Dbs(gcBrand,
                       "",
                       ldaEventDate,
                       ldaEventDate).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("TAPFILE",RETURN-VALUE).
   QUIT.
END.

RUN tapfilecr.p("",
                ldaEventDate,
                ldaEventDate,
                "",
                FALSE).

fELog("TAPFILE","Finished").

