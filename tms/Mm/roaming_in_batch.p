/* roaming_in_batch.p    04.01.11/aam 
*/

{commpaa.i}
katun = "cron".
gcBrand = "1".

{direct_dbconnect.i}
{eventlog.i}

DEF VAR ldaEventDate AS DATE NO-UNDO.
DEF VAR ldaOldDb     AS DATE NO-UNDO.

fELog("ROAM_IN_DUMP","Started").

ldaEventDate = TODAY - 1.

/* connect to correct cdr dbs */
fInitializeConnectTables("RoamCDR","").

RUN pDirectConnect2Dbs(gcBrand,
                       "",
                       ldaEventDate,
                       ldaEventDate).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("ROAM_IN_DUMP",RETURN-VALUE).
   QUIT.
END.

RUN roaming_in_dump.p(ldaEventDate).

/* do two runs if db has been renewed on the event day -> some tickets have
   been saved to old db on the 1. day */
ldaOldDb = ?.
FOR FIRST ttDB WHERE 
          ttDb.ConnName = "" AND
          ttDb.TableName = "RoamCDR",
    FIRST DBConfig NO-LOCK WHERE
          DBConfig.DBConfigId = ttDb.DbConfigId:
  IF DBConfig.FromDate = ldaEventDate THEN ldaOldDb = DbConfig.FromDate - 1.
END.
      
IF ldaOldDb NE ? THEN DO:
   fInitializeConnectTables("RoamCDR","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          ldaOldDb,
                          ldaOldDb).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fELog("ROAM_IN_DUMP",RETURN-VALUE).
      QUIT.
   END.

   RUN roaming_in_dump.p(ldaEventDate).

END.

fELog("ROAM_IN_DUMP","Finished").

