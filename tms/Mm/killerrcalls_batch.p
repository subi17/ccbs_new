/* killerrcalls_batch.p   20.01.10/aam (separated from mdaily) 
*/

{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".
Syst.CUICommon:katun = "cron".

{Syst/eventlog.i}
{Func/direct_dbconnect.i}

DEF VAR ldaLimitDate AS DATE NO-UNDO.

fELog("DAILY","killerrcallsRepStarted" + SESSION:PARAMETER).

/* connect to correct cdr dbs */
fInitializeConnectTables("MobCDR,McdrDtl2,ErrorCDR","").

ldaLimitDate = TODAY - 90.

RUN pDirectConnect2Dbs(Syst.CUICommon:gcBrand,
                       "",
                       ldalimitDate - 1,
                       ldaLimitDate - 1).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fELog("DAILY","killerrcallsRep:" + RETURN-VALUE).
   QUIT.
END.

RUN Mm/killerrcalls.p(ldaLimitDate).

fELog("DAILY","killerrcallsRepStopped" + SESSION:PARAMETER).

QUIT.
