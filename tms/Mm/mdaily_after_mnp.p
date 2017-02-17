/* mdaily_after_mnp cron job */
{Syst/commpaa.i}
{Func/timestamp.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}
{Syst/tmsconst.i}
{Func/multitenantfunc.i}

def var period  as int    no-undo.
DEF VAR liQty   AS INT    NO-UNDO.
DEF VAR liDel   AS INT    NO-UNDO. 
DEF VAR lcError AS CHAR   NO-UNDO. 
DEF VAR oiQty   AS INT    NO-UNDO.
DEF VAR liid    AS INT    NO-UNDO.

fELog("MDAILY_AMNP","HighSpenderStarted").
DO liid = 0 to {&MAX_TENANT_ID}:
   SET-EFFECTIVE-TENANT(liid, LDBNAME(1)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      LEAVE.
   RUN Mm/highusagerep.p(INPUT fMake2Dt(INPUT today - 90, INPUT 0),0).
END.
fELog("MDAILY_AMNP","HighSpenderStopped").
fELog("MDAILY_AMNP","IccMSISDNRepStarted").
DO liid = 0 to {&MAX_TENANT_ID}:
   fsetEffectiveTenantIdForAllDB(liid).
   IF ERROR-STATUS:ERROR THEN DO:
      LEAVE.
   END.
   RUN Mm/icc_msisdn_rep.p.
END.
fELog("MDAILY_AMNP","IccMSISDNRepStopped").

quit.
