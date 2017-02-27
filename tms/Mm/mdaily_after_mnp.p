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
DEF VAR limaxtenantid AS INT NO-UNDO.

limaxtenantid = fgetMaxtenantid().
fELog("MDAILY_AMNP","HighSpenderStarted").
/* Go through all tenants */
DO liid = 0 to limaxtenantid:
   IF NOT fsetEffectiveTenantIdForAllDB(liid) THEN
      LEAVE.
   RUN Mm/highusagerep.p(INPUT fMake2Dt(INPUT today - 90, INPUT 0),0).
END.
fELog("MDAILY_AMNP","HighSpenderStopped").
fELog("MDAILY_AMNP","IccMSISDNRepStarted").
/* Go through all tenants */
DO liid = 0 to limaxtenantid:
   IF NOT fsetEffectiveTenantIdForAllDB(liid) THEN 
      LEAVE.
   RUN Mm/icc_msisdn_rep.p.
END.
fELog("MDAILY_AMNP","IccMSISDNRepStopped").

quit.
