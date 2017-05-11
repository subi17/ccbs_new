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
DEF VAR lcIdList AS CHAR NO-UNDO.

lcIdList = fgetTenantIds().
/* Go through all possible tenants */
/* MB-736 high usage report to dumptool
   MB-737 icc/msisdn stock to dumptool
DO liid = 1 to NUM-ENTRIES(lcIdList):
   fELog("MDAILY_AMNP","HighSpenderStarted for " + 
      fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))).
   IF TENANT-ID(LDBNAME(1)) EQ -1 AND 
      NOT fsetEffectiveTenantIdForAllDB(INT(ENTRY(liid,lcIdList))) THEN
      LEAVE.
   RUN Mm/highusagerep.p(INPUT fMake2Dt(INPUT today - 90, INPUT 0),0).
   fELog("MDAILY_AMNP","HighSpenderStopped for " +
         fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))).
END.

DO liid = 1 to NUM-ENTRIES(lcIdList):
fELog("MDAILY_AMNP","IccMSISDNRepStarted for " +
      fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))).
/* Go through all tenants */
   IF TENANT-ID(LDBNAME(1)) EQ -1 AND 
      NOT fsetEffectiveTenantIdForAllDB(INT(ENTRY(liid,lcIdList))) THEN 
      LEAVE.
   RUN Mm/icc_msisdn_rep.p.

   fELog("MDAILY_AMNP","IccMSISDNRepStopped for " +
         fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))).
END.
*/
quit.
