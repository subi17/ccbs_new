/* ----------------------------------------------------------------------
  MODULE .......: unbarr_premium.p
  TASK .........: Automatic release of Prod_TotalPremium barring (YDR-119) 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 07/2010
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{tmsconst.i}

DEF VAR lcResult  AS CHAR NO-UNDO.

/* don't run 1st day of month because of invoice run */
IF SESSION:BATCH AND DAY(TODAY) = 1 THEN RETURN.

FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand = gcbrand and
         MobSub.MsStatus = {&MSSTATUS_BARRED} and
         MobSub.ActivationDate < TODAY - 90 AND
         MobSub.ActivationDate > TODAY - 95 AND
         MobSub.PayType = FALSE USE-INDEX MsStatus:
   
   FIND FIRST Barring NO-LOCK WHERE
              Barring.MsSeq = MobSub.MsSeq AND
              Barring.BarringCode = "Prod_TotalPremium_Off"
   USE-INDEX MsSeq NO-ERROR.

   IF AVAIL Barring AND
            Barring.BarringStatus EQ {&BARR_STATUS_ACTIVE} AND
            Barring.UserCode EQ "CreSub / CreSub" THEN
      RUN barrengine.p (MobSub.MsSeq,
                        "Prod_TotalPremium_Off=0",
                        {&REQUEST_SOURCE_SCRIPT}, /* source  */
                        "", /* creator */
                        fMakeTS(), /* activate */
                        "", /* SMS */
                        OUTPUT lcResult).
END.

