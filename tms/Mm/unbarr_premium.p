/* ----------------------------------------------------------------------
  MODULE .......: unbarr_premium.p
  TASK .........: Automatic release of Y_HURP barring (YDR-119) 
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
{barrfunc.i}

DEF VAR lrBarring AS ROWID NO-UNDO.
DEF VAR lcBarring AS CHAR NO-UNDO. 
DEF VAR lcResult AS CHAR NO-UNDO.

/* don't run 1st day of month because of invoice run */
IF SESSION:BATCH AND DAY(TODAY) = 1 THEN RETURN.

FOR EACH MobSub NO-LOCK WHERE
   MobSub.Brand = gcbrand and
   MobSub.MsStatus = {&MSSTATUS_BARRED} and
   MobSub.ActivationDate < TODAY - 90 AND
   MobSub.ActivationDate > TODAY - 95 AND
   MobSub.PayType = FALSE USE-INDEX MsStatus:
   
   /* check current barring (or pending) */
   lcBarring  = fCheckBarrStatus(MobSub.MsSeq, OUTPUT lrBarring).
   
   IF lcBarring = "Y_HURP" THEN lcBarring = "UN" + lcBarring.
   ELSE NEXT.
   
   FIND MsRequest WHERE ROWID(MsRequest) = lrBarring NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest OR MsRequest.UserCode NE "CreSub / CreSub" THEN NEXT.

   /* create barring request */
   RUN barrengine.p (MobSub.MsSeq,
                   lcBarring,
                   {&REQUEST_SOURCE_SCRIPT}, /* source  */
                   "", /* creator */
                   fMakeTS(), /* activate */
                   "", /* SMS */
                   OUTPUT lcResult).
END.
