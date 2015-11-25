/* dpmember.i         20.08.12/aam
*/
&IF "{&DPMEMBER_I}" NE "YES"
&THEN
&GLOBAL-DEFINE DPMEMBER_I YES

{commali.i}
{date.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDPMember AS HANDLE NO-UNDO.
END.

FUNCTION fCalcDPMemberValidTo RETURNS DATE
   (idaValidFrom AS DATE,
    iiValidPeriods AS INT):

   DEF VAR ldaValidTo AS DATE NO-UNDO.
          
   IF idaValidFrom = ? THEN RETURN ?.
   IF iiValidPeriods <= 0 THEN RETURN ?.
   
   ASSIGN
      ldaValidTo = ADD-INTERVAL(idaValidFrom,iiValidPeriods - 1,"months")
      ldaValidTo = fLastDayOfMonth(ldaValidTo).
      
   RETURN ldaValidTo.   
    
END FUNCTION.

FUNCTION fAddDiscountPlanMember RETURNS INTEGER
   (INPUT iiMsSeq           AS INT,
    INPUT icDiscountPlan    AS CHAR,
    INPUT idDiscountAmt     AS DEC,
    INPUT idaFromDate       AS DATE,
    INPUT iiDiscountPeriods AS INT,
    OUTPUT ocError          AS CHAR):

   DEF VAR ldValidTo AS DATE NO-UNDO.

   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER DiscountPlan FOR DiscountPlan.
   DEF BUFFER DPMember FOR DPMember.

   FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      ocError = "ERROR: Subscription not available".
      RETURN 1.
   END.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPRuleID = icDiscountPlan NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DiscountPlan THEN DO:
      ocError = "ERROR: Unknown Discount Plan".
      RETURN 1.
   END.

   ldValidTo = fCalcDPMemberValidTo(idaFromDate,iiDiscountPeriods).

   FIND FIRST DPMember WHERE
              DPMember.DPId = DiscountPlan.DPId AND
              DPMember.HostTable = "MobSub" AND
              DPMember.KeyValue  = STRING(iiMsSeq) AND
              DPMember.ValidTo >= idaFromDate AND
              DPMember.ValidFrom <= idaFromDate NO-LOCK NO-ERROR.
   IF AVAILABLE DPMember THEN DO:
      ocError = "ERROR: Discount Plan Member already exists".
      RETURN 1.
   END.

   IF idDiscountAmt > 0 THEN DO:
      CREATE DPMember.
      ASSIGN 
         DPMember.DPId      = DiscountPlan.DPId
         DPMember.HostTable = "MobSub" 
         DPMember.KeyValue  = STRING(MobSub.MsSeq) 
         DPMember.ValidFrom = idaFromDate
         DPMember.ValidTo   = ldValidTo
         DPMember.DiscValue = idDiscountAmt.
   END. /* IF idDiscountAmt > 0 THEN DO: */

   RETURN 0.
END FUNCTION. /* fAddDiscountPlanMember */


FUNCTION fCloseDiscount RETURNS LOGICAL
   (icDiscountPlan AS CHAR,
    iiMsSeq        AS INT,
    idaEndDate     AS DATE,
    ilCleanEventObjects AS LOG):

   DEF BUFFER DiscountPlan FOR DiscountPlan.
   DEF BUFFER DPMember FOR DPMember.

   FOR FIRST DiscountPlan WHERE
             DiscountPlan.Brand    = gcBrand AND
             DiscountPlan.DPRuleID = icDiscountPlan NO-LOCK,
       EACH  DPMember WHERE
             DPMember.DPId      = DiscountPlan.DPId AND
             DPMember.HostTable = "MobSub"          AND
             DPMember.KeyValue  = STRING(iiMsSeq)   AND
             DPMember.ValidTo   > idaEndDate        AND
             DPMember.ValidTo  >= DPMember.ValidFrom EXCLUSIVE-LOCK:
      
      /* Log DPMember modification */
      IF llDoEvent THEN DO:
         lhDPMember = BUFFER DPMember:HANDLE.
         RUN StarEventInitialize(lhDPMember).
         RUN StarEventSetOldBuffer(lhDPMember).
      END.
      DPMember.ValidTo = idaEndDate.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).

   END. /* FOR FIRST DiscountPlan WHERE */

   IF llDoEvent AND
      ilCleanEventObjects THEN
      fCleanEventObjects().

   RETURN TRUE.

END FUNCTION.

&ENDIF
