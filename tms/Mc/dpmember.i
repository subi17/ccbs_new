/* dpmember.i         20.08.12/aam
*/
&IF "{&DPMEMBER_I}" NE "YES"
&THEN
&GLOBAL-DEFINE DPMEMBER_I YES

{Syst/commali.i}
{Syst/eventval.i}
{Syst/tmsconst.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

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
      ldaValidTo = Func.Common:mLastDayOfMonth(ldaValidTo).
      
   RETURN ldaValidTo.   
    
END FUNCTION.

FUNCTION fCloseIncompatibleDiscounts RETURNS LOGICAL
   ( iiMSSeq        AS INTEGER,
     icDiscountPlan AS CHARACTER ):
      
   FOR EACH TMSRelation NO-LOCK USE-INDEX ParentValue WHERE
            TMSRelation.TableName   = "DiscountPlan"   AND
            TMSRelation.KeyType     = "Compatibility"  AND
            TMSRelation.ParentValue = icDiscountPlan:

      /* We need to close every discount for the MobSub
         which are not allowed to exists the same time
         as the new discount */
      IF TMSRelation.RelationType EQ "ParentValue"
      THEN DO:
         IF fCloseDiscount(TMSRelation.ChildValue, /* The dpruleid to close */
                           iiMsSeq,
                           /* last day of the previous month */,
                           NO)
         THEN /* do some memo etc. */
      END.
   END.
   
   FINALLY:
      IF llDoEvent
      THEN fCleanEventObjects().
   END FINALLY.

END FUNCTION.

/* Function checks is the discount allowed for the mobsub.
   It might not be if it is not compatible with the existing
   discounts */
FUNCTION fDiscountAllowed RETURNS LOGICAL
   ( iiMSSeq        AS INTEGER,
     icDiscountPlan AS CHARACTER,
     idaDate        AS DATE ):

   FOR
      EACH TMSRelation NO-LOCK USE-INDEX ParentValue WHERE
         TMSRelation.TableName    = "DiscountPlan"   AND
         TMSRelation.KeyType      = "Compatibility"  AND
         TMSRelation.ParentValue  = icDiscountPlan   AND
         TMSRelation.RelationType = "ChildValue",
      FIRST DiscountPlan NO-LOCK WHERE
         DiscountPlan.Brand = Syst.Var:gcBrand AND
         DiscountPlan.DPRuleID = TMSRelation.ChildValue,
      FIRST DPMember WHERE
         DPMember.DPId = DiscountPlan.DPId    AND
         DPMember.HostTable = "MobSub"        AND
         DPMember.KeyValue  = STRING(iiMsSeq) AND
         DPMember.ValidTo  >= idaDate         AND
         DPMember.ValidFrom <= idaDate:
            
      RETURN FALSE.
   END.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fAddDiscountPlanMember RETURNS CHARACTER
   (INPUT iiMsSeq           AS INT,
    INPUT icDiscountPlan    AS CHAR,
    INPUT idDiscountAmt     AS DEC,
    INPUT idaFromDate       AS DATE,
    INPUT idaToDate         AS DATE,
    INPUT iiDiscountPeriods AS INT,
    INPUT iiOrderId         AS INT):

   DEF VAR ldValidTo AS DATE NO-UNDO.

   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER DiscountPlan FOR DiscountPlan.
   DEF BUFFER DPMember FOR DPMember.

   FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub
   THEN RETURN "ERROR: Subscription not available".

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = icDiscountPlan NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DiscountPlan
   THEN RETURN "ERROR: Unknown Discount Plan".

   IF idaToDate NE ?
   THEN ldValidTo = idaToDate.
   ELSE ldValidTo = fCalcDPMemberValidTo(idaFromDate,iiDiscountPeriods).

   FIND FIRST DPMember WHERE
              DPMember.DPId = DiscountPlan.DPId AND
              DPMember.HostTable = "MobSub" AND
              DPMember.KeyValue  = STRING(iiMsSeq) AND
              DPMember.ValidTo >= idaFromDate AND
              DPMember.ValidFrom <= idaFromDate NO-LOCK NO-ERROR.
   IF AVAILABLE DPMember
   THEN RETURN "ERROR: Discount Plan Member already exists".

   IF idDiscountAmt > 0 THEN DO:
      
      IF ldValidTo EQ ? THEN 
         ldValidTo = 12/31/49.
      
      CREATE DPMember.
      ASSIGN 
         DPMember.DPMemberID = NEXT-VALUE(DPMemberID)
         DPMember.DPId      = DiscountPlan.DPId
         DPMember.HostTable = "MobSub" 
         DPMember.KeyValue  = STRING(MobSub.MsSeq) 
         DPMember.ValidFrom = idaFromDate
         DPMember.ValidTo   = ldValidTo
         DPMember.DiscValue = idDiscountAmt
         DPMember.OrderId   = iiOrderId.
   END. /* IF idDiscountAmt > 0 THEN DO: */

   RETURN "".

END FUNCTION. /* fAddDiscountPlanMember */


FUNCTION fCloseDiscount RETURNS LOGICAL
   (icDiscountPlan AS CHAR,
    iiMsSeq        AS INT,
    idaEndDate     AS DATE,
    ilCleanEventObjects AS LOG):

   DEFINE VARIABLE llDiscountClosed AS LOGICAL INITIAL FALSE NO-UNDO.
   DEF BUFFER DiscountPlan FOR DiscountPlan.
   DEF BUFFER DPMember FOR DPMember.

   FOR FIRST DiscountPlan WHERE
             DiscountPlan.Brand    = Syst.Var:gcBrand AND
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
      llDiscountClosed = TRUE.
   END. /* FOR FIRST DiscountPlan WHERE */

   IF llDoEvent AND
      ilCleanEventObjects THEN
      fCleanEventObjects().

   RETURN llDiscountClosed.

END FUNCTION.

FUNCTION fCloseDPMember RETURNS LOGICAL
   (iiDPMemberID   AS INTEGER,
    idaEndDate     AS DATE,
    ilCleanEventObjects AS LOG):

   DEF BUFFER DPMember FOR DPMember.

   FIND DPMember EXCLUSIVE-LOCK WHERE DPMember.DPMemberId = iiDPMemberID NO-ERROR.
   
   IF AVAILABLE DPMember
   THEN DO:
      /* Log DPMember modification */
      IF llDoEvent THEN DO:
         lhDPMember = BUFFER DPMember:HANDLE.
         RUN StarEventInitialize(lhDPMember).
         RUN StarEventSetOldBuffer(lhDPMember).
      END.
      DPMember.ValidTo = idaEndDate.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).      

      IF llDoEvent AND
         ilCleanEventObjects THEN
         fCleanEventObjects().
   END.
   ELSE RETURN FALSE.

   RETURN TRUE.

END FUNCTION.

FUNCTION fCreateAddLineDiscount RETURNS CHARACTER
   (iiMsSeq    AS INT,
    icCLIType  AS CHAR,
    idtDate    AS DATE,
    icDPRuleID AS CHAR):

   DEF VAR lcNewAddLineDisc AS CHAR NO-UNDO.
   DEF VAR lcResult         AS CHAR NO-UNDO.

   IF icDPRuleID NE "" THEN lcNewAddLineDisc = icDPRuleID. /* reactivation */
   ELSE lcNewAddLineDisc = ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}),
                               {&ADDLINE_DISCOUNTS}).

   FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = Syst.Var:gcBrand          AND
             DiscountPlan.DPRuleID = lcNewAddLineDisc AND
             DiscountPlan.ValidTo >= idtDate,
       FIRST DPRate NO-LOCK WHERE
             DPRate.DPId       = DiscountPlan.DPId AND
             DPRate.ValidFrom <= idtDate           AND
             DPRate.ValidTo   >= idtDate:

      fCloseDiscount(DiscountPlan.DPRuleID,
                     iiMsSeq,
                     idtDate - 1,
                     FALSE).

      lcResult = fAddDiscountPlanMember(iiMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         ?,
                                         DiscountPlan.ValidPeriods,
                                         0).

      IF lcResult > ""
      THEN RETURN "ERROR:Additional Line Discount not created; " + lcResult.
   END.

END FUNCTION.

FUNCTION fCreateExtraLineDiscount RETURNS CHARACTER
   (INPUT iExtraLineMsSeq AS INT,
    INPUT lcExtraLineDisc AS CHAR,
    INPUT idtDate         AS DATE):
   
   DEF VAR lcResult         AS CHAR NO-UNDO.

   FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = Syst.Var:gcBrand         AND
             DiscountPlan.DPRuleID = lcExtraLineDisc AND
             DiscountPlan.ValidTo >= idtDate,
       FIRST DPRate NO-LOCK WHERE
             DPRate.DPId       = DiscountPlan.DPId AND
             DPRate.ValidFrom <= idtDate           AND
             DPRate.ValidTo   >= idtDate:

      fCloseDiscount(DiscountPlan.DPRuleID,
                     iExtraLineMsSeq,
                     idtDate - 1,
                     FALSE).

      lcResult = fAddDiscountPlanMember(iExtraLineMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         ?,
                                         DiscountPlan.ValidPeriods,
                                         0).

      IF lcResult > ""
      THEN RETURN "ERROR:Extra Line Discount not created; " + lcResult.
   END.
END FUNCTION.    

FUNCTION fCloseExtraLineDiscount RETURNS LOGICAL
   (INPUT iExtraLineMsSeq AS INT,
    INPUT lcExtraLineDisc AS CHAR,
    INPUT idtDate         AS DATE):

  FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = Syst.Var:gcBrand         AND
             DiscountPlan.DPRuleID = lcExtraLineDisc AND
             DiscountPlan.ValidTo >= idtDate,
       FIRST DPRate NO-LOCK WHERE
             DPRate.DPId       = DiscountPlan.DPId AND
             DPRate.ValidFrom <= idtDate           AND
             DPRate.ValidTo   >= idtDate:

      fCloseDiscount(DiscountPlan.DPRuleID,
                     iExtraLineMsSeq,
                     idtDate - 1,
                     FALSE).

   END. 

   RETURN TRUE.

END.    

&ENDIF
