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

FUNCTION fCloseDiscount RETURNS LOGICAL
   (icDiscountPlan AS CHAR,
    iiMsSeq        AS INT,
    idaEndDate     AS DATE,
    ilJustCheck    AS LOGICAL):

   DEFINE VARIABLE llDiscountClosed AS LOGICAL INITIAL FALSE NO-UNDO.
   DEF BUFFER DiscountPlan FOR DiscountPlan.
   DEF BUFFER DPMember FOR DPMember.

   IF llDoEvent AND NOT ilJustCheck THEN DO:
      lhDPMember = BUFFER DPMember:HANDLE.
      /* This also calls StarEventInitialize */
      RUN StarEventSetOldBuffer(lhDPMember).
   END.

   FOR FIRST DiscountPlan WHERE
             DiscountPlan.Brand    = Syst.Var:gcBrand AND
             DiscountPlan.DPRuleID = icDiscountPlan NO-LOCK,
       EACH  DPMember WHERE
             DPMember.DPId      = DiscountPlan.DPId AND
             DPMember.HostTable = "MobSub"          AND
             DPMember.KeyValue  = STRING(iiMsSeq)   AND
             DPMember.ValidTo   > idaEndDate        AND
             DPMember.ValidTo  >= DPMember.ValidFrom NO-LOCK:
      
      IF NOT ilJustCheck
      THEN DO TRANSACTION:
         BUFFER DPMember:FIND-CURRENT(EXCLUSIVE-LOCK).

         /* Log DPMember modification */
         DPMember.ValidTo = idaEndDate.

         IF llDoEvent
         THEN RUN StarEventMakeModifyEvent(lhDPMember).

         RELEASE DPMember.
      END.

      llDiscountClosed = TRUE.
   END. /* FOR FIRST DiscountPlan WHERE */

   RETURN llDiscountClosed.

   FINALLY:
      IF llDoEvent AND NOT ilJustCheck
      THEN fCleanEventObject(lhDPMember).
   END FINALLY.

END FUNCTION.

FUNCTION fCloseDPMember RETURNS LOGICAL
   (iiDPMemberID   AS INTEGER,
    idaEndDate     AS DATE):

   DEF BUFFER DPMember FOR DPMember.

   FIND DPMember EXCLUSIVE-LOCK WHERE DPMember.DPMemberId = iiDPMemberID NO-ERROR.
   
   IF AVAILABLE DPMember
   THEN DO:
      /* Log DPMember modification */
      IF llDoEvent THEN DO:
         lhDPMember = BUFFER DPMember:HANDLE.
         /* This also calls StarEventInitialize */
         RUN StarEventSetOldBuffer(lhDPMember).
      END.
      DPMember.ValidTo = idaEndDate.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).
   END.
   ELSE RETURN FALSE.

   RETURN TRUE.

   FINALLY:
      IF llDoEvent
      THEN fCleanEventObject(lhDPMember).
   END FINALLY.

END FUNCTION.

FUNCTION fCloseIncompatibleDiscounts RETURNS CHARACTER
   ( iiMSSeq        AS INTEGER,
     icDiscountPlan AS CHARACTER,
     idaDate        AS DATE,
     ilJustCheck    AS LOGICAL ):

   DEFINE BUFFER TMSRelation FOR TMSRelation.
   DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.

   /* We need to close every discount for the MobSub
      which are not allowed to exists the same time
      as the new discount */
   FOR EACH TMSRelation NO-LOCK WHERE
            TMSRelation.TableName    = "DiscountPlan"   AND
            TMSRelation.KeyType      = "Compatibility"  AND
            TMSRelation.ChildValue   = icDiscountPlan   AND
            TMSRelation.RelationType = "ChildValue":

      IF TMSRelation.ToTime   < NOW OR
         TMSRelation.FromTime > NOW
      THEN NEXT.

      IF fCloseDiscount(TMSRelation.ParentValue, /* The dpruleid to close */
                        iiMsSeq,
                        idaDate,
                        ilJustCheck)
      THEN DO:
         lcReturnValue = lcReturnValue + ", " + TMSRelation.ParentValue.

         IF NOT ilJustCheck
         THEN Func.Common:mWriteMemo("MobSub",
                                     STRING(MobSub.MsSeq),
                                     MobSub.CustNum,
                                     "Automatic discount plan member closing",
                                     SUBSTITUTE("Closing the existing discount plan member &1 to date &2 as it is not compatible with &3",
                                                TMSRelation.ParentValue,
                                                idaDate,
                                                icDiscountPlan)).
      END.
   END.
   
   RETURN SUBSTRING(lcReturnValue,3).

END FUNCTION.

/* Function checks is the discount allowed for the mobsub.
   It might not be if it is not compatible with the existing
   discounts.
   The return value is the DPRuleID of the incompatible
   discount or empty if the discount is allowed */
FUNCTION fDiscountAllowed RETURNS CHARACTER
   ( iiMSSeq        AS INTEGER,
     icDiscountPlan AS CHARACTER,
     idaDate        AS DATE ):

   DEFINE BUFFER TMSRelation FOR TMSRelation.
   DEFINE BUFFER DiscountPlan FOR DiscountPlan.
   DEFINE BUFFER DPMember FOR DPMember.

   FOR
      EACH TMSRelation NO-LOCK USE-INDEX ParentValue WHERE
         TMSRelation.TableName    = "DiscountPlan"   AND
         TMSRelation.KeyType      = "Compatibility"  AND
         TMSRelation.ChildValue   = icDiscountPlan   AND
         TMSRelation.RelationType = "ParentValue":

      IF TMSRelation.ToTime   < NOW OR
         TMSRelation.FromTime > NOW
      THEN NEXT.

      FOR
         FIRST DiscountPlan NO-LOCK WHERE
            DiscountPlan.Brand    = Syst.Var:gcBrand AND
            DiscountPlan.DPRuleID = TMSRelation.ParentValue,
         FIRST DPMember NO-LOCK WHERE
            DPMember.DPId = DiscountPlan.DPId    AND
            DPMember.HostTable = "MobSub"        AND
            DPMember.KeyValue  = STRING(iiMsSeq) AND
            DPMember.ValidTo  >= idaDate         AND
            DPMember.ValidFrom <= idaDate:
                  
         RETURN DiscountPlan.DPRuleID.
      END.
   END.
   
   RETURN "".

END FUNCTION.

FUNCTION fAddDiscountPlanMember RETURNS CHARACTER
   (INPUT iiMsSeq           AS INT,
    INPUT icDiscountPlan    AS CHAR,
    INPUT idDiscountAmt     AS DEC,
    INPUT idaFromDate       AS DATE,
    INPUT idaToDate         AS DATE,
    INPUT iiDiscountPeriods AS INT,
    INPUT iiOrderId         AS INT):

   DEFINE VARIABLE ldValidTo AS DATE      NO-UNDO.
   DEFINE VARIABLE lcResult  AS CHARACTER NO-UNDO.

   IF NOT idDiscountAmt > 0
   THEN RETURN "ERROR: Discount member value must be greater than zero".

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
   THEN DO:
      IF fCloseDPMember(DPMember.DPMember,
                        DATE(MONTH(idaFromDate), 1, YEAR(idaFromDate)) - 1) /* last day of the previous month */
      THEN DO:
         lcResult = SUBSTITUTE("Resetting the discount plan member &1 to start from date &2.",
                               icDiscountPlan,
                               idaFromDate).
         Func.Common:mWriteMemo("MobSub",
                                  STRING(MobSub.MsSeq),
                                  MobSub.CustNum,
                                  "Discount plan member reset",
                                  lcResult).
   END.
   ELSE DO:
      lcResult = fDiscountAllowed(MobSub.MsSeq, icDiscountPlan, idaFromDate).
      IF lcResult > ""
      THEN RETURN SUBSTITUTE("ERROR: Discount &1 is not allowed as it is not compatible with &2", icDiscountPlan, lcResult).
      lcResult = fCloseIncompatibleDiscounts(MobSub.MsSeq, icDiscountPlan, DATE(MONTH(idaFromDate), 1, YEAR(idaFromDate)) - 1, NO).

      IF lcResult > ""
      THEN lcResult = SUBSTITUTE("Closed the existing discount plan members &1 to date &2 as they are not compatible with &3",
                                 lcResult,
                                 idaFromDate,
                                 icDiscountPlan).
   END.

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

   RETURN lcResult.

END FUNCTION. /* fAddDiscountPlanMember */

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
                     NO).

      lcResult = fAddDiscountPlanMember(iiMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         ?,
                                         DiscountPlan.ValidPeriods,
                                         0).

      IF lcResult BEGINS "ERROR"
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
                     NO).

      lcResult = fAddDiscountPlanMember(iExtraLineMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         ?,
                                         DiscountPlan.ValidPeriods,
                                         0).

      IF lcResult BEGINS "ERROR"
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
                     NO).

   END. 

   RETURN TRUE.

END.    

&ENDIF
