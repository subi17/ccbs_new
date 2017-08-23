/* dpmember.i         20.08.12/aam
*/
&IF "{&DPMEMBER_I}" NE "YES"
&THEN
&GLOBAL-DEFINE DPMEMBER_I YES

{Syst/commali.i}
{Func/date.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fixedlinefunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

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
      ldaValidTo = fLastDayOfMonth(ldaValidTo).
      
   RETURN ldaValidTo.   
    
END FUNCTION.

FUNCTION fAddDiscountPlanMember RETURNS INTEGER
   (INPUT iiMsSeq           AS INT,
    INPUT icDiscountPlan    AS CHAR,
    INPUT idDiscountAmt     AS DEC,
    INPUT idaFromDate       AS DATE,
    INPUT iiDiscountPeriods AS INT,
    INPUT iiOrderId         AS INT,
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

FUNCTION fCreateAddLineDiscount RETURNS CHARACTER
   (iiMsSeq    AS INT,
    icCLIType  AS CHAR,
    idtDate    AS DATE,
    icDPRuleID AS CHAR):

   DEF VAR lcNewAddLineDisc AS CHAR NO-UNDO.
   DEF VAR liRequest        AS INT  NO-UNDO.
   DEF VAR lcResult         AS CHAR NO-UNDO.

   IF icDPRuleID NE "" THEN lcNewAddLineDisc = icDPRuleID. /* reactivation */
   ELSE lcNewAddLineDisc = ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}),
                               {&ADDLINE_DISCOUNTS}).

   FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = gcBrand          AND
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

      liRequest = fAddDiscountPlanMember(iiMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         DiscountPlan.ValidPeriods,
                                         0,
                                         OUTPUT lcResult).

      IF liRequest NE 0 THEN
         RETURN "ERROR:Additional Line Discount not created; " + lcResult.
   END.

END FUNCTION.

FUNCTION fCloseAddLineDiscount RETURNS LOGICAL
   (iiCustNum AS INT,
    iiMsSeq   AS INT,
    icCLIType AS CHAR,
    idtDate   AS DATE):
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum = iiCustNum NO-ERROR.

   IF NOT fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS}),
                     iiMsSeq,
                     idtDate,
                     FALSE).
   IF NOT fCheckExisting2PConvergent(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_20}),
                     iiMsSeq,
                     idtDate,
                     FALSE).

   /* Additional Line with mobile only ALFMO-5 */
   IF NOT fCheckExistingMobileOnly(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}),
                     iiMsSeq,
                     idtDate,
                     FALSE).

   RETURN TRUE.

END FUNCTION.

FUNCTION fCreateExtraLineDiscount RETURNS CHARACTER
   (INPUT iExtraLineMsSeq AS INT,
    INPUT lcExtraLineDisc AS CHAR,
    INPUT idtDate         AS DATE):
   
   DEF VAR liRequest        AS INT  NO-UNDO.
   DEF VAR lcResult         AS CHAR NO-UNDO.

   FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = gcBrand         AND
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

      liRequest = fAddDiscountPlanMember(iExtraLineMsSeq,
                                         DiscountPlan.DPRuleID,
                                         DPRate.DiscValue,
                                         idtDate,
                                         DiscountPlan.ValidPeriods,
                                         0,
                                         OUTPUT lcResult).

      IF liRequest NE 0 THEN
         RETURN "ERROR:Extra Line Discount not created; " + lcResult.
   END.
END FUNCTION.    

FUNCTION fCloseExtraLineDiscount RETURNS LOGICAL
   (INPUT iExtraLineMsSeq AS INT,
    INPUT lcExtraLineDisc AS CHAR,
    INPUT idtDate         AS DATE):

  FOR FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand    = gcBrand         AND
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
