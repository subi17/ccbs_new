&IF "{&INVOICETARGET_I}" NE "YES"
&THEN

&GLOBAL-DEFINE INVOICE_TARGET YES

/* ----------------------------------------------------------------------
  MODULE .......: invoicetarget.i 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 12.05.10
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/femailinvoice.i}

DEFINE VARIABLE lcInvoiceTargetMode AS CHARACTER NO-UNDO.

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
   {Func/create_eventlog.i}
                                    
END.

FUNCTION fAddInvoiceTargetGroup RETURNS INT
   (iiCustnum AS INTEGER,
    iiDelType AS INTEGER,
    OUTPUT ocError AS CHARACTER):

   DEF VAR liITGroupId AS INT NO-UNDO. 
   DEF VAR lcResult    AS CHAR NO-UNDO.

   DEF BUFFER bCustomer FOR Customer. 
   DEF BUFFER bInvoiceTargetGroup FOR InvoiceTargetGroup.

   FIND bCustomer WHERE
        bCustomer.CustNum = iiCustNum NO-LOCK NO-ERROR. 
   IF NOT AVAIL bCustomer THEN DO:
      ocError = "Customer not found".
      RETURN 0.
   END.

   liITGroupId = NEXT-VALUE(ITGroupID).

   CREATE bInvoiceTargetGroup.
   ASSIGN bInvoiceTargetGroup.Brand = gcBrand
          bInvoiceTargetGroup.ITGroupId = liITGroupId
          bInvoiceTargetGroup.AgrCust = bCustomer.AgrCust
          bInvoiceTargetGroup.CustNum = bCustomer.CustNum 
          bInvoiceTargetGroup.FromDate = TODAY 
          bInvoiceTargetGroup.ToDate = 12/31/2049
          bInvoiceTargetGroup.DelType = iiDelType.
      
   IF llDoEvent THEN fMakeCreateEvent((BUFFER bInvoiceTargetGroup:HANDLE),
                                      "",
                                      katun,
                                      "").

   RELEASE bInvoiceTargetGroup.

   /* Send validation email to customer */
   IF iiDelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} AND
      NOT fPendingEmailActRequest(INPUT bCustomer.Custnum) THEN
      fEmailInvoiceRequest(INPUT fMakeTS(),
                           INPUT TODAY,
                           INPUT katun,
                           INPUT 0, /* msseq */
                           INPUT "", /* cli */
                           INPUT bCustomer.CustNum,
                           INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                           INPUT bCustomer.Email,
                           INPUT 0, /*orderid*/
                           OUTPUT lcResult).

   RETURN liITGroupId.
END FUNCTION. 

FUNCTION fSetDefaultInvoiceTargetGroup RETURNS LOGICAL
   (iiITGroupID AS INT,
    OUTPUT ocError AS CHARACTER):

   DEF BUFFER bInvoiceTargetGroup FOR InvoiceTargetGroup. 
   DEF BUFFER bInvoiceTargetGroupOther FOR InvoiceTargetGroup. 
   
   FIND bInvoiceTargetGroup EXCLUSIVE-LOCK WHERE
        bInvoiceTargetGroup.ITGroupId = iiITGroupId NO-ERROR. 
   IF NOT AVAIL bInvoiceTargetGroup THEN DO:
      ocError = "Invoice target group not found".
      RETURN FALSE.
   END.
   
   IF bInvoiceTargetGroup.DefaultGroup EQ TRUE THEN DO:
      ocError = "Group is already default group".
      RETURN FALSE.
   END.  

   IF NOT bInvoiceTargetGroup.ToDate >= TODAY THEN DO:
      ocError = "Cannot set inactive group as default group".
      RETURN FALSE.
   END.  

   IF llDoEvent THEN DO:
      RUN StarEventInitialize((BUFFER binvoicetargetGroup:HANDLE)).
      RUN StarEventInitialize((BUFFER binvoicetargetGroupOther:HANDLE)).
      RUN StarEventSetOldBuffer ((BUFFER bInvoiceTargetGroup:HANDLE)).
   END.

   ASSIGN bInvoiceTargetGroup.DefaultGroup = TRUE.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent ((BUFFER bInvoiceTargetGroup:HANDLE)).

   FOR EACH bInvoiceTargetGroupOther EXCLUSIVE-LOCK WHERE
            bInvoiceTargetGroupOther.Custnum = bInvoiceTargetGroup.Custnum AND
            bInvoiceTargetGroupOther.DefaultGroup = TRUE AND
            ROWID(bInvoiceTargetGroupOther) NE ROWID(bInvoiceTargetGroup)
            USE-INDEX Custnum:
     IF llDoEvent THEN RUN StarEventSetOldBuffer ((BUFFER bInvoiceTargetGroupOther:HANDLE)).
     bInvoiceTargetGroupOther.DefaultGroup = FALSE.
     IF llDoEvent THEN RUN StarEventMakeModifyEvent ((BUFFER bInvoiceTargetGroupOther:HANDLE)).
   END.
   
   RELEASE bInvoiceTargetGroup. 

   IF llDoEvent THEN fCleanEventObjects().

   RETURN TRUE.

END FUNCTION. 

FUNCTION fGetDefaultInvoiceTargetGroup RETURNS INT
   (iiCustnum AS INTEGER):
   
   DEF BUFFER bInvoiceTargetGroup FOR InvoiceTargetGroup.

   FOR EACH bInvoiceTargetGroup NO-LOCK WHERE
            bInvoiceTargetGroup.Custnum = iiCustnum AND
            bInvoiceTargetGroup.ToDate >= TODAY AND
            bInvoiceTargetGroup.DefaultGroup = TRUE USE-INDEX Custnum:
      RETURN bInvoiceTargetGroup.ITGroupId.
   END.

   RETURN 0.

END FUNCTION. 

FUNCTION fGetMultiLineInvoiceTargetGroup RETURNS INT
   (iiCustnum AS INTEGER,
    icAllowedCLIType AS CHARACTER):
   
   DEF BUFFER InvoiceTargetGroup FOR InvoiceTargetGroup.
   DEF BUFFER InvoiceTarget      FOR InvoiceTarget.
   DEF BUFFER MobSub             FOR MobSub.

   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Custnum = iiCustnum :

      IF LOOKUP(MobSub.CLiType,icAllowedCLIType) = 0 THEN NEXT.
      
      FIND FIRST InvoiceTarget NO-LOCK WHERE
                 InvoiceTarget.MsSeq = MobSub.MsSeq AND
                 InvoiceTarget.FromDate <= TODAY AND
                 InvoiceTarget.ToDate >= TODAY NO-ERROR.
      IF NOT AVAIL InvoiceTarget THEN NEXT.

      FIND FIRST InvoiceTargetGroup NO-LOCK WHERE
                 InvoiceTargetGroup.ITGroupId = InvoiceTarget.ITGroupId AND
                 InvoiceTargetGroup.ToDate >= TODAY NO-ERROR.
      IF AVAIL InvoiceTargetGroup THEN RETURN InvoiceTargetGroup.ITGroupId.
   END.

   RETURN 0.

END FUNCTION. 

FUNCTION fGetFusionInvoiceTargetGroup RETURNS INT
   (iiCustnum AS INTEGER):
   
   DEF BUFFER InvoiceTargetGroup FOR InvoiceTargetGroup.
      
   FIND FIRST InvoiceTargetGroup NO-LOCK WHERE
              InvoiceTargetGroup.Custnum = iiCustnum AND
              InvoiceTargetGroup.ToDate >= TODAY AND
             (InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
              InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL}) NO-ERROR.

   IF AVAIL InvoiceTargetGroup THEN RETURN InvoiceTargetGroup.ITGroupId.

   RETURN 0.

END FUNCTION. 

FUNCTION fDeactivateInvoiceTargetGroup RETURNS LOGICAL
   (iiITGroupID AS INT, 
    OUTPUT ocError AS CHARACTER):

   DEF BUFFER lbInvoiceTargetGroup FOR InvoiceTargetGroup. 
   DEF BUFFER lbInvoiceTarget FOR InvoiceTarget.

   FIND lbInvoiceTargetGroup WHERE
        lbInvoiceTargetGroup.ITGroupId = iiITGroupID NO-LOCK NO-ERROR. 
   IF NOT AVAIL lbInvoiceTargetGroup THEN DO:
      ocError = "Invoice target group not found".
      RETURN FALSE.
   END.

   IF lbInvoiceTargetGroup.Todate < TODAY THEN DO:
      ocError = "Invoice target group is already deactivated".
      RETURN FALSE.
   END.
   
   IF lbInvoiceTargetGroup.DefaultGroup = TRUE THEN DO:
      ocError = "Default invoice target group cannot be deactivated".
      RETURN FALSE.
   END.

   FOR EACH lbInvoiceTarget WHERE
            lbInvoiceTarget.ITGroupID EQ lbInvoiceTargetGroup.ITGroupID AND
            lbInvoiceTarget.ToDate >= TODAY NO-LOCK:
      IF CAN-FIND(FIRST MobSub WHERE
                        MobSub.MsSeq = lbInvoiceTarget.MsSeq AND
                        MobSub.PayType = FALSE) THEN DO:
         ocError = "Invoice target group contains active targets".
         RETURN FALSE.
      END.
   END.

   IF llDoEvent THEN DO:
      RUN StarEventInitialize ((BUFFER lbInvoiceTargetGroup:HANDLE)).
      RUN StarEventSetOldBuffer ((BUFFER lbInvoiceTargetGroup:HANDLE)).
   END.
   
   FIND CURRENT lbInvoiceTargetGroup EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN lbInvoiceTargetGroup.Todate = TODAY - 1.
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent ((BUFFER lbInvoiceTargetGroup:HANDLE)).
   
   RELEASE lbInvoiceTargetGroup. 
   
   IF llDoEvent THEN fCleanEventObjects().
   
   RETURN TRUE.

END FUNCTION. 

FUNCTION fActivateInvoiceTargetGroup RETURNS LOGICAL
   (iiITGroupID AS INT, 
    OUTPUT ocError AS CHARACTER):

   DEF BUFFER bInvoiceTargetGroup FOR InvoiceTargetGroup. 

   FIND bInvoiceTargetGroup WHERE
        bInvoiceTargetGroup.ITGroupId = iiITGroupID NO-LOCK NO-ERROR. 
   IF NOT AVAIL bInvoiceTargetGroup THEN DO:
      ocError = "Invoice target group not found".
      RETURN FALSE.
   END.
   
   IF bInvoiceTargetGroup.Todate >= TODAY THEN DO:
      ocError = "Invoice target group is already activated".
      RETURN FALSE.
   END.

   IF llDoEvent THEN DO:
      RUN StarEventInitialize ((BUFFER bInvoiceTargetGroup:HANDLE)).
      RUN StarEventSetOldBuffer ((BUFFER bInvoiceTargetGroup:HANDLE)).
   END.
   
   FIND CURRENT bInvoiceTargetGroup EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN bInvoiceTargetGroup.Todate = 12/31/2049.
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent ((BUFFER bInvoiceTargetGroup:HANDLE)).
   
   RELEASE bInvoiceTargetGroup. 
   
   IF llDoEvent THEN fCleanEventObjects().
   
   RETURN TRUE.

END FUNCTION. 

FUNCTION _fAddInvoiceTarget RETURNS INT
   (iiITGroupID AS INTEGER,
    iiMsSeq AS INTEGER,
    OUTPUT ocError AS CHARACTER):

   DEF BUFFER lbMobsub FOR MobSub.
   DEF VAR liInvoiceTargetId AS INT NO-UNDO.
   DEF BUFFER lbInvoiceTargetGroup FOR InvoiceTargetGroup.
   DEF BUFFER bInvoiceTarget FOR InvoiceTarget. 

   FIND lbInvoiceTargetGroup WHERE
        lbInvoiceTargetGroup.ITGroupId = iiITGroupId NO-LOCK NO-ERROR. 
   IF NOT AVAIL lbInvoiceTargetGroup THEN DO:
      ocError = SUBST("Invoice target group &1 not found", iiITGroupId).
      RETURN 0.
   END.

   FIND lbMobSub WHERE
        lbMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL lbMobSub THEN DO:
      ocError = SUBST("Subcription &1 is not found or terminated", iiMsSeq).
      RETURN 0.
   END.

   IF lbMobSub.PayType EQ TRUE THEN DO:
      ocError = "Invoice target is prepaid".
      RETURN 0.
   END.
   
   IF lbMobSub.AgrCust NE lbInvoiceTargetGroup.AgrCust OR
      lbMobSub.InvCust NE lbInvoiceTargetGroup.Custnum THEN DO:
      ocError = "Invoice target and group customer roles are different".
      RETURN 0.
   END.

   FIND FIRST bInvoiceTarget WHERE
      bInvoiceTarget.MsSeq = iiMsSeq AND
      bInvoiceTarget.FromDate <= TODAY AND
      bInvoiceTarget.ToDate >= TODAY
   NO-LOCK NO-ERROR.
   IF AVAIL bInvoiceTarget THEN DO:
      ocError = "Invoice target is active in group " + STRING(bInvoiceTarget.ITGroupId).
      RETURN 0.
   END.

   liInvoiceTargetId = NEXT-VALUE(InvoiceTargetID).

   CREATE bInvoiceTarget. 
   ASSIGN bInvoiceTarget.InvoiceTargetId = liInvoiceTargetId
          bInvoiceTarget.ITGroupId = lbInvoiceTargetGroup.ITGroupId 
          bInvoiceTarget.MsSeq = iiMsSeq 
          bInvoiceTarget.FromDate = TODAY 
          bInvoiceTarget.ToDate = 12/31/2049.
   
   IF llDoEvent THEN fMakeCreateEvent((BUFFER bInvoiceTarget:HANDLE),
                                      "",
                                      katun,
                                      "").
   
   RELEASE bInvoiceTarget.

   RETURN liInvoicetargetId.

END FUNCTION. 

FUNCTION fIsActiveInvoiceTarget RETURNS LOGIC
   (iiInvoiceTargetId AS INTEGER,
    icSource AS CHAR,
    OUTPUT ocError AS CHARACTER):
   
   DEF BUFFER lbInvoiceTarget FOR InvoiceTarget. 
   DEF BUFFER lbMobsub FOR MobSub. 

   FIND lbInvoiceTarget WHERE
        lbInvoiceTarget.InvoiceTargetId = iiInvoiceTargetId NO-LOCK NO-ERROR.
   IF NOT AVAIL lbInvoiceTarget THEN DO:
      ocError = "Invoice target not found".
      RETURN FALSE.
   END.

   FIND lbMobSub WHERE
        lbMobSub.MsSeq = lbInvoiceTarget.MsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAIL lbMobSub THEN DO:
      ocError = "Invoice target is terminated".
      RETURN FALSE.
   END.

   IF LOOKUP(icSource,"STC") = 0 AND lbMobSub.PayType EQ TRUE THEN DO:
      ocError = "Invoice target is prepaid".
      RETURN FALSE.
   END.
   
   IF lbInvoiceTarget.ToDate < TODAY THEN DO: 
      ocError = "Invoice target is deactivated".
      RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION. 

FUNCTION _fDeactivateInvoiceTarget RETURNS LOGICAL
   (iiInvoiceTargetId AS INTEGER,
    icSource AS CHARACTER,
    OUTPUT ocError AS CHARACTER):

   DEF BUFFER bInvoiceTarget FOR InvoiceTarget. 
   
   FIND bInvoiceTarget WHERE
        bInvoiceTarget.InvoiceTargetId = iiInvoiceTargetId NO-LOCK NO-ERROR.
   IF NOT AVAIL bInvoiceTarget THEN DO:
      ocError = "Invoice target not found".
      RETURN FALSE.
   END.

   IF NOT fIsActiveInvoiceTarget(iiInvoiceTargetId,
                                 icSource,
                                 OUTPUT ocError) THEN RETURN FALSE.

   FIND CURRENT bInvoiceTarget EXCLUSIVE-LOCK NO-ERROR.

   IF llDoEvent THEN RUN StarEventInitialize((BUFFER binvoicetarget:HANDLE)).

   IF bInvoiceTarget.FromDate = TODAY AND 
      CAN-FIND(FIRST InvoiceTarget WHERE
                     InvoiceTarget.MsSeq = bInvoiceTarget.MsSeq AND
                     InvoiceTarget.ITGroupID = bInvoiceTarget.ITGroup AND
                     InvoiceTarget.FromDate = TODAY AND
                     InvoiceTarget.ToDate = TODAY - 1 AND
               ROWID(InvoiceTarget) NE ROWID(bInvoiceTarget) USE-INDEX MsSeq)
               THEN DO:
      
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent((BUFFER binvoicetarget:HANDLE)).
      DELETE bInvoiceTarget.
   END.
   ELSE DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer ((BUFFER bInvoiceTarget:HANDLE)).
      bInvoiceTarget.ToDate = TODAY - 1. 
      IF llDoEvent THEN RUN StarEventMakeModifyEvent ((BUFFER bInvoiceTarget:HANDLE)).
      RELEASE bInvoiceTarget.
   END.
   
   IF llDoEvent THEN fCleanEventObjects().
   
   RETURN TRUE.

END FUNCTION. 

FUNCTION fMoveInvoiceTarget RETURNS LOGICAL
   (iiInvoiceTargetId AS INT, 
    iiNewITGroupID AS INT,
    icSource AS CHAR,
    OUTPUT ocError AS CHARACTER):
    
   DEF BUFFER bbInvoiceTarget FOR InvoiceTarget.    
   DEF VAR liInvoiceTargetId AS INT NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.
   DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO.
      
   FIND bbInvoiceTarget WHERE
        bbInvoiceTarget.InvoiceTargetId = iiInvoiceTargetId NO-LOCK NO-ERROR.
   IF NOT AVAIL bbInvoiceTarget THEN DO:
      ocError = "Not found Invoice Target with InvoiceTargetId " + STRING(iiInvoiceTargetId).
      RETURN FALSE.
   END.

   IF bbInvoiceTarget.ITGroupID EQ iiNewITGroupID THEN DO:
      ocError = "Cannot move to same group".
      RETURN FALSE.
   END.

   DO TRANS :
      
      liMsSeq = bbInvoiceTarget.MsSeq.
      IF _fDeactivateInvoiceTarget(bbInvoiceTarget.InvoiceTargetId,
                               icSource,
                               OUTPUT ocError) THEN 
         liInvoiceTargetId =  _fAddInvoiceTarget(iiNewITGroupID,
                                                 liMsSeq,
                                                 OUTPUT ocError).
      IF ocError NE "" THEN UNDO, RETURN FALSE.
   END.
   RETURN TRUE.   

END FUNCTION. 

FUNCTION fGetInvoiceTargetRule RETURNS INT
   (iiCustnum AS INTEGER):
   
   DEF BUFFER lbCustomer FOR Customer.

   FIND lbCustomer WHERE
        lbCustomer.Custnum = iiCustnum NO-LOCK NO-ERROR.
   IF AVAIL lbCustomer THEN RETURN lbCustomer.InvoiceTargetRule.

   RETURN 0.

END FUNCTION. 

FUNCTION _fCreateNewInvoiceTarget RETURNS LOGICAL
   (iiMsSeq AS INTEGER,
    iiCustnum AS INTEGER,
    icCLiType AS CHAR,
    OUTPUT ocError AS CHARACTER):

   DEF VAR liInvoiceTargetGroup AS INT  NO-UNDO.
   DEF VAR liInvoiceTargetRule  AS INT  NO-UNDO.
   DEF VAR lcMultiLineSubsType  AS CHAR NO-UNDO.
   DEF VAR lcFusionSubsType     AS CHAR NO-UNDO.
   DEF VAR liDelType            AS INT  NO-UNDO INIT ?.
   DEF VAR liDefaultInvoiceTargetGroup AS INT  NO-UNDO.

   ASSIGN lcMultiLineSubsType = fCParamC("MULTILINE_SUBS_TYPE")
          lcFusionSubsType    = fCParamC("FUSION_SUBS_TYPE").

   liInvoiceTargetRule = fGetInvoiceTargetRule(iiCustnum).
   
   IF liInvoiceTargetRule = {&INVOICE_TARGET_RULE_UNDEFINED} THEN 
      liInvoiceTargetRule = {&INVOICE_TARGET_RULE_SEPARATE_GROUP}.
   
   IF liInvoiceTargetRule = {&INVOICE_TARGET_RULE_DEFAULT_GROUP} OR
      LOOKUP(icCLiType,lcMultiLineSubsType) > 0 OR
      LOOKUP(icCLiType,lcFusionSubsType) > 0 THEN DO:
   
      IF LOOKUP(icCLiType,lcFusionSubsType) > 0 THEN
         liInvoiceTargetGroup = fGetFusionInvoiceTargetGroup(iiCustnum).

      ELSE IF LOOKUP(icCLiType,lcMultiLineSubsType) > 0 THEN
         liInvoiceTargetGroup = fGetMultiLineInvoiceTargetGroup(iiCustnum,
                                                          lcMultiLineSubsType).

      IF liInvoiceTargetGroup <= 0 THEN DO:
         liDefaultInvoiceTargetGroup = fGetDefaultInvoiceTargetGroup(iiCustnum).

         /* If not Fusion subscription then use default group */
         IF LOOKUP(icCLiType,lcFusionSubsType) = 0 THEN
            liInvoiceTargetGroup = liDefaultInvoiceTargetGroup.
      END.

      IF liInvoiceTargetGroup <= 0 THEN DO:

         IF LOOKUP(icCLiType,lcFusionSubsType) > 0 THEN DO:
            FIND FIRST Customer WHERE
                       Customer.CustNum = iiCustnum NO-LOCK NO-ERROR.
            IF AVAIL Customer AND Customer.DelType = {&INV_DEL_TYPE_EMAIL} THEN
               liDelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
            ELSE
               liDelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
         END. /* IF LOOKUP(icCLiType,lcFusionSubsType) > 0 THEN DO: */
         
         DO TRANS:
            liInvoiceTargetGroup = fAddInvoiceTargetGroup(iiCustNum,liDelType,
                                                          OUTPUT ocError).
            
            IF ocError EQ "" AND liInvoiceTargetGroup NE 0 AND
               liInvoiceTargetRule EQ {&INVOICE_TARGET_RULE_DEFAULT_GROUP} AND
               liDefaultInvoiceTargetGroup <= 0 AND
               LOOKUP(icCLiType,lcFusionSubsType) = 0 THEN
               fSetDefaultInvoiceTargetGroup(liInvoiceTargetGroup, ocError).
            
            IF ocError EQ "" AND liInvoiceTargetGroup NE 0 THEN
               _fAddInvoiceTarget(liInvoiceTargetGroup,iiMsSeq,OUTPUT ocError).

            IF ocError NE "" THEN UNDO, RETURN FALSE.
         END.

      END.
      ELSE _fAddInvoiceTarget(liInvoiceTargetGroup, iiMsSeq, OUTPUT ocError).
   END.
   ELSE DO TRANS:
      liInvoiceTargetGroup = fAddInvoiceTargetGroup(iiCustNum,liDelType,OUTPUT ocError).
      
      IF ocError EQ "" AND liInvoiceTargetGroup NE 0 THEN
         _fAddInvoiceTarget(liInvoiceTargetGroup,iiMsSeq,OUTPUT ocError).

      IF ocError NE "" THEN UNDO, RETURN FALSE.
   END.

   RETURN (ocError NE "").

END FUNCTION. 

/* special handling for prepaid because of possible
   postpaid->prepaid->acc->postpaid */
FUNCTION fACCInvoiceTarget RETURNS LOGICAL
   (iiMsSeq AS INTEGER,
    OUTPUT ocError AS CHARACTER):

   DEF VAR liInvoiceTargetGroup AS INT NO-UNDO. 
   DEF VAR liInvoiceTargetRule AS INT NO-UNDO. 
   DEF VAR lcFusionSubsType AS CHAR NO-UNDO. 
   DEF VAR liDelType AS INT NO-UNDO INIT ?. 

   DEF BUFFER lbInvoiceTarget FOR InvoiceTarget.
   DEF BUFFER lbMobSub FOR MobSub.
   DEF BUFFER lbInvoiceTargetGroup FOR InvoiceTargetGroup.
   DEF BUFFER Customer FOR Customer.
   
   lcFusionSubsType    = fCParamC("FUSION_SUBS_TYPE").

   FIND lbMobSub NO-LOCK WHERE
        lbMobSub.MsSeq = iiMsSeq NO-ERROR.
   IF NOT AVAIL lbMobSub THEN DO:
      ocError = "Subscription not found".
      RETURN FALSE.
   END.

   FIND lbInvoiceTarget NO-LOCK WHERE
        lbInvoiceTarget.MsSeq = iiMsSeq AND
        lbInvoiceTarget.ToDate >= TODAY USE-INDEX MsSeq NO-ERROR.

   IF AVAIL lbInvoiceTarget THEN DO:
      
      FIND lbInvoiceTargetGroup WHERE
           lbInvoiceTargetGroup.ITGroup = lbInvoiceTarget.ITGroup
      NO-LOCK NO-ERROR.
      IF AVAIL lbInvoiceTargetGroup THEN DO:
         IF lbMobSub.Custnum = lbInvoiceTargetGroup.Custnum THEN DO:
            ocError = "Invoice target is already active".
            RETURN FALSE.
         END.
      END.
      ELSE DO:
         ocError = "Invoice target group not found".
         RETURN FALSE.
      END.
   END.

   /* close but don't move possible prepaid invoice target */
   IF lbMobSub.PayType EQ TRUE THEN DO:

      IF NOT AVAIL lbInvoiceTarget THEN RETURN TRUE.

      RETURN _fDeactivateInvoiceTarget(
            lbInvoiceTarget.InvoiceTargetId,
            "ACC",
            OUTPUT ocError).
   END.
   
   IF NOT AVAIL lbInvoiceTarget THEN DO:
      ocError = "Active invoice target was not found from old customer".
      RETURN FALSE.
   END.

   liInvoiceTargetRule = fGetInvoiceTargetRule(lbMobsub.Custnum).
   
   IF liInvoiceTargetRule = {&INVOICE_TARGET_RULE_UNDEFINED} THEN 
      liInvoiceTargetRule = {&INVOICE_TARGET_RULE_SEPARATE_GROUP}.

   IF liInvoiceTargetRule = {&INVOICE_TARGET_RULE_DEFAULT_GROUP} OR
      LOOKUP(lbMobsub.CLIType,lcFusionSubsType) > 0 THEN DO:
      
      IF LOOKUP(lbMobsub.CLItype,lcFusionSubsType) > 0 THEN
         liInvoiceTargetGroup = fGetFusionInvoiceTargetGroup(lbmobsub.Custnum).
      ELSE liInvoiceTargetGroup = fGetDefaultInvoiceTargetGroup(lbMobsub.Custnum).
      
      IF liInvoiceTargetGroup <= 0 THEN DO:
         
         IF LOOKUP(lbMobsub.CLIType,lcFusionSubsType) > 0 THEN DO:
            FIND FIRST Customer WHERE
                       Customer.CustNum = lbMobsub.Custnum NO-LOCK NO-ERROR.
            IF AVAIL Customer AND Customer.DelType = {&INV_DEL_TYPE_EMAIL} THEN
               liDelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
            ELSE
               liDelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
         END. /* IF LOOKUP(icCLiType,lcFusionSubsType) > 0 THEN DO: */
      
         IT_ACC_TRANS:
         DO TRANS:
            liInvoiceTargetGroup = fAddInvoiceTargetGroup(lbMobsub.Custnum,liDelType,OUTPUT ocError).
            
            IF ocError EQ "" AND
               LOOKUP(lbMobSub.CLIType,lcFusionSubsType) = 0 THEN
               fSetDefaultInvoiceTargetGroup(liInvoiceTargetGroup, ocError).
            
            IF ocError EQ "" THEN
               fMoveInvoiceTarget(lbInvoiceTarget.InvoiceTargetId,
                                  liInvoiceTargetGroup,
                                  "ACC",
                                  OUTPUT ocError).

            IF ocError NE "" THEN UNDO IT_ACC_TRANS, RETURN FALSE.
         END.

      END.
      ELSE fMoveInvoiceTarget(lbInvoiceTarget.InvoiceTargetId,
                              liInvoiceTargetGroup,
                              "ACC",
                              OUTPUT ocError).
   END.
   ELSE DO TRANS:
      
      liInvoiceTargetGroup = fAddInvoiceTargetGroup(lbMobsub.Custnum,?,OUTPUT ocError).
      IF ocError NE "" THEN UNDO, RETURN FALSE.
      
      fMoveInvoiceTarget(lbInvoiceTarget.InvoiceTargetId,
                         liInvoiceTargetGroup,
                         "ACC",
                         OUTPUT ocError).
      IF ocError NE "" THEN UNDO, RETURN FALSE.
   END.

   RETURN (ocError NE "").

END FUNCTION. 

FUNCTION fSTCInvoiceTarget RETURNS LOGICAL
   (iiMsSeq AS INTEGER,
    icOldCLIType AS CHARACTER,
    icNewCLIType AS CHARACTER,
    OUTPUT ocError AS CHARACTER):
   
   DEF BUFFER lbMobSub FOR MobSub.
   DEF BUFFER lbInvoiceTarget FOR InvoiceTarget.
   
   FIND lbMobSub NO-LOCK WHERE
        lbMobSub.MsSeq = iiMsSeq NO-ERROR.
   IF NOT AVAIL lbMobSub THEN DO:
      ocError = "Subscription not found".
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST CLIType WHERE
                     CLIType.Brand = gcBrand AND
                     CLIType.CLIType = icNewCLIType AND
                     CLIType.PayType = {&CLITYPE_PAYTYPE_PREPAID} )
      THEN RETURN TRUE. 

   FIND lbInvoiceTarget NO-LOCK WHERE
        lbInvoiceTarget.MsSeq = iiMsSeq AND
        lbInvoiceTarget.ToDate >= TODAY USE-INDEX MsSeq NO-ERROR.
   
   IT_STC_TRANS:
   DO TRANS:
      /* close old prepaid invoice target and create new */
      IF AVAIL lbInvoiceTarget THEN
         _fDeactivateInvoiceTarget(lbInvoiceTarget.InvoiceTargetID,
                                   "STC",
                                   OUTPUT ocError).
         
      IF ocError EQ "" THEN
         _fCreateNewInvoiceTarget(
             lbMobSub.MsSeq,
             lbMobSub.Custnum,
             lbMobSub.CLIType,
             OUTPUT ocError).
      
      IF ocError NE "" THEN UNDO IT_STC_TRANS, RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION. 

FUNCTION fMoveListOfInvoiceTarget RETURNS LOGICAL 
         (INPUT icListInvoiceTarget AS CHAR,
          INPUT iiNewITGroupId AS INT,
          OUTPUT ocError AS CHAR ):

   DEF VAR li AS INT NO-UNDO. 
   DEF VAR liInvoiceTargetId AS INT.

   DO li = 1 TO NUM-ENTRIES(icListInvoiceTarget):
      
      liInvoiceTargetId = INT(ENTRY(li,icListInvoiceTarget)).
      IF NOT fMoveInvoiceTarget(liInvoiceTargetId,
                                iiNewITGroupId,
                                "",
                                OUTPUT ocError) THEN RETURN FALSE.
   END.

   RETURN TRUE.
END FUNCTION.


FUNCTION fGetCustomerCurrentGrouping RETURNS CHAR
   (INPUT iiCustnum AS INT,
    OUTPUT oiGroupCount AS INT,
    OUTPUT oiSubCount AS INT):

   DEF VAR liSubCount AS INT NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR llMoreThanOne AS LOG NO-UNDO. 
   DEF VAR lcInvoiceTarget AS CHAR NO-UNDO. 
   
   FOR EACH InvoiceTargetGroup NO-LOCK WHERE
            InvoiceTargetGroup.Brand   = gcBrand AND
            InvoiceTargetGroup.CustNum = iiCustNum AND
            InvoiceTargetGroup.ToDate >= TODAY:
     
      liSubCount = 0.

      FOR EACH InvoiceTarget WHERE
               InvoiceTarget.ITGroupID = InvoiceTargetGroup.ITGroup AND
               InvoiceTarget.ToDate >= TODAY NO-LOCK:

         IF fIsActiveInvoiceTarget(InvoiceTarget.invoicetargetid,
                                   "",
                                   OUTPUT lcError) then liSubCount = liSubCount + 1.
      END.
      
      IF liSubCount > 0 THEN ASSIGN
         oiGroupCount = oiGroupCount + 1.

      IF liSubCount > 1 THEN
         llMoreThanOne = True.

      oiSubCount = oiSubCount + 1.
      
   END. /* FOR EACH InvoiceTargetGroup NO-LOCK WHERE */

   IF oiGroupCount > 1 THEN DO:
      IF llMoreThanOne THEN lcInvoiceTarget = {&INVOICE_TARGET_CUSTOMIZED}.
      ElSE lcInvoiceTarget = {&INVOICE_TARGET_ALL_SPLIT}.
   END.
   ELSE IF oiGroupCount = 1 AND llMoreThanOne THEN 
      lcInvoiceTarget = {&INVOICE_TARGET_ALL_GROUPED}.
   ElSE DO:
      IF Customer.InvoiceTargetRule = {&INVOICE_TARGET_RULE_DEFAULT_GROUP} THEN
         lcInvoiceTarget = {&INVOICE_TARGET_ALL_GROUPED}.
      ELSE lcInvoiceTarget = {&INVOICE_TARGET_ALL_SPLIT}.
   END.

   RETURN lcInvoiceTarget.
END.

FUNCTION fGroupAllInvoiceTargets RETURNS LOG 
   (INPUT iiCustnum AS INT,
    OUTPUT ocError AS CHAR):

   DEF VAR liGroupCount AS INT NO-UNDO.
   DEF VAR liSubCount AS INT NO-UNDO. 
   DEF VAR lcCurrentGrouping AS CHAR NO-UNDO. 
   DEF VAR liDefaultITGroup AS INT NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 

   lcCurrentGrouping = fGetCustomerCurrentGrouping(iiCustnum, 
                            output liGroupCount,
                            output liSubCount).
   IF lcCurrentGrouping EQ {&INVOICE_TARGET_ALL_GROUPED} THEN RETURN TRUE.

   IF liGroupCount <= 1 THEN RETURN TRUE.
      
   liDefaultITGroup = fGetDefaultInvoiceTargetGroup(iiCustnum).
   IF liDefaultITGroup EQ 0 THEN DO:
      ocError = "Default invoice group not found".
      RETURN FALSE.
   END.

   DO TRANS:
   FOR EACH InvoiceTargetGroup NO-LOCK WHERE
            InvoiceTargetGroup.Brand   = gcBrand AND
            InvoiceTargetGroup.CustNum = iiCustNum AND
            InvoiceTargetGroup.ToDate >= TODAY AND
            InvoiceTargetGroup.ITGroup NE liDefaultITGroup:

      IF InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL} OR
         InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}
      THEN NEXT.
     
      FOR EACH InvoiceTarget WHERE
               InvoiceTarget.ITGroupID = InvoiceTargetGroup.ITGroup AND
               InvoiceTarget.ToDate >= TODAY NO-LOCK:

         IF NOT fIsActiveInvoiceTarget(InvoiceTarget.invoicetargetid,
                                       "",
                                       OUTPUT lcError) THEN NEXT.

         IF NOT fMoveInvoiceTarget(InvoiceTarget.InvoiceTargetId,
                                   liDefaultITGroup,
                                   "",
                                   OUTPUT ocError) THEN UNDO, RETURN FALSE.
      END.
      IF NOT fDeactivateInvoiceTargetGroup(
               InvoiceTargetGroup.ITGroup, 
               OUTPUT ocError) THEN RETURN FALSE.
   END.
   END /* DO TRANS */.

   RETURN TRUE.
END.

FUNCTION fSplitAllInvoiceTargets RETURNS LOG 
   (INPUT iiCustnum AS INT,
    OUTPUT ocError AS CHAR):

   DEF VAR liGroupCount AS INT NO-UNDO.
   DEF VAR liSubCount AS INT NO-UNDO. 
   DEF VAR lcCurrentGrouping AS CHAR NO-UNDO. 
   DEF VAR liNewITGroup AS INT NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR lcCreateGroups AS CHAR NO-UNDO. 

   lcCurrentGrouping = fGetCustomerCurrentGrouping(iiCustnum, 
                            output liGroupCount,
                            output liSubCount).
   IF lcCurrentGrouping EQ {&INVOICE_TARGET_ALL_SPLIT} THEN RETURN TRUE.

   DO TRANS:
   FOR EACH InvoiceTargetGroup NO-LOCK WHERE
            InvoiceTargetGroup.CustNum = iiCustNum AND
            InvoiceTargetGroup.ToDate >= TODAY:
      
      IF InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL} OR
         InvoiceTargetGroup.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}
      THEN NEXT.
      
      liSubCount = 0.
      FOR EACH InvoiceTarget WHERE
               InvoiceTarget.ITGroupID = InvoiceTargetGroup.ITGroup AND
               InvoiceTarget.ToDate >= TODAY NO-LOCK:

         IF fIsActiveInvoiceTarget(InvoiceTarget.invoicetargetid,
                                   "",
                                   OUTPUT lcError) THEN DO:

            liSubCount = liSubCount + 1.
            IF liSubCount <= 1 THEN NEXT.
            
            liNewITGroup = fAddInvoiceTargetGroup(iiCustNum,?,OUTPUT ocError).
            IF ocError NE "" THEN UNDO, RETURN FALSE.

            IF NOT fMoveInvoiceTarget(InvoiceTarget.InvoiceTargetId,
                                      liNewITGroup,
                                      "",
                                      OUTPUT ocError) THEN UNDO, RETURN FALSE.
         END.
      END.
   END.
   END.

   RETURN TRUE.
END.

&ENDIF
