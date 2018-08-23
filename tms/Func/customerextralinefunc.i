{Func/extralinefunc.i}
{Func/fmakemsreq.i}
{Func/dss_activation.i}
{Func/dss_deactivation.i}

DEFINE TEMP-TABLE ttExtraLines NO-UNDO
   FIELD MsSeq   AS INTEGER
   FIELD CustNum AS INTEGER
   FIELD CLIType AS CHARACTER.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

      {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSubStatusChange AS HANDLE NO-UNDO.

END.

FUNCTION fCheckMainlineAvailForCustomer RETURNS LOGICAL
   (INPUT liMLMsSeq      AS INT,
    INPUT icCustIdType   AS CHAR,
    INPUT icCustId       AS CHAR,
    OUTPUT lcMLMsSeqList AS CHAR):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMLMobSub FOR MobSub.
   DEFINE BUFFER bELMobSub FOR MobSub.   

   FOR FIRST bCustomer NO-LOCK WHERE
             bCustomer.Brand      EQ Syst.Var:gcBrand AND
             bCustomer.OrgId      EQ icCustID         AND
             bCustomer.CustidType EQ icCustIDType     AND
             bCustomer.Roles      NE "inactive":

      FOR EACH bMLMobSub NO-LOCK USE-INDEX CustNum WHERE
               bMLMobSub.Brand    EQ Syst.Var:gcBrand      AND
               bMLMobSub.CustNum  EQ bCustomer.CustNum     AND
               bMLMobSub.PayType  EQ FALSE                 AND
              (bMLMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
               bMLMobSub.MsStatus EQ {&MSSTATUS_BARRED})   BY bMLMobSub.ActivationTS:

         IF bMLMobSub.MsSeq EQ liMLMsSeq THEN NEXT.

         IF NOT fCLITypeIsMainLine(bMLMobSub.CLIType) THEN NEXT.
     
         IF NOT CAN-FIND(FIRST bELMobSub NO-LOCK WHERE 
                               bELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                               bELMobSub.MultiSimId   EQ bMLMobSub.MsSeq           AND
                               bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
                               bELMobSub.CustNum      EQ bMLMobSub.CustNum         AND 
                              (bELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                               bELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}))      THEN NEXT.
         
         IF lcMLMsSeqList = "" THEN 
            lcMLMsSeqList = STRING(bMLMobSub.MsSeq).
         ELSE     
            lcMLMsSeqList = lcMLMsSeqList + "," + STRING(bMLMobSub.MsSeq).

      END.

   END.

   IF lcMLMsSeqList NE "" THEN
      RETURN TRUE.
   ELSE 
      RETURN FALSE.

END FUNCTION.

FUNCTION fCreateORAddDSSAccount RETURNS LOGICAL 
   (INPUT iiMsSeq       AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT ideActStamp   AS DEC):

   DEFINE BUFFER lbMobSub      FOR MobSub.
   DEFINE BUFFER lbMLMobSub    FOR MobSub.
   DEFINE BUFFER lbELMobSub    FOR MobSub.              
   DEFINE BUFFER lbCustomer    FOR Customer.
   DEFINE BUFFER lbPriMobSub   FOR MobSub.
   DEFINE BUFFER bTerMsRequest FOR MsRequest.
 
   DEF VAR lcDSSBundleId         AS CHAR NO-UNDO. 
   DEF VAR lcBundleId            AS CHAR NO-UNDO. 
   DEF VAR lcDSSId               AS CHAR NO-UNDO.  
   DEF VAR lcMLMsSeqList         AS CHAR NO-UNDO.  
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO. 
   DEF VAR lcAllowedDSS4SubsType AS CHAR NO-UNDO. 
   DEF VAR llgMatrixAvailable    AS LOG  NO-UNDO.  
   DEF VAR liDSSPriMsSeq         AS INT  NO-UNDO. 
   DEF VAR lcResult              AS CHAR NO-UNDO. 
   DEF VAR llgOtherMainline      AS LOG  NO-UNDO. 
   DEF VAR ldeDataBundleLimit    AS DEC  NO-UNDO. 
   DEF VAR ldeDSSLimit           AS DEC  NO-UNDO. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN
      RETURN FALSE.
   
   FIND lbCustomer OF lbMobsub NO-LOCK.

   IF NOT AVAIL lbCustomer THEN
      RETURN FALSE.

   IF fCLITypeIsMainLine(lbMobSub.CLIType) THEN DO:
      FIND FIRST lbELMobSub NO-LOCK WHERE 
                 lbELMobSub.Brand        EQ Syst.Var:gcBrand          AND 
                 lbELMobSub.MultiSimId   EQ lbMobSub.MsSeq            AND 
                 lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
                 lbELMobSub.CustNum      EQ lbMobSub.CustNum          AND
                (lbELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                 lbELMobSub.MsStatus     EQ {&MSSTATUS_BARRED})       NO-ERROR.

      IF NOT AVAIL lbELMobSub THEN            
         RETURN FALSE.
   END.
   ELSE IF fCLITypeIsExtraLine(lbMobSub.CLIType) THEN DO:
      FIND FIRST lbELMobSub NO-LOCK WHERE 
                 lbELMobSub.MsSeq        EQ lbMobSub.MsSeq            AND
                 lbELMobSub.MultiSimId   NE 0                         AND
                 lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
                 lbELMobSub.CustNum      EQ lbMobSub.CustNum          AND
                (lbELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                 lbELMobSub.MsStatus     EQ {&MSSTATUS_BARRED})       NO-ERROR.

      IF NOT AVAIL lbELMobSub THEN            
         RETURN FALSE.
   END.

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
          lcBundleId            = fGetActiveDSSId(INPUT lbMobSub.CustNum,
                                                  INPUT ideActStamp).
  
   IF fCLITypeIsMainLine(lbMobSub.CLIType)  OR 
      fCLITypeIsExtraLine(lbMobSub.CLIType) THEN DO:

      ASSIGN llgOtherMainline   = fCheckMainlineAvailForCustomer(lbMobSub.MsSeq,
                                                                 lbCustomer.CustIdType,
                                                                 lbCustomer.OrgID,
                                                                 OUTPUT lcMLMsSeqList)
             llgMatrixAvailable = fCheckActiveExtraLinePair(lbMobSub.MsSeq,
                                                            lbMobSub.CLIType,
                                                            OUTPUT lcDSSBundleId).

      IF NOT llgMatrixAvailable THEN 
         RETURN FALSE. 

      IF LOOKUP(lbMobSub.CLIType,lcAllowedDSS4SubsType) > 0 AND
         lcDSSBundleId EQ {&DSS4}                           AND
         fIsDSSActivationAllowed(lbELMobSub.CustNum,
                                 lbELMobSub.MsSeq,
                                 ideActStamp,
                                 {&DSS4},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS4}.
      ELSE IF LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
         lcDSSBundleId EQ {&DSS2}                                AND
         fIsDSSActivationAllowed(lbELMobSub.CustNum,
                                 lbELMobSub.MsSeq,
                                 ideActStamp,
                                 {&DSS2},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS2}.
   END.
   ELSE DO:
      IF LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
         fIsDSSActivationAllowed(lbMobSub.CustNum,
                                 0,
                                 ideActStamp,
                                 {&DSS2},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN DO: 
         lcDSSId = {&DSS2}.

         IF fCanDSSKeepActive(INPUT lbMobsub.CustNum,
                              INPUT lbMobsub.MsSeq,
                              INPUT ideActStamp,
                              INPUT lcDSSId,
                              OUTPUT lcResult) THEN
            lcMLMsSeqList = STRING(lbMobsub.MsSeq).
      END.      
   END.

   FIND FIRST lbPriMobSub NO-LOCK WHERE 
              lbPriMobSub.MsSeq EQ liDSSPriMsSeq NO-ERROR.

   IF NOT AVAIL lbPriMobSub THEN 
      RETURN FALSE.

   IF (lcBundleId EQ "" OR lcMLMsSeqList EQ "") AND
      lcDSSId     GT ""                         AND
      NOT fOngoingDSSAct(lbPriMobSub.CustNum)   THEN DO:

      fDSSCreateRequest(lbPriMobSub.MsSeq,
                        lbPriMobSub.CustNum,
                        lcDSSId,
                        icMsReqSource,
                        iiMsRequest,
                        ideActStamp,
                        "DSS creation failed in STC Request",
                        OUTPUT lcResult).
      RETURN TRUE.
   END.
   ELSE IF llgMatrixAvailable    AND
           lcBundleId EQ {&DSS2} AND 
           lcDSSId    EQ {&DSS4} THEN DO:
      
      fDSSCreateDSS2ToDSS4(lbPriMobSub.MsSeq,
                           iiMsRequest,
                           icMsReqSource,
                           ideActStamp,
                           "DSS creation failed in STC Request").
      RETURN TRUE.
   END.        
   ELSE IF (LOOKUP(lbMobSub.CLIType,lcAllowedDSS4SubsType) GT 0 OR
            LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) GT 0) AND  
           lcBundleId GT ""                                      THEN DO:
      IF llgMatrixAvailable THEN
         fDSSAddExtralineGroup(lbMobSub.MsSeq,
                               lcBundleId,
                               iiMsRequest,
                               icMsReqSource,
                               ideActStamp).
      ELSE
         fDSSAddRequest(lbMobSub.MsSeq,
                        lcBundleId,
                        iiMsRequest,
                        icMsReqSource,
                        ideActStamp).
     
      ldeDataBundleLimit = fGetActiveBonoLimit(INPUT lbMobSub.MsSeq,
                                               INPUT ideActStamp).
      IF ldeDataBundleLimit > 0 THEN DO:
         ldeDSSLimit = 0.
         RUN pUpdateDSSLimit(INPUT lbMobSub.CustNum,
                             INPUT "UPDATE",
                             INPUT ldeDataBundleLimit,
                             INPUT 0,
                             INPUT ideActStamp,
                             OUTPUT ldeDSSLimit).

         IF ldeDSSLimit > 0 THEN
            RUN pUpdateDSSNetworkLimit(INPUT lbMobsub.MsSeq,
                                       INPUT lbMobSub.CustNum,
                                       INPUT ldeDSSLimit,
                                       INPUT "LIMIT",
                                       INPUT FALSE,
                                       INPUT iiMsRequest,
                                       INPUT ideActStamp,
                                       INPUT icMsReqSource,
                                       INPUT lcBundleId).
      END.
 
      RETURN TRUE.
   END.

   RETURN TRUE.

END FUNCTION.    

FUNCTION fDeleteORRemoveDSSAccount RETURNS LOGICAL 
   (INPUT iiMsSeq       AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT ideActStamp   AS DEC):

   DEFINE BUFFER lbMobSub   FOR MobSub.
   DEFINE BUFFER lbCustomer FOR Customer.
   DEFINE BUFFER lbELMobSub FOR MobSub.

   DEF VAR liDSSMsSeq         AS INT  NO-UNDO. 
   DEF VAR ldeDSSLimit        AS DEC  NO-UNDO.
   DEF VAR lcBundleId         AS CHAR NO-UNDO. 
   DEF VAR llgRemoveELMatrix  AS LOG  NO-UNDO.  
   DEF VAR lcError            AS CHAR NO-UNDO.  
   DEF VAR ldeDataBundleLimit AS DEC  NO-UNDO. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN
      RETURN FALSE.
   
   FIND lbCustomer OF lbMobsub NO-LOCK.

   IF NOT AVAIL lbCustomer THEN
      RETURN FALSE.
   
   IF NOT fGetDSSMsSeqLimit(INPUT lbMobSub.CustNum,
                            INPUT ideActStamp,
                            OUTPUT liDSSMsSeq,
                            OUTPUT ldeDSSLimit,
                            OUTPUT lcBundleId) THEN 
      RETURN FALSE.                         

   IF fOngoingDSSTerm(INPUT lbMobSub.CustNum,
                      INPUT ideActStamp) THEN 
      RETURN FALSE.

   IF fCLITypeIsMainLine(lbMobSub.CLIType)  OR
      fCLITypeIsExtraLine(lbMobSub.CLIType) THEN DO:
   
      IF LOOKUP(lcBundleId,SUBSTITUTE("&1,&2",{&DSS2},{&DSS4})) GT 0 AND
         (fCLITypeIsMainLine(MobSub.CLIType) OR 
          fCLITypeIsExtraLine(MobSub.CLIType))                       THEN
         llgRemoveELMatrix = TRUE.

   END.   

   IF lbMobSub.MsSeq EQ liDSSMsSeq THEN DO:

      RUN pUpdateDSSNetwork(INPUT lbMobsub.MsSeq,
                            INPUT lbMobsub.CLI,
                            INPUT lbMobsub.CustNum,
                            INPUT "DELETE",
                            INPUT "",      /* Optional param list */
                            INPUT iiMsRequest,
                            INPUT ideActStamp,
                            INPUT icMsReqSource,
                            INPUT lcBundleId).
      RETURN TRUE.                      
   END.                         
   ELSE DO:
      IF NOT fCanDSSKeepActive(INPUT  lbMobsub.CustNum,
                               INPUT  lbMobsub.MsSeq,
                               INPUT  ideActStamp,
                               INPUT  lcBundleId,
                               OUTPUT lcError) THEN DO:
         RUN pUpdateDSSNetwork(INPUT lbMobsub.MsSeq,
                               INPUT lbMobsub.CLI,
                               INPUT lbMobsub.CustNum,
                               INPUT "DELETE",
                               INPUT "",     /* Optional param list */
                               INPUT iiMsRequest,
                               INPUT ideActStamp,
                               INPUT icMsReqSource,
                               INPUT lcBundleId).
         RETURN TRUE.                      
      END.                         
      ELSE DO:
         fDSSRemoveRequest(lbMobSub.MsSeq,
                           lcBundleId,
                           iiMsRequest,
                           icMsReqSource,
                           ideActStamp,
                           llgRemoveELMatrix).

         ldeDataBundleLimit = fGetActiveBonoLimit(INPUT lbMobSub.MsSeq,
                                                  INPUT ideActStamp).
         IF ldeDataBundleLimit > 0 THEN DO:
            ldeDSSLimit = 0.
            RUN pUpdateDSSLimit(INPUT lbMobSub.CustNum,
                                INPUT "REMOVE",
                                INPUT ldeDataBundleLimit,
                                INPUT 0,
                                INPUT ideActStamp,
                                OUTPUT ldeDSSLimit).

            IF ldeDSSLimit > 0 THEN
               RUN pUpdateDSSNetworkLimit(INPUT lbMobsub.MsSeq,
                                          INPUT lbMobSub.CustNum,
                                          INPUT ldeDSSLimit,
                                          INPUT "LIMIT",
                                          INPUT FALSE,
                                          INPUT iiMsRequest,
                                          INPUT ideActStamp,
                                          INPUT icMsReqSource,
                                          INPUT lcBundleId).
         END.

         RETURN TRUE.
      END.

   END.
  
   RETURN TRUE.

END FUNCTION.

FUNCTION fResetExtralineSubscription RETURNS LOGICAL
   (INPUT liMsSeq        AS INT,
    INPUT icCLIType      AS CHAR,
    INPUT liMultiSimId   AS INT,
    INPUT liMultiSimType AS INT,    
    INPUT llgDiscount    AS LOG):

   DEFINE BUFFER lbELMobSub     FOR MobSub. 
   DEFINE BUFFER lbDiscountPlan FOR DiscountPlan.

   FIND FIRST lbELMobSub EXCLUSIVE-LOCK WHERE 
              lbELMobSub.MsSeq EQ liMsSeq NO-ERROR.

   IF NOT AVAIL lbELMobSub THEN 
      RETURN FALSE.

   IF llgDiscount THEN 
      IF NOT CAN-FIND(FIRST lbDiscountPlan NO-LOCK WHERE
                            lbDiscountPlan.Brand     EQ Syst.Var:gcBrand            AND
                            lbDiscountPlan.DPRuleId  EQ lbELMobSub.CLIType + "DISC" AND
                            lbDiscountPlan.ValidFrom <= TODAY                       AND
                            lbDiscountPlan.ValidTo   >= TODAY)                      THEN 
         RETURN FALSE.                      

   IF llDoEvent THEN DO:
      lhMobSubStatusChange = BUFFER lbELMobSub:HANDLE.
      RUN StarEventInitialize(lhMobSubStatusChange).
      RUN StarEventSetOldBuffer(lhMobSubStatusChange).
   END.

   ASSIGN lbELMobSub.MultiSimId   = liMultiSimId
          lbELMobSub.MultiSimType = liMultiSimType.
 
   IF fCLITypeIsExtraLine(icCLIType) THEN DO:

      IF NOT CAN-FIND(FIRST ttExtraLines NO-LOCK WHERE 
                            ttExtralines.MsSeq   EQ lbELMobSub.MsSeq    AND 
                            ttExtralines.CustNum EQ lbELMobSub.CustNum  AND 
                            ttExtralines.CLIType EQ lbELMobSub.CLIType) THEN DO: 
         CREATE ttExtraLines.
         ASSIGN ttExtraLines.MsSeq   = lbELMobSub.MsSeq
                ttExtraLines.CustNum = lbELMobSub.CustNum  
                ttExtraLines.CLIType = lbELMobSub.CLIType.
      END.

   END.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhMobSubStatusChange).
      fCleanEventObjects().
   END.

   CASE llgDiscount:
      WHEN TRUE THEN DO:
         FIND FIRST lbDiscountPlan NO-LOCK WHERE
                    lbDiscountPlan.Brand     EQ Syst.Var:gcBrand            AND
                    lbDiscountPlan.DPRuleId  EQ lbELMobSub.CLIType + "DISC" AND
                    lbDiscountPlan.ValidFrom <= TODAY                       AND
                    lbDiscountPlan.ValidTo   >= TODAY                       NO-ERROR.
            
         IF AVAIL lbDiscountPlan THEN
            fCreateExtraLineDiscount(lbELMobSub.MsSeq,
                                     lbDiscountPlan.DPRuleID,
                                     TODAY).   
      END.
      WHEN FALSE THEN
         fCloseExtraLineDiscount(lbELMobSub.MsSeq,
                                 lbELMobSub.CLIType + "DISC",
                                 TODAY).
   
   END CASE.

   RETURN TRUE.

END FUNCTION.    

FUNCTION fReassigningExtralines RETURNS LOGICAL
   (INPUT liMLMsSeq   AS INT,
    INPUT lcReqSource AS CHAR,
    INPUT liMsRequest AS INT):

   DEFINE BUFFER lbCustomer  FOR Customer.
   DEFINE BUFFER lbMLMobSub  FOR MobSub.
   DEFINE BUFFER lbELMobSub  FOR MobSub.
   DEFINE BUFFER lELMobSub   FOR MobSub.
   DEFINE BUFFER bSTCRequest FOR MsRequest.
   
   DEF VAR lcExtralineCLITypes   AS CHAR NO-UNDO.
   DEF VAR liELCLITypeCount      AS INT  NO-UNDO.
   DEF VAR llgMainLineAvail      AS LOG  NO-UNDO INITIAL FALSE.  
   DEF VAR lcMLMsSeqList         AS CHAR NO-UNDO. 
   DEF VAR lcMandatoryExtraLines AS CHAR NO-UNDO. 
   DEF VAR liManELCount          AS INT  NO-UNDO. 
   DEF VAR liRequest             AS INT  NO-UNDO. 
   DEF VAR lcError               AS CHAR NO-UNDO. 
   DEF VAR liMLSubId             AS INT  NO-UNDO.
   DEF VAR lcMandatoryCLITypes   AS CHAR NO-UNDO. 
   DEF VAR llgMandatory          AS LOG  NO-UNDO. 
   DEF VAR llgReturnValue        AS LOG  NO-UNDO INITIAL TRUE. 
   DEF VAR lcAssignSubId         AS CHAR NO-UNDO. 

   FIND FIRST lbMLMobSub NO-LOCK WHERE 
              lbMLMobSub.MsSeq EQ liMLMsSeq NO-ERROR.
   
   IF NOT AVAIL lbMLMobSub THEN 
      RETURN FALSE.

   FIND lbCustomer OF lbMLMobsub NO-LOCK.

   IF NOT AVAIL lbCustomer THEN 
      RETURN FALSE.

   ASSIGN lcExtralineCLITypes = fExtraLineCLITypes()
          lcAssignSubId       = "".
   
   ASSIGN lcMandatoryExtraLines = fGetMandatoryExtraLineForMainLine(lbMLMobSub.CLIType)
          lcMandatoryCLITypes   = ""
          llgMainLineAvail      = FALSE.

   DO liManELCount = 1 TO NUM-ENTRIES(lcMandatoryExtraLines):

      FIND FIRST ttExtraLines NO-LOCK WHERE 
                 ttExtraLines.CLIType EQ ENTRY(liManELCount,lcMandatoryExtraLines) NO-ERROR.

      IF AVAIL ttExtraLines THEN 
         llgMainLineAvail = TRUE.
      ELSE 
         ASSIGN lcMandatoryCLITypes = IF lcMandatoryCLITypes EQ "" THEN 
                                         ENTRY(liManELCount,lcMandatoryExtraLines)
                                      ELSE lcMandatoryCLITypes + "," + ENTRY(liManELCount,lcMandatoryExtraLines)   
                llgMainLineAvail    = FALSE.
   
   END.

   IF lcMandatoryCLITypes NE "" AND
      NOT llgMainLineAvail THEN DO:

      MandatoryCLIType:   
      DO liManELCount = 1 TO NUM-ENTRIES(lcMandatoryCLITypes):
     
         FOR EACH ttExtraLines NO-LOCK:

            FIND FIRST bSTCRequest NO-LOCK WHERE
                       bSTCRequest.MsSeq      EQ ttExtraLines.MsSeq                  AND
                       bSTCRequest.ReqType    EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
         LOOKUP(STRING(bSTCRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") EQ 0  NO-ERROR.

            /* 1. If Ongoing STC Request with Mandatory subscription tariff type is available */
            /*    then check for another mandatory subscription tariff                        */
            /* 2. If Ongoing STC Request is available then just check for another extraline   */
            IF AVAIL bSTCRequest THEN DO:

               IF bSTCRequest.ReqCParam2 EQ ENTRY(liManELCount,lcMandatoryCLITypes) THEN 
                  NEXT MandatoryCLIType.
               ELSE NEXT.

            END.

            liRequest = fCTChangeRequest(ttExtraLines.MsSeq,                       /* The MSSeq of the subscription to where the STC is made */
                                         ENTRY(liManELCount,lcMandatoryCLITypes),  /* The CLIType of where to do the STC */
                                         "",                                       /* lcBundleID */
                                         "",                                       /* bank code validation is already done */
                                         Func.Common:mMakeTS(),
                                         0,                                        /* 0 = Credit check ok */
                                         0,                                        /* extend contract */
                                         ""                                        /* pcSalesman */,
                                         FALSE,                                    /* charge */
                                         TRUE,                                     /* send sms */
                                         "",
                                         0,
                                         lcReqSource,
                                         0,
                                         liMsRequest,
                                         "",                                       /* contract id */
                                         OUTPUT lcError).

            IF liRequest = 0 THEN
               Func.Common:mWriteMemo("MobSub",
                                      STRING(ttExtraLines.MsSeq),
                                      ttExtraLines.Custnum,
                                      "Extraline STC request creation failed",
                                      lcError).
            ELSE NEXT MandatoryCLIType.

         END.         

      END.

      IF liRequest GT 0 THEN DO:
         
         FOR EACH ttExtraLines NO-LOCK:

            IF CAN-FIND(FIRST bSTCRequest NO-LOCK WHERE
                              bSTCRequest.MsRequest  EQ liRequest                           AND
                              bSTCRequest.MsSeq      EQ ttExtraLines.MsSeq                  AND
                              bSTCRequest.ReqType    EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                LOOKUP(STRING(bSTCRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") EQ 0) THEN NEXT.  
            ELSE DO:
               lcAssignSubId = IF lcAssignSubId EQ "" THEN STRING(ttExtraLines.MsSeq) 
                               ELSE lcAssignSubId + CHR(255) + STRING(ttExtraLines.MsSeq).
            END.

         END.

         IF lcAssignSubId GT "" THEN 
            fCreateMsRequestParam(liRequest,
                                  {&EXTRALINE_STC},
                                  {&CHARVAL},
                                  lcAssignSubId).

         llgReturnValue = FALSE.                         
      END.   

   END. 
   ELSE DO:
      
      FOR EACH ttExtraLines NO-LOCK:

         FIND FIRST lbELMobSub NO-LOCK WHERE 
                    lbELMobSub.MsSeq        EQ ttExtraLines.MsSeq   AND
                    lbELMobSub.MultiSimId   EQ 0                    AND
                    lbELMobSub.MultiSimType EQ 0                    AND 
                    lbELMobSub.CustNum      EQ ttExtraLines.CustNum AND 
                    lbELMobSub.CLIType      EQ ttExtraLines.CLIType AND
                   (lbELMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
                    lbELMobSub.MsStatus EQ {&MSSTATUS_BARRED})      NO-ERROR.
        
         IF NOT AVAIL lbELMobSub THEN NEXT.

         fResetExtralineSubscription(lbELMobSub.MsSeq,
                                     lbELMobSub.CLIType,
                                     lbMLMobSub.MsSeq,
                                     {&MULTISIMTYPE_EXTRALINE},
                                     TRUE).

         llgReturnValue = TRUE.                            
      END.   
  
   END.

   RETURN llgReturnValue.

END FUNCTION.

FUNCTION fUpdateDSSAccount RETURNS LOGICAL 
   (INPUT iiMsSeq       AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT ideActStamp   AS DEC,
    INPUT icAction      AS CHAR):

   DEF VAR llgReturnValue AS LOG NO-UNDO. 

   CASE icAction:
      WHEN "CREATE" THEN
         llgReturnValue = fCreateORAddDSSAccount(iiMsSeq,
                                                 icMsReqSource,
                                                 iiMsRequest,
                                                 ideActStamp).
      WHEN "DELETE" THEN
         llgReturnValue = fDeleteORRemoveDSSAccount(iiMsSeq,
                                                    icMsReqSource,
                                                    iiMsRequest,
                                                    ideActStamp).
   END CASE.

   RETURN llgReturnValue.

END FUNCTION.    
