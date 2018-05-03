{Func/extralinefunc.i}
{Func/fmakemsreq.i}
{Func/dss_activation.i}

FUNCTION fCheckMainlineAvailForCustomer RETURNS LOGICAL
   (INPUT liMLMsSeq      AS INT,
    INPUT icCustIdType   AS CHAR,
    INPUT icCustId       AS CHAR,
    OUTPUT lcMLMsSeqList AS CHAR):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMLMobSub FOR MobSub.

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

FUNCTION fReassigningExtralines RETURNS LOGICAL
   (INPUT liAssignMLMsSeq AS INT,
    INPUT liCurrMLMsSeq   AS INT,
    INPUT liMsRequest     AS INT,
    INPUT lcReqSource     AS CHAR,
    INPUT llgDSSRequest   AS LOG):

   DEFINE BUFFER lbMobSub       FOR MobSub.
   DEFINE BUFFER lbOrder        FOR Order.
   DEFINE BUFFER lbELMobSub     FOR MobSub.
   DEFINE BUFFER lbMLMobSub     FOR MobSub. 
   DEFINE BUFFER lbELOrder      FOR Order.
   DEFINE BUFFER lELMobSub      FOR MobSub.
   DEFINE BUFFER lbDiscountPlan FOR DiscountPlan.
   DEFINE BUFFER bTerMsRequest  FOR MsRequest.

   DEF VAR lcMandatoryExtraLines   AS CHAR NO-UNDO. 
   DEF VAR liManELCount            AS INT  NO-UNDO.  
   DEF VAR liRequest               AS INT  NO-UNDO.
   DEF VAR lcError                 AS CHAR NO-UNDO. 
   DEF VAR liConfigExtraLineCount  AS INT  NO-UNDO. 
   DEF VAR liAvailExtraLineCount   AS INT  NO-UNDO. 
   DEF VAR lcExtralineCLITypes     AS CHAR NO-UNDO. 
   DEF VAR liELCLITypeCount        AS INT  NO-UNDO.
   DEF VAR llgConfigExtraLineCount AS LOG  NO-UNDO. 
   DEF VAR llgDSSReqAllowed        AS LOG  NO-UNDO. 
   DEF VAR lcDSSBundleId           AS CHAR NO-UNDO. 
   DEF VAR lcMsSeqList             AS CHAR NO-UNDO. 
   DEF VAR liMsSeqCount            AS INT  NO-UNDO. 
   DEF VAR llgSTCRequest           AS LOG  NO-UNDO.  
   DEF VAR lcAllowedDSS2SubsType   AS CHAR NO-UNDO. 
   DEF VAR lcAllowedDSS4SubsType   AS CHAR NO-UNDO. 
   DEF VAR liDSSPriMsSeq           AS INT  NO-UNDO. 
   DEF VAR lcResult                AS CHAR NO-UNDO. 

   ASSIGN liDSSPriMsSeq = 0
          lcDSSBundleId = ""
          lcMsSeqList   = "".

   FIND FIRST lbMLMobSub NO-LOCK WHERE 
              lbMLMobSub.MsSeq EQ liAssignMLMsSeq NO-ERROR.

   IF NOT AVAIL lbMLMobSub THEN 
      RETURN FALSE.

   IF fExtraLineCountForMainLine(liCurrMLMsSeq,
                                 lbMLMobSub.CustNum) EQ 0 THEN NEXT.
   
   ASSIGN liAvailExtraLineCount = 0
          lcExtralineCLITypes   = fExtraLineCLITypes().

   DO liELCLITypeCount = 1 TO NUM-ENTRIES(lcExtralineCLITypes):

      ASSIGN llgConfigExtraLineCount = fCLITypeAllowedForExtraLine(lbMLMobSub.CLIType,
                                                                   ENTRY(liELCLITypeCount,lcExtralineCLITypes),
                                                                   liConfigExtraLineCount)
             lcMandatoryExtraLines   = fGetMandatoryExtraLineForMainLine(lbMLMobSub.CLIType).   

      IF liConfigExtraLineCount EQ 0 THEN NEXT.         

      /* STC to Mandatory Extralines */
      IF lcMandatoryExtraLines NE "" THEN DO: 
         
         DO liManELCount = 1 TO NUM-ENTRIES(lcMandatoryExtraLines): 
        
            FIND FIRST lELMobSub EXCLUSIVE-LOCK WHERE
                       lELMobSub.Brand        EQ Syst.Var:gcBrand                          AND
                       lELMobSub.MultiSimId   EQ liCurrMLMsSeq                             AND
                       lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}                 AND
                       lELMobSub.CLIType      EQ ENTRY(liManELCount,lcMandatoryExtraLines) AND
                      (lELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                       lELMobSub.MsStatus     EQ {&MSSTATUS_BARRED})                       NO-ERROR.

            IF AVAIL lELMobSub THEN 
               ASSIGN liAvailExtraLineCount = liAvailExtraLineCount + 1
                      lELMobSub.MultiSimId  = lbMLMobSub.MsSeq
                      llgDSSReqAllowed      = TRUE.
            ELSE DO: 

               FOR EACH lELMobSub EXCLUSIVE-LOCK WHERE
                        lELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                        lELMobSub.MultiSimId   EQ liCurrMLMsSeq             AND
                        lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
                       (lELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                        lELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}):

                  /* Exclude subs. if STC request is ongoing */
                  IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                                     MsRequest.MsSeq      EQ lELMobSub.MsSeq                           AND
                                     MsRequest.ReqType    EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}       AND
                                     MsRequest.ReqStatus  EQ {&REQUEST_STATUS_NEW}                     AND
                                     MsRequest.ReqCParam2 EQ ENTRY(liManELCount,lcMandatoryExtraLines) AND
                       LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0)        THEN NEXT.

                  liRequest = fCTChangeRequest(lELMobSub.MsSeq,                          /* The MSSeq of the subscription to where the STC is made */
                                               ENTRY(liManELCount,lcMandatoryExtraLines),/* The CLIType of where to do the STC */
                                               "",                                       /* lcBundleID */
                                               "",                                       /* bank code validation is already done */
                                               TRUNC(Func.Common:mMakeTS(),0),
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
                                            STRING(lELMobSub.MsSeq),
                                            lELMobSub.Custnum,
                                            "Extraline STC request creation failed",
                                            lcError).
                  ELSE 
                     ASSIGN liAvailExtraLineCount = liAvailExtraLineCount + 1
                            llgSTCRequest         = TRUE.
                  
               END. /* FOR EACH lELMobSub */

            END. /* ELSE DO */

         END. /* DO liManELCount = 1 TO NUM-ENTRIES(lcMandatoryExtraLines) */

      END. /* IF lcMandatoryExtraLines NE "" */

      /* If STC request is created then skip assigning other extralines to mainline 
         until mandatory extraline STC request is processed sucessfully */
      IF lcMandatoryExtraLines NE "" AND 
         llgSTCRequest               THEN NEXT.

      IF liAvailExtraLineCount >= liConfigExtraLineCount THEN NEXT.

      FOR EACH lELMobSub EXCLUSIVE-LOCK WHERE
               lELMobSub.Brand        EQ Syst.Var:gcBrand                            AND
               lELMobSub.MultiSimId   EQ liCurrMLMsSeq                               AND
               lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}                   AND
               lELMobSub.CLIType      EQ ENTRY(liELCLITypeCount,lcExtralineCLITypes) AND 
              (lELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
               lELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}):

         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq      EQ lELMobSub.MsSeq AND
                           MsRequest.ReqType    EQ 0               AND
                           MsRequest.ReqStatus  EQ 0               AND
                           MsRequest.ReqCParam1 EQ lELMobSub.CLIType AND
                    LOOKUP(MsRequest.ReqCParam2,lcMandatoryExtraLines) GT 0) THEN NEXT.

         IF LOOKUP(lELMobSub.CLIType,lcMandatoryExtraLines) > 0 THEN NEXT.

         liAvailExtraLineCount = liAvailExtraLineCount + 1.
 
         IF liAvailExtraLineCount <= liConfigExtraLineCount THEN DO:
            ASSIGN lcMsSeqList          = IF lcMsSeqList EQ "" THEN STRING(lELMobSub.MsSeq) 
                                          ELSE lcMsSeqList + "," + STRING(lELMobSub.MsSeq)
                   lELMobSub.MultiSimId = lbMLMobSub.MsSeq
                   llgDSSReqAllowed     = TRUE.   
            NEXT.
         END.   
         ELSE DO:
            FIND FIRST lbDiscountPlan NO-LOCK WHERE
                       lbDiscountPlan.Brand     EQ Syst.Var:gcBrand           AND
                       lbDiscountPlan.DPRuleId  EQ lELMobSub.CLIType + "DISC" AND
                       lbDiscountPlan.ValidFrom <= TODAY                      AND
                       lbDiscountPlan.ValidTo   >= TODAY                      NO-ERROR.

            IF AVAIL lbDiscountPlan THEN DO:

               fCloseExtraLineDiscount(lELMobSub.MsSeq,
                                       lbDiscountPlan.DPRuleID,
                                       TODAY).

               Func.Common:mWriteMemo("MobSub",
                                      STRING(lELMobSub.MsSeq),
                                      0,
                                      "ExtraLine Discount is Closed",
                                      "STC done from Extra line associated Main line to different mainline or independent clitype").

               ASSIGN lELMobSub.MultiSimId   = 0
                      lELMobSub.MultiSimType = 0.

            END.

         END.

      END. /* FOR EACH lELMobSub */   

   END. /* DO liELCLITypeCount = 1 TO NUM-ENTRIES(lcExtralineCLITypes) */

   IF llgDSSRequest    AND 
      llgDSSReqAllowed THEN DO:

      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").

      IF LOOKUP(lbMLMobSub.CLIType,lcAllowedDSS4SubsType) > 0  AND
         fIsDSSActivationAllowed(lbMLMobSub.CustNum,
                                 lbMLMobSub.MsSeq,
                                 lbMLMobSub.ActivationTS,
                                 {&DSS4},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSBundleId = {&DSS4}.
      ELSE IF LOOKUP(lbMLMobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
         fIsDSSActivationAllowed(lbMLMobSub.CustNum,
                                 lbMLMobSub.MsSeq,
                                 lbMLMobSub.ActivationTS,
                                 {&DSS2},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSBundleId = {&DSS2}.

      FIND FIRST lbMobSub WHERE
                 lbMobSub.MsSeq = liDSSPriMsSeq NO-LOCK NO-ERROR.

      IF AVAIL lbMobSub THEN DO:
         
         FIND FIRST bTerMsRequest NO-LOCK USE-INDEX CustNum WHERE
                    bTerMsRequest.Brand      EQ Syst.Var:gcBrand AND
                    bTerMsRequest.ReqType    EQ 83               AND
                    bTerMsRequest.Custnum    EQ lbMobSub.Custnum AND
                    bTerMsRequest.ReqCParam3 BEGINS "DSS"        AND
                    bTerMsRequest.ReqCParam1 EQ "DELETE"         AND
      LOOKUP(STRING(bTerMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") EQ 0 NO-ERROR.
      
         IF NOT fOngoingDSSAct(lbMLMobSub.CustNum) THEN 
            liRequest = fDSSCreateRequest(lbMobSub.MsSeq,
                                          lbMobSub.CustNum,
                                          lcDSSBundleId,
                                          lcReqSource,
                                          liMsRequest,
                                          lbMobSub.ActivationTS,
                                          "DSS activation failed", /* Error Msg */
                                          OUTPUT lcError).     
         ELSE DO liMsSeqCount = 1 TO NUM-ENTRIES(lcMsSeqList):
            fDSSAddRequest(Mobsub.MsSeq,
                           lcDSSBundleId,
                           liMsRequest,
                           lcReqSource).
         END. 

      END.  

   END. 

END FUNCTION.
