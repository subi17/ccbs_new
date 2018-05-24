{Func/extralinefunc.i}
{Func/fmakemsreq.i}
{Func/dss_activation.i}

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
         fCloseExtraLineDiscount(MobSub.MsSeq,
                                 icCLIType + "DISC",
                                 TODAY).
   END CASE.

   RETURN TRUE.

END FUNCTION.    

FUNCTION fReassigningExtralines RETURNS LOGICAL
   (INPUT liMLMsSeq   AS INT,
    INPUT lcReqSource AS CHAR,
    INPUT liMsRequest AS INT):

   DEFINE BUFFER lbCustomer FOR Customer.
   DEFINE BUFFER lbMLMobSub FOR MobSub.
   DEFINE BUFFER lbELMobSub FOR MobSub.
   DEFINE BUFFER lELMobSub  FOR MobSub.

   DEF VAR lcExtralineCLITypes   AS CHAR NO-UNDO.
   DEF VAR liELCLITypeCount      AS INT  NO-UNDO.
   DEF VAR llgMainLineAvail      AS LOG  NO-UNDO INITIAL FALSE.  
   DEF VAR lcMLMsSeqList         AS CHAR NO-UNDO. 
   DEF VAR lcMandatoryExtraLines AS CHAR NO-UNDO. 
   DEF VAR liManELCount          AS INT  NO-UNDO. 
   DEF VAR liRequest             AS INT  NO-UNDO. 
   DEF VAR lcError               AS CHAR NO-UNDO. 
   DEF VAR liMLSubId             AS INT  NO-UNDO.

   FIND FIRST lbMLMobSub NO-LOCK WHERE 
              lbMLMobSub.MsSeq EQ liMLMsSeq NO-ERROR.
   
   IF NOT AVAIL lbMLMobSub THEN 
      RETURN FALSE.

   FIND lbCustomer OF lbMLMobsub NO-LOCK.

   IF NOT AVAIL lbCustomer THEN 
      RETURN FALSE.

   lcExtralineCLITypes = fExtraLineCLITypes().
   
   fCheckMainlineAvailForCustomer(0,
                                  lbCustomer.CustIdType,
                                  lbCustomer.OrgID,
                                  OUTPUT lcMLMsSeqList).

   DO liELCLITypeCount = 1 TO NUM-ENTRIES(lcExtralineCLITypes):
      
      FOR EACH lbELMobSub EXCLUSIVE-LOCK WHERE 
               lbELMobSub.Brand        EQ Syst.Var:gcBrand                            AND 
               lbELMobSub.MultiSimId   EQ lbMLMobSub.MsSeq                            AND 
               lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}                   AND
               lbELMobSub.CLIType      EQ ENTRY(liELCLITypeCount,lcExtralineCLITypes) AND
              (lbELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
               lbELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}):

         /* We have two STC cases to Mainline                 */ 
         /* Case 1: STC is done from Non Mainline to Mainline */
         /* Case 2: STC is done from Mainline to Mainline     */

         /* Here in both the cases, first reset the exsisting values of the extralines    */
         /* Because when we check for availability of existing mainline, we can reassign  */
         /* to current mainline, and if by any chance reseting the values due to          */
         /* non-availability of mainline of a customer, we will recheck all the extraline */
         /* subscriptions again about mainline availability and will reassign to it       */
         /* We will handle this process via cron job process                              */
         fResetExtralineSubscription(lbELMobSub.MsSeq,
                                     lbELMobSub.CLIType,
                                     0,
                                     0,
                                     FALSE).
         
         fCheckExistingMainLineAvailForExtraLine(INPUT lbELMobSub.CLIType,
                                                 INPUT lbCustomer.CustIdType,
                                                 INPUT lbCustomer.OrgID,
                                                 OUTPUT liMLSubId).

         IF liMLSubId > 0 THEN DO:
            llgMainLineAvail = TRUE.
            fResetExtralineSubscription(lbELMobSub.MsSeq,
                                        "",
                                        lbMLMobSub.MsSeq,
                                        {&MULTISIMTYPE_EXTRALINE},
                                        TRUE).
            NEXT.
         END.
      
      END.  

   END.

   /* In case no extraline is assigned to any available mainline of a customer */
   /* due to mandatory extraline business rule, STC one of the extraline to    */
   /* mandatory extralines, and once this STC is processed sucessfully we can  */
   /* reassign other orphan extralines of a customer to mainline               */
   /* This is handled via cron job process                                     */
   IF lcMLMsSeqList > ""   AND 
      NOT llgMainLineAvail THEN DO: 

      lcMandatoryExtraLines = fGetMandatoryExtraLineForMainLine(lbMLMobSub.CLIType).

      DO liManELCount = 1 TO NUM-ENTRIES(lcMandatoryExtraLines): 
     
         FOR EACH lELMobSub EXCLUSIVE-LOCK WHERE
                  lELMobSub.Brand        EQ Syst.Var:gcBrand      AND
                  lELMobSub.MultiSimId   EQ 0                     AND
                  lELMobSub.MultiSimType EQ 0                     AND
                  lELMobSub.CustNum      EQ lbMLMobSub.CustNum    AND
                 (lELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                  lELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}):

            IF NOT fCLITypeIsExtraLine(lELMobSub.CliType) THEN NEXT.

            IF lELMobSub.CLIType EQ ENTRY(liManELCount,lcMandatoryExtraLines) THEN DO:
               ASSIGN lELMobSub.MultiSimId   = lbMLMobSub.MsSeq
                      lELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}.
               LEAVE.
            END.

            /* Exclude subs. if STC request is ongoing */
            IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                               MsRequest.MsSeq      EQ lELMobSub.MsSeq                           AND
                               MsRequest.ReqType    EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}       AND
                               MsRequest.ReqCParam2 EQ ENTRY(liManELCount,lcMandatoryExtraLines) AND
                 LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0)        THEN NEXT.

            liRequest = fCTChangeRequest(lELMobSub.MsSeq,                          /* The MSSeq of the subscription to where the STC is made */
                                         ENTRY(liManELCount,lcMandatoryExtraLines),/* The CLIType of where to do the STC */
                                         "",                                       /* lcBundleID */
                                         "",                                       /* bank code validation is already done */
                                         TRUNC(Func.Common:mDate2TS(TODAY + 1),0),
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
            ELSE LEAVE.

         END.
      
      END.

   END.

   RETURN TRUE.

END FUNCTION.

