
{Syst/tmsconst.i}
{Gwy/solog_create.i}
{Func/dss_activation.i}
{Func/ftransdir.i}

DEFINE VARIABLE liSologRequest     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcBundleId         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCLIList          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llgMatrixAvailable AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE lcDSSBundleId      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDSSAction        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMessage          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLogData          AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcSpoolDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcToday            AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeEndTS           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldtYesDate         AS DATE      NO-UNDO.
DEFINE VARIABLE lcOutputLine       AS CHARACTER NO-UNDO. 

DEFINE BUFFER bMLMobSub FOR MobSub.
DEFINE BUFFER bELMobSub FOR MobSub.
DEFINE BUFFER bMsOwner  FOR MsOwner.
DEFINE BUFFER bCustomer FOR Customer.

DEFINE STREAM strout.

ASSIGN
   lcSpoolDir = fCParam("ExtralinesCron","OutSpoolDir")
   lcOutDir   = fCParam("ExtralinesCron","OutDir")
   lcToday    = STRING(YEAR(TODAY),"9999") +
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   lcLogFile  = "create_dss_group_for_extralines" + "_" +
                lcToday                           + "_" +
                STRING(TIME)                      + ".log"
   lcLogFile  = lcSpoolDir + lcLogFile
   ldeEndTS   = Func.Common:mDate2TS(TODAY + 1)
   ldtYesDate = TODAY - 1.

OUTPUT STREAM strout TO VALUE(lcLogFile).

PUT STREAM strout UNFORMATTED
   "CUSTID;MLSubId;MLMSISDN;MLCliType;ELSubId;ELMSISDN;ELCliType;ELMultiSimId;STATUS" SKIP.

FUNCTION fCheckMSISDNInNetwork RETURNS CHARACTER
   (INPUT liMLMsSeq AS INT):

   DEFINE VARIABLE lcResponseLine AS CHARACTER NO-UNDO INITIAL "".
   DEFINE VARIABLE liCount        AS INTEGER NO-UNDO.

   FOR EACH Solog NO-LOCK WHERE
            Solog.MsSeq EQ liMLMsSeq AND
            Solog.Stat  EQ 2         AND
      INDEX(Solog.CommLine,"DISPLAY") > 0 BY Solog.CompletedTS DESC:

      IF INDEX(Solog.Response,"MSISDNS=") > 0 THEN DO:

         DO liCount = 1 TO NUM-ENTRIES(Solog.Response):

            lcResponseLine = ENTRY(liCount,Solog.Response).

            IF lcResponseLine BEGINS "MSISDNS" THEN do:

               ASSIGN lcResponseLine = REPLACE(lcResponseLine,"MSISDNS=","")
                      lcResponseLine = REPLACE(lcResponseLine,"RESP","").

               RETURN TRIM(lcResponseLine).

            END.

         END.

      END.

   END.

   RETURN lcResponseLine.

END FUNCTION.

FOR EACH TMSRelation NO-LOCK WHERE
         TMSRelation.TableName     EQ {&ELTABLENAME} AND
         TMSRelation.KeyType       EQ {&ELKEYTYPE}   AND
         TMSRelation.ParentValue   NE ""             AND
         TMSRelation.RelationType  NE "MANDATORY"    BREAK BY TMSRelation.ParentValue:

   IF INT(TMSRelation.RelationType) < 0 THEN NEXT.

   IF LAST-OF(TMSRelation.ParentValue) THEN DO:

      FOR EACH bMLMobSub NO-LOCK WHERE 
               bMLMobSub.brand    EQ Syst.Var:gcBrand        AND
               bMLMobSub.CLIType  EQ TMSRelation.ParentValue AND 
              (bMLMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
               bMLMobSub.MsStatus EQ {&MSSTATUS_BARRED}):

         ASSIGN lcMessage          = ""
                lcOutputLine       = ""
                liSologRequest     = 0
                lcBundleId         = fGetActiveDSSId(INPUT bMLMobSub.CustNum,
                                                     INPUT Func.Common:mMakeTS())
                llgMatrixAvailable = fCheckActiveExtraLinePair(bMLMobSub.MsSeq,
                                                               bMLMobSub.CLIType,
                                                               OUTPUT lcDSSBundleId).
         
         IF NOT llgMatrixAvailable THEN NEXT.   
         
         FIND FIRST bCustomer NO-LOCK WHERE
                    bCustomer.Brand   EQ Syst.Var:gcBrand  AND
                    bCustomer.CustNum EQ bMLMobSub.CustNum NO-ERROR.

         IF NOT AVAIL bCustomer THEN NEXT.

         FIND LAST bMsOwner NO-LOCK WHERE
                   bMsOwner.MsSeq   EQ bMLMobSub.MsSeq   AND
                   bMsOwner.CLI     EQ bMLMobSub.CLI     AND
                   bMsOwner.CLIType EQ bMLMobSub.CLIType AND
                   bMsOwner.TsEnd   GT ldeEndTS       NO-ERROR.

         IF NOT AVAIL bMsOwner THEN NEXT.

         MESSAGE "Func.Common:mTSToDate(bMsOwner.TsBegin) : " Func.Common:mTSToDate(bMsOwner.TsBegin) SKIP
                 "ldtYesDate                              : " ldtYesDate VIEW-AS ALERT-BOX. 

         IF Func.Common:mTSToDate(bMsOwner.TsBegin) NE ldtYesDate THEN NEXT.

         IF lcBundleId NE "" THEN DO:
            liSologRequest = fCreateDisplayDSSSolog(bMLMobSub.MsSeq). 

            PAUSE 20.
         END. 

         lcCLIList = fCheckMSISDNInNetwork(INPUT bMLMobSub.MsSeq).  

         IF LOOKUP(("34" + bMLMobSub.CLI),lcCLIList,";") > 0 THEN .
         ELSE DO: 
            
            IF (lcBundleId EQ "")            OR 
               (lcBundleId NE lcDSSBundleId) THEN DO:
               FIND FIRST bELMobSub NO-LOCK WHERE 
                          bELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                          bELMobSub.CustNum      EQ bMLMobSub.CustNum         AND
                          bELMobSub.PayType      EQ FALSE                     AND
                          bELMobSub.MultiSimId   EQ bMLMobSub.MsSeq           AND
                          bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} NO-ERROR.   

               IF NOT AVAIL bELMobSub THEN NEXT.
            END.
            
            RUN pDSSAccount(IF AVAIL bELMobSub THEN bELMobSub.MsSeq 
                            ELSE bMLMobSub.MsSeq,
                            lcBundleId,
                            lcDSSBundleId,
                            bMsOwner.TsBegin,
                            llgMatrixAvailable,
                            OUTPUT lcDSSAction,
                            OUTPUT lcMessage). 
         END.                   

         IF lcMessage > "" THEN DO: 
            lcOutputLine = STRING(bCustomer.OrgId)   + ";" +
                           STRING(bMLMobSub.MsSeq)   + ";" +
                           STRING(bMLMobSub.CLI)     + ";" +
                           STRING(bMLMobSub.CLIType) + ";" +
                           ""                        + ";" +
                           ""                        + ";" +
                           ""                        + ";" +
                           ""                        + ";" +
                           lcMessage.

            PUT STREAM strout UNFORMATTED
               lcOutputLine SKIP.
         END.

         IF lcDSSAction NE "CREATE" THEN DO:
            FOR EACH bELMobSub NO-LOCK WHERE
                     bELMobSub.Brand        EQ Syst.Var:gcBrand  AND
                     bELMobSub.CustNum      EQ bMLMobSub.CustNum AND
                     bELMobSub.PayType      EQ FALSE             AND
                     bELMobSub.MultiSimId   EQ bMLMobSub.MsSeq   AND
                     bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:

               IF LOOKUP(("34" + bELMobSub.CLI),lcCLIList,";") > 0 THEN NEXT.

               lcMessage = "".

               RUN pDSSAccount(bELMobSub.MsSeq,
                               lcBundleId,
                               lcDSSBundleId,
                               bMsOwner.TsBegin,
                               llgMatrixAvailable,
                               OUTPUT lcDSSAction,
                               OUTPUT lcMessage). 

               IF lcMessage > "" THEN DO:
                  lcOutputLine = STRING(bCustomer.OrgId)      + ";" +
                                 STRING(bMLMobSub.MsSeq)      + ";" +
                                 STRING(bMLMobSub.CLI)        + ";" +
                                 STRING(bMLMobSub.CLIType)    + ";" +
                                 STRING(bELMobSub.MsSeq)      + ";" +
                                 STRING(bELMobSub.CLI)        + ";" +
                                 STRING(bELMobSub.CLIType)    + ";" +
                                 STRING(bELMobSub.MultiSimId) + ";" +
                                 lcMessage.

                  PUT STREAM strout UNFORMATTED
                     lcOutputLine SKIP.
               END.
       
            END.
         END.                    

         IF lcOutputLine EQ "" THEN 
            PUT STREAM strout UNFORMATTED 
               STRING(bCustomer.OrgId) + ";" + 
               STRING(bMLMobSub.CLI)   + ";" +
               "Checked Subscriptions" SKIP.

      END.           

   END.

END.

OUTPUT STREAM strout CLOSE.

fMove2TransDir(lcLogFile, "", lcOutDir). 

PROCEDURE pDSSAccount:
   DEFINE INPUT PARAMETER iiMsSeq       AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER icBundleId    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lcDSSBundleId AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER ldeActStamp   AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER llgExtraLine  AS LOGICAL   NO-UNDO. 
   DEFINE OUTPUT PARAMETER lcDSSAction  AS CHAR      NO-UNDO.
   DEFINE OUTPUT PARAMETER lcResult     AS CHAR      NO-UNDO.

   DEFINE VARIABLE lcAllowedDSS2SubsType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDSS2PrimarySubsType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcAllowedDSS4SubsType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDSS4PrimarySubsType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDSSId               AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liDSSPriMsSeq         AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liDSSRequest          AS INTEGER   NO-UNDO. 

   DEFINE BUFFER lbMobSub      FOR MobSub.
   DEFINE BUFFER lbPriMobSub   FOR MobSub.
   DEFINE BUFFER bTerMsRequest FOR MsRequest.

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN LEAVE.

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
          lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

   IF (lcBundleId GT ""      AND lcDSSBundleId NE {&DSS4})              OR
      (lcBundleId EQ {&DSS4} AND lcDSSBundleId EQ {&DSS4})              OR
      CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.Brand      EQ Syst.Var:gcBrand      AND
                     MsRequest.ReqType    EQ {&REQTYPE_DSS}        AND
                     MsRequest.Custnum    EQ lbMobSub.CustNum      AND
                     MsRequest.ReqCParam1 EQ "CREATE"              AND
                     MsRequest.ActStamp   <= Func.Common:mMakeTS() AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"5,6,7,8") > 0) THEN
   DO:
      IF lcBundleId = {&DSS} OR
        (lcBundleId = {&DSS2}                                AND
         LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) GT 0 AND
         NOT fCLITypeIsMainLine(lbMobSub.CLIType))           THEN DO:
         fDSSAddRequest(lbMobsub.MsSeq,
                        lcBundleId,
                        0,
                        {&REQUEST_SOURCE_SCRIPT},
                        0).
         
         ASSIGN lcDSSAction = "ADD"
                lcResult    = lcBundleId + " ADD Request created".

      END.                  
      ELSE IF llgExtraLine                                             AND
              ((lcBundleId EQ {&DSS2} AND lcDSSBundleId EQ {&DSS2}) OR
               (lcBundleId EQ {&DSS4}))                                THEN DO:
            fDSSAddRequest(lbMobsub.MsSeq,
                           lcBundleId,
                           0,
                           {&REQUEST_SOURCE_SCRIPT},
                           0).

            ASSIGN lcDSSAction = "ADD"
                   lcResult    = lcBundleId + " ADD Request created".               
      END.
   END.
   ELSE IF NOT fOngoingDSSAct(lbMobSub.CustNum) THEN DO:

      IF LOOKUP(lbMobSub.CLIType,lcAllowedDSS4SubsType) > 0 AND
         lcDSSBundleId EQ {&DSS4}                           AND
         fIsDSSActivationAllowed(lbMobSub.CustNum,
                                 lbMobSub.MsSeq,
                                 ldeActStamp,
                                 {&DSS4},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS4}.
      ELSE IF LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
         lcDSSBundleId EQ {&DSS2}                                AND
         fIsDSSActivationAllowed(lbMobSub.CustNum,
                                 lbMobSub.MsSeq,
                                 ldeActStamp,
                                 {&DSS2},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS2}.

      FIND FIRST lbPriMobSub WHERE
                 lbPriMobSub.MsSeq = liDSSPriMsSeq NO-LOCK NO-ERROR.

      IF AVAIL lbPriMobSub THEN DO:

         FIND FIRST bTerMsRequest NO-LOCK USE-INDEX CustNum WHERE
                    bTerMsRequest.Brand      EQ Syst.Var:gcBrand    AND
                    bTerMsRequest.ReqType    EQ 83                  AND
                    bTerMsRequest.Custnum    EQ lbPriMobSub.CustNum AND
                    bTerMsRequest.ReqCParam3 BEGINS "DSS"           AND
                    bTerMsRequest.ReqCParam1 EQ "DELETE"            AND
                    LOOKUP(STRING(bTerMsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") EQ 0  NO-ERROR.

         IF NOT AVAIL bTerMsRequest THEN DO:        
            liDSSRequest = fDSSCreateRequest(lbPriMobSub.MsSeq,
                                             lbPriMobSub.CustNum,
                                             lcDSSId,
                                             {&REQUEST_SOURCE_SCRIPT},
                                             0,
                                             ldeActStamp,
                                             "DSS activation failed through cron", /* Error Msg */
                                             OUTPUT lcResult).
            ASSIGN lcDSSAction = "CREATE"
                   lcResult    = lcDSSId + " CREATE Request Created".
         END.
         ELSE DO:
            
            ASSIGN ldeActStamp = 0
                   ldeActStamp = Func.Common:mDate2TS((Func.Common:mLastDayOfMonth(TODAY) + 1)).
            
            liDSSRequest = fDSSCreateRequest(lbPriMobSub.MsSeq,
                                             lbPriMobSub.CustNum,
                                             lcDSSId,
                                             {&REQUEST_SOURCE_SCRIPT},
                                             0,
                                             ldeActStamp,
                                             "DSS activation failed through cron", /* Error Msg */
                                             OUTPUT lcResult).
            ASSIGN lcDSSAction = "CREATE"
                   lcResult    = lcDSSId + " CREATE Request Created first day of next month".

         END.

      END.

   END.

END PROCEDURE.
