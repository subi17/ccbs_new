&IF "{&ADD_LINES_REQUEST_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ADD_LINES_REQUEST_I YES

{Func/main_add_lines.i}
{Func/fsubstermreq.i}
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Func/msisdn_prefix.i}
{Func/fmakesms.i}

DEF TEMP-TABLE tt_AdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.
    
FUNCTION fCancelPendingSTCToAddLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):
   
   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER MsRequest FOR Msrequest.
   
   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsSeq   = iiMsseq                             AND
             MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
             LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
       FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                                THEN MsRequest.ReqCParam5
                                ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN 
         fChangeReqStatus(MsRequest.Msrequest,
                          4,
                          "STC has to be done for CONT9").
   END.
   
   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsSeq   = iiMsseq                  AND
             MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
             LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
       FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = MsRequest.ReqCParam2:
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN 
         fChangeReqStatus(MsRequest.Msrequest,
                          4,
                          "STC has to be done for CONT9").
   END.
   
   RETURN FALSE.

END.

FUNCTION fTermAdditionalSim RETURNS LOGICAL
    (INPUT iiMsSeq       AS INT,
     INPUT icCLI         AS CHAR,
     INPUT iiCustNum     AS INT,
     INPUT iiTermReason  AS INT,
     INPUT idaTermDate   AS DATE,
     INPUT icSource      AS CHAR,
     INPUT iiOrigRequest AS INT,
     OUTPUT lvcError     AS CHAR).

   DEFINE VARIABLE lvdaSecSIMTermDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE lvdeSecSIMTermStamp AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lviMsisdnStat       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviSimStat          AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviQuarTime         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviRequest          AS INTEGER   NO-UNDO.

   IF icSource EQ {&REQUEST_SOURCE_STC} OR 
      icSource EQ {&REQUEST_SOURCE_BTC} THEN DO:
      lvdaSecSIMTermDate = Func.Common:mLastDayOfMonth(idaTermDate).

      IF DAY(idaTermDate) <> 1 THEN
         ASSIGN lvdaSecSIMTermDate = lvdaSecSIMTermDate + 1
                lvdaSecSIMTermDate = Func.Common:mLastDayOfMonth(lvdaSecSIMTermDate).

      lvdeSecSIMTermStamp = Func.Common:mMake2DT(lvdaSecSIMTermDate,86399).          
   END.
   ELSE IF icSource EQ {&REQUEST_SOURCE_ACC} THEN DO:
      IF DAY(idaTermDate) EQ 1 THEN 
         lvdaSecSIMTermDate = Func.Common:mLastDayOfMonth(idaTermDate).
      ELSE ASSIGN
         lvdaSecSIMTermDate  = ADD-INTERVAL(idaTermDate,1,"months")
         lvdaSecSIMTermDate  = Func.Common:mLastDayOfMonth(lvdaSecSIMTermDate).

      lvdeSecSIMTermStamp = Func.Common:mMake2DT(lvdaSecSIMTermDate,86399).
   END.
   ELSE
      ASSIGN lvdaSecSIMTermDate  = ADD-INTERVAL(idaTermDate,1,"months")
             lvdaSecSIMTermDate  = Func.Common:mLastDayOfMonth(lvdaSecSIMTermDate)
             lvdeSecSIMTermStamp = Func.Common:mMake2DT(lvdaSecSIMTermDate,86399).

   fInitialiseValues(iiTermReason,
                     fIsYoigoCLI(icCLI),
                     fIsMasmovilCLI(icCLI),
                     OUTPUT lviMsisdnStat,
                     OUTPUT lviSimStat,
                     OUTPUT lviQuarTime).

   lviRequest = fTerminationRequest(
                       iiMsSeq,
                       lvdeSecSIMTermStamp,
                       lviMsisdnStat,
                       lviSimStat,
                       lviQuarTime,
                       1, /* create fees */
                       "", /* out oper. */
                       STRING(iiTermReason),
                       icSource,
                       Syst.Var:katun,
                       iiOrigRequest, /* orig. request */
                       {&TERMINATION_TYPE_FULL},
                       OUTPUT lvcError).
   IF lviRequest EQ 0 THEN
      Func.Common:mWriteMemo("MobSub",
                       STRING(iiMsSeq),
                       iiCustNum,
                       "Multi SIM termination failed",
                       lvcError).

   RETURN TRUE.

END FUNCTION.

FUNCTION fAdditionalSimTermination RETURNS LOGICAL
   (INPUT iiMsSeq   AS INTEGER,
    INPUT icSource  AS CHARACTER):
   
   DEFINE BUFFER MsRequest FOR MsRequest.     
   DEFINE BUFFER MobSub    FOR MobSub.
   DEFINE BUFFER lbMobSub  FOR MobSub.
   DEFINE BUFFER CLIType   FOR CLIType.
   
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
   
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.brand = Syst.Var:gcBrand AND 
              MobSub.MsSeq = iiMsSeq NO-ERROR.
          
   EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
          
   IF NOT CAN-FIND(
          FIRST CLIType NO-LOCK WHERE
                CLIType.Brand = Syst.Var:gcBrand AND
                CLIType.CLIType = (IF MobSub.TariffBundle > ""
                                      THEN MobSub.TariffBundle
                                   ELSE MobSub.CLIType) AND
                CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN RETURN FALSE.
                       
   FOR EACH lbMobSub NO-LOCK WHERE
            lbMobSub.Brand   = Syst.Var:gcBrand        AND
            lbMobSub.InvCust = Mobsub.CustNum AND
            lbMobSub.PayType = FALSE,
      FIRST CLitype NO-LOCK WHERE
            CLitype.Brand   = Syst.Var:gcBrand                         AND
            CLitype.CLIType = (IF lbMobsub.TariffBundle > ""
                                  THEN lbMobsub.TariffBundle
                               ELSE lbMobsub.CLIType)         AND
            (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
             CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL}):
               
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
         fHasPendingSTCToMainLine(lbMobSub.Msseq)     THEN DO:
         EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
         LEAVE.
      END.

      CREATE tt_AdditionalSIM.
      ASSIGN tt_AdditionalSIM.MsSeq   = lbMobSub.MsSeq
             tt_AdditionalSIM.CustNum = lbMobSub.CustNum
             tt_AdditionalSIM.CLI     = lbMobSub.CLI.
   END.

   FOR EACH tt_AdditionalSIM NO-LOCK:
                
      IF fHasPendingRequests(tt_AdditionalSIM.Msseq,
                             tt_AdditionalSIM.CLI,
                             {&CLITYPE_LINETYPE_ADDITIONAL}) THEN NEXT.
                             
      fTermAdditionalSim(tt_AdditionalSIM.MsSeq,
                         tt_AdditionalSIM.CLI,
                         tt_AdditionalSIM.CustNum,
                         {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                         TODAY,
                         icSource,
                         0,
                         OUTPUT lcError).
                         
   END. /* FOR EACH ttAdditionalSIM NO-LOCK: */
   
   RETURN TRUE.
                        
END FUNCTION.

/* YDR-1847 */
/* IF mainline is terminated/STC is done, then STC has to be done for Additional Line */
FUNCTION fAdditionalLineSTC RETURNS LOGICAL 
    (INPUT iiMsRequest  AS INTEGER,
     INPUT ldeActStamp  AS DECIMAL,
     INPUT icTermReason AS CHARACTER):
   
   DEFINE VARIABLE ldeSMSStamp AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcSMSText   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE oiRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO.
   DEF VAR llAdditionalSTC AS LOG NO-UNDO. 
   DEF VAR lcAdditionalSMS AS CHAR NO-UNDO. 
   DEF VAR lcMainSMS AS CHAR NO-UNDO. 
         
   DEFINE BUFFER MsRequest FOR MsRequest.     
   DEFINE BUFFER bMsRequest FOR MsRequest.     
   DEFINE BUFFER MobSub    FOR MobSub.
   DEFINE BUFFER lbMobSub  FOR MobSub.
   DEFINE BUFFER CLIType   FOR CLIType.
   DEFINE BUFFER bCLIType  FOR CLIType.

   FIND MsRequest NO-LOCK WHERE 
        MsRequest.Brand     = Syst.Var:gcBrand     AND 
        MsRequest.MsRequest = iiMsRequest NO-ERROR. 
   
   IF NOT AVAILABLE MsRequest THEN RETURN FALSE.
                
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.brand = Syst.Var:gcBrand AND 
              MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   
   IF NOT AVAIL Mobsub THEN RETURN FALSE.
              
   EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.

   /* In case of STC_FINAL, the subs.type is already changed */
   IF icTermReason NE "STC_FINAL" THEN DO:
      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.Brand   = Syst.Var:gcBrand             AND
                 CLIType.CLIType = MobSub.TariffBundle NO-ERROR.
      IF NOT AVAIL CLIType OR
                   CLIType.LineType NE {&CLITYPE_LINETYPE_MAIN}
         THEN RETURN FALSE.
   END.
   
   MOBSUB_LOOP:
   FOR EACH lbMobSub NO-LOCK WHERE
            lbMobSub.Brand   = Syst.Var:gcBrand        AND
            lbMobSub.InvCust = Mobsub.CustNum AND
            lbMobSub.PayType = FALSE          AND
            lbMobSub.MsSeq  NE Mobsub.MsSeq,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF lbMobSub.TariffBundle > "" 
                                  THEN lbMobSub.TariffBundle
                               ELSE lbMobSub.CLIType) AND
           (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
            CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL}):
      
      /* check main line existence */
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN DO:
         
         FOR EACH bMsRequest NO-LOCK WHERE
                  bMsRequest.MsSeq = lbMobSub.msseq AND
                  bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                  bMsRequest.ActStamp <= Func.Common:mMakeTS() AND
          LOOKUP(STRING(bMsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
            FIRST bCLIType NO-LOCK WHERE
                  bCLIType.Brand = Syst.Var:gcBrand AND
                  bCLIType.CLIType = (IF bMsRequest.ReqCParam5 > ""
                                     THEN bMsRequest.ReqCParam5
                                     ELSE bMsRequest.ReqCParam2):
            IF bCLIType.LineType NE {&CLITYPE_LINETYPE_MAIN} THEN
               NEXT MOBSUB_LOOP.
         END.

         FOR EACH bMsRequest NO-LOCK WHERE
                  bMsRequest.MsSeq = lbMobSub.MsSeq AND
                  bMsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
                  bMsRequest.ActStamp <= Func.Common:mMakeTS() AND
          LOOKUP(STRING(bMsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
            FIRST bCLIType NO-LOCK WHERE
                  bCLIType.Brand = Syst.Var:gcBrand AND
                  bCLIType.CLIType = bMsRequest.ReqCParam2:
            IF bCLIType.LineType NE {&CLITYPE_LINETYPE_MAIN} THEN
               NEXT MOBSUB_LOOP.
         END.

         IF CAN-FIND (FIRST bMsRequest WHERE
                bMsRequest.MsSeq = lbMobSub.MsSeq AND
                bMsRequest.ActStamp <= Func.Common:mMakeTS() AND
                MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                LOOKUP(STRING(bMsRequest.ReqStatus),
                       {&REQ_INACTIVE_STATUSES}) = 0) THEN NEXT MOBSUB_LOOP.

         EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
         RETURN FALSE.
      END.

      /* check main line existence */
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:
         CREATE tt_AdditionalSIM.
         ASSIGN tt_AdditionalSIM.MsSeq   = lbMobSub.MsSeq
                tt_AdditionalSIM.CustNum = lbMobSub.CustNum
                tt_AdditionalSIM.CLI     = lbMobSub.CLI.
      END.

   END. /* FOR EACH lbMobSub WHERE */
     
   FIND Customer NO-LOCK WHERE
        Customer.Custnum = MobSub.CustNum NO-ERROR.
      
   CASE icTermReason:
      WHEN "DELETE" THEN ASSIGN
         lcAdditionalSMS = "MainTermToAddSTC"
         lcMainSMS = "".
      WHEN "STC" OR WHEN "STC_FINAL" THEN ASSIGN
         lcAdditionalSMS = "MainSTCToAddSTC"
         lcMainSMS = "MainSTCToNonMain".
      WHEN "MNP" THEN ASSIGN
         lcAdditionalSMS = "MNPOutToAddSTC"
         lcMainSMS = "MNPOutToMainLine".
   END CASE.    
         
   lcAdditionalSMS = fGetSMSTxt(lcAdditionalSMS,
                          TODAY,
                         (IF AVAIL Customer
                          THEN Customer.Language ELSE 1),
                          OUTPUT ldeSMSStamp).
      
   FIND FIRST CLIType NO-LOCK WHERE 
              CLIType.Brand   = Syst.Var:gcBrand AND
              CLIType.CLIType = "CONT9" NO-ERROR.
        
   FOR EACH tt_AdditionalSIM NO-LOCK:

      /* If there is no ongoing STC/termination request for secondary line */
      IF fHasPendingRequests(tt_AdditionalSIM.MsSeq,
                             tt_AdditionalSIM.CLI,
                            {&CLITYPE_LINETYPE_ADDITIONAL}) THEN NEXT.
         
      IF fHasPendingSTCToNonAddLine(tt_AdditionalSIM.MsSeq) THEN NEXT.
      
      fCancelPendingSTCToAddLine(tt_AdditionalSIM.MsSeq).   
        
      oiRequest = fCTChangeRequest(tt_AdditionalSIM.MsSeq,
                                   CLIType.CLIType,
                                   "",   /* lcBundleID */
                                   "",   /* lcBankAcc = bank code validation is already done in newton */
                                   IF ldeActStamp > 0 THEN ldeActStamp 
                                   ELSE MSRequest.ActStamp,
                                   0,   /* liCreditcheck 0 = Credit check ok */
                                   0, /* extend contract 0=no extend_term_contract */
                                   ""    /* pcSalesman */,
                                   FALSE, /* charge */
                                   TRUE,  /* send sms */
                                   "",
                                   0,
                                   {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION}, 
                                   0,     /* piOrderID */
                                   MsRequest.MsRequest,
                                   "", /*contract_id*/
                                   OUTPUT lcError).

      IF oiRequest = 0 THEN DO:
         Func.Common:mWriteMemo("MsRequest",
                          STRING(MsRequest.MsRequest),
                          MsRequest.custnum,
                          "Additional line STC failed",
                          tt_AdditionalSIM.CLI).
         NEXT.
      END.
      ELSE llAdditionalSTC = TRUE.

      lcSMSText = REPLACE(lcAdditionalSMS,"#MSISDN",MobSub.CLI).
      
      IF lcError = "" AND lcSMSText > "" THEN
         fMakeSchedSMS2(tt_AdditionalSIM.CustNum,
                        tt_AdditionalSIM.CLI,
                        11, /* service change */
                        lcSMSText,
                        ldeSMSStamp,
                        "22622",
                        "").
   END. /* FOR EACH ttAdditionalSIM */
      
   IF llAdditionalSTC AND lcMainSMS > "" THEN DO:

      lcSMSText = fGetSMSTxt(lcMainSMS,
                             TODAY,
                             (IF AVAIL Customer
                             THEN Customer.Language ELSE 1),
                             OUTPUT ldeSMSStamp).
      
      IF lcSMSText > "" THEN
         fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        11, /* service change */
                        lcSMSText,
                        ldeSMSStamp,
                        "22622",
                        "").
   END.
                        
   EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
   
   RETURN TRUE.
   
END FUNCTION.        

FUNCTION fAddLineSTCCancellation RETURN LOGICAL
    (iiMsRequest AS INTEGER,
     iiCustnum   AS INTEGER):

   DEFINE BUFFER bMsRequest FOR MsRequest.

   FOR EACH bMsRequest NO-LOCK WHERE
            bMsRequest.Brand       = Syst.Var:gcBrand                                  AND
            bMsRequest.ReqType     = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}      AND
            bMsrequest.Custnum     = iiCustnum                                AND
            bMsRequest.OrigRequest = iiMsRequest                              AND
            bMsRequest.ReqSource   = {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION} AND
      LOOKUP(STRING(bMsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0:

      fChangeReqStatus(bMsRequest.Msrequest,
                       4,
                       "Additional STC cancellation").
   END.

END FUNCTION.   

FUNCTION fNonAddLineSTCCancellationToAddLineSTC RETURN LOGICAL
   (iiMsRequest AS INTEGER):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER MobSub    FOR MobSub.
   DEF BUFFER lbMobSub  FOR MobSub.
   DEF BUFFER lbCLIType FOR CLIType.
   DEF BUFFER bCLIType  FOR CLIType.
   DEF BUFFER CLIType  FOR CLIType.

   DEF VAR llgMainLine AS LOG  NO-UNDO.    
   DEF VAR lcError     AS CHAR NO-UNDO.    

   FIND FIRST MsRequest NO-LOCK WHERE 
              MsRequest.Brand     = Syst.Var:gcBrand     AND 
              MsRequest.MsRequest = iiMsRequest NO-ERROR.   
   
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.Brand   = Syst.Var:gcBrand           AND
              MobSub.MsSeq   = MsRequest.MsSeq   AND 
              MobSub.InvCust = MsRequest.CustNum AND
              MobSub.PayType = FALSE             NO-ERROR.
  
   IF AVAIL MobSub THEN 
      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.Brand    = Syst.Var:gcBrand                        AND
                 CLIType.CLIType  = (IF Mobsub.TariffBundle > ""
                                       THEN Mobsub.TariffBundle
                                     ELSE Mobsub.CLIType)          AND
                 CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL} NO-ERROR.

   IF AVAIL CLIType THEN DO:
      
      llgMainLine = NO.

      MAINLINE:
      FOR EACH lbMobSub NO-LOCK WHERE
               lbMobSub.Brand   = Syst.Var:gcBrand           AND
               lbMobSub.InvCust = MsRequest.CustNum AND
               lbMobSub.PayType = FALSE,
         FIRST lbCLIType NO-LOCK WHERE
               lbCLIType.Brand   = Syst.Var:gcBrand                         AND
               lbCLIType.CLIType = (IF lbMobsub.TariffBundle > ""
                                       THEN lbMobsub.TariffBundle
                                    ELSE lbMobsub.CLIType)         AND
              (lbCLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
               lbCLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL}):

         IF lbCLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN DO:
            llgMainLine = YES.
            LEAVE MAINLINE.
         END.
      END.

      FIND FIRST bCLIType NO-LOCK WHERE
                 bCLIType.Brand   = Syst.Var:gcBrand AND
                 bCLIType.CLIType = "CONT9" NO-ERROR.

      IF AVAIL bCLIType AND NOT llgMainLine THEN
         fCTChangeRequest(MsRequest.MsSeq,
                          bCLIType.CLIType,
                          "",   /* lcBundleID */
                          "",   /* lcBankAcc = bank code validation is already done in newton */
                          Func.Common:mMake2DT(TODAY + 1,0),
                          0,   /* liCreditcheck 0 = Credit check ok */
                          0, /* extend contract 0=no extend_term_contract */
                          ""    /* pcSalesman */,
                          FALSE, /* charge */
                          TRUE,  /* send sms */
                          "",
                          0,
                          {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION},
                          0,     /* piOrderID */
                          0,
                          "", /*contract_id*/
                          OUTPUT lcError). 

   END.

   RETURN TRUE.

END FUNCTION.   

/* Main line termination makes additional line termination request in future.
   Remove additional line termination when STC done. */
FUNCTION fRemoveAdditionalLineTerminationReq RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):

   DEF BUFFER MsRequest FOR MsRequest.
      
   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq      EQ iiMsseq AND
              MsRequest.ReqType    EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
              MsRequest.ReqStatus  EQ {&REQUEST_STATUS_NEW} AND
              MsRequest.ReqCParam3 EQ 
              STRING({&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM}) 
              NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN DO:
      fChangeReqStatus(MsRequest.Msrequest,
                       4,
                       "Additional line termination cancelled by STC").
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

&ENDIF
