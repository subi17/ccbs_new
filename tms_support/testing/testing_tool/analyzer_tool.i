/* ----------------------------------------------------------------------
  Module .......: analyzer_tool.i
  Task .........: Include files and procedures
  Application ..: TMS
  Author .......: Vikas
  Created ......: 20.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

FUNCTION fLogEntry RETURNS LOG ():
   PUT STREAM sOutput UNFORMATTED
       ttAnalyzerReport.CLI               lcDel
       ttAnalyzerReport.ActionType        lcDel
       ttAnalyzerReport.ActionValue       lcDel
       STRING(ttAnalyzerReport.FromTS)    lcDel
       STRING(ttAnalyzerReport.EndTS)     lcDel
       STRING(ttAnalyzerReport.Limit)     lcDel
       STRING(ttAnalyzerReport.FixedFee)  lcDel
       STRING(ttAnalyzerReport.SingleFee) lcDel
       ttAnalyzerReport.Shaper            lcDel
       ttAnalyzerReport.Remark            SKIP.
END FUNCTION.

FUNCTION fCheckDCCLIContract RETURNS LOGICAL
         (INPUT icDcEvent   AS CHAR,
          INPUT iiMsSeq     AS INT,
          INPUT idActDate   AS DATE,
          INPUT icAction    AS CHAR,
          BUFFER bbttAnalyzerReport FOR ttAnalyzerReport):

   FOR FIRST DayCampaign WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DcEvent = icDcEvent NO-LOCK,
       FIRST DCCLI WHERE
             DCCLI.MsSeq = iiMsSeq AND
             DCCLI.DcEvent = DayCampaign.DcEvent AND
             DCCLI.ValidTo >= idActDate NO-LOCK:

      ASSIGN bbttAnalyzerReport.FromTS = fMake2Dt(DCCLI.ValidFrom,0)
             bbttAnalyzerReport.EndTS  = fMake2Dt(DCCLI.ValidTo,0).

      IF DayCampaign.FeeModel > "" THEN DO:
      FIND FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                 FixedFee.Brand     = gcBrand AND
                 FixedFee.HostTable = "MobSub" AND
                 FixedFee.KeyValue  = STRING(DCCLI.MsSeq) AND
                 FixedFee.FeeModel  = DayCampaign.FeeModel AND
                 FixedFee.CalcObj   = DayCampaign.DCEvent AND
                 FixedFee.InUse     = TRUE AND
                 FixedFee.BegDate  >= DCCLI.ValidFrom NO-ERROR.
      IF NOT AVAIL FixedFee THEN
         bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                     "FixedFee is not created".
      ELSE DO:
         FIND FIRST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF AVAIL FFItem THEN
            bbttAnalyzerReport.FixedFee = FFItem.Amt.
         ELSE
            bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                        "FFItem is not created".
      END. /* ELSE DO: */
      END. /* IF DayCampaign.FeeModel > "" THEN DO: */

      IF icAction = "Termination" AND DayCampaign.TermFeeModel > "" THEN DO:
         FIND FIRST SingleFee WHERE
                    SingleFee.Brand = gcBrand AND
                    SingleFee.HostTable = "MobSub" AND
                    SingleFee.KeyValue  = STRING(DCCLI.MsSeq) AND
                    SingleFee.FeeModel  = DayCampaign.TermFeeModel AND
                    SingleFee.BillPeriod = YEAR(DCCLI.ValidFrom) * 100 +
                                           MONTH(DCCLI.ValidFrom) NO-LOCK NO-ERROR.
         IF NOT AVAIL SingleFee THEN
            bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                        "SingleFee is not created".
         ELSE bbttAnalyzerReport.SingleFee = SingleFee.Amt.
      END. /* IF icAction = "Termination" AND DayCampaign.TermFeeModel > "" */

      bbttAnalyzerReport.Remark = TRIM(bbttAnalyzerReport.Remark,lcDel).
   END. /* FOR FIRST DayCampaign WHERE */
END. /* FUNCTION fCheckDCCLIContract RETURNS LOGICAL */

FUNCTION fCheckServLimitContract RETURNS LOGICAL
         (INPUT icDcEvent AS CHAR,
          INPUT iiMsSeq   AS INT,
          INPUT iiMainRequest AS INT,
          INPUT idActDate AS DATE,
          INPUT ideActStamp AS DEC,
          BUFFER bbttAnalyzerReport FOR ttAnalyzerReport):

   DEF VAR ldNextMonthActStamp    AS DEC  NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   DEF BUFFER bSubMsRequest       FOR MsRequest.

   ldNextMonthActStamp = fMake2Dt(fLastDayOfMonth(idActDate) + 1,0).

   FOR FIRST DayCampaign WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DcEvent = icDcEvent NO-LOCK,
       EACH  ServiceLimit WHERE
             ServiceLimit.GroupCode = DayCampaign.DcEvent NO-LOCK,
       FIRST MServiceLimit WHERE
             MServiceLimit.MsSeq = iiMsSeq AND
             MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.FromTS   < ldNextMonthActStamp AND
             MServiceLimit.EndTS   >= ideActStamp NO-LOCK:

      ASSIGN bbttAnalyzerReport.FromTS = MServiceLimit.FromTS
             bbttAnalyzerReport.EndTS  = MServiceLimit.EndTS
             bbttAnalyzerReport.Limit  = MServiceLimit.InclAmt.

      fSplitTS(MServiceLimit.FromTS,OUTPUT ldtActDate,OUTPUT liActTime).

      IF DayCampaign.FeeModel > "" THEN DO:
         FIND FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                    FixedFee.Brand     = gcBrand AND
                    FixedFee.HostTable = "MobSub" AND
                    FixedFee.KeyValue  = STRING(MServiceLimit.MsSeq) AND
                    FixedFee.FeeModel  = DayCampaign.FeeModel AND
                    FixedFee.CalcObj   = DayCampaign.DCEvent AND
                    FixedFee.InUse     = TRUE AND
                    FixedFee.BegDate  >= ldtActDate NO-ERROR.
         IF NOT AVAIL FixedFee THEN
            bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                        "FixedFee is not created".
         ELSE DO:
            FIND FIRST FFItem OF FixedFee NO-LOCK NO-ERROR.
            IF AVAIL FFItem THEN
               bbttAnalyzerReport.FixedFee = FFItem.Amt.
            ELSE
               bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                           "FFItem is not created".
         END. /* ELSE DO: */
      END. /* IF DayCampaign.FeeModel > "" THEN DO: */

      /* Check shaper sub-request */
      FIND FIRST bSubMsRequest WHERE
                 bSubMsRequest.OrigRequest = iiMainRequest AND
                 bSubMsRequest.ReqType     = 1 AND
                 bSubMsRequest.ReqCparam1  = "SHAPER" NO-LOCK NO-ERROR.
      IF AVAIL bSubMsRequest THEN
         bbttAnalyzerReport.Shaper = bSubMsRequest.ReqCparam2.

      bbttAnalyzerReport.Remark = TRIM(bbttAnalyzerReport.Remark,lcDel).
   END. /* FOR FIRST DayCampaign WHERE */
END. /* FUNCTION fCheckServLimitContract RETURNS LOGICAL */

FUNCTION fCheckService RETURNS LOGICAL
         (INPUT icServCode AS CHAR,
          INPUT iiMsSeq    AS INT,
          INPUT idActDate  AS DATE,
          BUFFER bbttAnalyzerReport FOR ttAnalyzerReport):

   FIND FIRST SubSer WHERE
              SubSer.ServCom =  icServCode AND
              SubSer.MsSeq   =  iiMsSeq AND
              SubSer.SsDate  <= idActDate NO-LOCK NO-ERROR.
   IF AVAIL SubSer THEN DO:
      bbttAnalyzerReport.FromTS = fMake2Dt(SubSer.SsDate,0).

      IF CAN-FIND(FIRST MobSub WHERE
                        MobSub.MsSeq = MsRequest.MsSeq AND
                        MobSub.PayType = FALSE) THEN DO:
         FIND FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                    FixedFee.Brand     = gcBrand AND
                    FixedFee.HostTable = "MobSub" AND
                    FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
                    FixedFee.FeeModel  = "BBMF" AND
                    FixedFee.InUse     = TRUE AND
                    FixedFee.BegDate  >= SubSer.SsDate NO-ERROR.
         IF NOT AVAIL FixedFee THEN
            bbttAnalyzerReport.Remark = bbttAnalyzerReport.Remark + lcDel +
                                        "FixedFee is not created".
         ELSE
            bbttAnalyzerReport.FixedFee = FixedFee.Amt.
      END. /*IF CAN-FIND(FIRST MobSub WHERE */
      bbttAnalyzerReport.Remark = TRIM(bbttAnalyzerReport.Remark,lcDel).
   END. /* IF AVAIL SubSer THEN DO: */
END. /* FUNCTION fCheckService RETURNS LOGICAL */

PROCEDURE pContractActivation:

   DEF INPUT PARAMETER iiMsSeq    AS INT  NO-UNDO.

   DEF VAR llDefault              AS LOG  NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   FOR EACH MsRequest NO-LOCK WHERE MsRequest.MsSeq = iiMsSeq AND MsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} USE-INDEX MsSeq:

      /* Exclude Request part of STC or BTC */
      IF MsRequest.ReqSource = {&REQUEST_SOURCE_STC} OR MsRequest.ReqSource = {&REQUEST_SOURCE_BTC} THEN 
          NEXT.

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN 
          NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Contract Activation"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.ActionValue = MsRequest.ReqCparam3
             llDefault = TRUE.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "Contract request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "Contract request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "Contract request is ongoing".

      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand = gcBrand AND
                 DayCampaign.DcEvent = MsRequest.ReqCparam3
           NO-LOCK NO-ERROR.
      IF NOT AVAIL DayCampaign THEN
         ttAnalyzerReport.Remark = "Contract not found".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      fSplitTS(MsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).
      IF LOOKUP(DayCampaign.DCType,"1,4,6") = 0 THEN
         fCheckDCCLIContract(INPUT MsRequest.ReqCparam3,
                             INPUT MsRequest.MsSeq,
                             INPUT ldtActDate,
                             INPUT "Activation",
                             BUFFER ttAnalyzerReport).
      ELSE
         fCheckServLimitContract(INPUT MsRequest.ReqCparam3,
                                 INPUT MsRequest.MsSeq,
                                 INPUT MsRequest.MsRequest,
                                 INPUT ldtActDate,
                                 INPUT MsRequest.ActStamp,
                                 BUFFER ttAnalyzerReport).

      ttAnalyzerReport.Remark = ttAnalyzerReport.Remark + lcDel + MsRequest.Memo.
      ttAnalyzerReport.Remark = TRIM(ttAnalyzerReport.Remark,lcDel).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN 
   DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Contract Activation"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No Contract Activation Request".
   END. /* IF NOT llDefault THEN DO: */

END PROCEDURE.

PROCEDURE pContractDeactivation:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.

   DEF VAR llDefault              AS LOG  NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = iiMsSeq AND
            MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION}
            USE-INDEX MsSeq:

      /* Exclude Request part of STC or BTC */
      IF MsRequest.ReqSource = {&REQUEST_SOURCE_STC} OR
         MsRequest.ReqSource = {&REQUEST_SOURCE_BTC} THEN NEXT.

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND
         MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Contract Termination"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.ActionValue = MsRequest.ReqCparam3
             llDefault = TRUE.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "Contract request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "Contract request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "Contract request is ongoing".

      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand = gcBrand AND
                 DayCampaign.DcEvent = MsRequest.ReqCparam3
           NO-LOCK NO-ERROR.
      IF NOT AVAIL DayCampaign THEN
         ttAnalyzerReport.Remark = "Contract not found".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      fSplitTS(MsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).
      IF LOOKUP(DayCampaign.DCType,"1,4,6") = 0 THEN
         fCheckDCCLIContract(INPUT MsRequest.ReqCparam3,
                             INPUT MsRequest.MsSeq,
                             INPUT ldtActDate,
                             INPUT "Termination",
                             BUFFER ttAnalyzerReport).
      ELSE
         fCheckServLimitContract(INPUT MsRequest.ReqCparam3,
                                 INPUT MsRequest.MsSeq,
                                 INPUT MsRequest.MsRequest,
                                 INPUT ldtActDate,
                                 INPUT MsRequest.ActStamp,
                                 BUFFER ttAnalyzerReport).

      ttAnalyzerReport.Remark = ttAnalyzerReport.Remark + lcDel + MsRequest.Memo.
      ttAnalyzerReport.Remark = TRIM(ttAnalyzerReport.Remark,lcDel).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Contract Termination"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No Contract Termination Request".
   END. /* IF NOT llDefault THEN DO: */
END PROCEDURE.

PROCEDURE pServiceActivation:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.

   DEF VAR llDefault              AS LOG  NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = iiMsSeq AND
            MsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
            MsRequest.ReqCparam1 = "BB" AND
            MsRequest.ReqIparam1 = 1
            USE-INDEX MsSeq:

      /* Exclude Request part of STC or BTC */
      IF MsRequest.ReqSource = {&REQUEST_SOURCE_STC} OR
         MsRequest.ReqSource = {&REQUEST_SOURCE_BTC} THEN NEXT.

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND
         MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Service Activation"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.ActionValue = MsRequest.ReqCparam1
             llDefault = TRUE.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "Service request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "Service request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "Service request is ongoing".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      fSplitTS(MsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).
      fCheckService(INPUT MsRequest.ReqCparam1,
                    INPUT MsRequest.MsSeq,
                    INPUT ldtActDate,
                    BUFFER ttAnalyzerReport).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Service Activation"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No Service Activation Request".
   END. /* IF NOT llDefault THEN DO: */
END PROCEDURE.

PROCEDURE pServiceDeactivation:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.

   DEF VAR llDefault              AS LOG  NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = iiMsSeq AND
            MsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
            MsRequest.ReqCparam1 = "BB" AND
            MsRequest.ReqIparam1 = 2
            USE-INDEX MsSeq:

      /* Exclude Request part of STC or BTC */
      IF MsRequest.ReqSource = {&REQUEST_SOURCE_STC} OR
         MsRequest.ReqSource = {&REQUEST_SOURCE_BTC} THEN NEXT.

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND
         MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Service Termination"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.ActionValue = MsRequest.ReqCparam1
             llDefault = TRUE.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "Service request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "Service request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "Service request is ongoing".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      fSplitTS(MsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).
      fCheckService(INPUT MsRequest.ReqCparam1,
                    INPUT MsRequest.MsSeq,
                    INPUT ldtActDate,
                    BUFFER ttAnalyzerReport).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "Service Termination"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No Service Termination Request".
   END. /* IF NOT llDefault THEN DO: */
END PROCEDURE.

PROCEDURE pSTC:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.

   DEF VAR llDefault              AS LOG NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   DEF BUFFER bSubMsRequest       FOR MsRequest.
   DEF BUFFER bMsRequest          FOR MsRequest.
   DEF BUFFER bttAnalyzerReport   FOR ttAnalyzerReport.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = iiMsSeq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}
            USE-INDEX MsSeq:

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND
         MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "STC"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             llDefault = TRUE.

      IF MsRequest.ReqCparam5 = "" THEN
         ttAnalyzerReport.ActionValue = MsRequest.ReqCparam1 + "->" +
                                        MsRequest.ReqCparam2.
      ELSE
         ttAnalyzerReport.ActionValue = MsRequest.ReqCparam1 + "->" +
                                        MsRequest.ReqCparam5.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "STC request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "STC request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "STC request is ongoing".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                 MsOwner.MsSeq  = MsRequest.MsSeq AND
                 MsOwner.TsEnd >= 99999999 NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN 
         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      IF AVAILABLE MsOwner THEN
         ASSIGN ttAnalyzerReport.FromTS = MSOwner.TsBegin
                ttAnalyzerReport.EndTS  = MsOwner.TsEnd.

      /* Check shaper sub-request */
      FOR EACH bMsRequest WHERE
               bMsRequest.MsSeq = MsRequest.MsSeq AND
               bMsRequest.ReqType = 1 AND
               bMsRequest.ReqCparam1 = "SHAPER" AND
               bMsRequest.ReqStatus  = 2 AND
               bMsRequest.DoneStamp >= MsRequest.DoneStamp NO-LOCK
               BY bMsRequest.DoneStamp DESC:
          ttAnalyzerReport.Shaper = bMsRequest.ReqCparam2.
          LEAVE.
      END. /* FOR EACH bMsRequest WHERE */

      /* Check all sub-requests */
      FOR EACH bSubMsRequest WHERE
               bSubMsRequest.OrigRequest = MsRequest.MsRequest AND
               LOOKUP(STRING(bSubMsRequest.ReqType),"1,8,9") > 0 NO-LOCK:

         CREATE bttAnalyzerReport.
         ASSIGN bttAnalyzerReport.FileName    = ttInputFileContent.FileName
                bttAnalyzerReport.CLI         = ttInputFileContent.CLI
                bttAnalyzerReport.ActionType  = "STC-SubRequest_" +
                                                STRING(bSubMsRequest.ReqType).

         IF bSubMsRequest.ReqType = 1 THEN
            bttAnalyzerReport.ActionValue = bSubMsRequest.ReqCparam1.
         ELSE bttAnalyzerReport.ActionValue = bSubMsRequest.ReqCparam3.

         IF bSubMsRequest.ReqStatus = 3 THEN
            ttAnalyzerReport.Remark = "Subrequest is rejected".
         ELSE IF bSubMsRequest.ReqStatus = 4 THEN
            bttAnalyzerReport.Remark = "Subrequest is cancelled".
         ELSE IF bSubMsRequest.ReqStatus <> 2 AND bSubMsRequest.ReqStatus <> 9 THEN
            bttAnalyzerReport.Remark = "Subrequest is ongoing".

         IF bttAnalyzerReport.Remark > "" THEN NEXT.

         fSplitTS(bSubMsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).

         IF LOOKUP(STRING(bSubMsRequest.ReqType),"8,9") > 0 THEN DO:
            FIND FIRST DayCampaign WHERE
                       DayCampaign.Brand = gcBrand AND
                       DayCampaign.DcEvent = bSubMsRequest.ReqCparam3
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL DayCampaign THEN DO:
               bttAnalyzerReport.Remark = "Contract not found".
               NEXT.
            END. /* IF NOT AVAIL DayCampaign THEN DO: */

            IF LOOKUP(DayCampaign.DCType,"1,4,6") = 0 THEN
               fCheckDCCLIContract(INPUT bSubMsRequest.ReqCparam3,
                                   INPUT bSubMsRequest.MsSeq,
                                   INPUT ldtActDate,
                                   INPUT (IF bSubMsRequest.ReqType = 9 THEN
                                          "Termination" ELSE "Activation"),
                                   BUFFER bttAnalyzerReport).
            ELSE
               fCheckServLimitContract(INPUT bSubMsRequest.ReqCparam3,
                                       INPUT bSubMsRequest.MsSeq,
                                       INPUT bSubMsRequest.MsRequest,
                                       INPUT ldtActDate,
                                       INPUT bSubMsRequest.ActStamp,
                                       BUFFER bttAnalyzerReport).
         END. /* IF LOOKUP(STRING(bSubMsRequest.ReqType),"8,9") > 0 THEN DO: */
         ELSE
            fCheckService(INPUT bSubMsRequest.ReqCparam1,
                          INPUT bSubMsRequest.MsSeq,
                          INPUT ldtActDate,
                          BUFFER bttAnalyzerReport).
      END. /* FOR EACH bSubMsRequest WHERE */

      ttAnalyzerReport.Remark = TRIM(ttAnalyzerReport.Remark,lcDel).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "STC"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No STC Request".
   END. /* IF NOT llDefault THEN DO: */
END PROCEDURE.

PROCEDURE pBTC:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.

   DEF VAR llDefault              AS LOG NO-UNDO.
   DEF VAR ldtActDate             AS DATE NO-UNDO.
   DEF VAR liActTime              AS INT  NO-UNDO.

   DEF BUFFER bSubMsRequest       FOR MsRequest.
   DEF BUFFER bMsRequest          FOR MsRequest.
   DEF BUFFER bttAnalyzerReport   FOR ttAnalyzerReport.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = iiMsSeq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}
            USE-INDEX MsSeq:

      /* Exclude if source is not script and not a new subscription */
      IF NOT ttInputFileContent.NewSubs AND
         MsRequest.ReqSource <> {&REQUEST_SOURCE_SCRIPT} THEN NEXT.

      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "BTC"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.ActionValue = MsRequest.ReqCparam1 + "->" +
                                            MsRequest.ReqCparam2
             llDefault = TRUE.

      IF MsRequest.ReqStatus = 3 THEN
         ttAnalyzerReport.Remark = "BTC request is rejected".
      ELSE IF MsRequest.ReqStatus = 4 THEN
         ttAnalyzerReport.Remark = "BTC request is cancelled".
      ELSE IF MsRequest.ReqStatus <> 2 AND MsRequest.ReqStatus <> 9 THEN
         ttAnalyzerReport.Remark = "BTC request is ongoing".

      IF ttAnalyzerReport.Remark > "" THEN NEXT.

      FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                 MsOwner.MsSeq  = MsRequest.MsSeq AND
                 MsOwner.TsEnd >= 99999999 NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN 
         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      IF AVAILABLE MsOwner THEN
         ASSIGN ttAnalyzerReport.FromTS = MSOwner.TsBegin
                ttAnalyzerReport.EndTS  = MsOwner.TsEnd.

      /* Check shaper sub-request */
      FOR EACH bMsRequest WHERE
               bMsRequest.MsSeq = MsRequest.MsSeq AND
               bMsRequest.ReqType = 1 AND
               bMsRequest.ReqCparam1 = "SHAPER" AND
               bMsRequest.ReqStatus  = 2 AND
               bMsRequest.DoneStamp >= MsRequest.DoneStamp NO-LOCK
               BY bMsRequest.DoneStamp DESC:
          ttAnalyzerReport.Shaper = bMsRequest.ReqCparam2.
          LEAVE.
      END. /* FOR EACH bMsRequest WHERE */

      /* Check all sub-requests */
      FOR EACH bSubMsRequest WHERE
               bSubMsRequest.OrigRequest = MsRequest.MsRequest AND
               LOOKUP(STRING(bSubMsRequest.ReqType),"1,8,9") > 0 NO-LOCK:

         CREATE bttAnalyzerReport.
         ASSIGN bttAnalyzerReport.FileName    = ttInputFileContent.FileName
                bttAnalyzerReport.CLI         = ttInputFileContent.CLI
                bttAnalyzerReport.ActionType  = "BTC-SubRequest_" +
                                                STRING(bSubMsRequest.ReqType).

         IF bSubMsRequest.ReqType = 1 THEN
            bttAnalyzerReport.ActionValue = bSubMsRequest.ReqCparam1.
         ELSE bttAnalyzerReport.ActionValue = bSubMsRequest.ReqCparam3.

         IF bSubMsRequest.ReqStatus = 3 THEN
            ttAnalyzerReport.Remark = "Subrequest is rejected".
         ELSE IF bSubMsRequest.ReqStatus = 4 THEN
            bttAnalyzerReport.Remark = "Subrequest is cancelled".
         ELSE IF bSubMsRequest.ReqStatus <> 2 AND bSubMsRequest.ReqStatus <> 9 THEN
            bttAnalyzerReport.Remark = "Subrequest is ongoing".

         IF bttAnalyzerReport.Remark > "" THEN NEXT.

         fSplitTS(bSubMsRequest.ActStamp,OUTPUT ldtActDate,OUTPUT liActTime).
         IF LOOKUP(STRING(bSubMsRequest.ReqType),"8,9") > 0 THEN DO:
            FIND FIRST DayCampaign WHERE
                       DayCampaign.Brand = gcBrand AND
                       DayCampaign.DcEvent = bSubMsRequest.ReqCparam3
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL DayCampaign THEN DO:
               bttAnalyzerReport.Remark = "Contract not found".
               NEXT.
            END. /* IF NOT AVAIL DayCampaign THEN DO: */

            IF LOOKUP(DayCampaign.DCType,"1,4,6") = 0 THEN
               fCheckDCCLIContract(INPUT bSubMsRequest.ReqCparam3,
                                   INPUT bSubMsRequest.MsSeq,
                                   INPUT ldtActDate,
                                   INPUT (IF bSubMsRequest.ReqType = 9 THEN
                                          "Termination" ELSE "Activation"),
                                   BUFFER bttAnalyzerReport).
            ELSE
               fCheckServLimitContract(INPUT bSubMsRequest.ReqCparam3,
                                       INPUT bSubMsRequest.MsSeq,
                                       INPUT bSubMsRequest.MsRequest,
                                       INPUT ldtActDate,
                                       INPUT bSubMsRequest.ActStamp,
                                       BUFFER bttAnalyzerReport).
         END. /* IF LOOKUP(STRING(bSubMsRequest.ReqType),"8,9") > 0 THEN DO: */
         ELSE
            fCheckService(INPUT bSubMsRequest.ReqCparam1,
                          INPUT bSubMsRequest.MsSeq,
                          INPUT ldtActDate,
                          BUFFER bttAnalyzerReport).
      END. /* FOR EACH bSubMsRequest WHERE */

      ttAnalyzerReport.Remark = TRIM(ttAnalyzerReport.Remark,lcDel).
   END. /* FOR EACH MsRequest WHERE */

   IF NOT llDefault THEN DO:
      CREATE ttAnalyzerReport.
      ASSIGN ttAnalyzerReport.FileName    = ttInputFileContent.FileName
             ttAnalyzerReport.ActionType  = "BTC"
             ttAnalyzerReport.CLI         = ttInputFileContent.CLI
             ttAnalyzerReport.Remark      = "No BTC Request".
   END. /* IF NOT llDefault THEN DO: */
END PROCEDURE.

