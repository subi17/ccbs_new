{Syst/commpaa.i}
Syst.Var:katun = "Qvantel".
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/tmsconst.i}

DEFINE VARIABLE lcOutputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount            AS INTEGER   NO-UNDO. 
DEFINE VARIABLE i                  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcKey              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCustnum          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldeActStamp        AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liPeriod           AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldaFromDate        AS DATE      NO-UNDO.
DEFINE VARIABLE ldaToDate          AS DATE      NO-UNDO.
DEFINE VARIABLE ldeFromStamp       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldaContractDate    AS DATE      NO-UNDO. 
DEFINE VARIABLE liContractTime     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liContractPeriod   AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcCli              AS CHARACTER NO-UNDO FORMAT "x(10)". 
DEFINE VARIABLE lcDataContracts    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFFItemKey        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEventLogDetails  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTerminated      AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE lcBundleOutputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSubBundle        AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttServiceLimit NO-UNDO
   FIELD groupcode AS char
   INDEX groupcode IS PRIMARY UNIQUE groupcode. 

DEFINE TEMP-TABLE ttSubDetails NO-UNDO
   FIELD MsSeq  AS INT
   FIELD CLI    AS CHAR
   FIELD Bundle AS CHAR
   INDEX MsSeq MsSeq. 

DEFINE BUFFER bEventLog FOR EventLog.

DEF STREAM sout.
DEF STREAM strout.

IF DAY(TODAY) = 1 THEN
   ldaToDate = Func.Common:mLastDayOfMonth(TODAY - 1).
ELSE
   ldaToDate = Func.Common:mLastDayOfMonth(TODAY).

ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate)).

ASSIGN ldeFromStamp = Func.Common:mMake2DT(ldaFromDate,0)
       liPeriod     = YEAR(ldaFromDate) * 100 + MONTH(ldaFromDate).

ASSIGN 
   lcOutputFile       = "/apps/yoigo/tms_support/billing/monthly_fee_check_"        + STRING(liPeriod) + ".txt"
   lcBundleOutputFile = "/apps/yoigo/tms_support/billing/monthly_fee_check_bundle_" + STRING(liPeriod) + ".txt".

UPDATE 
   lcOutputFile FORMAT "x(60)" 
                LABEL  "Output" SKIP(1)
                WITH WIDTH 80  
                OVERLAY SIDE-LABELS 1 
                COLUMN ROW 1 
                TITLE " Missing monthly fee check "
                FRAME fcontrolrep.

IF lcOutputFile eq "" OR 
   lcOutputFile eq ?  THEN quit.

DISP "running.." WITH FRAME fcontrolrep.

OUTPUT STREAM sout   TO VALUE(lcOutputFile).
OUTPUT STREAM strout TO VALUE(lcBundleOutputFile).

PUT STREAM sout UNFORMATTED 
      "CUSTNUM|MsSeq|MSISDN|CONTRACT|CONTRACT_FROM|CONTRACT_TO|MsRequest_CREATE_FEES|DELETE_FixedFee_EventLog|DELETE_FFItem_EventLog|EventLog_DETAIL|TERMINATION_TIME" SKIP.

PUT STREAM strout UNFORMATTED 
    "CUSTNUM|MsSeq|MSISDN|TariffBundle/BaseBundle" SKIP.


looppi:
FOR EACH DayCampaign WHERE
         DayCampaign.Brand = Syst.Var:gcBrand AND
         DayCampaign.DCEvent BEGINS "PAYTERM" NO-LOCK,
    EACH DCCLI where
         DCCLI.Brand    = Syst.Var:gcBrand    AND
         DCCLI.DCEvent  = DayCampaign.dcevent AND
         DCCLI.ValidTo >= ldaFromDate NO-LOCK:
   
   i = i + 1.

   IF i MOD 1000 = 0 THEN DO:
      DISP i                    LABEL "checked" 
           liCount              LABEL "found" 
           DCCLI.dcevent COLUMN-LABEL "contract".
      PAUSE 0.
   END.

   ASSIGN
      lcEventLogDetails = ""
      lcFFItemKey       = ""
      ldeTerminated     = 0.

   RELEASE bEventLog.

   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.MsSeq = DCCLI.MsSeq NO-ERROR.

   IF NOT AVAIL MobSub THEN DO:
      FIND FIRST TermMobSub NO-LOCK WHERE 
                 TermMobSub.MsSeq = DCCLI.MsSeq NO-ERROR.
      liCustnum = TermMobSub.CustNum.
      
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.MsSeq = TermMobSub.MsSeq USE-INDEX MsSeq.
      ldeTerminated = MsOwner.TsEnd.
   END.     
   ELSE liCustnum = MobSub.CustNum.
         
   FIND FIRST FixedFee NO-LOCK WHERE 
              FixedFee.Brand     = Syst.Var:gcBrand    AND
              FixedFee.CustNum   = liCustnum           AND
              FixedFee.HostTable = "mobsub"            AND
              FixedFee.KeyValue  = STRING(DCCLI.MsSeq) AND
              FixedFee.CalcObj   = DCCLI.DCEvent       AND
              FixedFee.BegDate  >= DCCLI.ValidFrom     NO-ERROR.
   
   IF NOT AVAIL FixedFee THEN DO:
      
      /* If fixed fee is not found, check any ACC request is handled for current month */
      FOR EACH MsRequest NO-LOCK where  
               MsRequest.MsSeq     = DCCLI.MsSeq                       AND
               MsRequest.ActStamp >= Func.Common:mDate2TS(ldaFromDate) AND
               MsRequest.ReqType   = 10                                AND
               MsRequest.ReqStatus = 2 :

         FIND FIRST FixedFee NO-LOCK WHERE 
                    FixedFee.brand     = Syst.Var:gcBrand    AND
                    FixedFee.custnum   = MsRequest.custnum   AND
                    FixedFee.hosttable = "mobsub"            AND
                    FixedFee.keyvalue  = string(DCCLI.MsSeq) AND
                    FixedFee.calcobj   = DCCLI.dcevent       AND
                    FixedFee.begdate  >= DCCLI.validfrom     NO-ERROR.
         IF AVAIL FixedFee THEN LEAVE.
      END.
   END.
   
   IF AVAIL FixedFee THEN DO:
      FIND FIRST FFItem NO-LOCK WHERE 
                 FFItem.FFNum      = FixedFee.FFNum AND
                 FFItem.billperiod = liPeriod       NO-ERROR.

      IF AVAIL FFItem THEN DO:
         
         IF (FFItem.Billed EQ TRUE  AND FFItem.InvNum = 0) OR
            (FFItem.Billed EQ FALSE AND FFItem.InvNum > 0) THEN DO:
            PUT STREAM sout UNFORMATTED 
                liCustnum       "|" 
                DCCLI.MsSeq     "|"
                DCCLI.dcevent   "|"
                DCCLI.validfrom "|"
                DCCLI.validto   "|"
                "ERROR:FFItem billed and invoice not found (or vice versa)"
            SKIP.
         END.
         ELSE IF FFItem.InvNum > 0 AND 
            NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                               Invoice.InvNum = FFItem.InvNum) THEN DO:
            PUT STREAM sout UNFORMATTED 
               liCustnum       "|" 
               DCCLI.MsSeq     "|"
               DCCLI.dcevent   "|"
               DCCLI.validfrom "|"
               DCCLI.validto   "|"
               "ERROR:FFItem invoice not found" SKIP.
         END.

         next.
      END.
   
      IF DCCLI.amount > 0 THEN DO:
         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand       = Syst.Var:gcBrand            AND
                    SingleFee.Custnum     = liCustnum                   AND
                    SingleFee.HostTable   = "mobsub"                    AND
                    SingleFee.KeyValue    = string(DCCLI.MsSeq)         AND
                    SingleFee.SourceKey   = STRING(DCCLI.PerContractID) AND
                    SingleFee.SourceTable = "DCCLI"                     AND
                    SingleFee.CalcObj     = "RVTERM"                    NO-ERROR.
         IF NOT AVAIL SingleFee THEN DO:
            PUT STREAM sout UNFORMATTED 
                liCustnum       "|" 
                DCCLI.MsSeq     "|"
                DCCLI.dcevent   "|"
                DCCLI.validfrom "|"
                DCCLI.validto   "|"
                "ERROR:Residual singlefee not found"
             SKIP.
         END.
      END.

   END.

   ASSIGN lcKey = "1"      + CHR(255) + STRING(liCustnum) + CHR(255) +
                  "MobSub" + CHR(255) + STRING(DCCLI.MsSeq)
          ldeActStamp = Func.Common:mMake2DT(DCCLI.validfrom, 0).

   IF AVAIL FixedFee THEN
      lcFFItemKey = string(FixedFee.FFNum) + CHR(255) + string(liPeriod).

   FIND FIRST MsRequest NO-LOCK WHERE 
              MsRequest.MsSeq      = DCCLI.MsSeq   AND
              MsRequest.reqtype    = 8             AND
              MsRequest.reqstatus  = 2             AND
              MsRequest.reqcparam3 = DCCLI.dcevent AND
              MsRequest.actstamp  >= ldeActStamp   NO-ERROR.

   FIND FIRST EventLog NO-LOCK WHERE 
              EventLog.TableName = "FixedFee" AND
              EventLog.Key  BEGINS lcKey      AND
              EventLog.Action    = "delete"   NO-ERROR.
   
   IF AVAIL EventLog THEN
      lcEventLogDetails = EventLog.UserCode + "-" + STRING(EventLog.EventDate).
   ELSE IF lcFFItemKey > "" THEN DO:
      FIND FIRST bEventLog NO-LOCK WHERE 
                 bEventLog.Tablename = "FFItem"   AND
                 bEventLog.Key BEGINS lcFFItemKey AND
                 bEventLog.Action    = "delete"   NO-ERROR.
      IF NOT AVAIL bEventLog AND
             AVAIL FixedFee  AND 
             FixedFee.EndPeriod < liPeriod THEN NEXT.
      ELSE IF AVAIL bEventLog THEN
         lcEventLogDetails = bEventLog.UserCode + "-" + STRING(bEventLog.EventDate).
   END.
   
   liCount = liCount + 1.

   PUT STREAM sout UNFORMATTED 
      liCustnum                                 "|"
      DCCLI.MsSeq                               "|" 
      DCCLI.CLI                                 "|" 
      DCCLI.DCEvent                             "|" 
      DCCLI.ValidFrom                           "|"
      DCCLI.ValidTo                             "|"
      (IF AVAIL MsRequest THEN
       STRING(MsRequest.CreateFees) ELSE "N/A") "|"
      AVAIL(EventLog)                           "|"
      AVAIL(bEventLog)                          "|"
      lcEventLogDetails                         "|"
      (IF ldeTerminated > 0 THEN Func.Common:mTS2HMS(ldeTerminated) ELSE "") SKIP.

END.

looppi2:
FOR EACH ServiceLimit NO-LOCK,
    FIRST DayCampaign NO-LOCK WHERE
          DayCampaign.brand   = Syst.Var:gcBrand AND
          DayCampaign.dcevent = ServiceLimit.groupcode:

   IF DayCampaign.feemodel EQ "" THEN NEXT.
   IF DayCampaign.dcevent BEGINS "dss" THEN NEXT.

   FIND FIRST FMItem NO-LOCK WHERE 
              FMItem.Brand    = Syst.Var:gcBrand     AND
              FMItem.FeeModel = DayCampaign.FeeModel AND
              FMItem.ToDate   > TODAY                NO-ERROR.

   IF FMItem.BillMethod THEN NEXT.
   IF FMItem.BillType EQ "NF" THEN NEXT. /* no fee (prepaid) */

   FIND FIRST ttServiceLimit NO-LOCK WHERE
              ttServiceLimit.groupcode = ServiceLimit.groupcode NO-ERROR.

   IF AVAIL ttServiceLimit THEN NEXT.

   CREATE ttServiceLimit.
   ASSIGN ttServiceLimit.groupcode = ServiceLimit.groupcode.

   FOR EACH MServiceLimit NO-LOCK WHERE 
            MServiceLimit.SLSeq    = ServiceLimit.SLSeq    AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.EndTs   >= ldeFromStamp:

      IF MServiceLimit.endts < MServiceLimit.fromts THEN NEXT.
      
      IF ServiceLimit.groupcode = "contdata"     AND
         MServiceLimit.fromts   = 20090301.00000 THEN NEXT.

      i = i + 1.

      IF i MOD 1000 = 0 THEN DO:
         DISP i                             LABEL "checked" 
              liCount                       LABEL "found" 
              ServiceLimit.groupcode COLUMN-LABEL "contract".
         PAUSE 0.
      END.

      ASSIGN
         lcFFItemKey       = ""
         lcEventLogDetails = ""
         ldeTerminated     = 0
         lcSubBundle       = "".
   
      RELEASE bEventLog.
       
      Func.Common:mSplitTS(MServiceLimit.fromTs, 
                           OUTPUT ldaContractDate, 
                           OUTPUT liContractTime).

      liContractPeriod = YEAR(ldaContractDate) * 100 + MONTH(ldaContractDate).
      
      FIND FIRST MobSub NO-LOCK WHERE 
                 MobSub.MsSeq = MServiceLimit.MsSeq NO-ERROR.

      IF NOT AVAIL MobSub THEN DO:
         FIND FIRST TermMobSub NO-LOCK WHERE  
                    TermMobSub.MsSeq = MServiceLimit.MsSeq NO-ERROR.
         
         FIND FIRST CliType NO-LOCK WHERE  
                    CliType.CLitype = TermMobSub.CLIType NO-ERROR.
         /* for staging */
         IF NOT AVAIL CLIType THEN NEXT.
         
         ASSIGN liCustnum = TermMobSub.CustNum
                lcCli     = TermMobSub.CLI.

         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.MsSeq = TermMobSub.MsSeq USE-INDEX MsSeq.
         ldeTerminated = MsOwner.TsEnd.

         IF TermMobSub.TariffBundle <> "" THEN 
            lcSubBundle = TermMobSub.TariffBundle.
         ELSE IF CLIType.BaseBundle <> "" THEN 
            lcSubBundle = CLIType.BaseBundle.

         IF lcSubBundle EQ ServiceLimit.groupcode THEN DO: 
            CREATE ttSubDetails.
            ASSIGN ttSubDetails.MsSeq  = TermMobSub.MsSeq 
                   ttSubDetails.cli    = TermMobSub.CLI
                   ttSubDetails.Bundle = lcSubBundle.
         END.                               
      END.     
      ELSE DO: 
         ASSIGN liCustnum = MobSub.CustNum
                lcCli     = MobSub.CLI.
      
         FIND FIRST CliType NO-LOCK WHERE 
                    CliType.CLitype = MobSub.CLIType NO-ERROR.
         
         IF MobSub.TariffBundle <> "" THEN 
            lcSubBundle = MobSub.TariffBundle.
         ELSE IF CLIType.BaseBundle <> "" THEN 
            lcSubBundle = CLIType.BaseBundle.

         IF lcSubBundle EQ ServiceLimit.groupcode THEN DO: 
            CREATE ttSubDetails.
            ASSIGN ttSubDetails.MsSeq  = MobSub.MsSeq 
                   ttSubDetails.cli    = MobSub.CLI
                   ttSubDetails.Bundle = lcSubBundle.
         END.          
      END.

      FIND FIRST FixedFee NO-LOCK WHERE
                 FixedFee.Brand      = Syst.Var:gcBrand            AND
                 FixedFee.HostTable  = "mobsub"                    AND
                 FixedFee.CustNum    = liCustnum                   AND
                 FixedFee.KeyValue   = STRING(MServiceLimit.MsSeq) AND
                 FixedFee.CalcObj    = ServiceLimit.GroupCode      AND
                 FixedFee.EndPeriod >= liContractPeriod            NO-ERROR.

      IF AVAIL FixedFee THEN DO:
         FIND FIRST FFItem NO-LOCK WHERE 
                    FFItem.FFNum      = FixedFee.ffnum AND
                    FFItem.billperiod = liPeriod       NO-ERROR.
      
         IF AVAIL FFItem THEN DO:
            IF (FFItem.billed          AND FFItem.invnum = 0) OR
               (FFItem.billed EQ FALSE AND FFItem.invnum > 0) THEN DO:
               PUT STREAM sout UNFORMATTED 
                  liCustnum           "|" 
                  MServiceLimit.MsSeq "|"
                  "ERROR:FFItem billed and invoice not found (or vice versa)"
               SKIP.
            END.
            ELSE IF FFItem.InvNum > 0 AND
                 NOT CAN-FIND(FIRST Invoice WHERE
                                    Invoice.InvNum = FFItem.InvNum) THEN DO:
               PUT STREAM sout UNFORMATTED 
                  liCustnum           "|" 
                  MServiceLimit.MsSeq "|"
                  "ERROR:FFItem invoice not found" SKIP.
            END.

            NEXT.
         END.
      END.
      
      IF AVAIL FixedFee THEN
         lcFFItemKey = STRING(FixedFee.FFNum) + CHR(255) + STRING(liPeriod).

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq      = MServiceLimit.MsSeq    AND
                 MsRequest.ReqType    = 8                      AND 
                 MsRequest.ReqStatus  = 2                      AND
                 MsRequest.ReqCParam3 = ServiceLimit.GroupCode AND
                 MsRequest.ActStamp   = MServiceLimit.FromTS   NO-ERROR.
   
      lcKey = "1"      + CHR(255) + STRING(liCustnum) + CHR(255) + 
              "MobSub" + CHR(255) + STRING(MServiceLimit.MsSeq). 
      
      FIND FIRST EventLog NO-LOCK WHERE
                 EventLog.TableName = "FixedFee" AND 
                 EventLog.Key  BEGINS lcKey      AND 
                 EventLog.Action    = "delete"   NO-ERROR.

      IF AVAIL EventLog THEN
         lcEventLogDetails = EventLog.usercode + "-" + STRING(EventLog.eventdate).
      ELSE IF lcFFItemKey > "" THEN DO:
         FIND FIRST bEventLog NO-LOCK WHERE
                    bEventLog.Tablename = "FFItem"    AND 
                    bEventLog.Key  BEGINS lcFFItemKey AND 
                    bEventLog.Action    = "delete"    NO-ERROR.
         IF AVAIL bEventLog THEN
            lcEventLogDetails = bEventLog.usercode + "-" + STRING(bEventLog.eventdate).
      END.
      
      PUT STREAM sout UNFORMATTED
         liCustnum                        "|"
         MServiceLimit.MsSeq              "|" 
         lcCli                            "|"
         ServiceLimit.GroupCode           "|"
         MServiceLimit.FromTS             "|"
         MServiceLimit.EndTS              "|"
        (IF AVAIL MsRequest THEN
            STRING(MsRequest.createfees) 
         ELSE "N/A")                      "|"
         AVAIL(EventLog)                  "|"
         AVAIL(bEventLog)                 "|"
         lcEventLogDetails                "|"
        (IF ldeTerminated > 0 THEN 
            Func.Common:mTS2HMS(ldeTerminated) 
         ELSE "")                         SKIP.
   
      liCount = liCount + 1.
   END.
END.


DEF VAR lcBundleBasedCLITypes AS CHAR NO-UNDO.  
DEF VAR lcBundle              AS CHAR NO-UNDO. 
DEF VAR liBundleCount         AS INT  NO-UNDO. 

ASSIGN liBundleCount = 0
       lcBundleBasedCLITypes = fCParam("Bundles","BUNDLE_BASED_CLITYPES").

FOR EACH MobSub NO-LOCK WHERE 
         MobSub.Brand = Syst.Var:gcBrand:             
    RUN pCheckSubscription((BUFFER Mobsub:HANDLE)).
END.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = Syst.Var:gcBrand AND 
         MsRequest.ReqType   = 18               AND 
         MsRequest.ReqStatus = 2                AND 
         MsRequest.ActStamp >= ldeFromStamp:

   FIND FIRST TermMobSub NO-LOCK WHERE 
              TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   
   IF NOT AVAIL TermMobSub THEN NEXT.

    RUN pCheckSubscription((BUFFER TermMobSub:HANDLE)).
END. 

PROCEDURE pCheckSubscription:

   DEF INPUT PARAM ihSub AS HANDLE.
   
   FIND FIRST CLIType NO-LOCK WHERE 
              CLIType.CLIType = ihSub::CLIType NO-ERROR.
     
   IF NOT AVAIL CLIType THEN NEXT.
   
   IF CLIType.FixedLineDownLoad > "" THEN DO:

      FIND RequestAction NO-LOCK WHERE
           RequestAction.Brand      = Syst.Var:gcBrand AND
           RequestAction.CLIType    = ihSub::CLIType   AND
           RequestAction.ReqType    = 14               AND
           RequestAction.ValidTo   >= TODAY            AND
           RequestAction.ActionType = "DayCampaign"    AND
           RequestAction.Action     = 1 NO-ERROR.

      IF AVAIL RequestAction THEN
         RUN pBundleCheck(ihSub::MsSeq,
                          ihSub::CustNum,
                          ihSub::CLI,
                          RequestAction.ActionKey).

      IF ihSub::MSStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} THEN NEXT.

      /* TODO: missing partial convergent termination support */
      IF ihSub::MSStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN NEXT.
   END.

   /* This need to be checked, because basebundle is not necessary for prepaid */
   IF CLIType.BaseBundle BEGINS "TARJ" THEN NEXT.

   IF ihSub::TariffBundle EQ "" AND
      CLIType.BaseBundle  EQ "" THEN NEXT.
  
   IF LOOKUP(CLIType.CLIType,lcBundleBasedCLITypes) > 0 AND 
      ihSub::TariffBundle EQ ""                         THEN  
   DO:
      PUT STREAM strout UNFORMATTED 
         ihSub::MsSeq                  "|"
         ihSub::CustNum                "|"
         ihSub::CLI                    "|"
         "Tariff Bundle not available" SKIP.

      liBundleCount = liBundleCount + 1.
      NEXT.
   END.

   IF ihSub::TariffBundle <> "" THEN 
      lcBundle = ihSub::TariffBundle. 
   ELSE IF CLIType.BaseBundle <> "" THEN
      lcBundle = CLIType.BaseBundle.
   ELSE lcBundle = "".
   
   IF CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_FIXEDONLY} AND  
      lcBundle           EQ "DUB"                           AND 
      ihSub::IMSI        NE ""                              THEN DO:
      
      IF (ihSub::MSStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR 
          ihSub::MSStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN NEXT. 
        
   END. 

   RUN pBundleCheck(ihSub::MsSeq,
                    ihSub::CustNum,
                    ihSub::CLI,
                    lcBundle).

END PROCEDURE. 

PROCEDURE pBundleCheck:

   DEFINE INPUT PARAMETER iiMsSeq   AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER iiCustNum AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER icCLI     AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER icBundle  AS CHARACTER NO-UNDO. 

   DEF VAR llgAvailable AS LOGICAL NO-UNDO. 

   IF NOT CAN-FIND(FIRST ttSubDetails WHERE 
                         ttSubDetails.MsSeq  EQ iiMsSeq   AND 
                         ttSubDetails.Bundle EQ icBundle) THEN DO:
     
      llgAvailable = FALSE.

      /* Double check in MServiceLimit */
      FOR EACH MServiceLimit NO-LOCK WHERE 
               MServiceLimit.MsSeq  = iiMsSeq AND
               MServiceLimit.endts >= ldeFromStamp:
        
         FIND FIRST ServiceLimit NO-LOCK WHERE 
                    ServiceLimit.slseq     = MServiceLimit.slseq    AND 
                    ServiceLimit.dialtype  = MServiceLimit.dialtype AND 
                    ServiceLimit.groupcode = icBundle               NO-ERROR.
         IF AVAIL ServiceLimit THEN DO:
            llgAvailable = TRUE.
            LEAVE.
         END.
      END.         
     
      IF NOT llgAvailable THEN DO:
         PUT STREAM strout UNFORMATTED 
            iiCustNum "|"
            iiMsSeq   "|"
            icCLI     "|"
            icBundle  SKIP.

         liBundleCount = liBundleCount + 1.
      END.
   END.                      

END PROCEDURE.

OUTPUT STREAM sout close.
OUTPUT STREAM strout CLOSE.

MESSAGE "Done," liCount "missing contracts found" VIEW-AS ALERT-BOX.

MESSAGE "Bundle tariff Count : " liBundleCount VIEW-AS ALERT-BOX.

