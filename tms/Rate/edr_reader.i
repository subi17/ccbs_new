/* ----------------------------------------------------------------------
  MODULE .......: edr_reader.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.12.13
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/tmsconst.i}
{Func/detailvalue.i}
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Mm/fbundle.i}
{Func/fmakesms.i}

&GLOBAL-DEFINE EDR_SUCCESS_CODE_OK 1 
&GLOBAL-DEFINE EDR_SUCCESS_CODE_ERROR 3 

/* EDR specific errors */
&GLOBAL-DEFINE EDR_ERROR_UNKNOWN_MSISDN                   1001
&GLOBAL-DEFINE EDR_ERROR_MSISDN_NOT_ACTIVE                1002
&GLOBAL-DEFINE EDR_ERROR_SUCCESS_CODE_VALUE               5001 
&GLOBAL-DEFINE EDR_ERROR_SERVICE_CLASS_VALUE              5002 
&GLOBAL-DEFINE EDR_ERROR_SERVICE_CLASS_INCONSISTENT       5003
&GLOBAL-DEFINE EDR_ERROR_SUBSCRIPTION_TYPE                5004
&GLOBAL-DEFINE EDR_ERROR_DELAYED                          5005
&GLOBAL-DEFINE EDR_ERROR_SERVICE_CLASS_MISMATCH           5006
&GLOBAL-DEFINE EDR_ERROR_INVALID_DATE_TIME                7002

DEF TEMP-TABLE ttEDR NO-UNDO LIKE prepedr.prepedr
   FIELD ServiceFeeType AS CHAR.

DEFINE TEMP-TABLE ttCSVPos NO-UNDO
   FIELD SourceName AS INT
   FIELD FormatVersion AS INT
   FIELD RecordType AS INT
   FIELD SubscriberNumber AS INT
   FIELD EventDate AS INT
   FIELD EventTime AS INT
   FIELD NewServiceClass AS INT
   FIELD ServFeeExpDateBefore AS INT
   FIELD BalanceAfter AS INT
   FIELD Subscriberfee AS INT
   FIELD SuccessCode AS INT
   FIELD ServiceFeeType AS INT.

DEF VAR lcCsvVersion AS CHARACTER NO-UNDO. 

lcCsvVersion = "0101YD".
CREATE ttCSVPos.
ASSIGN
   ttCSVPos.SourceName       = fGetPosition(lcCsvVersion,"Source name")
   ttCSVPos.FormatVersion    = fGetPosition(lcCsvVersion,"Format version")
   ttCSVPos.RecordType       = fGetPosition(lcCsvVersion,"Record type")
   ttCSVPos.SubscriberNumber = fGetPosition(lcCsvVersion,"Subscriber number")
   ttCSVPos.EventDate        = fGetPosition(lcCsvVersion,"Event date")
   ttCSVPos.EventTime        = fGetPosition(lcCsvVersion,"Event time")
   ttCSVPos.ServFeeExpDateBefore = fGetPosition(lcCsvVersion,
                                      "Service fee exp date before")
   ttCSVPos.NewServiceClass  = fGetPosition(lcCsvVersion,"New service class")
   ttCSVPos.SuccessCode      = fGetPosition(lcCsvVersion,"Success code")
   ttCSVPos.SubscriberFee    = fGetPosition(lcCsvVersion,"Subscriber fee")
   ttCSVPos.BalanceAfter = fGetPosition(lcCsvVersion,"Balance after deduction")
   ttCSVPos.ServiceFeeType   = fGetPosition(lcCsvVersion,"Service fee type").
      

PROCEDURE pAnalyzeEDR:
   
   DEF PARAM BUFFER ttEDR FOR TEMP-TABLE ttEDR.

   DEF VAR ldeCallTS    AS DEC   NO-UNDO. 

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.CLI = ttEDR.CLI NO-ERROR.

   IF AVAIL MobSub THEN ASSIGN
      ttEDR.MsSeq = MobSub.MsSeq
      ttEDR.Custnum = MobSub.Custnum
      ttEDR.CLIType = MobSub.CLIType.
   ELSE DO:
   
      ldeCallTS = Func.Common:mMake2DT(ttEDR.Datest, ttEDR.TimeStart). 
      
      FIND FIRST msowner NO-LOCK WHERE
                 msowner.CLI       =  ttEDR.CLI AND
                 msowner.tsend    >= ldeCallTS  AND
                 msowner.tsbegin  <= ldeCallTS NO-ERROR.
      IF AVAIL msowner THEN ASSIGN
         ttEDR.MsSeq = msowner.MsSeq
         ttEDR.Custnum = msowner.Custnum
         ttEDR.CLIType = msowner.CLIType.
      ELSE DO:
         FIND FIRST msowner NO-LOCK WHERE
                    msowner.CLI = ttEDR.CLI NO-ERROR.
         
         IF NOT AVAIL msowner THEN
            ttEDR.ErrorCode = {&EDR_ERROR_UNKNOWN_MSISDN}.
         ELSE ttEDR.ErrorCode = {&EDR_ERROR_MSISDN_NOT_ACTIVE}.
         
         RETURN.
      END.
   END.
   
   IF LOOKUP(STRING(ttEDR.SuccessCode),"1,3") = 0 THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SUCCESS_CODE_VALUE}.
   ELSE IF LOOKUP(STRING(ttEDR.NewSC),"0,7,107,3,103,9,109,10,110,11,111,12,112,20,120") = 0 THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SERVICE_CLASS_VALUE}.
   ELSE IF ttEDR.SuccessCode EQ 1 AND
      LOOKUP(STRING(ttEDR.NewSC),"0,7,3,9,10,11,12,20") = 0 THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SERVICE_CLASS_INCONSISTENT}.
   ELSE IF ttEDR.SuccessCode EQ 3 AND
      LOOKUP(STRING(ttEDR.NewSC),"0,107,103,109,110,111,112,120") = 0 THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SERVICE_CLASS_INCONSISTENT}.
   ELSE IF LOOKUP(ttEDR.clitype,"TARJ6,TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") = 0 THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SUBSCRIPTION_TYPE}.
   ELSE IF (ttEDR.clitype EQ "TARJ6" AND
            ttEDR.ServiceFeeType NE "SC7") OR
           (ttEDR.clitype EQ "TARJ7" AND
            ttEDR.ServiceFeeType NE "SC3") OR
           (ttEDR.clitype EQ "TARJ9" AND
            ttEDR.ServiceFeeType NE "SC9") OR
           (ttEDR.clitype EQ "TARJ10" AND
            ttEDR.ServiceFeeType NE "SC10") OR
           (ttEDR.clitype EQ "TARJ11" AND
            ttEDR.ServiceFeeType NE "SC11") OR
           (ttEDR.clitype EQ "TARJ12" AND
            ttEDR.ServiceFeeType NE "SC12") OR
           (ttEDR.clitype EQ "TARJ13" AND
            ttEDR.ServiceFeeType NE "SC20") THEN
      ttEDR.ErrorCode = {&EDR_ERROR_SERVICE_CLASS_MISMATCH}.
   ELSE IF (ttEDR.DateST < TODAY - 1) OR
           (ttEDR.DateSt < TODAY AND
           ((ttEDR.CLIType EQ "TARJ6"  AND TIME > 1200)  OR /* 00:20 */
            (ttEDR.CLIType EQ "TARJ7"  AND TIME > 43200) OR /* 12:00 */
            (ttEDR.CLIType EQ "TARJ9"  AND TIME > 43200) OR
            (ttEDR.CLIType EQ "TARJ10" AND TIME > 43200) OR
            (ttEDR.CLIType EQ "TARJ11" AND TIME > 43200) OR
            (ttEDR.CLIType EQ "TARJ12" AND TIME > 43200) OR
            (ttEDR.CLIType EQ "TARJ13" AND TIME > 43200))) THEN
      ttEDR.ErrorCode = {&EDR_ERROR_DELAYED}.

END.

PROCEDURE pHandleEDR:
   
   DEF PARAM BUFFER ttEDR FOR TEMP-TABLE ttEDR.

   DEF VAR lcShaperConf AS CHAR NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR ldeNow AS DEC NO-UNDO. 
   DEF VAR lcBundles AS CHAR NO-UNDO. 
   DEF VAR ldaLastDay AS DATE NO-UNDO.  
   DEF VAR llActivatePromo AS LOG NO-UNDO. 

   DEF BUFFER bPrepEDR FOR prepEDR.PrepEDR.
   DEF BUFFER bActReq FOR MsRequest.
   DEF BUFFER bTermReq FOR MsRequest.
   DEF BUFFER bOrigrequest FOR MsRequest.
   DEF BUFFER bmservicelimit FOR MServiceLimit.
               
   DEF VAR ldaResetDate AS DATE NO-UNDO. 
   DEF VAR ldeSMSTime AS DEC NO-UNDO. 
   DEF VAR ldaFromDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO. 
   DEF VAR liOngStats AS CHAR NO-UNDO INIT "0,1,5,6,7,8,15,16,17,19".
   DEF VAR licount    AS INT NO-UNDO.

   licount =  NUM-ENTRIES(liOngStats).
   ldeNow  = Func.Common:mMakeTS().

   FIND FIRST Mobsub NO-LOCK WHERE
              Mobsub.msseq = ttEDR.msseq NO-ERROR.
   IF NOT AVAIL Mobsub OR Mobsub.CLIType NE ttEDR.CLIType THEN RETURN.

   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq = ttEDR.MsSeq AND
              MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              MsRequest.ActStamp < ldeNow AND
              LOOKUP(STRING(MsRequest.ReqStatus),
                     {&REQ_INACTIVE_STATUSES}) = 0 NO-ERROR.
   IF AVAIL MsRequest AND MsRequest.ReqCParam2 NE ttEDR.CLIType THEN RETURN.

   /* check if edrs have arrived to TMS in wrong order (unlikely) */
   FOR EACH bPrepEDR NO-LOCK WHERE
            bPrepEDR.MsSeq = ttEDR.MsSeq AND
            bPrepEDR.Datest = ttEDR.DateST AND
            bPrepEDR.TimeStart > ttEDR.TimeStart AND
            bPrepEDR.ErrorCode = 0:

      IF bPrepEDR.SuccessCode EQ 3 OR 
         bPrepEDR.SuccessCode EQ 1 THEN RETURN.
   END.
   
   IF ttEDR.CLIType EQ "TARJ6" THEN DO:
   
      IF ttEDR.SuccessCode EQ 1 AND
         ttEDR.ServFeeExpDateBefore > "" THEN RETURN.

      lcShaperConf = (IF ttEDR.SuccessCode EQ {&EDR_SUCCESS_CODE_OK}
                      THEN "MEGA5_PRE" ELSE "MEGA5_PRE_NB").

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq = ttEDR.MsSeq AND
                 MsRequest.ReqType = 1 AND
                 MsRequest.ReqStatus = 0 AND
                 MsRequest.ReqCparam1 = "SHAPER" AND
                 MsRequest.ReqIParam1 = 1 AND
                 MsRequest.ActStamp > Func.Common:mMakeTS() NO-ERROR.

      IF AVAIL MsRequest THEN DO:
         IF MsRequest.ReqCparam2 EQ lcShaperConf THEN RETURN.
         ELSE fReqStatus(4,"Automatic cancellation by EDR").
      END.
      
      IF lcShaperConf EQ "MEGA5_PRE_NB" AND
         TIME >= 33600 AND TIME < 36900 THEN /* 09:20-10:15 */
         ldeNow = Func.Common:mHMS2TS(TODAY,"10:15:00").
      
      liRequest = fServiceRequest(ttEDR.MsSeq,
                                  "SHAPER",
                                  1, /* activate */
                                  lcShaperConf,
                                  ldeNow,
                                  "",         /* salesman */
                                  FALSE,      /* fees */
                                  FALSE,      /* sms */          
                                  "",
                                  "5",
                                  0,
                                  FALSE,
                                  OUTPUT lcResult).

      IF liRequest = 0 THEN 
         Func.Common:mWriteMemo("Mobsub",
                          STRING(Mobsub.MsSeq),
                          MobSub.CustNum,
                          "SHAPER creation failed",
                          lcResult).

   END.
   ELSE IF LOOKUP(ttEDR.CLIType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:
   
      /* Restriction for termination and RESET provisioning */
      IF TIME >= 33600 AND TIME < 36900 THEN /* 09:20-10:15 */
         ldeNow = Func.Common:mHMS2TS(TODAY,"10:15:00").

      FIND FIRST bActReq NO-LOCK WHERE
                 bActReq.MsSeq = ttEDR.MsSeq AND
                 bActReq.ReqType = 8 AND
                 bActReq.ReqStatus = 0 AND
                 bActReq.ReqCparam3 = ttEDR.CLIType AND
                 bActReq.ActStamp > Func.Common:mMakeTS() NO-ERROR.
   
      /*
      FIND FIRST bTermReq NO-LOCK WHERE
                 bTermReq.MsSeq = ttEDR.MsSeq AND
                 bTermReq.ReqType = 9 AND
                 LOOKUP(STRING(bTermReq.ReqStatus),
                        {&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                 bTermReq.ReqCparam3 = ttEDR.CLIType NO-ERROR.
      */

      CHECK_LOOP:
      DO liLoop = 1 TO licount:
         FIND FIRST bTermReq NO-LOCK WHERE
                    bTermReq.MsSeq = ttEDR.MsSeq AND
                    bTermReq.ReqType = 9 AND
                    bTermReq.ReqStatus = INT(ENTRY(liLoop,(liOngStats))) AND
                    bTermReq.ReqCparam3 = ttEDR.CLIType NO-ERROR.
         IF AVAIL bTermReq THEN LEAVE CHECK_LOOP.
      END.
      
      IF ttEDR.SuccessCode EQ {&EDR_SUCCESS_CODE_OK} THEN DO:
         
         IF AVAIL bActReq THEN RETURN.

         IF AVAIL bTermReq AND bTermReq.ReqStatus EQ 0 THEN DO:
            FIND Msrequest NO-LOCK WHERE 
                 ROWID(Msrequest) = ROWID(bTermReq).
            fReqStatus(4,"Automatic cancellation by EDR").
            RELEASE bTermReq.
         END.
      
         lcBundles = fGetCurrentBundle(MobSub.Msseq).
         
         IF LOOKUP(ttEDR.CLIType,lcBundles) > 0 THEN DO:
            
            /* Benefit is already active, just send the renewal SMS 
               and possible counter reset*/
            FOR EACH ServiceLimit NO-LOCK WHERE
                      ServiceLimit.GroupCode EQ ttEDR.CLIType,
                FIRST MServiceLimit NO-LOCK WHERE
                      MServiceLimit.MsSeq = MobSub.MsSeq AND
                      MServiceLimit.DialType = ServiceLimit.DialType AND
                      MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                      MServiceLimit.EndTs >= ldeNow AND
                      MServiceLimit.FromTS <= ldeNow:
         
               llActivatePromo = FALSE.
               IF ttEDR.CLIType = "TARJ7" AND
                  ttEDR.DateSt < 3/1/2015 AND 
                  MServiceLimit.InclAmt EQ 600 THEN
                  FOR EACH Order NO-LOCK WHERE
                           Order.MsSeq = MobSub.MsSeq AND
                           Order.CLItype = ttEDR.CLIType AND
                           Order.OrderType = 2 AND
                    LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0,
                     FIRST OrderAction NO-LOCK WHERE
                           OrderAction.Brand = Syst.Var:gcBrand AND
                           OrderAction.OrderId = Order.OrderId AND
                           OrderAction.ItemType = "Promotion" AND
                           OrderAction.ItemKey  = ttEDR.CLIType,
                     FIRST bOrigrequest NO-LOCK WHERE
                           bOrigrequest.MsSeq = Order.MsSeq AND
                           bOrigrequest.ReqType = 46 AND
                           bOrigrequest.ReqStatus = 2 AND
                           bOrigrequest.ReqIParam1 = Order.OrderId:

                     IF ttEDR.DateSt >= 2/1/2015 AND 
                        CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                                       MsRequest.MsSeq = MobSub.MsSeq AND
                                       MsRequest.ReqType = 1 AND
                                       MsRequest.ReqCParam1 EQ "SHAPER" AND
                                       MsRequest.ReqCParam2 EQ ttEDR.CLIType + "_PROMO")
                     THEN llActivatePromo = FALSE.
                     ELSE ASSIGN
                        llActivatePromo = TRUE
                        liRequest = 0. /* to check if _RESET request was created */

                     LEAVE.
                  END.

               IF MONTH(ttEDR.DateST) EQ 2 AND
                  ttEDR.DateST EQ Func.Common:mLastDayOfMonth(ttEDR.DateST) AND
                  ttEDR.CLIType NE "TARJ13" THEN DO:
               
                  Func.Common:mSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).

                  IF DAY(ldaFromdate) EQ 29 OR DAY(ldaFromdate) EQ 30 THEN DO:
                     liRequest = fServiceRequest(MobSub.MsSeq,
                                                 "SHAPER",
                                                 1, /* activate */
                                                 ttEDR.CLIType + "_RESET",
                                                 ldeNow,
                                                 "",         /* salesman */
                                                 FALSE,      /* fees */
                                                 FALSE,      /* sms */          
                                                 "",
                                                 {&REQUEST_SOURCE_SCRIPT},
                                                 0,
                                                 FALSE,
                                                 OUTPUT lcResult).
                  
                     IF liRequest = 0 THEN  
                        Func.Common:mWriteMemo("Mobsub",
                                         STRING(Mobsub.MsSeq),
                                         MobSub.CustNum,
                                         ttEDR.CLIType + " reset failed",
                                         lcResult).
                  END.
               END.

               /* YPR-2200 - Reset Voice Package during renewal */
               IF ((ttEDR.CLIType EQ "TARJ9"  AND ttEDR.ServiceFeeType = "SC9" ) OR
                   (ttEDR.CLIType EQ "TARJ10" AND ttEDR.ServiceFeeType = "SC10") OR
                   (ttEDR.CLIType EQ "TARJ11" AND ttEDR.ServiceFeeType = "SC11") OR
                   (ttEDR.CLIType EQ "TARJ12" AND ttEDR.ServiceFeeType = "SC12")) THEN 
               DO:
                  liRequest = fServiceRequest(MobSub.MsSeq,
                                              "TEMPLATE",
                                              1,
                                              "LADEL1_PRE_PLUS_RESET",
                                              ldeNow,
                                              "",
                                              FALSE, /* fees */
                                              FALSE, /* sms */
                                              "",
                                              {&REQUEST_SOURCE_SCRIPT},
                                              0, /* father request */
                                              FALSE,
                                              OUTPUT lcResult).
                  IF liRequest = 0 THEN
                     Func.Common:mWriteMemo("MobSub",
                                      STRING(MobSub.MsSeq),
                                      MobSub.CustNum,
                                      "PREP_VOICE",
                                      "PREP_VOICE deactivation request failed; " +
                                      lcResult).
               END.

               /* YDR-2625 - Reset Data Package during renewal */
               IF ((ttEDR.CLIType EQ "TARJ7"  AND ttEDR.ServiceFeeType = "SC3" ) OR
                   (ttEDR.CLIType EQ "TARJ9"  AND ttEDR.ServiceFeeType = "SC9" ) OR
                   (ttEDR.CLIType EQ "TARJ10" AND ttEDR.ServiceFeeType = "SC10") OR
                   (ttEDR.CLIType EQ "TARJ11" AND ttEDR.ServiceFeeType = "SC11") OR
                   (ttEDR.CLIType EQ "TARJ12" AND ttEDR.ServiceFeeType = "SC12")) THEN
               DO:
                  liRequest = fServiceRequest(MobSub.MsSeq,
                                              "TEMPLATE",
                                              1,
                                              "DATA_RESET",
                                              ldeNow,
                                              "",
                                              FALSE, /* fees */
                                              FALSE, /* sms */
                                              "",
                                              {&REQUEST_SOURCE_SCRIPT},
                                              0, /* father request */
                                              FALSE,
                                              OUTPUT lcResult).
                  IF liRequest = 0 THEN
                     Func.Common:mWriteMemo("MobSub",
                                      STRING(MobSub.MsSeq),
                                      MobSub.CustNum,
                                      "PREP_DATA",
                                      "PREPAID DATA reset request failed; " +
                                      lcResult).
               END.

               IF llActivatePromo THEN DO:

                  FIND FIRST bOrigrequest NO-LOCK where
                             bOrigrequest.msseq = MobSub.msseq and
                             bOrigrequest.reqtype = 8 and
                             bOrigrequest.reqstatus = 2 and
                             bOrigrequest.reqcparam3 = ttEDR.CLIType and
                             bOrigrequest.actstamp = MServiceLimit.fromts no-error.

                  IF NOT AVAIL bOrigrequest then do:
                     Func.Common:mWriteMemo("Mobsub",
                                      STRING(Mobsub.MsSeq),
                                      MobSub.CustNum,
                                      ttEDR.CLIType + " renewal promotion failed",
                                      lcResult).
                  END.
                  ELSE DO:
   
                     liRequest = fServiceRequest(MobSub.MsSeq,
                                                 "SHAPER",
                                                 1, /* activate */
                                                 ttEDR.CLIType + "_PROMO",
                                                 (IF liRequest EQ 0
                                                  THEN ldeNow
                                                  ELSE Func.Common:mSecOffSet(ldeNow,60)),
                                                 "",         /* salesman */
                                                 FALSE,      /* fees */
                                                 FALSE,      /* sms */
                                                 "",
                                                 {&REQUEST_SOURCE_SCRIPT},
                                                 bOrigrequest.msrequest,
                                                 FALSE,
                                                 OUTPUT lcResult).
     
                     if liRequest eq 0 then
                        Func.Common:mWriteMemo("Mobsub",
                                         STRING(Mobsub.MsSeq),
                                         MobSub.CustNum,
                                         ttEDR.CLIType + " renewal promotion request failed",
                                         lcResult).
                     ELSE DO:
                        FIND FIRST bmservicelimit EXCLUSIVE-LOCK where
                                   rowid(bmservicelimit) = rowid(MServiceLimit).
                        assign
                           bmservicelimit.inclamt = 1228.
                        
                        release bmservicelimit.
                     end.
                  END. 
               END.

               /* YDA-650 Need to send SHAPER until 15.10.2015 after that
                  it will be correct for also for old subscriptions */
               IF ttEDR.CLIType EQ "TARJ7" AND
                  ttEDR.DateST >= 9/15/15 AND 
                  MServiceLimit.inclamt = 600 THEN DO:
                  
                  FIND FIRST bOrigrequest NO-LOCK where
                             bOrigrequest.msseq = MobSub.msseq and
                             bOrigrequest.reqtype = 8 and
                             bOrigrequest.reqstatus = 2 and
                             bOrigrequest.reqcparam3 = ttEDR.CLIType and
                             bOrigrequest.actstamp = MServiceLimit.fromts no-error.
                  IF AVAIL bOrigrequest then do:

                     liRequest = fServiceRequest(MobSub.MsSeq,
                                                 "SHAPER",
                                                 1, /* activate */
                                                 ttEDR.CLIType,
                                                 (IF liRequest EQ 0
                                                  THEN ldeNow
                                                  ELSE Func.Common:mSecOffSet(ldeNow,60)),
                                                 "",         /* salesman */
                                                 FALSE,      /* fees */
                                                 FALSE,      /* sms */
                                                 "",
                                                 {&REQUEST_SOURCE_SCRIPT},
                                                 bOrigrequest.msrequest,
                                                 FALSE,
                                                 OUTPUT lcResult).

                     if liRequest eq 0 then
                        Func.Common:mWriteMemo("Mobsub",
                                         STRING(Mobsub.MsSeq),
                                         MobSub.CustNum,
                                         ttEDR.CLIType + " renewal new tariff request failed",
                                         lcResult).
                     else do:
                        FIND FIRST bmservicelimit EXCLUSIVE-LOCK where
                                   rowid(bmservicelimit) = rowid(MServiceLimit).
                        IF AVAIL bmservicelimit THEN DO:
                           assign
                              bmservicelimit.inclamt = 650.

                           release bmservicelimit. 
                        END.
                     end.
                  end.
               END.
               
               LEAVE.
            END.
            

            FIND Customer NO-LOCK WHERE
                 Customer.custnum = MobSub.Custnum NO-ERROR.
            IF NOT AVAIL Customer THEN RETURN.

            lcSMSText = fGetSMSTxt(ttEDR.CLIType + "RenewalOk", 
                                   TODAY,
                                   Customer.Language,
                                   OUTPUT ldeSMSTime).

            IF lcSMSText > "" THEN DO:

               FOR EACH ServiceLimit NO-LOCK WHERE
                         ServiceLimit.GroupCode EQ ttEDR.CLIType,
                   FIRST MServiceLimit NO-LOCK WHERE
                         MServiceLimit.MsSeq = MobSub.MsSeq AND
                         MServiceLimit.DialType = ServiceLimit.DialType AND
                         MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                         MServiceLimit.EndTs >= ldeNow AND
                         MServiceLimit.FromTS <= ldeNow:

                  Func.Common:mSplitTS(MServiceLimit.FromTS,
                           OUTPUT ldaFromdate,OUTPUT liTime).

                  ldaLastDay = ADD-INTERVAL(ttEDR.DateSt,1,"months").
                  ldaLastDay = Func.Common:mLastDayOfMonth(ldaLastDay).
                  
                  IF DAY(ldaFromDate) < DAY(ldaLastDay)
                  THEN ldaResetDate = DATE(MONTH(ldaLastDay),
                                                DAY(ldaFromDate),
                                                YEAR(ldaLastDay)).
                  ELSE ldaResetDate = ldaLastDay.

                  lcSMSText = REPLACE(lcSMSText,"#DATE",
                              STRING(DAY(ldaResetDate),"99") + "/" +
                              STRING(MONTH(ldaResetDate),"99")).
                  
                  fMakeSchedSMS2(Mobsub.CustNum,
                                 Mobsub.CLI,
                                 {&SMSTYPE_CONTRACT_ACTIVATION},
                                 lcSMSText,
                                 ldeSMSTime,
                                 "22622",
                                 "").
                   LEAVE.
               END.
            END.
         END.
         ELSE DO:

            IF ttEDR.DateSt EQ TODAY - 1 THEN
               ldeNow = Func.Common:mHMS2TS(TODAY - 1,"23:59:59").
            ELSE ldeNow = Func.Common:mMakeTS().

            /* Add 1 second to handle termination and activation 
               request with the same activation stamp */
            IF AVAIL bTermReq AND
                     bTermReq.Actstamp EQ ldeNow THEN 
               ldeNow = Func.Common:mSecOffSet(ldeNow,1).

            liRequest = fPCActionRequest(MobSub.MsSeq,
                                         ttEDR.CLIType,
                                         "act",
                                         ldeNow,
                                         TRUE,    /* fees */
                                         {&REQUEST_SOURCE_SCRIPT},
                                         "",   /* creator */
                                         0,    /* no father request */
                                         FALSE,
                                         "",
                                         0,
                                         0,
                                         "",
                                         OUTPUT lcResult). 
            IF liRequest = 0 THEN  
               Func.Common:mWriteMemo("Mobsub",
                                STRING(Mobsub.MsSeq),
                                MobSub.CustNum,
                                ttEDR.CLIType + " activation failed",
                                lcResult).
            ELSE IF AVAIL bTermReq THEN DO:
               FIND Msrequest EXCLUSIVE-LOCK WHERE
                    Msrequest.Msrequest = liRequest NO-ERROR.
               ASSIGN
                  Msrequest.ReqIParam2 = bTermReq.Msrequest.
                RELEASE Msrequest.

            END.
         END.
      END.
      ELSE DO:
         
         IF AVAIL bTermReq THEN RETURN.

         IF AVAIL bActReq THEN DO:
            FIND Msrequest NO-LOCK WHERE 
                 ROWID(Msrequest) = ROWID(bActReq).
            fReqStatus(4,"Automatic cancellation by EDR").
            RETURN.
         END.
         
         lcBundles = fGetCurrentBundle(MobSub.Msseq).
         
         IF LOOKUP(ttEDR.CLIType,lcBundles) = 0 THEN RETURN.

         liRequest = fPCActionRequest(MobSub.MsSeq,
                                      ttEDR.CLIType,
                                      "term",
                                      ldeNow,
                                      TRUE,    /* fees */
                                      {&REQUEST_SOURCE_SCRIPT},
                                      "",   /* creator */
                                      0,    /* no father request */
                                      FALSE,
                                      "",
                                      0,
                                      0,
                                      "",
                                      OUTPUT lcResult). 
         IF liRequest = 0 THEN  
            Func.Common:mWriteMemo("Mobsub",
                             STRING(Mobsub.MsSeq),
                             MobSub.CustNum,
                             ttEDR.CLIType + " termination failed",
                             lcResult).
      END. 
   END.

END PROCEDURE.
