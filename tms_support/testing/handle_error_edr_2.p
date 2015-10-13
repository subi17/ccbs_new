/* ----------------------------------------------------------------------
  MODULE .......: handle_error_edr.P
  TASK .........: Handle error EDRs 
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 28.11.2013
  Version ......: xfera 
  ---------------------------------------------------------------------- */
{commpaa.i}
gcBrand = "1".
Katun = "Qvantel".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{cparam2.i}
{fbundle.i}
/*
DEF INPUT PARAMETER icCLI        AS CHAR NO-UNDO.
DEF INPUT PARAMETER idCallDate   AS DATE NO-UNDO.
*/
&SCOPED-DEFINE EDR_SUCCESS_CODE_OK 1 
&SCOPED-DEFINE EDR_SUCCESS_CODE_ERROR 3 

DEF VAR ldeCallTS    AS DEC   NO-UNDO. 
   
FOR EACH PrepEDR EXCLUSIVE-LOCK WHERE
         PrepEDR.DateSt    = 12/1/2013 AND
         PrepEDR.ErrorCode = 1001:

   ldeCallTS = fMake2Dt(PrepEDR.Datest,PrepEDR.TimeStart). 
      
   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.CLI = PrepEDR.CLI NO-ERROR.
   IF NOT AVAIL MobSub THEN next.


   ASSIGN
      PrepEDR.MsSeq   = MobSub.MsSeq
      PrepEDR.Custnum = MobSub.Custnum
      PrepEDR.CLIType = MobSub.CLIType. 

   disp MobSub.cli
   prepedr.successcode
   prepedr.SubscriberFee
   prepedr.readints
   prepedr.ServFeeExpDateBefore.

   if prepedr.successcode eq 1 then
   RUN pHandleEDR.

   PrepEDR.ErrorCode = 0.
   RELEASE PrepEDR.    
END.


PROCEDURE pHandleEDR:

   DEF VAR lcShaperConf       AS CHAR NO-UNDO. 
   DEF VAR lcResult           AS CHAR NO-UNDO. 
   DEF VAR liRequest          AS INT  NO-UNDO. 
   DEF VAR ldeNow             AS DEC  NO-UNDO. 
   DEF VAR lcBundles          AS CHAR NO-UNDO. 
   DEF VAR ldaTARJ7ResetDate  AS DATE NO-UNDO. 
   DEF VAR ldeSMSTime         AS DEC  NO-UNDO. 
   DEF VAR ldaFromDate        AS DATE NO-UNDO. 
   DEF VAR liTime             AS INT  NO-UNDO.
   

   DEF BUFFER bPrepEDR FOR prepEDR.PrepEDR.
   DEF BUFFER bActReq  FOR MsRequest.
   DEF BUFFER bTermReq FOR MsRequest.
               
   ldeNow = fMakeTS().
         
   FIND FIRST Mobsub NO-LOCK WHERE
              Mobsub.MsSeq = PrepEDR.MsSeq NO-ERROR.
   IF NOT AVAIL Mobsub OR Mobsub.CLIType NE PrepEDR.CLIType THEN RETURN.

   /* check if edrs have arrived to TMS in wrong order (unlikely) */
   FOR EACH bPrepEDR NO-LOCK USE-INDEX MsSeq WHERE
            bPrepEDR.MsSeq = PrepEDR.MsSeq AND
            bPrepEDR.Datest = PrepEDR.DateST AND
            bPrepEDR.TimeStart > PrepEDR.TimeStart AND
            bPrepEDR.ErrorCode = 0:

      IF bPrepEDR.SuccessCode EQ 3 THEN RETURN.
      ELSE IF bPrepEDR.SuccessCode EQ 1 AND
         bPrepEDR.ServFeeExpDateBefore EQ "" THEN RETURN.
   END.
   
   IF (PrepEDR.CLIType EQ "TARJ7" OR
      (PrepEDR.CLIType EQ "TARJ6" AND lcShaperConf EQ "MEGA5_PRE_NB")) AND
      TIME >= 33600 AND TIME < 36900 THEN /* 09:20-10:15 */
      ldeNow = fHMS2TS(TODAY,"10:15:00").
   
   IF PrepEDR.CLIType EQ "TARJ6" THEN DO:
   
      IF PrepEDR.SuccessCode EQ 1 AND
         PrepEDR.ServFeeExpDateBefore > "" THEN RETURN.

      lcShaperConf = (IF PrepEDR.SuccessCode EQ {&EDR_SUCCESS_CODE_OK}
                      THEN "MEGA5_PRE" ELSE "MEGA5_PRE_NB").

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq = PrepEDR.MsSeq AND
                 MsRequest.ReqType = 1 AND
                 MsRequest.ReqStatus = 0 AND
                 MsRequest.ReqCparam1 = "SHAPER" AND
                 MsRequest.ReqIParam1 = 1 AND
                 MsRequest.ActStamp > fMakeTS() NO-ERROR.

      IF AVAIL MsRequest THEN DO:
         IF MsRequest.ReqCparam2 EQ lcShaperConf THEN RETURN.
         ELSE fReqStatus(4,"Automatic cancellation by EDR").
      END.
      
      liRequest = fServiceRequest(PrepEDR.MsSeq,
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
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "Mobsub",
                          STRING(Mobsub.MsSeq),
                          MobSub.CustNum,
                          "SHAPER creation failed",
                          lcResult).

   END.
   ELSE IF PrepEDR.CLIType EQ "TARJ7" THEN DO:
      
      FIND FIRST bActReq NO-LOCK WHERE
                 bActReq.MsSeq = PrepEDR.MsSeq AND
                 bActReq.ReqType = 8 AND
                 bActReq.ReqStatus = 0 AND
                 bActReq.ReqCparam3 = "TARJ7" AND
                 bActReq.ActStamp > fMakeTS() NO-ERROR.
      
      FIND FIRST bTermReq NO-LOCK WHERE
                 bTermReq.MsSeq = PrepEDR.MsSeq AND
                 bTermReq.ReqType = 9 AND
                 bTermReq.ReqStatus = 0 AND
                 bTermReq.ReqCparam3 = "TARJ7" AND
                 bTermReq.ActStamp > fMakeTS() NO-ERROR.

      lcBundles = fGetCurrentBundle(MobSub.Msseq).

      IF PrepEDR.SuccessCode EQ {&EDR_SUCCESS_CODE_OK} THEN DO:
         
         IF AVAIL bActReq THEN RETURN.

         IF AVAIL bTermReq THEN DO:
            FIND Msrequest NO-LOCK WHERE 
                 ROWID(Msrequest) = ROWID(bTermReq).
            fReqStatus(4,"Automatic cancellation by EDR").
            RETURN.
         END.
         
         IF LOOKUP("TARJ7",lcBundles) > 0 THEN DO:
            
            /* Benefit is already active, just send the renewal SMS 
               and possible counter reset*/
            IF PrepEDR.ServFeeExpDateBefore > "" THEN DO:
   
               IF MONTH(PrepEDR.DateST) EQ 2 AND
                  PrepEDR.DateST EQ fLastDayOfMonth(PrepEDR.DateST) THEN
               FOR FIRST ServiceLimit NO-LOCK WHERE
                         ServiceLimit.GroupCode EQ "TARJ7",
                   FIRST MServiceLimit NO-LOCK WHERE
                         MServiceLimit.MsSeq = MobSub.MsSeq AND
                         MServiceLimit.DialType = ServiceLimit.DialType AND
                         MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                         MServiceLimit.EndTs >= ldeNow AND
                         MServiceLimit.FromTS <= ldeNow:
                  
                  fSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).
                  
                  IF DAY(ldaFromdate) EQ 29 OR DAY(ldaFromdate) EQ 30 THEN DO:
                     liRequest = fServiceRequest(MobSub.MsSeq,
                                                 "SHAPER",
                                                 1, /* activate */
                                                 "TARJ7_RESET",
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
                        DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                         "Mobsub",
                                         STRING(Mobsub.MsSeq),
                                         MobSub.CustNum,
                                         "TARJ7 reset failed",
                                         lcResult).
                  END.
               END.
                 
               FIND Customer NO-LOCK WHERE
                    Customer.custnum = MobSub.Custnum NO-ERROR.
               IF NOT AVAIL Customer THEN RETURN.

               lcSMSText = fGetSMSTxt("TARJ7Act", 
                                      TODAY,
                                      Customer.Language,
                                      OUTPUT ldeSMSTime).

               IF lcSMSText > "" THEN DO:
          
                  ldaTARJ7ResetDate = ADD-INTERVAL(PrepEDR.DateSt,1,"months").

                  lcSMSText = REPLACE(lcSMSText,"#DATE",
                              STRING(DAY(ldaTARJ7ResetDate),"99") + "/" +
                              STRING(MONTH(ldaTARJ7ResetDate),"99")).
                  
                  fMakeSchedSMS2(Mobsub.CustNum,
                                 Mobsub.CLI,
                                 {&SMSTYPE_CONTRACT_ACTIVATION},
                                 lcSMSText,
                                 ldeSMSTime,
                                 "22622",
                                 "").
               END.
            END.
            RETURN.
         END.

         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsSeq = PrepEDR.MsSeq AND
                    MsRequest.ReqType = 9 AND
                    MsRequest.ReqStatus = 0 AND
                    MsRequest.ReqCparam3 = "TARJ7" AND
                    MsRequest.ActStamp > fMakeTS() NO-ERROR.
         IF AVAIL MsRequest THEN fReqStatus(4,"Automatic cancellation by EDR").
         ELSE DO:
         
            IF AVAIL bActReq THEN RETURN.

            IF AVAIL bTermReq THEN DO:
               FIND Msrequest NO-LOCK WHERE 
                    ROWID(Msrequest) = ROWID(bTermReq).
               fReqStatus(4,"Automatic cancellation by EDR").
               RETURN.
            END.

            fSplitTS(PrepEDR.ReadInTS,OUTPUT ldaFromdate,OUTPUT liTime).

            IF PrepEDR.DateSt EQ (ldaFromdate - 1) THEN
               ldeNow = fHMS2TS(PrepEDR.DateSt,"23:59:59").
            ELSE ldeNow = PrepEDR.ReadInTS.

            liRequest = fPCActionRequest(MobSub.MsSeq,
                                         "TARJ7",
                                         "act",
                                         ldeNow,
                                         TRUE,    /* fees */
                                         {&REQUEST_SOURCE_SCRIPT},
                                         "",   /* creator */
                                         0,    /* no father request */
                                         FALSE,
                                         "",
                                         OUTPUT lcResult). 
            IF liRequest = 0 THEN  
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "Mobsub",
                                STRING(Mobsub.MsSeq),
                                MobSub.CustNum,
                                "TARJ7 activation failed",
                                lcResult).
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
         
         IF LOOKUP("TARJ7",lcBundles) = 0 THEN RETURN.

         liRequest = fPCActionRequest(MobSub.MsSeq,
                                      "TARJ7",
                                      "term",
                                      PrepEDR.ReadInTS,
                                      TRUE,    /* fees */
                                      {&REQUEST_SOURCE_SCRIPT},
                                      "",   /* creator */
                                      0,    /* no father request */
                                      FALSE,
                                      "",
                                      OUTPUT lcResult). 
         IF liRequest = 0 THEN  
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "Mobsub",
                             STRING(Mobsub.MsSeq),
                             MobSub.CustNum,
                             "TARJ7 termination failed",
                             lcResult).
      END. 
   END.

END PROCEDURE.
