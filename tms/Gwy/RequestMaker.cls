ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}
{Syst/common_definitions.i}

&SCOPED-DEFINE ENVIRONMENT "MASMOVIL"
&SCOPED-DEFINE JSON_ERROR  "READ-JSON encountered an error while parsing the JSON Document: FATAL ERROR: file 'MEMPTR',"

CLASS Gwy.RequestMaker:

   DEF VAR gcBrand     AS CHAR NO-UNDO INITIAL "1".
   DEF VAR liRequestId AS INT  NO-UNDO INITIAL  0.
   DEF VAR llIsReqSucc AS LOG  NO-UNDO INITIAL  NO.
   DEF VAR lcCorrelationId  AS CHAR NO-UNDO.
   DEF VAR lcReplyTo   AS CHAR NO-UNDO.
   DEF VAR lcSource   AS CHAR NO-UNDO.
   
   CONSTRUCTOR PUBLIC RequestMaker ():
   END CONSTRUCTOR.    

   CONSTRUCTOR PUBLIC RequestMaker (iCorrelationID AS CHAR,
                                    iReplyTo AS CHAR,
                                    iSource AS CHAR):
      ASSIGN
         lcCorrelationId = iCorrelationID
         lcReplyTo       = iReplyTo
         lcSource        = iSource.
   END CONSTRUCTOR.
   
   METHOD PUBLIC CHAR get_reply_to():
      RETURN lcReplyTo.
   END METHOD.

   &IF {&ENVIRONMENT} <> "MASMOVIL" &THEN 
   METHOD PUBLIC CHAR get_external_id(iType AS CHAR, iInternalID AS CHAR):

      FIND FIRST ExternalID NO-LOCK
           WHERE ExternalID.ExtSource EQ lcSource 
             AND ExternalID.Type EQ iType
             AND ExternalID.InternalID EQ iInternalID NO-ERROR.
      IF AVAILABLE ExternalID THEN
         RETURN ExternalID.ExternalID.
      ELSE
         RETURN "".
   END METHOD.

   METHOD PUBLIC CHAR get_internal_id(iType AS CHAR, iExternalID AS CHAR):

      FIND FIRST ExternalID NO-LOCK
           WHERE ExternalID.ExtSource EQ lcSource
             AND ExternalID.Type EQ iType
             AND ExternalID.ExternalID EQ iExternalID NO-ERROR.
      IF AVAILABLE ExternalID THEN
         RETURN ExternalID.InternalID.
      ELSE
         RETURN "".
   END METHOD.

   METHOD PUBLIC CHAR get_tmscodes_bssapi_internalid
      (icTableName  AS CHAR,
       icFieldName  AS CHAR,
       icExternalID AS CHAR):
 
      DEF VAR lcInternalID AS CHAR NO-UNDO.
   
      lcInternalID = "".
      FOR FIRST ExternalID NO-LOCK WHERE
                ExternalID.Type = "TMSCodes" AND
                ExternalID.ExternalID = icExternalID AND
                ExternalID.ExtSource = "BSSAPI" AND
                ExternalID.InternalID BEGINS 
                   SUBSTITUTE("&1|&2|",
                              icTableName,
                              icFieldName):
         lcInternalID = ENTRY(3,ExternalID.InternalID,"|").      
      END.

      RETURN lcInternalID.
   END METHOD.   
      
   METHOD PUBLIC CHAR get_target_of_main_contract(iMainContractId AS INT):
      DEF VAR lTarget AS CHAR NO-UNDO.

      /*possible return value are "MobSub" or "Customer"*/
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.MainContractID = iMainContractId:
          lTarget = FixedFee.HostTable.
      END.
      IF lTarget = "" THEN
      FOR FIRST SingleFee NO-LOCK WHERE
                SingleFee.MainContractID = iMainContractId:
          lTarget = SingleFee.HostTable.
      END.
      RETURN lTarget.
   END METHOD.

   METHOD PUBLIC CHAR get_sub_ctr_extid(iMainContract AS CHAR):
      /* if needed subcontract external id using main contract external id
         Note: this is needed for only subscriptions since for other product
         like device and service products we get the sub contract ids already
         from ProQ*/
      DEF VAR lIntMainCtrId AS INT NO-UNDO.

      lIntMainCtrId = INT(get_internal_id("MainContract", iMainContract)).
      IF lIntMainCtrId GT 0 THEN DO:
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.MainContractID = INT(lIntMainCtrId) NO-ERROR.
         IF AVAIL MObSub THEN
            RETURN get_external_id(STRING(MobSub.MsSeq), "MobSub").
         ELSE DO:
            /*check TermMobSub. this could happen in some exceptional case*/
            FIND FIRST TermMobSub NO-LOCK WHERE
                       TermMobSub.MainContractID = INT(lIntMainCtrId) NO-ERROR.
            IF AVAIL TermMobSub THEN
               RETURN get_external_id(STRING(TermMobSub.MsSeq), "MobSub").
            ELSE RETURN "".
         END.
      END.
   END METHOD.

   METHOD PUBLIC INT make_request(iType AS INT, iActStamp AS DEC):
      
      CREATE MSRequest.
      ASSIGN
         MSRequest.Brand = gcBrand
         MSRequest.MsRequest = NEXT-VALUE(MsRequest)
         MSRequest.ReqType = iType
         MSRequest.ReqStatus = ?  /* mark to 0 after all params have been added etc. */
         MSRequest.CreStamp = {&NOW_TS}
         MSRequest.UserCode = "CM-RBS-IF" 
         MSRequest.ActStamp = iActStamp
         MSRequest.ReqSource = lcSource.
      IF iType = {&REQTYPE_PERIODICAL_CONTRACT_ACTIVATION} OR
         iType = {&REQTYPE_PERIODICAL_CONTRACT_TERMINATION} THEN DO:
         IF iType = {&REQTYPE_PERIODICAL_CONTRACT_TERMINATION} THEN
            MsRequest.ReqCParam2 = "Term".
         ELSE MsRequest.ReqCParam2 = "Act".
         ASSIGN   
            MsRequest.ReqDtParam1 = ?
            MsRequest.ReqDtParam2 = ?.
      END.
      
      liRequestId = MSRequest.MsRequest.
      llIsReqSucc = YES.
      
      IF lcCorrelationId > "" THEN DO:

         CREATE ExternalID.
         ASSIGN 
            ExternalID.Type       = "MsRequest"
            ExternalID.ExtSource  = lcSource
            ExternalID.InternalID = STRING(MsRequest.MsRequest)
            ExternalID.ExternalID = lcCorrelationId.
         
         IF lcReplyTo > "" THEN DO:
            make_requestparam("ReplyTo", lcReplyTo).
            ASSIGN
               /* assumed to have one internal request
                  per one external request */
               MsRequest.ResponseStatus = {&REQUEST_RESPONSE_CANSEND}
               lcCorrelationId = ""
               lcReplyTo = "".
         END.
      END.
      
      RETURN liRequestId.
   END METHOD.

   METHOD PUBLIC INT make_request(iType AS INT, iExternalId AS CHAR,
                                   iRequestParamName AS CHAR, iActStamp AS DEC):
      make_request(iType, iActStamp).
      make_requestparam(iRequestParamName, iExternalId).
      RETURN liRequestId.
   END METHOD.

   METHOD PUBLIC VOID make_requestparam(iName AS CHAR, iValue AS CHAR):
      IF NOT llIsReqSucc THEN RETURN. 
      CREATE MsRequestParam.
      ASSIGN
         MsRequestParam.MsRequest = liRequestId
         MsRequestParam.ParamName = iName
         /* assumed that external ID parameters don't contain semicolons */
         MsRequestParam.ParamType = "List" WHEN INDEX(iValue, ";") GT 0
         MsRequestParam.CharValue = iValue.
   END METHOD.

   METHOD PUBLIC VOID make_requestparam(iName AS CHAR, iValue AS DEC):
      IF NOT llIsReqSucc THEN RETURN.
      CREATE MsRequestParam.
      ASSIGN
         MsRequestParam.MsRequest = liRequestId
         MsRequestParam.ParamName = iName
         MsRequestParam.NumericValue = iValue.
   END METHOD.
   
   METHOD PUBLIC VOID make_requestparam(iName AS CHAR, iDate AS DATE):
      IF NOT llIsReqSucc THEN RETURN.
      CREATE MsRequestParam.
      ASSIGN
         MsRequestParam.MsRequest = liRequestId
         MsRequestParam.ParamName = iName
         MsRequestParam.DateValue = iDate.
   END METHOD.
   
   METHOD PUBLIC VOID make_requestparam(iName AS CHAR,
                                        icValue AS CHAR,
                                        icParamSet AS CHAR):
      IF NOT llIsReqSucc THEN RETURN. 
      CREATE MsRequestParam.
      ASSIGN
         MsRequestParam.MsRequest = liRequestId
         MsRequestParam.ParamName = iName
         /* assumed that external ID parameters don't contain semicolons */
         MsRequestParam.ParamType = "List" WHEN INDEX(icValue, ";") GT 0
         MsRequestParam.CharValue = icValue
         MsRequestParam.ParamSet  = icParamSet.
   END METHOD.

   METHOD PUBLIC LOG valid_servfee_code(iServiceID AS CHAR, iServiceType AS CHAR):
      IF iServiceType = "" OR iServiceType = ? THEN RETURN TRUE.
      IF CAN-FIND(FIRST ServFee NO-LOCK WHERE
                        ServFee.Brand    EQ gcBrand AND
                        ServFee.ServType EQ iServiceType AND
                        ServFee.ServKey  EQ iServiceID) THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END METHOD.


   METHOD PUBLIC INT create_charge_request(iServiceID AS CHAR,
                                             iServiceType AS CHAR,
                                             iExternalId AS CHAR,
                                             iReqParamId AS CHAR,
                                             iMessage AS CHAR,
                                             iActTimeStamp AS DEC,
                                             iParentReqID AS INT):
      
      make_request({&REQTYPE_CHARGE_AND_COMPENSATION}, iActTimeStamp).
      IF llIsReqSucc THEN 
         ASSIGN
            MSRequest.ReqCParam1 = iServiceID WHEN iServiceType = ""
            MSRequest.ReqCParam2 = iServiceID WHEN iServiceType > ""
            MSRequest.ReqCParam3 = iServiceType 
            MSRequest.ReqIParam2 = iParentReqID.
      make_requestparam(iReqParamId, iExternalId).
      create_log(iMessage + " charge: MsRequest=" + STRING(MSRequest.MSRequest),
                 "DEBUG").
      RETURN liRequestId.
   END METHOD.

   METHOD PUBLIC VOID release_resources():

      IF AVAILABLE MsRequest AND MsRequest.ReqStatus = ? THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.ReqStatus = 0.
      END.
      
      RELEASE MSRequest.
      RELEASE MSRequestParam.
   END METHOD.
   &ENDIF

   METHOD PUBLIC DATE make_date(INPUT iTimeStamp AS DEC):

      DEF VAR lyear  AS INT  NO-UNDO.
      DEF VAR lmonth AS INT  NO-UNDO.
      DEF VAR lday   AS INT  NO-UNDO.
      DEF VAR ldate  AS DATE NO-UNDO INITIAL ?.
      DEF VAR lCharDate AS CHAR NO-UNDO.

      ASSIGN
         lCharDate = SUBSTR(STRING(iTimeStamp,"99999999.99999"),1,8)
         lyear = INT(SUBSTR(lCharDate,1,4))
         lmonth = INT(SUBSTR(lCharDate,5,2))
         lday = INT(SUBSTR(lCharDate,7,2))
         ldate = DATE(lmonth, lday, lyear)
      NO-ERROR.
      RETURN ldate.
   END METHOD.

   METHOD PUBLIC DEC make_timestamp (iUTCStamp AS CHAR):
   /* activation time comes in format YYYY-MM-DDTHH:MM:SS+Z
      for example 2011-08-19 18:25:04+03:00
      convert it to internal timestamp format YYYYMMDD.sssss*/
      DEF VAR lTimeStamp AS DATETIME-TZ NO-UNDO.
      DEF VAR lActivationStamp AS DEC NO-UNDO FORMAT "99999999.99999".
      DEF VAR lDateFormat AS CHAR NO-UNDO.
      DEF VAR yy AS INT NO-UNDO.
      DEF VAR mm AS INT NO-UNDO.
      DEF VAR dd AS INT NO-UNDO.
      DEF VAR lClock AS CHAR NO-UNDO.
      DEF VAR lClockTime AS INT NO-UNDO.

      lDateFormat = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
      lTimeStamp = DATETIME-TZ(iUTCStamp) NO-ERROR.
      SESSION:DATE-FORMAT = lDateFormat.

      yy = YEAR(lTimeStamp).
      mm = MONTH(lTimeStamp).
      dd = DAY(lTimeStamp).
      lClock = SUBSTR(STRING(lTimeStamp),12,8).
      lClockTime  = INT(ENTRY(1,lClock,":")) * 3600 +
                    INT(ENTRY(2,lClock,":")) * 60   +
                    INT(ENTRY(3,lClock,":")).
      
      IF ERROR-STATUS:ERROR THEN RETURN 0.

      lActivationStamp = yy * 10000 + mm * 100 + dd + lClockTime / 100000.

      RETURN lActivationStamp.

   END METHOD.

   METHOD PUBLIC LOG create_response(
      icTableName AS CHAR,
      icKeyValue  AS CHAR,
      icResult    AS CHAR,
      icMessage   AS CHAR):

      &IF {&MQ_BASED} 
      &THEN 
      CREATE AckMessage.
      ASSIGN 
         AckMessage.HostTable = icTableName
         AckMessage.KeyValue  = icKeyValue
         AckMessage.AckTarget = lcCorrelationId
         AckMessage.QueueName = lcReplyTo
         AckMessage.Created   = NOW
         AckMessage.AckStatus = 0
         AckMessage.AckResult = icResult
         AckMessage.ResultDescription = icMessage.
      RELEASE AckMessage.
      &ENDIF
   
   END METHOD.

   METHOD PUBLIC LOG logError ( iError AS CHAR ):
       MESSAGE iError VIEW-AS ALERT-BOX.
      UNDO, THROW NEW Progress.Lang.AppError(iError, 80000).
   END METHOD.
      
   METHOD PUBLIC VOID create_log(iMessage AS CHAR, iLogType as CHAR):
      LOG-MANAGER:WRITE-MESSAGE(iMessage, iLogType).
   END METHOD.
 
   METHOD PUBLIC LOG parser_error
      (DATASET-HANDLE iParser BIND, iJson AS LONGCHAR):
      DEF VAR lError AS CHAR NO-UNDO.
      DEF VAR lcc AS CHAR NO-UNDO.
      DEF VAR lii AS INT NO-UNDO.
      DEF VAR lReturn AS LOG NO-UNDO.
      
      lReturn = iParser:READ-JSON("LONGCHAR", iJson, "EMPTY") NO-ERROR.
      
      IF lReturn THEN
         RETURN FALSE.
      DO lii = 1 TO ERROR-STATUS:NUM-MESSAGES:
         lcc = ERROR-STATUS:GET-MESSAGE(lii).
         IF lcc MATCHES "*(13036)" THEN NEXT.
         IF lcc BEGINS {&JSON_ERROR} THEN ASSIGN
            lcc = SUBSTRING(lcc, LENGTH({&JSON_ERROR}) + 1).
            lcc = REPLACE(lcc, " (13064)", "").
         lcc = REPLACE(lcc, " (76)", "").
         lcc = REPLACE(lcc, " (13052)", "").
         lError = lError + lcc + "~n".
      END.
      logError(TRIM(lError)).
      
      RETURN TRUE.
   END METHOD.

END CLASS.

