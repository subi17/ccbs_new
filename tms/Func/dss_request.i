&IF "{&dss_request_i}" NE "YES" 
&THEN

&GLOBAL-DEFINE dss_request_i YES

{Func/fparse.i}
{Func/fmakemsreq.i}
{Func/fdss.i}

FUNCTION fDSSRequest RETURNS INTEGER
   (INPUT iiMsSeq        AS INT,    /* subscription */
    INPUT iiCustNum      AS INT,    /* customer */
    INPUT icAction       AS CHAR,   /* Action: Mandatory */
    INPUT icParams       AS CHAR,   /* Parameters: Optional */
    INPUT icBundle       AS CHAR,   /* bundle: Optional */
    INPUT idActStamp     AS DEC,    /* when request should be handled */
    INPUT icSource       AS CHAR,
    INPUT icCreator      AS CHAR,   
    INPUT ilCreateFees   AS LOG,    
    INPUT iiOrigRequest  AS INT,    /* main request */
    INPUT ilMandatory    AS LOG,    /* is subrequest mandatory */
    OUTPUT ocResult      AS CHAR):
 
   DEF VAR liReqCreated AS INT  NO-UNDO.
   DEF VAR lcCLI        AS CHAR NO-UNDO.
   DEF VAR lcCLIType    AS CHAR NO-UNDO.

   DEF BUFFER bMobSub     FOR MobSub.
   DEF BUFFER bTermMobSub FOR TermMobSub.

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq EQ iiMsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL bMobSub THEN DO:
      FIND FIRST bTermMobSub NO-LOCK WHERE
                 bTermMobSub.MsSeq EQ iiMsSeq NO-ERROR.

      IF NOT AVAIL bTermMobSub THEN DO:
         ocResult = "MobSub not found".
         RETURN 0.
      END. 

      ASSIGN lcCLI     = bTermMobSub.CLI
             lcCLIType = bTermMobSub.CLIType.

   END. 
   ELSE ASSIGN lcCLI     = bMobSub.CLI
               lcCLIType = bMobSub.CLIType.

   ocResult = fChkRequest(iiCustNum,
                          {&REQTYPE_DSS},
                          icAction,
                          icCreator).

   IF ocResult > "" THEN RETURN 0.

   /* Pre-check only for CREATE, there is no ongoing ACC request */
   IF icAction = "CREATE" THEN DO:
      RUN Mm/requestaction_check.p(INPUT {&REQTYPE_DSS},
                                   INPUT lcCLIType,
                                   INPUT iiMsSeq,
                                   INPUT icSource,
                                   OUTPUT ocResult).
      IF ocResult > "" THEN RETURN 0.
   END. /* IF icAction = "CREATE" THEN DO: */

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_DSS},
                  idActStamp,
                  icCreator,
                  ilCreateFees, 
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.MsSeq       = iiMsSeq
      bCreaReq.CLI         = lcCLI
      bCreaReq.CustNum     = iiCustNum /* it can be different in case of ACC */
      bCreaReq.ReqCParam1  = icAction
      bCreaReq.ReqCParam2  = icParams
      bCreaReq.ReqCParam3  = icBundle
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = INTEGER(ilMandatory)
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.

   /* Send the SMS using Request Action Rules for DSS */
   RUN Mm/requestaction_sms.p(INPUT liReqCreated,
                           INPUT lcCLIType,
                           INPUT icSource).
  
   RETURN liReqCreated.
     
END FUNCTION.

PROCEDURE pUpdateDSSNetworkLimit:
   DEF INPUT PARAMETER iiMsSeq          AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum        AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideLimit         AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icActionType     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ilCheckDSSLimit  AS LOG  NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest    AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp      AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icSource         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icBundleId       AS CHAR NO-UNDO.
   
   DEF VAR liCreated       AS INT   NO-UNDO.
   DEF VAR lcError         AS CHAR  NO-UNDO.
   DEF VAR lcParam         AS CHAR  NO-UNDO.
   DEF VAR liLimit         AS INT64 NO-UNDO.

   DEF BUFFER bMsRequest   FOR MsRequest.

   ASSIGN liLimit      = ideLimit * 1024 * 1024.

   /* Compare latest DSS limit with limit specified in the "CREATE" request */
   IF ilCheckDSSLimit THEN DO:
      FIND LAST bMsRequest WHERE
                bMsRequest.MsSeq      = iiMsSeq        AND
                bMsRequest.ReqType    = {&REQTYPE_DSS} AND
                bMsRequest.ReqCparam1 = "CREATE"       AND
                bMsRequest.ActStamp  <= ideActStamp NO-LOCK NO-ERROR.
      IF AVAILABLE bMsRequest AND
         STRING(liLimit) = fParseKVP("LIMIT_UNSHAPED",bMsRequest.ReqCparam2,",")
      THEN RETURN.
   END. /* IF ilCheckDSSLimit THEN DO: */

   IF icActionType = "QUOTA" THEN
      lcParam = "DSS-ACCOUNT" + "=" + STRING(iiCustNum) + "," +
                "TEMPLATE=DSS"                          + "," +
                icActionType  + "=" + STRING(liLimit)   + "," +
                "GRACE"       + "=" + STRING(0).
   ELSE
      lcParam = "DSS-ACCOUNT="    + STRING(iiCustNum)       + "," +
                "TEMPLATE=DSS_MONTHLY"                      + "," +
                "TARIFF_TYPE=DSS"                           + "," +
                "TARIFF="         + icBundleId              + "," +
                "LIMIT_UNSHAPED=" + STRING(liLimit)         + "," +
                "LIMIT_SHAPED="   + STRING({&PL_LIMIT_SHAPED}).

   liCreated = fDSSRequest(iiMsSeq,
                           iiCustNum,
                           "MODIFY",
                           lcParam,
                           icBundleId,
                           ideActStamp,
                           icSource,
                           "",
                           FALSE,
                           iiMainRequest,
                           FALSE,
                           OUTPUT lcError).

   IF liCreated = 0 THEN
      Func.Common:mWriteMemo("Customer",
                 STRING(iiCustNum),
                 iiCustNum,
                 "Update DSS Network Limit/Quota",
                 "Failed Update DSS Network Limit/Quota. " + lcError).

END PROCEDURE.

PROCEDURE pUpdateDSSNetwork:
   DEF INPUT PARAMETER iiMsSeq          AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCLI            AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiCustNum        AS INT  NO-UNDO.
   DEF INPUT PARAMETER icAction         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icParamList      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest    AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp      AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icSource         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icBundleId       AS CHAR NO-UNDO.
   
   DEF VAR liCreated                    AS INT   NO-UNDO.
   DEF VAR lcError                      AS CHAR  NO-UNDO.
   DEF VAR lcParam                      AS CHAR  NO-UNDO.

   CASE icAction:
      /* Delete DSS Account */
      WHEN "DELETE" THEN .
      /* Update HSDPA MSISDN LIST */
      WHEN "MODIFY" THEN
         lcParam = "TEMPLATE=DSS," + icParamList.
      OTHERWISE
         lcParam = "MSISDN=34" + icCLI.
   END.
   
   lcParam = "DSS-ACCOUNT=" + STRING(iiCustNum) + "," + lcParam.

   liCreated = fDSSRequest(iiMsSeq,
                           iiCustNum,
                           icAction, 
                           lcParam,
                           icBundleId,
                           ideActStamp,
                           icSource,
                           "",
                           FALSE,
                           iiMainRequest,
                           FALSE,
                           OUTPUT lcError).

   IF liCreated = 0 THEN
      Func.Common:mWriteMemo("Customer",
                       STRING(iiCustNum),
                       iiCustNum,
                       "Unable to create " + icAction + " request",
                       "Unable to create " + icAction + " request. " +
                       lcError).

END PROCEDURE.

PROCEDURE pUpdateDSSConsumption:

   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER icSource      AS CHAR NO-UNDO.

   DEF VAR ldeDSSLimit               AS DEC  NO-UNDO.
   DEF VAR ldeDSSUsage               AS DEC  NO-UNDO.
   DEF VAR lcBundleId                AS CHAR NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType     AS CHAR NO-UNDO.
   DEF VAR ldaActDate                AS DATE NO-UNDO.
   DEF VAR liActTime                 AS INT  NO-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.

   FIND FIRST bMsRequest WHERE
              bMsRequest.MsRequest = iiMainRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN.

   Func.Common:mSplitTS(bMsRequest.ActStamp,OUTPUT ldaActDate,OUTPUT liActTime).

   /* Check whether DSS bundle is active or not for this customer */
   IF NOT fGetTotalDSSUsage(INPUT bMsRequest.CustNum,INPUT ldaActDate,
                            OUTPUT lcBundleId,OUTPUT ldeDSSLimit,
                            OUTPUT ldeDSSUsage) THEN RETURN.

   IF lcBundleId = "DSS2" THEN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   /* Adjust DSS counter if counter is more than usage */
   IF ldeDSSUsage > ldeDSSLimit THEN DO:
   
      /* YTS-6595 */
      IF fOngoingDSSTerm (bMsRequest.Custnum,
                          bMsRequest.ActStamp) THEN RETURN.

      RUN pUpdateDSSNetworkLimit(INPUT bMsRequest.MsSeq,
                                 INPUT bMsRequest.CustNum,
                                 INPUT (ldeDSSUsage - ldeDSSLimit),
                                 INPUT "QUOTA",
                                 INPUT FALSE,
                                 INPUT bMsRequest.MsRequest,
                                 INPUT bMsRequest.ActStamp,
                                 INPUT icSource,
                                 INPUT lcBundleId).
   END. /* IF ldeDSSUsage > ldeDSSLimit THEN DO: */

   /* Reset subscription based consumption counter if new tariff is not
      compatible with DSS2 */
   IF lcBundleId = "DSS2" AND
      LOOKUP(bMsRequest.ReqCparam1,lcAllowedDSS2SubsType) > 0 AND
      LOOKUP(bMsRequest.ReqCparam2,lcAllowedDSS2SubsType) = 0 THEN DO:
      liRequest = fServiceRequest(
                        bMsRequest.MsSeq,
                        "SHAPER",
                        1, /* on */
                        "0,GRACE=0,TEMPLATE=HSPA_SET",
                        bMsRequest.ActStamp,
                        "", /* salesman */
                        FALSE, /* fees */
                        FALSE, /* sms */
                        "", /* creator */
                        icSource,
                        bMsRequest.MsRequest, /* father request */
                        FALSE, /* mandatory for father request */
                        OUTPUT lcError).
      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(bMsRequest.MsSeq),
                          bMsRequest.Custnum,
                          "Contract consumption adjustment failed;",
                          lcError).
   END. /* IF lcBundleId = "DSS2" AND */

END PROCEDURE.

&ENDIF
