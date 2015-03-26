/**
 * Set service status of given service code
 *
 * @input       transaction_id;string;mandatory;transaction id
                msisdn;string;mandatory;subscription msisdn number
                service_code;string;mandatory;service code(eg:VMS,LANG,CF,IRDCUTOFF,BB,NAM)
                service_status;string;mandatory;on/off
                params;struct;optional;CF/LANG
 * @params      param;string;1 OR 5 for LANG
                param;string;Combinations of CFB (0,VM,MCA),CFNRC (0,VM,MCA),CFNRY (0,VM) - Eg:021 - CFB=0,CFNRC=MCA,CFNRY=VM
 * @output      struct;mandatory;response struct
 * @response    transaction_id;string;transaction id
                result;boolean;True
 * @exceptions  1;Subscription not found
                2;Service Component not found
                3;Invalid service status
                4;Ongoing network command
                5;Service change is not allowed
                6;Service is already active
                7;Service is already suspended
                8;Ongoing Fraud Barring Request
                9;Reset request (step 1-deactivation) was not accepted
                10;Reset request (step 2-reactivation) was not accepted
                11;BB service can not be activated since subscription has fraud barring
                12;Change request was not accepted
                13;New request was not accepted
                14;Service is not active
                15;Service can not be activated because subscription does not have active data bundle (eg:BB)
                16;Ongoing CLB Lost or Stolen Barring Request
                17;Internet service can not be activated/de-activated since subscription has CLB Lost or Stolen Barring
                18;Application Id does not match
                19;LANG service is already active with same language
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{fmakemsreq.i}
{subser.i}
{barrfunc.i}
{fbundle.i}
{fexternalapi.i}
{service.i}

/* Input parameters */
DEF VAR pcReqList       AS CHAR NO-UNDO.
DEF VAR pcParamStruct   AS CHAR NO-UNDO.
DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcServiceCode   AS CHAR NO-UNDO.
DEF VAR pcServiceStatus AS CHAR NO-UNDO.
DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.

DEF VAR liValue         AS INT  NO-UNDO.
DEF VAR liReq           AS INT  NO-UNDO.
DEF VAR ldActStamp      AS DEC  NO-UNDO.
DEF VAR lcBarringCode   AS CHAR NO-UNDO.
DEF VAR lcInfo          AS CHAR NO-UNDO.
DEF VAR lcParam         AS CHAR NO-UNDO.
DEF VAR orBarring       AS ROWID NO-UNDO.

DEF VAR lcAppId AS CHAR NO-UNDO. 

pcReqList = validate_request(param_toplevel_id,
                             "string,string,string,string,[struct]").
IF pcReqList EQ ? THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcCLI = get_string(param_toplevel_id,"1")
       pcServiceCode = get_string(param_toplevel_id,"2")
       pcServiceStatus = get_string(param_toplevel_id,"3").

IF NUM-ENTRIES(pcReqList) >= 5 THEN DO:
   pcParamStruct = get_struct(param_toplevel_id, "4").
   IF validate_request(pcParamStruct,"param!") EQ ? THEN RETURN.
   lcparam = get_string(pcParamStruct,"param").
END. /* IF NUM-ENTRIES(pcReqList) >= 5 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcAppId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName,lcAppId) THEN
   RETURN appl_err("Application Id does not match").

FIND MobSub NO-LOCK WHERE
     MobSub.CLI = pcCLI NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

IF LOOKUP(pcServiceCode,"Y_BPSUB,C_BPSUB") > 0 THEN DO:
   
   IF lcAppId NE "650" THEN 
      RETURN appl_err("Application Id does not match").
   
   ASSIGN
      lcParam = pcServiceCode WHEN pcServiceStatus = "on"
      pcServiceCode = "BPSUB".
END.

FIND FIRST ServCom NO-LOCK WHERE
           ServCom.Brand = gcBrand AND
           ServCom.ServCom = pcServiceCode NO-ERROR.
IF NOT AVAILABLE ServCom THEN
   RETURN appl_err("Service Component not found").

CASE pcServiceStatus:
   WHEN "off" THEN liValue = 0. 
   WHEN "on"  THEN liValue = 1.
   OTHERWISE RETURN appl_err("Invalid service status").
END.

/* Check ongoing service requests */
IF CAN-FIND(FIRST MsRequest WHERE
                  MsRequest.MsSeq      = MobSub.MsSeq AND
                  MsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
                  MsRequest.ReqCParam1 = ServCom.ServCom AND
                  LOOKUP(STRING(MsRequest.ReqStatus),
                         {&REQ_INACTIVE_STATUSES}) = 0) THEN
   RETURN appl_err("Ongoing network command").

/* Check BB allowed */
IF pcServiceCode = "BB" AND pcServiceStatus = "ON" AND
   NOT fIsBBAllowed(Mobsub.MsSeq,fMakeTS()) THEN
   RETURN appl_err("Service can not be activated because subscription " +
                   "does not have active data bundle").

FOR FIRST SubSer NO-LOCK WHERE
          SubSer.MsSeq = Mobsub.MsSeq AND
          SubSer.ServCom = ServCom.ServCom:
     
   liReq = fSubSerValidate(INPUT MobSub.MsSeq,
                           INPUT Subser.ServCom,
                           INPUT liValue,
                           OUTPUT lcInfo).
   IF liReq NE 0 THEN 
      CASE liReq:
         WHEN 3 THEN RETURN appl_err("Ongoing network command").
         OTHERWISE appl_err("Service change is not allowed").
      END.

   liReq = fSubSerSSStat(INPUT MobSub.MsSeq,
                         INPUT Subser.ServCom,
                         INPUT liValue,
                         OUTPUT lcInfo).
   IF liReq NE 0 THEN
      RETURN appl_err(SUBST("Unknown service value: &1", pcServiceStatus)).

   IF Subser.ServCom = "NAM" THEN DO:
      lcBarringCode = fCheckBarrStatus(INPUT MobSub.MsSeq,
                                       OUTPUT orBarring).
      IF lcBarringCode = "91" THEN DO:
         FIND FIRST MsRequest WHERE
                    ROWID(MsRequest) = orBarring AND
                    MsRequest.ReqCParam1 = "C_LOS" NO-LOCK NO-ERROR.
         IF AVAILABLE MsRequest THEN
            RETURN appl_err("Ongoing CLB Lost or Stolen Barring Request").
      END. /* IF lcBarringCode = "91" THEN DO: */
      ELSE IF lcBarringCode = "C_LOS" THEN
         RETURN appl_err("Internet service can not be " +
                (IF liValue = 0 THEN "de-activated" ELSE "activated") +
                 " since subscription has CLB Lost or Stolen Barring").
   END. /* IF Subser.ServCom = "NAM" THEN DO: */

   IF LOOKUP(SubSer.ServCom,"CF,LANG") > 0 THEN DO:
      IF SubSer.ServCom = "LANG" AND STRING(SubSer.SSStat) = lcparam THEN
         RETURN appl_err("LANG service is already active with same language").
   END. /* IF LOOKUP(SubSer.ServCom,"CF,LANG") > 0 THEN DO: */
   /* Return error if new value is same as existing value */ 
   ELSE DO:
     IF (liValue EQ 0 AND liValue EQ SubSer.SSStat) OR
        (liValue EQ SubSer.SSStat AND lcParam EQ SubSer.SSParam) OR 
        (SubSer.SSStat EQ 2 AND liValue EQ 0) THEN
        RETURN appl_err("Service is already " +
                        (IF liValue = 1 THEN "active" ELSE "suspended")).
   END. /* ELSE DO: */

   /* check the validity of change date */
   ldActStamp = fServiceActStamp(SubSer.MsSeq,
                                 SubSer.ServCom,
                                 liValue).
   IF ldActStamp > 0 THEN DO:
      fSplitTS(ldActStamp,
               OUTPUT ldtActDate,
               OUTPUT liReq).
      IF ldtActDate > SubSer.SSDate OR
         (DAY(ldtActDate) = 1 AND liReq < TIME - 120 AND
          DAY(SubSer.SSDate) NE 1)
      THEN .
      ELSE ldActStamp = fMakeTS().
   END.
   ELSE ldActStamp = fMakeTS().

   IF ldtActDate = TODAY
   THEN ldActStamp = fMakeTS().
   ELSE ldActStamp = fMake2DT(ldtActDate,1).

   /* Special handling for Black Berry service */
   IF Subser.ServCom = "BB" THEN DO:
      /* Suspended */
      IF liValue = 0 AND SubSer.SSStat = 1 THEN
         liValue = 2.
      /* Resume - Pass the new status in parameter */
      ELSE IF liValue = 1 AND SubSer.SSStat = 2 THEN
         lcParam = "3".
   END. /* IF Subser.ServCom = "BB" THEN DO: */

   /* Extra pre-caution, if fraud barring is applied then */
   /* don't allow to activate the BB service              */
   IF Subser.ServCom = "BB" AND liValue = 1 THEN DO:
      lcBarringCode = fCheckBarrStatus(INPUT MobSub.MsSeq,
                                       OUTPUT orBarring).
      IF lcBarringCode = "91" THEN DO:
         FIND FIRST MsRequest WHERE
              ROWID(MsRequest) = orBarring AND
              LOOKUP(MsRequest.ReqCParam1, {&FRAUD_BARR_CODES}) > 0
              NO-LOCK NO-ERROR.
         IF AVAILABLE MsRequest THEN
            RETURN appl_err("Ongoing Fraud Barring Request").
      END. /* IF lcBarringCode = "91" THEN DO: */
      ELSE IF LOOKUP(lcBarringCode, {&FRAUD_BARR_CODES}) > 0 THEN
         RETURN appl_err("BB service can not be activated since " +
                         "subscription has fraud barring").
   END. /* IF liValue = 1 THEN DO: */

   liReq = fServiceRequest(MobSub.MsSeq,
                           Subser.ServCom,
                           (IF Subser.ServCom = "LANG" THEN INT(lcParam)
                            ELSE liValue),
                           (IF Subser.ServCom = "LANG" THEN ""
                            ELSE lcParam),
                           ldActStamp,
                           katun,
                           TRUE,      /* fees */
                           TRUE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_EXTERNAL_API},
                           0, /* father request */
                           false, /* mandatory for father request */
                           OUTPUT lcInfo).
      
   IF liReq = 0 THEN
      RETURN appl_err("Change request was not accepted").

   /* Adding the details into Main struct */
   top_struct = add_struct(response_toplevel_id, "").
   add_string(top_struct, "transaction_id", pcTransId).
   add_boolean(top_struct, "result", True).

   RETURN.
END. /* FOR FIRST SubSer NO-LOCK WHERE */
   
/* Additional logic to add the new BB service to the subscription */
IF LOOKUP(pcServiceCode,"BB,BPSUB") > 0 AND
   pcServiceStatus = "ON" THEN DO:
   /* Extra pre-caution, if fraud barring is applied then */
   /* don't allow to activate the BB service              */
   IF pcServiceCode EQ "BB" THEN DO:
      lcBarringCode = fCheckBarrStatus(INPUT MobSub.MsSeq,
                                       OUTPUT orBarring).
      IF lcBarringCode = "91" THEN DO:
         FIND FIRST MsRequest WHERE
              ROWID(MsRequest) = orBarring AND
              LOOKUP(MsRequest.ReqCParam1, {&FRAUD_BARR_CODES}) > 0
              NO-LOCK NO-ERROR.
         IF AVAILABLE MsRequest THEN
            RETURN appl_err("Ongoing Fraud Barring Request").
      END. /* IF lcBarringCode = "91" THEN DO: */
      ELSE IF LOOKUP(lcBarringCode, {&FRAUD_BARR_CODES}) > 0 THEN
         RETURN appl_err("BB service can not be activated since " +
                         "subscription has fraud barring").
   END. /* IF pcServiceStatus = "ON" THEN DO: */

   liReq = fServiceRequest(MobSub.MsSeq,
                           ServCom.ServCom,
                           1,
                           lcParam,
                           fMakeTS(),
                           katun,
                           TRUE,      /* fees */
                           TRUE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_EXTERNAL_API},
                           0, /* father request */
                           false, /* mandatory for father request */
                           OUTPUT lcInfo).
   IF liReq = 0 THEN
      RETURN appl_err("New request was not accepted").

END. /* IF pcServiceID = "BB" THEN DO: */
ELSE IF pcServiceStatus = "OFF" THEN
   RETURN appl_err("Service is not active").

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
