/**
 * Set subscription values 
 *
 * @input  id;int;subscription id 
           subscription;struct;
 * @struct id_code;int;optional;identification id
           username;string;mandatory;user who made the request
           billing_permission;int;optional;billing permission status code
           permanent_contract_valid_to;date;optional;new end date for active periodical contract 
           sms_bundle;struct;set a new sms bundle
 * @sms_bundle current_amount;int;mandatory
               change;int;mandatory
               user_limit;int;mandatory
 * @output struct;empty
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcIDCode AS CHAR NO-UNDO.
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR liIDCode AS INTEGER NO-UNDO.
DEF VAR piBillingPermission AS INT NO-UNDO.
DEF VAR pdaTermContractValidTo AS DATE NO-UNDO.
DEF VAR lcDCEvent AS CHAR NO-UNDO. 
DEF VAR liReq AS INT NO-UNDO.
DEF VAR lcInfo AS CHAR NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO. 
DEF VAR ldaValidToOrig AS DATE NO-UNDO.

DEF VAR pcSMSBundleStruct AS CHARACTER NO-UNDO. 
DEF VAR liCurrValue AS INTEGER NO-UNDO. 
DEF VAR liAddValue AS INTEGER NO-UNDO. 
DEF VAR liUserLimit AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,struct") EQ ? THEN RETURN.

piMsSeq  = get_int(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
lcstruct = validate_struct(pcStruct, "id_code,username!," + 
                                     "billing_permission," + 
                                     "permanent_contract_valid_to," +
                                     "sms_bundle").

IF LOOKUP("id_code", lcStruct) GT 0 THEN 
   pcIDCode = get_string(pcStruct, "id_code").

pcUsername = "VISTA_" + get_string(pcStruct, "username").

IF LOOKUP("billing_permission", lcStruct) GT 0 THEN 
  piBillingPermission = get_int(pcStruct, "billing_permission").

IF LOOKUP("permanent_contract_valid_to", lcStruct) GT 0 THEN
   pdaTermContractValidTo = get_date(pcStruct, "permanent_contract_valid_to").

IF LOOKUP("sms_bundle", lcStruct) GT 0 THEN
   pcSMSBundleStruct = get_struct(pcStruct, "sms_bundle").


IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND MobSub NO-LOCK WHERE
   MobSub.MsSeq = piMsSeq NO-ERROR.

IF NOT AVAIL MobSub THEN DO:
   RETURN appl_err(SUBST("Unknown subscription id &1", piMsSeq)).
END.

IF pcIDCode NE "0000" AND pcIDCode NE "" THEN DO:
   liIDCode = INT(pcIDCode) NO-ERROR.
   IF LENGTH(pcIDCode) NE 4 OR liIDCode = 0 THEN 
      RETURN appl_err(SUBST("Identification code &1 is not valid", pcIDCode)).
END.

{commpaa.i}
{eventval.i}
katun = pcUserName.
gcbrand = "1".
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
END.

{flimitreq.i}
{tmsconst.i}
{fmakemsreq.i}
{subser.i}
{fcounter.i}

FUNCTION fSetSMSBundle RETURN CHARACTER 
         (INPUT piCurrValue AS INT,
          INPUT piAddValue  AS INT,
          INPUT piUserLimit AS INT):

   DEFINE VARIABLE ocError AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liValidate AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldActStamp AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldtActDate AS DATE NO-UNDO.
   DEFINE VARIABLE liReq AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldTS1 AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldTS2 AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldTS3 AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldMonthAmt AS DECIMAL NO-UNDO.
   
   FIND FIRST SubSer  WHERE
              SubSer.MsSeq = Mobsub.MsSeq AND
              SubSer.ServCom = "SMSBundle" USE-INDEX ServCom  NO-LOCK NO-ERROR.
          
   liValidate = fSubSerValidate(
                 MobSub.MsSeq,
                 "SMSBundle",
                  1,
                  OUTPUT ocError).

    IF liValidate NE 0 THEN 
           CASE liValidate:
           WHEN 3 THEN RETURN "Ongoing network command".
           OTHERWISE RETURN "Service change is not allowed".
    END.

    /* check the validity of change date */
    ldActStamp = fServiceActStamp(SubSer.MsSeq,
                                  "SMSBundle",
                                  1).

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
        

    /* pick up monthly amount of SMSBundle given 
       to this mobsub 
    */
    fMonthlyStamps(TODAY,
                   ldTS1,
                   ldTS2). 
    ldMonthAmt = fGetCounterAmt("MobSub",
                                STRING(MobSub.MsSeq),
                                1,
                                ldTS1,
                                ldTS2).

       /* check monthly limits */
    IF (INT(ldMonthAmt) + piAddValue) > piUserLimit THEN
       RETURN "Change exceeds the monthly limit ".

    /* create service request */
    liReq = fServiceRequest(MobSub.MsSeq,
                            "SMSBUNDLE",
                            1,
                            STRING(piCurrValue + piAddValue),
                            ldActStamp,
                            "", /* salesman */
                            FALSE,      /* fees */
                            FALSE,      /* sms */
                            "",
                            {&REQUEST_SOURCE_NEWTON},
                            0, /* father request */
                            false, /* mandatory for father request */
                            OUTPUT ocError).
    IF liReq = 0 THEN DO:
        RETURN "Change request was not accepted for service SMSBundle; " + ocError.
    END.

    fUpdateCounter("MobSub",
                   STRING(MobSub.MsSeq),
                   1,
                   ldTS2,
                   ldTS1,
                   DECIMAL(piAddValue)).

  RETURN "".
  
END FUNCTION.


/* update terminal periodical contract end date */
IF LOOKUP("permanent_contract_valid_to", lcStruct) GT 0 THEN DO:

   /* pick up DCEvent */
   i = 0.
   FOR EACH DCCLI WHERE
      DCCLI.Brand = gcBrand AND
      DCCLI.MsSeq = Mobsub.Msseq AND
      DCCLI.ValidTo >= TODAY NO-LOCK,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} NO-LOCK:
      
      i = i + 1.
      IF i > 1 THEN RETURN appl_err("System error").
         
      ldaValidToOrig = (IF DCCLI.ValidToOrig NE ? THEN DCCLI.ValidToOrig ELSE DCCLI.ValidTo).
      IF pdaTermContractValidTo > ldaValidToOrig OR 
         pdaTermContractValidTo < TODAY THEN
         RETURN appl_err("Incorrect periodical contract end date").  

      IF DCCLI.TermDate NE ? THEN
         RETURN appl_err("Cannot change terminating contract end date").  

       lcDCEvent = DCCLI.DCEvent.
    END.

    IF lcDCEvent = "" THEN RETURN appl_err("Periodical contract not found").

   /* update periodical contract */
   liReq = fPCUpdateRequest( MobSub.MsSeq, /* subscription         */
                             lcDCEvent, /* daycampaign.dcevent  */
                             0, /* action stamp TODAY  */
                             "ValidTo", /* fields to be updated */
                             STRING(pdaTermContractValidTo), /* field values */
                             FALSE, /* create fees */
                             {&REQUEST_SOURCE_NEWTON} , /* where created */
                             katun, /* who made the request */
                             lcInfo).
   IF liReq = 0 THEN DO:
        RETURN appl_err("Request to update terminal periodical contract end date: " + lcInfo).
   END.

END.


IF LOOKUP("billing_permission", lcStruct) GT 0 THEN DO:
   
   FIND FIRST Limit WHERE
      Limit.MsSeq = MobSub.MsSeq AND
      Limit.LimitType = {&LIMIT_TYPE_BILLPERM} AND
      Limit.TMRuleSeq = 0 AND
      Limit.Todate >= TODAY AND
      Limit.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
      
   IF piBillingPermission = 0 THEN DO:
      IF AVAIL Limit THEN 
      fSetLimit (
         ROWID(Limit),
         Limit.LimitAmt,
         FALSE, /* default value */ 
         Limit.FromDate,
         TODAY - 1).
   END.
   ELSE
   fCreateLimitHistory(
      MobSub.InvCust,
      MobSub.MsSeq,
      {&LIMIT_TYPE_BILLPERM},
      piBillingPermission,
      0, /* limit id */
      0, /* tmruleseq */
      FALSE, /* default value */
      TODAY,
      12/31/2049).
END.

IF LOOKUP("id_code", lcStruct) GT 0 THEN DO:
   
   FIND CURRENT MobSub EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      {lib/eventlog.i}
      DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
      lhMobSub = BUFFER MobSub:HANDLE.
      RUN StarEventInitialize(lhMobSub).
      RUN StarEventSetOldBuffer(lhMobSub).
   END.

   ASSIGN
      MobSub.IDCode = pcIDCode.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhMobSub).
      fCleanEventObjects().
   END.

   RELEASE MobSub.
END.

IF LOOKUP("sms_bundle",lcStruct) GT 0 THEN DO:

   liCurrValue = get_int(pcSMSBundleStruct,"current_amount").
   liAddValue  = get_int(pcSMSBundleStruct,"change").
   liUserLimit = get_int(pcSMSBundleStruct,"user_limit").

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   lcInfo = fSetSMSBundle(liCurrValue,
                          liAddValue,
                          liUserLimit).
   IF lcInfo > "" THEN DO:
      RETURN appl_err(lcInfo).
   END.

END.

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
