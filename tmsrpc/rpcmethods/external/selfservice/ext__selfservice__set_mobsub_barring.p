/**
 * Change the status of given only customer level barring code
 *
 * @input       transaction_id;string;mandatory;transaction id
                msisdn;string;mandatory;subscription msisdn number
                barr_code;string;mandatory;barring code
                barr_status;string;mandatory;barring status (on/off)
 * @output      struct;mandatory;response struct
 * @response    transaction_id;string;transaction id
                result;boolean;True
 * @Examples    1;C_BPAC;CLB Premium and Content
                2;C_BRAIC;CLB Roaming and International
                3;C_BRIAP;CLB Roaming and Int.calls + Premium
                4;C_LOS;CLB Lost or Stolen
 * @exceptions  1;Subscription not found
                2;CLIType Service Package not found
                3;Service Package not found
                4;Ongoing network command
                5;Operator or debt level barring is on
                6;Barring is not allowed
                7;Barring is already active
                8;Barring is already inactive
                9;Application Id does not match
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{Syst/commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/barrfunc.i}
{Func/fexternalapi.i}
{Func/transname.i}

/* Input parameters */
DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcBCode   AS CHAR NO-UNDO.
DEF VAR pcServiceStatus AS CHAR NO-UNDO.
DEF VAR pcSetServiceId  AS CHAR NO-UNDO.
DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.

DEF VAR lcStatus        AS CHAR NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId  AS CHAR NO-UNDO.
DEF VAR lcOnOffEn       AS CHAR NO-UNDO.
DEF VAR lcOnOffEs       AS CHAR NO-UNDO.
DEF VAR lcBarrEntry AS CHAR NO-UNDO. 
DEF VAR liLoop AS INT NO-UNDO. 

DEF VAR lrBarring AS ROWID NO-UNDO.
DEF VAR llOngoing AS LOGICAL NO-UNDO.
DEF VAR lcBarrings AS CHAR NO-UNDO.
DEF VAR liReq AS INT NO-UNDO.
DEF VAR lcItemName AS CHAR NO-UNDO.
DEF VAR lcDetailedUser AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,string") EQ ?
THEN RETURN.
                                                            /*Example*/
ASSIGN pcTransId = get_string(param_toplevel_id, "0")       /*501....*/
       pcCLI     = get_string(param_toplevel_id,"1")        /*622689226*/
       pcBCode = get_string(param_toplevel_id,"2")    /*C_BRAIC*/
       pcServiceStatus = get_string(param_toplevel_id,"3"). /*off*/

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = gbAuthLog.EndUserId.

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + gbAuthLog.EndUserId.

FIND MobSub NO-LOCK WHERE
     MobSub.Brand = gcBrand AND
     MobSub.CLI = pcCLI NO-ERROR.
IF NOT AVAILABLE MobSub THEN
  RETURN appl_err("Subscription not found").

/*YPR-4774*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
IF MobSub.MsStatus EQ {&MSSTATUS_FIXED_PROV_ONG} /*16*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").

FIND FIRST BarringConf NO-LOCK WHERE
           BarringConf.BarringCode EQ pcBCode NO-ERROR.
IF NOT AVAIL BarringConf THEN DO:
   /*Legacy barring code translations*/
   FIND FIRST BarringConf NO-LOCK WHERE
              BarringConf.OldCode EQ pcBCode AND
              BarringConf.AllowedAppIds NE "" NO-ERROR.
   IF AVAIL BarringConf THEN pcBCode = BarringConf.BarringCode.
END.

IF NOT AVAIL BarringConf OR
             BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_INACTIVE}
   THEN RETURN appl_err("Barring not found").

/*Also in configuration*/   
IF LOOKUP(lcApplicationId, BarringConf.AllowedAppIds) EQ 0 THEN
   RETURN appl_err("Application ID does not match").

/*Check subscription and payment type*/
IF BarringConf.AllowedPaymentType NE "" AND
   LOOKUP(string(MobSub.PayType,"prepaid/postpaid"),
          BarringConf.AllowedPaymentType) = 0 THEN
   RETURN appl_err("Incorrect payment type").

/*Check that command is reasonable*/
IF LOOKUP(pcServiceStatus,"on,off") = 0 THEN
   RETURN appl_err("Invalid service status").

/* Check ongoing service requests */
llOngoing = fCheckBarrStatus(INPUT MobSub.MsSeq,
                             OUTPUT lcBarrings,
                             OUTPUT lrBarring).
IF llOngoing EQ TRUE THEN
   RETURN appl_err("Ongoing Network command").

CASE pcServiceStatus:
   WHEN "on" THEN DO: 
      pcSetServiceId = pcBCode + "=1".
      IF fIsReasonableSet(pcSetServiceId, Mobsub.MsSeq) EQ FALSE THEN
         RETURN appl_err("Barring is already on active").
      lcOnOffEs = "Activar".
      lcOnOffEn = "applied".
   END.
   WHEN "off" THEN DO: 
      pcSetServiceId = pcBCode + "=0".
      IF fIsReasonableSet(pcSetServiceId, Mobsub.MsSeq) EQ FALSE THEN
         RETURN appl_err("Barring is already inactive").
      lcOnOffEs = "Desactivar".
      lcOnOffEn = "released".
   END.
END.

&SCOPED-DEFINE MIYOIGO_CUST_BARRINGS "Cust_TotalPremium_off,C_BRIAP,C_BRAIC"
/* YPR-2350 */
IF LOOKUP(lcApplicationId,"501,502") > 0 AND
   LOOKUP(pcBCode,{&MIYOIGO_CUST_BARRINGS}) > 0 THEN DO:

   DO liLoop = 1 TO NUM-ENTRIES({&MIYOIGO_CUST_BARRINGS}): 
      lcBarrEntry = ENTRY(liLoop,{&MIYOIGO_CUST_BARRINGS}).

      IF lcBarrEntry EQ pcBCode OR
         LOOKUP(lcBarrEntry,lcBarrings) = 0 THEN NEXT.

      pcSetServiceId = pcSetServiceId + "," + lcBarrEntry + "=0".
   END.
END.

RUN Mm/barrengine.p(MobSub.MsSeq,
                 pcSetServiceId,
                 {&REQUEST_SOURCE_EXTERNAL_API},
                 (IF lcApplicationId EQ "701" THEN "Collection"
                  ELSE ""),
                 fMakeTS(),
                 "",
                 OUTPUT lcStatus).

IF lcStatus EQ "ONC" THEN RETURN appl_err("Ongoing network command").

liReq = INT(lcStatus) NO-ERROR.
IF liReq > 0 THEN DO:

   /* Adding the details into Main struct */
   top_struct = add_struct(response_toplevel_id, "").
   add_string(top_struct, "transaction_id", pcTransId).
   add_boolean(top_struct, "result", True).

   lcDetailedUser = fgetAppDetailedUserId(INPUT lcApplicationId,
                                         INPUT Mobsub.CLI).  

   /*YPR-1966, add different memo writing*/
   IF lcApplicationId EQ "701" THEN DO:
      lcItemName = fGetItemName(gcBrand,
                                "BarringCode",
                                pcBCode,
                                5, /*en*/
                                TODAY).
      DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                    "MobSub",                             /* HostTable */
                    STRING(Mobsub.MsSeq),                 /* KeyValue  */
                    MobSub.CustNum,                       /* CustNum */
                    "Collection Action",                  /* MemoTitle */
                    pcBCode + " " +
                    "(" + 
                    lcItemName +
                    ") " +
                     lcOnOffEn,                 /* MemoText */
                    "Service",                            /* MemoType */
                    lcDetailedUser).
   END.
   ELSE DO:
      lcItemName = fGetItemName(gcBrand,
                                "BarringCode",
                                pcBCode,
                                1, /*es*/
                                TODAY).
      DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                    "MobSub",                             /* HostTable */
                    STRING(Mobsub.MsSeq),                 /* KeyValue  */
                    MobSub.CustNum,                       /* CustNum */
                    "Bloqueo modificado",                 /* MemoTitle */
                    "Solicitado por el cliente " + 
                    lcItemName +                    
                    " - " +
                    lcOnOffEs,                            /* MemoText */
                    "Service",                            /* MemoType */
                    lcDetailedUser).
   END.
END.
ELSE RETURN appl_err("ERROR: Unable to set barring").


FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
