/**
   Set charge event
   @ id;mandatory;string; charge event id
     charge_event;mandatory;struct
   @ charge_event    username;mandatory;
                     name;optional;
                     valid_to;date;optional; 
                     amount;double;optional; 
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcTenant      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcId          AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldToDate      AS DATE      NO-UNDO.

DEFINE BUFFER buffFMItem FOR FMItem.

IF validate_request(param_toplevel_id, "string,string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcId     = get_string(param_toplevel_id, "1").
pcStruct = get_struct(param_toplevel_id, "2").

lcStruct = validate_struct(pcStruct, "username!,name,valid_to,amount").
pcUserName = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUserName) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{newton/src/settenant.i pcTenant}

/* check Feemodel */
FIND FeeModel WHERE 
     FeeModel.Brand = "1" AND 
     FeeModel.FeeModel = pcId NO-LOCK NO-ERROR.

IF NOT AVAIL FeeModel THEN
   RETURN appl_err("Charge event " + pcId + " not found ").

/* check FMItem */
FIND FIRST FMItem OF FeeModel WHERE 
           FMItem.ToDate >= TODAY AND 
           FMItem.FromDate <= TODAY NO-LOCK NO-ERROR.

IF NOT AVAIL FMItem THEN
   RETURN appl_err("Charge event " + pcId + " doesn't contain active item").

/* check valid_to */
ASSIGN ldToDate = get_date(pcStruct, "valid_to")
        WHEN LOOKUP("valid_to",lcStruct) > 0 .

/* allow empty string and convert it to default value */
IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
   gi_xmlrpc_error = 0.
   gc_xmlrpc_error = "".
   IF get_string(pcStruct,"valid_to") EQ "" THEN DO:
      ldToDate = 12/31/2049. 
   END.
   ELSE RETURN
      appl_err("Incorrect valid_to value: " + get_string(pcStruct,"valid_to")).
END.

IF LOOKUP("valid_to",lcStruct) > 0  AND ldToDate < FMItem.FromDate THEN 
    RETURN appl_err("Valid To date should be after: " + STRING(FMItem.FromDate)).


FIND CURRENT FeeModel EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF NOT AVAIL FeeModel THEN DO: 
      IF LOCKED FeeModel THEN 
         RETURN appl_err("This Charge/Comp event has been locked by other user, please try late").
      ELSE 
         RETURN appl_err("This Charge/Comp event has been deleted by other user").
END.

FIND CURRENT FMItem EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF NOT AVAIL FMItem THEN DO:
    IF LOCKED FMItem THEN
       RETURN appl_err("Item has been locked, please try late").
    ELSE 
       RETURN appl_err("Item has been deleted by other user").
END.

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhFMItem AS HANDLE NO-UNDO.
   lhFMItem = BUFFER FMItem:HANDLE.
   RUN StarEventInitialize(lhFMItem).
   RUN StarEventSetOldBuffer(lhFMItem).
END.

ASSIGN  
       FMItem.Amount = get_double(pcStruct, "amount") 
       WHEN LOOKUP("amount",lcStruct) > 0
       FMItem.ToDate = ldToDate WHEN LOOKUP("valid_to",lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhFMItem).
   fCleanEventObjects().
END.

RELEASE FMItem.

/* update FeeModel name */
IF LOOKUP("name",lcStruct) > 0 THEN DO:
  
   IF llDoEvent THEN DO:
      DEF VAR lhFeeModel AS HANDLE NO-UNDO.
      lhFeeModel = BUFFER FeeModel:HANDLE.
      RUN StarEventInitialize(lhFeeModel).
      RUN StarEventSetOldBuffer(lhFeeModel).
   END.

   ASSIGN FeeModel.FeeName = get_string(pcStruct,"name").
  
   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhFeeModel).
      fCleanEventObjects().
   END.

END. /* update FeeModel name */

RELEASE FeeModel.

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
