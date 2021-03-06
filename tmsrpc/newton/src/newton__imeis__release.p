/**
 * Release IMEIs
 *
 * @input array of structs;
 * @input_struct;struct;mandatory;
    order_id;int;mandatory;
    imei;string;mandatory;
    imei_status;int;mandatory;0 = IMEI_STATUS_UNKNOWN ja 3 = IMEI_STATUS_RELEASED
 * @output boolean;true
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Newton".
{Syst/tmsconst.i}

DEFINE VARIABLE pcArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "array") EQ ? THEN RETURN.
pcArray = get_array(param_toplevel_id,"0").

DEFINE TEMP-TABLE ttIMEI NO-UNDO
   FIELD orderid AS INT
   FIELD imei AS CHAR
   FIELD imeiStatus AS int
INDEX orderid IS PRIMARY UNIQUE orderid. 

DO liCounter = 0 TO get_paramcount(pcArray) - 1:
   
   pcStruct = get_struct(pcArray, STRING(liCounter)).
   lcStruct = validate_request(pcStruct,"imei!,imei_status!,order_id!").
   
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   CREATE ttIMEI.
   ASSIGN
      ttIMEI.IMEI = get_string(pcStruct,"imei")
      ttIMEI.OrderId = get_int(pcStruct,"order_id")
      ttIMEI.IMEIStatus = get_int(pcStruct,"imei_status").
   
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   {newton/src/findtenant.i YES ordercanal Order OrderId ttIMEI.OrderId}

   IF ttIMEI.IMEIStatus NE {&IMEI_STATUS_RELEASED} AND
      ttIMEI.IMEIStatus NE {&IMEI_STATUS_UNKNOWN} THEN
      RETURN appl_err(SUBST("Unknown IMEI status &1, ttIMEI.ImeiStatus")).

   FIND OrderAccessory WHERE
        OrderAccessory.Brand = "1" AND
        OrderAccessory.OrderId = ttImei.OrderId AND
        OrderAccessory.IMEI = ttImei.IMEI AND
        OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderAccessory THEN RETURN  
      appl_err(SUBST("Order &1 with IMEI &2 was not found", 
            ttImei.OrderId, ttImei.IMEI)).
   
END.

{Syst/eventval.i}
   
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun 
   {Func/lib/eventlog.i}
   DEF VAR lhOrderAccessory AS HANDLE NO-UNDO.
   lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
   RUN StarEventInitialize(lhOrderAccessory).
END.

FOR EACH ttIMEI NO-LOCK:
   
   {newton/src/findtenant.i YES ordercanal Order OrderId ttIMEI.OrderId}
   
   FIND OrderAccessory WHERE
        OrderAccessory.Brand = Syst.Var:gcBrand AND
        OrderAccessory.OrderId = ttImei.OrderId AND
        OrderAccessory.IMEI = ttImei.IMEI AND
        OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL OrderAccessory THEN NEXT.
    
   RUN StarEventSetOldBuffer(lhOrderAccessory).

   ASSIGN
      OrderAccessory.IMEIStatus = ttImei.IMEIStatus
      OrderAccessory.IMEIReleased = TODAY.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAccessory).

END.

IF llDoEvent THEN fCleanEventObjects().

add_boolean(response_toplevel_id,"", true).

FINALLY:
   EMPTY TEMP-TABLE ttIMEI.
   END.
