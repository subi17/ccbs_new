/**
 * Update returned terminals
 *
 * @input  struct;
           imei;string;mandatory
           msisdn;string;mandatory
           bill_code;string;mandatory
           device_start;boolean;mandatory
           device_screen;boolean;mandatory
           return_channel;string;mandatory

 * @output success;boolean
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{tmsconst.i}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   DEFINE VARIABLE lhTermReturn AS HANDLE NO-UNDO.
   lhTermReturn = BUFFER TermReturn:HANDLE.
   RUN StarEventInitialize(lhTermReturn).
END.

DEF VAR pcStruct    AS CHAR NO-UNDO.
DEF VAR lcStruct    AS CHAR NO-UNDO.

DEF VAR lhBuff           AS HANDLE NO-UNDO.
DEF VAR lcIMEI           AS CHAR   NO-UNDO.
DEF VAR lcMSISDN         AS CHAR   NO-UNDO.
DEF VAR lcBillCode       AS CHAR   NO-UNDO.
DEF VAR llDeviceStart    AS LOG    NO-UNDO.
DEF VAR llDeviceScreen   AS LOG    NO-UNDO.
DEF VAR lcReturnChannel  AS CHAR   NO-UNDO.
DEF VAR ldReturnTS       AS DEC    NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_request(pcStruct, "imei!,msisdn!,bill_code!,device_start!,device_screen!,return_channel!").

ASSIGN
   lcIMEI            = get_string(pcStruct,"imei")
   lcMSISDN          = get_string(pcStruct,"msisdn")
   lcBillCode        = get_string(pcStruct,"bill_code")
   llDeviceStart     = get_bool(pcStruct,"device_start")
   llDeviceScreen    = get_bool(pcStruct,"device_screen")
   lcReturnChannel   = get_string(pcStruct,"return_channel")
   ldReturnTS        = fMakeTS().

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST TermReturn EXCLUSIVE-LOCK WHERE
           TermReturn.IMEI = lcIMEI 
           NO-ERROR.

IF NOT AVAILABLE TermReturn THEN 
   CREATE TermReturn.
ELSE IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTermReturn).

ASSIGN TermReturn.IMEI           = lcIMEI
       TermReturn.MSISDN         = lcMSISDN
       TermReturn.BillCode       = lcBillCode
       TermReturn.DeviceStart    = llDeviceStart
       TermReturn.DeviceScreen   = llDeviceScreen
       TermReturn.ReturnChannel  = lcReturnChannel
       TermReturn.ReturnTs       = ldReturnTS.

IF llDoEvent THEN DO:
   IF NEW TermReturn THEN RUN StarEventMakeCreateEvent(lhTermReturn).
   ELSE RUN StarEventMakeModifyEvent(lhTermReturn).
END.

RELEASE TermReturn.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.