/**
 * Add reseller
 *
 * @input struct;mandatory;reseller data
 * @struct id;string;mandatory;reseller id
    username;string;mandatory;
    name;string;mandatory;reseller name
    commisssion_percentage;double;optional;
    address;string;optional;
    email;string;optional;
    entity_code;int;optional;
    active;booleanoptional;
 * @output struct;mandatory;empty struct
*/

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}
{Syst/tmsconst.i}

DEFINE TEMP-TABLE ttReseller LIKE Reseller
   FIELD BankCode LIKE ResellerTF.TFBank
   FIELD BankCodeFrom LIKE ResellerTF.ValidFrom.

DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRespStruct AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

lcStruct = validate_request(pcStruct, 
   "id!,name!,email,entity_code,commission_percentage," +
   "address,email,active,username!").
 
IF lcStruct = ? THEN RETURN.

katun = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

CREATE ttReseller.
ASSIGN
   ttReseller.Brand      = gcBrand
   ttReseller.Reseller   = get_string(pcStruct, "id")
   ttReseller.RsName     = get_string(pcStruct, "name")
   ttReseller.EntityCode = (IF LOOKUP("entity_code",lcStruct) > 0
                            THEN get_int(pcStruct,"entity_code") ELSE ?)
   ttReseller.CommPerc   = get_double(pcStruct, "commission_percentage") 
                              WHEN LOOKUP("commission_percentage",lcStruct) > 0
   ttReseller.Address[1] = get_string(pcStruct,"address")
                              WHEN LOOKUP("address",lcStruct) > 0
   ttReseller.Email      = get_string(pcStruct,"email")
                              WHEN LOOKUP("email",lcStruct) > 0
   ttReseller.Active     = get_bool(pcStruct,"active")
                              WHEN LOOKUP("active",lcStruct) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.


FIND Reseller NO-LOCK WHERE
     Reseller.Brand = gcBrand AND
     Reseller.Reseller = ttReseller.Reseller NO-ERROR.
IF AVAIL Reseller THEN
   RETURN appl_err(SUBST("Reseller already exists: &1", ttReseller.Reseller)).

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {lib/eventlog.i}
   DEF VAR lhReseller AS HANDLE NO-UNDO.
   lhReseller = BUFFER Reseller:HANDLE.
   RUN StarEventInitialize(lhReseller).
END.

CREATE Reseller.
BUFFER-COPY ttReseller TO Reseller.
VALIDATE Reseller.

IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhReseller).

lcRespStruct = add_struct(response_toplevel_id, "").

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   EMPTY TEMP-TABLE ttReseller.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
