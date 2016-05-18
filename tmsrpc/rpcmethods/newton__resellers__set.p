/**
 * Set reseller
 *
 * @input string;mandatory;reseller id
    struct;mandatory;reseller data
 * @struct username;string;mandatory;
    name;string;optional;reseller name
    commisssion_percentage;double;optional;
    address;string;optional;
    email;string;optional;
    entity_code;int;optional;
    active;booleanoptional;
    bank_code_new;string;optional;
    bank_code_new_date;date;optional;
 * @output struct;mandatory;empty struct
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}
{Syst/tmsconst.i}

DEFINE TEMP-TABLE ttReseller LIKE Reseller.

DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
DEF VAR pcReseller AS CHAR NO-UNDO. 
DEF VAR pcBankCode AS CHAR NO-UNDO INIT ?.
DEF VAR pdaBankCodeFrom AS DATE NO-UNDO INIT ?.
DEF VAR llEqual AS LOG NO-UNDO. 
DEF VAR lcResellerTF AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,struct") = ? THEN RETURN.

pcReseller = get_nonempty_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct, 
   "name,email,entity_code,commission_percentage,bank_code_new," +
   "bank_code_new_date,address,email,active,username!").
 
IF lcStruct = ? THEN RETURN.

katun = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND Reseller EXCLUSIVE-LOCK WHERE
     Reseller.Brand = gcBrand AND
     Reseller.Reseller = pcReseller NO-ERROR.
IF NOT AVAIL Reseller THEN
   RETURN appl_err(SUBST("Reseller not found: &1",pcReseller)).

CREATE ttReseller.
BUFFER-COPY Reseller TO ttReseller.

ASSIGN
   ttReseller.RsName     = get_string(pcStruct, "name") 
                              WHEN LOOKUP("name",lcStruct) > 0
   ttReseller.EntityCode = get_int(pcStruct,"entity_code")
                              WHEN LOOKUP("entity_code",lcStruct) > 0
   ttReseller.CommPerc   = get_double(pcStruct, "commission_percentage") 
                              WHEN LOOKUP("commission_percentage",lcStruct) > 0
   ttReseller.Address[1] = get_string(pcStruct,"address")
                              WHEN LOOKUP("address",lcStruct) > 0
   ttReseller.Email      = get_string(pcStruct,"email")
                              WHEN LOOKUP("email",lcStruct) > 0
   ttReseller.Active     = get_bool(pcStruct,"active")
                              WHEN LOOKUP("active",lcStruct) > 0
   pcBankCode            = get_string(pcStruct,"bank_code_new")
                              WHEN LOOKUP("bank_code_new",lcStruct) > 0
   pdaBankCodeFrom       = get_date(pcStruct,"bank_code_new_date")
                              WHEN LOOKUP("bank_code_new_date",lcStruct) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcBankCode NE ? OR
   pdaBankCodeFrom NE ? THEN DO:

   IF pcBankCode EQ ? THEN
      RETURN appl_err("bank_code_new missing").
   IF pdaBankCodeFrom EQ ? THEN 
      RETURN appl_err("bank_code_new_date missing").

   IF LOOKUP(pcBankCode,{&TF_BANK_CODES} + ",0000") = 0 THEN  
      RETURN appl_err(SUBST("Incorrect bank code: &1",pcBankCode)).

   IF (Reseller.Fuc1 EQ "" OR 
       Reseller.Fuc2 EQ "") AND pcBankCode NE "0000" THEN
      RETURN appl_err("Reseller does not have FUC code defined").
         
   FIND FIRST ResellerTF OF Reseller NO-LOCK NO-ERROR.

   IF pdaBankCodeFrom <= TODAY THEN
      RETURN appl_err("Incorrect bank valid from date").

   IF AVAIL ResellerTF AND
            ResellerTF.ValidFrom > TODAY THEN DO:
      IF pcBankCode NE ResellerTF.TFBank OR
         pdaBankCodeFrom NE ResellerTF.ValidFrom THEN DO:
         FIND CURRENT ResellerTF EXCLUSIVE-LOCK.
         lcResellerTF = "MODIFY".
      END.
   END.
   ELSE IF pcBankCode NE "" OR
      AVAIL ResellerTF THEN lcResellerTF = "ADD".
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {Func/lib/eventlog.i}
   DEF VAR lhReseller AS HANDLE NO-UNDO.
   DEF VAR lhResellerTF AS HANDLE NO-UNDO.
   lhReseller = BUFFER Reseller:HANDLE.
   RUN StarEventInitialize(lhReseller).
   lhResellerTF = BUFFER ResellerTF:HANDLE.
   RUN StarEventInitialize(lhResellerTF).
END.

BUFFER-COMPARE ttReseller TO Reseller SAVE llEqual.

IF NOT llEqual THEN DO:
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhReseller).
   BUFFER-COPY ttReseller EXCEPT Brand Reseller TO Reseller.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhReseller).
END.

IF lcResellerTF EQ "ADD" THEN DO:
   CREATE ResellerTF.
   ASSIGN
      ResellerTF.Brand     = gcBrand
      ResellerTF.Reseller  = ttReseller.Reseller
      ResellerTF.TFBank    = pcBankCode
      ResellerTF.ValidFrom = pdaBankCodeFrom.
   IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhResellerTF).
END.
ELSE IF lcResellerTF EQ "MODIFY" THEN DO:

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhResellerTF).
   ASSIGN
      ResellerTF.TFBank = pcBankCode
      ResellerTF.ValidFrom = pdaBankcodeFrom.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhResellerTF).
END.

add_struct(response_toplevel_id, "").

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   EMPTY TEMP-TABLE ttReseller.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
