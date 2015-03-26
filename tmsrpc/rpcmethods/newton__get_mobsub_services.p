/**
 * List of services and their status for a subscription
 *
 * @Input msseq;int;mandatory;the subscription
 * @output services;array;a list of structs
 * @service service_id;string;newton alias for a service
            value;string/int;Status of the service (on/off)
            params;struct;Optionally additional information
 */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
katun    = "NewtonAd".
gcBrand  = "1".
{tmsconst.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Local variables */
DEF VAR lii             AS INT NO-UNDO.
DEF VAR lcBarrComList   AS CHAR NO-UNDO.
DEF VAR lcBarrStatus    AS CHAR NO-UNDO.

/* Output parameters */
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR params_struct   AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND MobSub NO-LOCK WHERE
     MobSub.MsSeq = piMsSeq AND
     MobSub.Brand = gcBrand NO-ERROR.
IF NOT AVAILABLE MobSub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

top_array = add_array(response_toplevel_id, "").

DEFINE VARIABLE OnOffAlias AS CHARACTER NO-UNDO INIT "VMS,LANG,CF,IRDCUTOFF,BB,NAM,LTE,LP,C_BPSUB,Y_BPSUB".
DEFINE VARIABLE OnOffTms   AS CHARACTER NO-UNDO INIT "VMS,LANG,CF,IRDCUTOFF,BB,NAM,LTE,LP,BPSUB,BPSUB".
DEFINE VARIABLE lcService  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServiceAlias  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO. 

/* Loop over all services of that Mobsub */
SERVICE_LOOP:
DO lii = 1 TO NUM-ENTRIES(OnOffTms):

   ASSIGN
      lcService      = ENTRY(lii,OnOffTms,",")
      lcServiceAlias = ENTRY(lii,OnOffAlias,",").

   top_struct = add_struct(top_array, "").
   add_string(top_struct, "service_id", lcServiceAlias).

   FOR FIRST SubSer NO-LOCK WHERE
             SubSer.MsSeq = MobSub.MsSeq AND
             SubSer.ServCom = lcService,
       FIRST ServCom NO-LOCK WHERE
             ServCom.Brand = gcBrand AND
             ServCom.ServCom = SubSer.ServCom:

      /* Easy On-Off services */
      IF SubSer.ServCom = "BB" AND
         SubSer.SSStat EQ 2 THEN /* BB reset status */ 
        lcValue = "off".
      ELSE IF SubSer.ServCom EQ "BPSUB" AND
              SubSer.SSStat EQ 1 THEN DO:
         lcValue = STRING(SubSer.SSParam EQ lcServiceAlias,"on/off").
      END.
      ELSE
        lcValue =  TRIM(STRING(SubSer.SSStat EQ 0, "off/on")).
      
      add_string(top_struct, "value", lcValue).

      IF LOOKUP(SubSer.ServCom,"LANG,CF") > 0 AND SubSer.SSStat NE 0 THEN DO:
         params_struct = add_struct(top_struct, "params").
         add_int(params_struct, lcServiceAlias, SubSer.SSStat).
      END.

   END.
      
   /* if ongoing request for this service */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq      = MobSub.MsSeq AND
              MsRequest.ReqType    = 1       AND
              MsRequest.ReqCParam1 = lcService AND
        LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0 NO-ERROR.
   IF AVAIL MsRequest AND
      (MsRequest.ReqCParam1 NE "BPSUB" OR
      (MsRequest.ReqCParam2 EQ lcServiceAlias OR
      (MsRequest.ReqCParam2 EQ "" AND
         AVAIL SubSer AND SubSer.SSParam EQ lcServiceAlias))) THEN
      add_boolean(top_struct,"ongoing_request",TRUE).

   IF NOT AVAIL SubSer THEN
      add_string(top_struct, "value", "off").
END.

RUN checkmsbarring.p(
      INPUT piMsSeq,
      INPUT katun,
      OUTPUT lcBarrComList,
      OUTPUT lcBarrStatus).

DO lii = 1 TO NUM-ENTRIES(lcBarrComList,"|"):
   top_struct = add_struct(top_array, ""). 
   IF ENTRY(lii,lcBarrComList,"|") BEGINS "UN" THEN DO:
      add_string(top_struct, "service_id",
         SUBSTRING(ENTRY(lii,lcBarrComList,"|"),3)).
      add_string(top_struct, "value", "on").
   END.
   ELSE DO:
      add_string(top_struct, "service_id", ENTRY(lii,lcBarrComList,"|")).
      add_string(top_struct, "value", "off").
   END.
   /* if ongoing request for this barring */
   IF lcBarrStatus NE "ONC" THEN NEXT. 
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = piMsSeq AND
                     MsRequest.ReqType    = 35       AND
                     INDEX(MsRequest.ReqCParam1,
                           ENTRY(lii,lcBarrComList,"|")) > 0  AND
                     LOOKUP(STRING(MsRequest.ReqStat),"2,4,9") = 0) THEN DO:
       add_boolean(top_struct,"ongoing_request",TRUE).
   END.

END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
