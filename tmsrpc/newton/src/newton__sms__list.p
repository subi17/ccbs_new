/**
 * Get call alarm ids.
 *
 * @input msseq;int;mandatory;subscription id
          limit;int;optional;
          offset;int;optional;
          type_name;string;optional;type of sms (normal/premium)
 * @output struct;array of callalarm rowid 
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcIdStruct AS CHARACTER NO-UNDO. 
DEF VAR liMsSeq AS INTEGER NO-UNDO. 
DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
DEF VAR liOffSet AS INTEGER NO-UNDO. 
DEF VAR liCount AS INTEGER NO-UNDO. 
DEF VAR lcType AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_struct(pcStruct, "msseq!,limit,offset,type_name").
IF gi_xmlrpc_error NE 0 THEN RETURN.

liMsSeq = get_int(pcStruct,"msseq").
IF LOOKUP("limit",lcStruct) > 0 THEN 
  liLimit = get_int(pcStruct,"limit").
IF LOOKUP("offset",lcStruct) > 0 THEN 
  liOffSet = get_int(pcStruct,"offset").
IF LOOKUP("type_name",lcStruct) > 0 THEN 
  lcType = get_string(pcStruct,"type_name").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(lcType,",normal,premium") = 0 THEN RETURN
   appl_err(SUBST("Unknown type: %1", lcType)).

/* find mobsub */
FIND MobSub WHERE
     MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR. 

IF NOT AVAIL MobSub THEN RETURN appl_err("MobSub not found").

lcResultStruct = add_struct(response_toplevel_id, "").
lcIdStruct = add_array(lcResultStruct, "results").

FOR EACH CallAlarm NO-LOCK WHERE
         CallAlarm.Brand = "1" AND
         CallAlarm.CLI = MobSub.CLI AND
         CallAlarm.DeliType = 1 AND
         CallAlarm.CreditType NE 25:

   IF (lcType EQ "normal" AND CallAlarm.CreditType EQ {&SMSTYPE_PREMIUM}) OR
      (lcType EQ "premium" AND CallAlarm.CreditType NE {&SMSTYPE_PREMIUM})
      THEN NEXT.
   
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.Brand = "1" AND
              MsOwner.CLI = CallAlarm.CLI AND
              MsOwner.TsEnd >= CallAlarm.ActStamp AND
              MSOwner.TsBegin <= CallAlarm.ActStamp AND 
              MsOwner.MsSeq = MobSub.MsSeq NO-ERROR. 

   IF NOT AVAIL MsOwner THEN NEXT .
   liCount = liCount + 1.
   IF liCount <= liOffSet THEN NEXT.
   IF liCount > liLimit + liOffSet THEN NEXT.

   add_string(lcIdStruct, "", STRING(ROWID(CallAlarm))). 
END.

add_int(lcResultStruct, "total_amount", liCount). 
