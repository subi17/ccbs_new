/**
 * Get events ids (VFR - Vista For Retailers)
 *
 * @input conditions;struct;mandatory;
          username;string;mandatory;
          event_type;string;optional;vm,icc,stc,acc,data,imei
          date_start;datetime;optional;
          date_end;datetime;optional;
   
 * @output struct;array of event ids
*/

{newton/src/flistrpc.i}
{Func/timestamp.i}

DEFINE VARIABLE pcTenant AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUsername AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcEvent AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pdeDateStart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE pdeDateEnd AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcQuery AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcParams AS CHARACTER NO-UNDO. 

lcStruct = validate_struct(pcStruct, "brand!,username!,event_type,date_start,date_end").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcTenant = get_string(pcStruct,"brand")
   pcUsername = get_string(pcStruct,"username")
   pdeDateStart = get_timestamp(pcStruct,"date_start") WHEN LOOKUP("date_start", lcStruct) > 0
   pdeDateEnd = get_timestamp(pcStruct,"date_end") WHEN LOOKUP("date_end", lcStruct) > 0
   pcEvent = get_string(pcStruct,"event_type") WHEN LOOKUP("event_type", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUserName) EQ "" THEN RETURN appl_err("username is empty").

pcUserName = "VISTA_" + pcUserName.

{newton/src/settenant.i pcTenant}

lcQuery = "FOR EACH MSRequest NO-LOCK WHERE MSRequest.Brand = "
            + QUOTER(gcBrand) + " AND MSRequest.UserCode = " + QUOTER(pcUserName).

IF pdeDateStart NE 0 THEN lcQuery = lcQuery + " AND MsRequest.ActStamp > " + QUOTER(pdeDateStart).
IF pdeDateEnd NE 0 THEN lcQuery = lcQuery + " AND MsRequest.ActStamp < " + QUOTER(pdeDateEnd).

CASE pcEvent:
   WHEN "vm" THEN lcParams = ' AND MsRequest.ReqType = 1 AND LOOKUP(MsRequest.ReqCParam1,"CF,LANG,VMS") > 0'.
   WHEN "icc" THEN lcParams =  " AND MsRequest.ReqType = 15".
   WHEN "stc" THEN lcParams = " AND MsRequest.ReqType = 0".
   WHEN "acc" THEN lcParams = " AND MsRequest.ReqType = 10".
   WHEN "data" THEN lcParams = " AND MsRequest.ReqType = 78".
   WHEN "imei" THEN lcParams = " AND MsRequest.ReqType = 80".
   WHEN "" THEN lcParams = ' AND (LOOKUP(STRING(MsRequest.ReqType),"0,10,15,78,80") > 0' +
                          ' OR (MsRequest.ReqType = 1 AND LOOKUP(MsRequest.ReqCParam1,"CF,LANG,VMS") > 0))'.
   OTHERWISE RETURN appl_err(SUBST("Unknown event type:i &1", pcEvent)).
END.

lcQuery = lcQuery + lcParams + " USE-INDEX UserCode".

fListQuery(
   "MsRequest",
   lcQuery,
   "MsRequest").
