/**
 * Send message via SMS to a given number.
 *
 * @input struct;params  
   @params msisdn;string;mandatory;receiver's number
           content;string;mandatory;message content
           sender_cli;string;optional;sender cli (default is "622")
           send_time;datetime;optional;timestamp when message should be sent (defaults to now)
 * @output boolean;True
 */

{xmlrpc/xmlrpc_access.i}
{date.i}

/* Input parameters */
DEF VAR pcCli AS CHAR NO-UNDO.
DEF VAR pcMessage AS CHAR NO-UNDO.
DEF VAR pfWhen AS DECIMAL NO-UNDO.
/* Local variables */
DEF VAR lcReq AS CHAR NO-UNDO.
DEF VAR pcReq AS CHAR NO-UNDO.
DEF VAR lcOrig  AS CHAR NO-UNDO INITIAL "622".
DEF VAR liCreditType AS INT NO-UNDO INITIAL 9 . /* info*/
DEF VAR ldeQuarantine AS DEC NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcReq = get_struct(param_toplevel_id, "0").
lcReq = validate_request(pcReq, "msisdn!,content!,sender_cli,send_time").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pfWhen = {&nowts}.

ASSIGN
   pcCli = get_string(pcReq, "msisdn")
   pcMessage = get_string(pcReq, "content")
   lcOrig = get_string(pcReq,"sender_cli")
      WHEN LOOKUP("sender_cli", lcReq) > 0
   pfWhen = get_timestamp(pcReq, "send_time")
      WHEN LOOKUP("sender_time", lcReq) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcCLi BEGINS "+" THEN pcCLi = REPLACE(pcCLI, "+","00").

IF lcOrig EQ "sms report" THEN liCreditType = 25 . 

ldeQuarantine = fSecOffSet(fMakeTS(), -10).

FIND FIRST CallAlarm WHERE
           CallAlarm.CLI = pcCLI AND
           CallAlarm.DeliStat = 1 AND
           CallAlarm.Actstamp >= ldeQuarantine AND
           CallAlarm.DeliMsg EQ pcMessage NO-LOCK USE-INDEX CLI_s NO-ERROR.
IF AVAIL CallAlarm THEN RETURN "duplicate_message".

CREATE CallAlarm.

ASSIGN
    CallAlarm.CLSeq      =  1
    CallAlarm.CASeq      =  NEXT-VALUE(CallAlarm)
    CallAlarm.CLI        =  pcCli
    CallAlarm.ActStamp   =  pfWhen
    CallAlarm.DeliStat   =  1
    CallAlarm.Delitype   =  1   /* SMS */
    CallAlarm.DeliPara   = "1"
    CallAlarm.Delimsg    =  pcMessage
    CallAlarm.Limit      =  0
    CallAlarm.CreditType =  liCreditType   
    CallAlarm.Brand      = "1"
    CallAlarm.Orig       = lcOrig
.

add_string(response_toplevel_id,"",STRING(ROWID(CallAlarm))).
