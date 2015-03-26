/**
 * Fetch YOIGOYOIGO daycampaign counter value 
 * and return SMS message containing the value
 *
 * @input   MSISDN;String
 *
 * @output smsMessage;String
 *
 */

{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{commpaa.i}
katun = "XMLRPC-Heat".
gcBrand = "1".
{fcreatereq.i}

FUNCTION fReqStat RETURNS LOGICAL
(iiStat AS INTEGER,
 icMsg AS CHAR):
   ASSIGN
      bCreaReq.donestamp = fMakeTS()
      bCreaReq.updatestamp = fMakeTS()
      bCreaReq.reqstatus = iiStat
      bCreaReq.memo = icMsg.
END FUNCTION. 

/* Input */
DEF VAR pcCLI AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
if gi_xmlrpc_error NE 0 THEN RETURN.

fCreateRequest(37, /* heat balance query request */
               0 , /* chgstamp */
               "XMLRPC", /* creator */
               FALSE, /* create fees */
               FALSE). /* send sms */

/* forced ok, no request handling in this version, just for log puposes */
fReqStat(2,"Handling done by XMLRPC agent").
bCreaReq.ReqCParam1 = pcCLI.


FIND FIRST invtext NO-LOCK WHERE 
           invtext.brand = "1" AND
           invtext.target = "SMS" AND
           invtext.keyvalue = "YOIGOYOIBALNOK" AND
           invtext.fromdate <= TODAY AND
           invtext.todate >= TODAY NO-ERROR.
IF NOT AVAIL invtext THEN DO:
   fReqStat(9, "YOIGOYOIBALNOK param missing").

   RETURN appl_err("Yoigo Info: Ahora mismo no podemos darte " +
      "esta informacion vuelve a enviarnos el mensaje mas " +
      "tarde, por favor. Gracias.").
END.      

DEFINE VARIABLE lcErrText AS CHARACTER NO-UNDO. 
lcErrText = invtext.invtext.

FIND FIRST mobsub NO-LOCK WHERE  
           mobsub.cli = pcCLI NO-ERROR.
IF NOT AVAIL mobsub THEN DO:
   fReqStat(9, "Mobsub missing").
   RETURN appl_err(lcErrText).
END.   

bCreaReq.msseq = mobsub.msseq.
bCreaReq.custnum = mobsub.custnum.
bCreaReq.CLI = mobsub.cli.

FIND FIRST DCCli NO-LOCK WHERE
           DCCli.Brand = "1" AND
           DCCli.dcevent = "YOIGOYOIGO" AND
           DCCli.msseq = mobsub.msseq AND
           DCCli.validto >= TODAY NO-ERROR.
IF NOT AVAIL DCCli THEN DO: 
   
   fReqStat(9, "No DCCli for mobsub").
   RETURN appl_err(lcErrText).
END.   

FIND FIRST DCCounter NO-LOCK WHERE  
           DCCounter.msseq = mobsub.msseq AND
           DCCounter.dcdate = TODAY AND
           DCCounter.dcevent = "YOIGOYOIGO" NO-ERROR.

DEFINE VARIABLE ldeAmount AS DECIMAL NO-UNDO. 

IF AVAIL DCCounter THEN 
   ldeAmount = DCCounter.Amount.
ELSE
   /* counter for today not yet generated,  saldo is 00:00 min) */
   ldeAmount = 0.
   

DEFINE VARIABLE lcMsg AS CHARACTER NO-UNDO. 

FIND FIRST invtext NO-LOCK WHERE 
           invtext.brand = "1" AND
           invtext.target = "SMS" AND
           invtext.keyvalue = "YOIYOIBALANCE" AND
           invtext.fromdate <= TODAY AND
           invtext.todate >= TODAY NO-ERROR.
IF NOT AVAIL invtext THEN DO:
   fReqStat(9, "YOIYOIBALANCE param missing").
   RETURN appl_err(lcErrText).
END.   

lcMsg = invtext.invtext.

/* Text message is something like this...
   "Yoigo Info:Hoy has hablado #TIME minutos por 0 " +
   "cent con otros Yoigos. Tienes hasta 60 min/dia a 0 cent, " +
   "resto de llamadas nac. a 12 cent/min. Establec. 12 cent.".
*/

/* 60 min can't be displayed in mm:ss format, hack that */
DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO. 

IF ldeAmount >= 3600 THEN
   lcTime = "60:00".
ELSE 
   lcTime = SUBSTRING(STRING(INT(ldeAmount) ,"hh:mm:ss"),4).

lcMsg = REPLACE(lcMsg,
               "#TIME", 
               lcTime).

/* set the response value */
add_string(response_toplevel_id, "", lcMsg).

fReqStat(2, lcMsg).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
