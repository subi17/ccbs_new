/**
 * Save ATM Topup event to TMS  
 * 
 * @input topup;struct;mandatory;topup event data
 * @topup 
   type;string;mandatory;recharge, cancel or enquiry
   received_at;datetime;mandatory;
   date;datetime;mandatory;
   entity_index;int;mandatory;
   reference;string;mandatory;
   num_oper;string;mandatory;
   local_code;string;mandatory;
   subscriber_number;string;mandatory;
   amount_with_tax;int;optional;
   amount_without_tax;int;optional;
   cancelled_amount_with_tax;int;optional;
   cancelled_amount_without_tax;int;optional;
   double_message_status;string;mandatory;
   tax_zone;int;optional;mandatory with recharge and cancel type
   tax_percent;int;optional;mandatory with recharge and cancel type
   origin_entity;string;mandatory;
   postal_code;string;mandatory;
   air_result_code;int;optional;
   netplus_result_code;int;mandatory;
 
 * @output returncode;integer
 * @returncode 0;OK
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO INIT "1".
{Func/date.i}
{Func/xmlfunction.i}

DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR lcFields AS CHARACTER NO-UNDO. 
DEF VAR ldtDate AS DATETIME NO-UNDO.
DEF VAR ldeReceived AS DEC NO-UNDO.
DEF VAR lcOut AS CHARACTER NO-UNDO. 
DEF VAR liNetPlusResponse AS INTEGER NO-UNDO. 
DEF VAR liAmountWithTax AS INTEGER NO-UNDO. 
DEF VAR liAmountWithoutTax AS INTEGER NO-UNDO. 
DEF VAR lcCommLine AS CHARACTER NO-UNDO. 
DEF VAR lcType AS CHARACTER NO-UNDO. 
DEF VAR lcEntIndex AS CHARACTER NO-UNDO. 
DEF VAR lcDoubleMessageStatus AS CHARACTER NO-UNDO.
DEF VAR lcNumOper AS CHARACTER NO-UNDO. 
DEF VAR lcTaxPerc AS CHARACTER NO-UNDO. 
DEF VAR liSign AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttPrepaidRequest NO-UNDO LIKE PrepaidRequest.

pcStruct = get_struct(param_toplevel_id, "0").

lcFields = validate_struct(pcStruct, "type!,received_at!,date!,entity_index!,reference!,num_oper!,local_code!,subscriber_number!,amount_with_tax,amount_without_tax,cancelled_amount_with_tax,cancelled_amount_without_tax,double_message_status,tax_zone,tax_percent,origin_entity!,postal_code!,air_result_code,netplus_result_code!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

PROCEDURE pDoubleCheck:

   DEFINE BUFFER bufPP FOR PrePaidRequest.
   DEF VAR lcReturn AS CHARACTER NO-UNDO init "OK". 

   FOR EACH bufPP NO-LOCK WHERE
            bufPP.Entidad    = PrepaidRequest.Entidad AND
            bufPP.Fecha      = PrepaidRequest.Fecha AND
            bufPP.ClaveLocal = PrepaidRequest.ClaveLocal AND
            bufPP.Source     = "ATM"        AND
            bufPP.PPStatus   = 2:            

      IF RECID(bufPP) NE RECID(PrepaidRequest) THEN DO:
         IF fGetNodeValue(bufPP.CommLine,"numOper") EQ lcNumOper 
         THEN lcReturn = "Double". 
      END.
   END.

   RETURN lcReturn.

END PROCEDURE.

liSign = 1.
lcType = get_string(pcStruct, "type").
CASE lcType:
   WHEN "recharge" THEN lcType = "RCG". 
   WHEN "cancel" THEN DO:
      lcType = "ANT". 
      liSign = -1.
   END.
   WHEN "enquiry" THEN lcType = "ENQ". 
   OTHERWISE RETURN appl_err("Unknown type: " + lcType).
END.

CREATE ttPrepaidRequest.
ttPrepaidRequest.Request = lcType.
ttPrepaidRequest.Brand = gcBrand.
ttPrepaidRequest.CLI = get_string(pcStruct, "subscriber_number").
ttPrepaidRequest.Reference = get_string(pcStruct, "reference").

IF lcType EQ "ENQ" THEN ASSIGN 
   ttPrepaidRequest.TaxZone = STRING(get_int(pcStruct, "tax_zone")) WHEN LOOKUP("tax_zone",lcFields) > 0
   lcTaxPerc = STRING(get_int(pcStruct, "tax_percent")) WHEN LOOKUP("tax_percent",lcFields) > 0.
ELSE ASSIGN
   ttPrepaidRequest.TaxZone = STRING(get_int(pcStruct, "tax_zone"))
   lcTaxPerc = STRING(get_int(pcStruct, "tax_percent")).

/* Adapter tax zones differ from tax zones in TMS */
CASE ttPrepaidRequest.TaxZone:
   WHEN "2" THEN ttPrepaidRequest.TaxZone = "3". /* Ceuta 8% */
   WHEN "3" THEN ttPrepaidRequest.TaxZone = "4". /* Melilla 4% */
   WHEN "4" THEN ttPrepaidRequest.TaxZone = "2". /* Canary Islands 0% */
END.

ttPrepaidRequest.ClaveLocal = get_string(pcStruct, "local_code").

ASSIGN
   ldtDate = get_datetime(pcStruct,"date")
   ldeReceived = get_timestamp(pcStruct,"received_at")
   ttPrepaidRequest.TSRequest = ldeReceived.

lcEntIndex = string(get_int(pcStruct, "entity_index")).
liNetPlusResponse = get_int(pcStruct,"netplus_result_code").

ASSIGN
   ttPrepaidRequest.Entidad = fill("0",2 - length(lcEntIndex)) + lcEntIndex
   ttPrepaidRequest.Fecha = string(year(ldtDate),"9999") + 
                            string(month(ldtDate),"99") + 
                            string(day(ldtDate),"99")
   ttPrepaidRequest.PPReqPrefix = "900"
   ttPrepaidRequest.PPStatus = 2
   ttPrepaidRequest.Source = "ATM"
   ttPrepaidRequest.UserCode = "ATM_ADAPTER".


IF ttPrepaidRequest.Request NE "ENQ" THEN DO: 
  
   IF ttPrepaidRequest.Request EQ "ANT" AND liNetPlusResponse NE 150 THEN ASSIGN
      liAmountWithoutTax = get_int(pcStruct, "amount_without_tax") WHEN LOOKUP("amount_without_tax",lcFields) > 0
      liAmountWithTax = get_int(pcStruct, "amount_with_tax") WHEN LOOKUP("amount_with_tax",lcFields) > 0.
   ELSE ASSIGN
      liAmountWithoutTax = get_int(pcStruct, "amount_without_tax")
      liAmountWithTax = get_int(pcStruct, "amount_with_tax").
   
   IF ttPrepaidRequest.Request EQ "ANT" AND 
      LOOKUP("cancelled_amount_without_tax", lcFields) > 0 THEN ASSIGN
      ttPrepaidRequest.TopupAmt = get_int(pcStruct, "cancelled_amount_without_tax")
      ttPrepaidRequest.VatAmt = get_int(pcStruct, "cancelled_amount_with_tax") - ttPrepaidRequest.TopupAmt.
   ELSE ASSIGN 
      ttPrepaidRequest.TopupAmt = liAmountWithoutTax 
      ttPrepaidRequest.VatAmt = liAmountWithTax - liAmountWithoutTax.

   ttPrepaidRequest.TopupAmt = ttPrepaidRequest.TopupAmt * liSign. 
   ttPrepaidRequest.VatAmt = ttPrepaidRequest.VatAmt * liSign.
   
END.

ASSIGN
   lcNumOper = get_string(pcStruct,"num_oper")
   lcDoubleMessageStatus = get_string(pcStruct,"double_message_status") WHEN LOOKUP("double_message_status",lcFields) > 0.

ttPrepaidRequest.CommLine =
   "<params>" + 
   "<amount_with_tax>" + (IF LOOKUP("amount_with_tax", lcFields) > 0 THEN 
                         STRING(get_int(pcStruct, "amount_with_tax")) ELSE "N/A") + "</amount_with_tax>" +
   "<amount_without_tax>" + (IF LOOKUP("amount_without_tax", lcFields) > 0 THEN 
                            STRING(get_int(pcStruct, "amount_without_tax")) ELSE "N/A") + "</amount_without_tax>" + 
   "<air_result_code>" + (IF LOOKUP("air_result_code", lcFields) > 0 THEN 
      STRING(get_int(pcStruct, "air_result_code")) ELSE "N/A") + 
   "</air_result_code>" + 
   "<double_message_status>" + (IF LOOKUP("double_message_status", lcFields) > 0 THEN lcDoubleMessageStatus ELSE "N/A") + "</double_message_status>" + 
   "<codFin>" + STRING(liNetPlusResponse) + "</codFin>" + 
   "<numOper>" + lcNumOper  + "</numOper>" + 
   "<entidadOrigen>" + get_string(pcStruct,"origin_entity") + "</entidadOrigen>" +
   "<codPos>" + get_string(pcStruct,"postal_code") + "</codPos>" + 
   "<tax_percent>" + (IF LOOKUP("tax_percent", lcFields) > 0 THEN lcTaxPerc ELSE "N/A") + "</tax_percent>"
   + "</params>" .

IF gi_xmlrpc_error NE 0 THEN RETURN.

ttPrepaidRequest.PPRequest = NEXT-VALUE(PrePaidReq).

FIND MobSub WHERE
     MobSub.CLI = ttPrepaidRequest.CLI NO-LOCK NO-ERROR.
IF AVAIL MobSub THEN ttPrepaidRequest.MsSeq = MobSub.MsSeq.

BUFFER-COPY ttPrepaidRequest TO PrepaidRequest.
EMPTY TEMP-TABLE ttPrepaidRequest.

CASE PrepaidRequest.Request:
   WHEN "RCG" THEN IF liNetPlusResponse NE 140 THEN 
      PrepaidRequest.RespCode = liNetPlusResponse.
   WHEN "ANT" THEN IF liNetPlusResponse NE 150 THEN
      PrepaidRequest.RespCode = liNetPlusResponse.
   WHEN "ENQ" THEN IF liNetPlusResponse NE 160 THEN 
      PrepaidRequest.RespCode = liNetPlusResponse.
END.

IF PrepaidRequest.RespCode = 0 AND LOOKUP(PrepaidRequest.Request,"RCG,ANT") > 0 THEN DO:
   IF LOOKUP(lcDoubleMessageStatus,"cancelled,expired") > 0 THEN
      PrepaidRequest.RespCode = 147.
END.

IF PrepaidRequest.RespCode = 0 THEN DO:

   RUN pDoubleCheck.

   IF RETURN-VALUE NE "OK" THEN 
      PrePaidRequest.RespCode = 148.
   ELSE IF (PrepaidRequest.Request = "RCG" AND liNetPlusResponse = 140 AND
            LOOKUP(lcDoubleMessageStatus,",successful") > 0) OR
       (PrepaidRequest.Request = "ANT" AND liNetPlusResponse = 150 AND
            LOOKUP(lcDoubleMessageStatus,",successful") > 0) 
       THEN DO:
      
      CREATE TopUpQueue.
      ASSIGN
         TopUpQueue.PPRequest = PrepaidRequest.PPRequest
         TopUpQueue.CLI       = PrepaidRequest.CLI
         TopUpQueue.TopUpAmt  = PrepaidRequest.TopUpAmt / 100
         TopUpQueue.VatAmt    = PrepaidRequest.VatAmt / 100
         TopUpQueue.Date      = TODAY
         TopUpQueue.Source    = PrepaidRequest.Source.

   END.

END.

add_int(response_toplevel_id, "", 0).
