/** Check preferred MNP porting date if not correct then return estimated porting date.
 * @input struct with following input parameters
           mnp_porting_date;date;mandatory;
           order_channel;string;mandatory;
           region;string;mandatory;
           product;string;optional;
           tariff;string;optional;
           delivery_type;int;optional
 * @output mnp_porting_date;datetime;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{Mnp/mnp.i}

/* Input parameters */
DEF VAR pdMNPPortingDate AS DATE NO-UNDO.
DEF VAR pcOrderChannel   AS CHAR NO-UNDO.
DEF VAR pcRegion         AS CHAR NO-UNDO.
DEF VAR pcStruct         AS CHAR NO-UNDO.
DEF VAR lcStruct         AS CHAR NO-UNDO.
DEF VAR pcProduct        AS CHAR NO-UNDO. 
DEF VAR pcTariff         AS CHAR NO-UNDO.
DEF VAR piDelType        AS INT  NO-UNDO.
DEF VAR pcTenant         AS CHAR NO-UNDO.
DEF VAR ldMNPPortingDate AS DATE NO-UNDO.
DEF VAR ldeCurrentTime   AS DEC  NO-UNDO.

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct,"mnp_porting_date!,order_channel!,region!,product,tariff,delivery_type").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN pdMNPPortingDate = get_date(pcStruct,"mnp_porting_date")
       pcOrderChannel   = get_string(pcStruct,"order_channel")
       pcRegion         = get_string(pcStruct,"region")
       pcProduct        = get_string(pcStruct,"product")
       pcTariff         = get_string(pcStruct,"tariff").

IF LOOKUP("delivery_type", pcStruct) GT 0
THEN piDelType = get_int(pcStruct,"delivery_type").

FIND FIRST TMSCodes NO-LOCK WHERE
   TMSCodes.TableName = "MNPCal"       AND
   TMSCodes.FieldName = "DeliveryType" AND
   TMSCodes.CodeValue = STRING(piDelType)
NO-ERROR.

IF NOT AVAILABLE TMSCodes
THEN RETURN appl_err("Invalid delivery type").

IF pdMNPPortingDate = ? OR pdMNPPortingDate < TODAY THEN
   RETURN appl_err("Invalid MNP Porting Date").

IF pcOrderChannel = "" OR pcOrderChannel = ? THEN
   RETURN appl_err("Missing Order Channel").

IF pcRegion = "" OR pcRegion = ? THEN
   RETURN appl_err("Missing Customer Region").

IF pcProduct NE "T" AND pcProduct NE "S" AND pcProduct NE "" THEN
   RETURN appl_err("Invalid MNP product code, expecting T/S/Empty").
   
ldeCurrentTime = Func.Common:mMake2DT(TODAY,28800).
IF ldeCurrentTime < Func.Common:mMakeTS() THEN
   ldeCurrentTime = Func.Common:mMakeTS().

{newton/src/settenant.i pcTenant}

ldMNPPortingDate = fMNPChangeWindowDate(ldeCurrentTime,
                                        pcOrderChannel,
                                        pcRegion,
                                        pcProduct,
                                        pcTariff,
                                        piDelType).

IF ldMNPPortingDate = ? THEN
   RETURN appl_err("Invalid Estimated MNP Porting Date").

IF pdMNPPortingDate < ldMNPPortingDate THEN
   pdMNPPortingDate = ldMNPPortingDate.

pdMNPPortingDate = fMNPHoliday(pdMNPPortingDate,TRUE).

add_datetime(response_toplevel_id, "", pdMNPPortingDate).

FINALLY:
   END.
