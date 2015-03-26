/** Check preferred MNP porting date if not correct then return estimated porting date.
 * @input struct with following input parameters
           mnp_porting_date;date;mandatory;
           order_channel;string;mandatory;
           region;string;mandatory;
 * @output mnp_porting_date;datetime;
 */

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{mnp.i}

/* Input parameters */
DEF VAR pdMNPPortingDate AS DATE NO-UNDO.
DEF VAR pcOrderChannel   AS CHAR NO-UNDO.
DEF VAR pcRegion         AS CHAR NO-UNDO.
DEF VAR pcStruct         AS CHAR NO-UNDO.
DEF VAR lcStruct         AS CHAR NO-UNDO.

DEF VAR ldMNPPortingDate AS DATE NO-UNDO.
DEF VAR ldeCurrentTime   AS DEC  NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct,"mnp_porting_date!,order_channel!,region!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN pdMNPPortingDate = get_date(pcStruct,"mnp_porting_date")
       pcOrderChannel   = get_string(pcStruct,"order_channel")
       pcRegion         = get_string(pcStruct,"region").

IF pdMNPPortingDate = ? OR pdMNPPortingDate < TODAY THEN
   RETURN appl_err("Invalid MNP Porting Date").

IF pcOrderChannel = "" OR pcOrderChannel = ? THEN
   RETURN appl_err("Missing Order Channel").

IF pcRegion = "" OR pcRegion = ? THEN
   RETURN appl_err("Missing Customer Region").

ldeCurrentTime = fMake2DT(TODAY,28800).
IF ldeCurrentTime < fMakeTS() THEN
   ldeCurrentTime = fMakeTS().

ldMNPPortingDate = fMNPChangeWindowDate(ldeCurrentTime,
                                        pcOrderChannel,
                                        pcRegion).

IF ldMNPPortingDate = ? THEN
   RETURN appl_err("Invalid Estimated MNP Porting Date").

IF pdMNPPortingDate < ldMNPPortingDate THEN
   pdMNPPortingDate = ldMNPPortingDate.

pdMNPPortingDate = fMNPHoliday(pdMNPPortingDate,TRUE).

add_datetime(response_toplevel_id, "", pdMNPPortingDate).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
