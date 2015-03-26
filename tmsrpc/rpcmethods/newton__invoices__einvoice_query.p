/**
 * Validate eInvoice PDF Link
 *
 * @input      struct;mandatory;list of parameters
               int;mandatory;period
               string;mandatory;dni
               string;mandatory;msisdn
               string;mandatory;hash keys
 * @output     int;mandatory;invoice number
 */
{xmlrpc/xmlrpc_access.i}

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "tmsrpc".
{cparam2.i}
{tmsconst.i}
{timestamp.i}

DEF VAR pcStruct       AS CHAR  NO-UNDO. 
DEF VAR lcStruct       AS CHAR  NO-UNDO.
DEF VAR piPeriod       AS INT   NO-UNDO.
DEF VAR pcDNI          AS CHAR  NO-UNDO.
DEF VAR pcMSISDN       AS CHAR  NO-UNDO.
DEF VAR pcHashKey      AS CHAR  NO-UNDO.

DEF VAR lcEncodedLink  AS CHAR   NO-UNDO.
DEF VAR lcSaltKey      AS CHAR   NO-UNDO.
DEF VAR liYear         AS INT    NO-UNDO.
DEF VAR liMonth        AS INT    NO-UNDO.
DEF VAR ldaDate        AS DATE   NO-UNDO.
DEF VAR liInvNum       AS INT    NO-UNDO.
DEF VAR liBillPeriod   AS INT    NO-UNDO.
DEF VAR ldeEndStamp    AS DEC    NO-UNDO.

IF validate_request(param_toplevel_id,"struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_request(pcStruct,"period,dni,msisdn,hash").
IF gi_xmlrpc_error NE 0 THEN RETURN.

piPeriod  = get_int(pcStruct,"period").
pcDNI     = get_string(pcStruct,"dni").
pcMSISDN  = get_string(pcStruct,"msisdn").
pcHashKey = get_string(pcStruct,"hash").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcHashKey = "" OR pcHashKey = ? THEN
   RETURN appl_err("Hash Key is blank or unknown").

IF piPeriod = 0 OR piPeriod = ? THEN
   RETURN appl_err("Invalid Period").

lcSaltKey = fCParam("EI","SaltKey").
IF lcSaltKey = "" OR lcSaltKey = ? THEN
   RETURN appl_err("Salt key is missing").

ASSIGN liYear   = INT(SUBSTRING(STRING(piPeriod),1,4))
       liMonth  = INT(SUBSTRING(STRING(piPeriod),5,2))
       ldaDate  = DATE(liMonth,1,liYear)
       ldeEndStamp = fMake2Dt(ldaDate,0).

FIND FIRST Customer WHERE
           Customer.Brand = gcBrand AND
           Customer.OrgId = pcDNI   AND
           Customer.Roles NE "inactive" NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not found").

FIND FIRST MsOwner WHERE
           MsOwner.Brand   = gcBrand AND
           MsOwner.CustNum = Customer.CustNum AND
           MsOwner.CLI     = pcMSISDN AND
           MsOwner.TsEnd  >= ldeEndStamp NO-LOCK NO-ERROR.
IF NOT AVAIL MsOwner THEN
   RETURN appl_err("MobSub not found").

FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.CustNum  = Customer.CustNum AND
         Invoice.InvDate >= ldaDate AND
         Invoice.InvType  = 1 NO-LOCK:

   liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).

   /* Should not allow to download OLD PDF if newest Invoice exists in TMS */
   IF liBillPeriod > piPeriod THEN RETURN appl_err("Invoice not found").

   ASSIGN lcEncodedLink = STRING(Invoice.InvNum) + STRING(Customer.Custnum)
          lcEncodedLink = HEX-ENCODE(SHA1-DIGEST(lcEncodedLink,lcSaltKey)).

   IF lcEncodedLink = pcHashKey THEN DO:
      liInvnum = Invoice.InvNum.
      LEAVE.
   END. /* IF lcEncodedLink = pcHashKey THEN DO: */
END. /* FOR EACH Invoice WHERE */

FIND FIRST Invoice NO-LOCK WHERE
           Invoice.InvNum = liInvNum NO-ERROR.
IF NOT AVAIL Invoice THEN
   RETURN appl_err("Invoice not found").

add_int(response_toplevel_id,?,Invoice.InvNum).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
