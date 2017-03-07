/**
 * Create credit note request
 *
 * @input         main_invoice;int;mandatory;invoice number
                  sub_invoices;string;optional;array of subinvoice and
                  invoice rows details
                  username;string;mandatory;
                  reason_category;string;mandatory; reason group
                  reason;string;mandatory;reason 
                  remark;string;mandatory;reason note
 * @output        boolean;true
  
*/
{xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
gcBrand = "1".
{Func/fcreditreq.i}

DEF VAR pcStruct          AS CHAR NO-UNDO.
DEF VAR lcStruct          AS CHAR NO-UNDO.
DEF VAR piInvNum          AS INT NO-UNDO.
DEF VAR pcReasonGrp       AS CHAR NO-UNDO.
DEF VAR pcReason          AS CHAR NO-UNDO.
DEF VAR lcError           AS CHAR NO-UNDO.
DEF VAR pcSubInvoiceList  AS CHAR NO-UNDO.
DEF VAR liSubInvCount     AS INT  NO-UNDO.
DEF VAR pcSubInvoiceArray AS CHAR NO-UNDO.
DEF VAR pcSubInvStruct    AS CHAR NO-UNDO.
DEF VAR lcSubInvStruct    AS CHAR NO-UNDO.
DEF VAR piSubInvoice      AS INT  NO-UNDO.
DEF VAR pcInvRowArray     AS CHAR NO-UNDO.
DEF VAR pcInvRowStruct    AS CHAR NO-UNDO.
DEF VAR lcInvRowStruct    AS CHAR NO-UNDO.
DEF VAR pcInvRowDetails   AS CHAR NO-UNDO.
DEF VAR liInvRowCount     AS INT  NO-UNDO.
DEF VAR piInvRow          AS INT  NO-UNDO.
DEF VAR pdeInvRowAmt      AS DEC  NO-UNDO.
DEF VAR pcRemark          AS CHAR NO-UNDO.
DEF VAR lcList            AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttSubInvoice NO-UNDO
       FIELD SubInvoice    AS INT
       FIELD InvRow        AS INT
       FIELD InvRowAmt     AS DEC
       INDEX SubInvRow IS PRIMARY UNIQUE SubInvoice InvRow.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_request(pcStruct,"main_invoice!,sub_invoices,username!,reason_category!,reason!,remark!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piInvNum = get_int(pcStruct,"main_invoice")
   katun = "VISTA_" + get_string(pcStruct,"username")
   pcReasonGrp = get_string(pcStruct,"reason_category")
   pcReason    = get_string(pcStruct,"reason")
   pcRemark    = get_string(pcStruct,"remark").

IF LOOKUP("sub_invoices",lcStruct) > 0 THEN
   pcSubInvoiceArray = get_array(pcStruct,"sub_invoices"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF pcRemark EQ "" THEN RETURN appl_err("remark is empty").

FIND Invoice WHERE
     Invoice.Brand = "1" AND
     Invoice.InvNum = piInvNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN
   RETURN appl_err("Invoice not found: " + STRING(piInvNum)).

SUB_INVOICES:
DO liSubInvCount = 0 TO get_paramcount(pcSubInvoiceArray) - 1:
 
   pcSubInvStruct = get_struct(pcSubInvoiceArray,STRING(liSubInvCount)).
   lcSubInvStruct = validate_request(pcSubInvStruct,"sub_invoice!,inv_rows").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   piSubInvoice = get_int(pcSubInvStruct,"sub_invoice").
   IF piSubInvoice = 0 OR piSubInvoice = ? OR
      LOOKUP(STRING(piSubInvoice), pcSubInvoiceList) > 0
   THEN NEXT SUB_INVOICES.

   FIND FIRST SubInvoice WHERE
              SubInvoice.InvNum    = Invoice.InvNum AND
              SubInvoice.SubInvNum = piSubInvoice NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SubInvoice THEN
      RETURN appl_err("SubInvoice not found: " + STRING(piSubInvoice)).

   ASSIGN pcSubInvoiceList = pcSubInvoiceList + "," + STRING(piSubInvoice)
          pcInvRowArray = "".

   IF LOOKUP("inv_rows",lcSubInvStruct) > 0 THEN
      pcInvRowArray = get_array(pcSubInvStruct,"inv_rows").

   INVROW_BLK:
   DO liInvRowCount = 0 TO get_paramcount(pcInvRowArray) - 1:
      pcInvRowStruct = get_struct(pcInvRowArray,STRING(liInvRowCount)).
      lcInvRowStruct = validate_request(pcInvRowStruct,"inv_row!,amount!").
      IF gi_xmlrpc_error NE 0 THEN RETURN.

      piInvRow     = get_int(pcInvRowStruct,"inv_row").
      pdeInvRowAmt = get_double(pcInvRowStruct,"amount").

      IF piInvRow = 0 OR piInvRow = ? THEN NEXT INVROW_BLK.

      FIND FIRST InvRow WHERE
                 InvRow.InvNum    = Invoice.InvNum AND
                 InvRow.SubInvNum = SubInvoice.SubInvNum AND
                 InvRow.InvRowNum = piInvRow NO-LOCK NO-ERROR.
      IF NOT AVAIL InvRow THEN
         RETURN appl_err("InvRow not found: " + STRING(piInvRow)).

      IF CAN-FIND(FIRST ttSubInvoice WHERE
                        ttSubInvoice.SubInvoice = piSubInvoice AND
                        ttSubInvoice.InvRow     = piInvRow)
      THEN NEXT INVROW_BLK.

      IF pdeInvRowAmt = 0 OR pdeInvRowAmt = ? THEN pdeInvRowAmt = InvRow.Amt.
      
      IF (InvRow.Amt > 0 AND pdeInvRowAmt < 0) OR
         (InvRow.Amt < 0 AND pdeInvRowAmt > 0) THEN
            RETURN appl_err("Entered invrow amount and original " +
                            "invrow amount must be whether positive or " +
                            "negative").

      IF InvRow.Amt < 0 THEN DO:
         IF abs(pdeInvRowAmt) > abs(InvRow.Amt) THEN
            RETURN appl_err("Entered invrow amount is greater than " +
                            "actual invrow amount").
      END.
      ELSE DO:
         IF pdeInvRowAmt > InvRow.Amt THEN
            RETURN appl_err("Entered invrow amount is greater than " +
                            "actual invrow amount").
      END.
      /* Create Temp-table with SubInvoice/InvRow details */
      CREATE ttSubInvoice.
      ASSIGN ttSubInvoice.SubInvoice = piSubInvoice
             ttSubInvoice.InvRow     = piInvRow
             ttSubInvoice.InvRowAmt  = pdeInvRowAmt.

   END. /* DO liInvRowCount = 0 TO get_paramcount(pcInvRowArray) - 1: */
END. /* DO liSubInvCount = 0 TO get_paramcount(pcSubInvoiceArray) - 1: */

FOR EACH ttSubInvoice NO-LOCK:
    pcInvRowDetails = pcInvRowDetails + "," +
                      "InvRow="       + STRING(ttSubInvoice.InvRow) + "|" +
                      "InvRowAmt="    + STRING(ttSubInvoice.InvRowAmt).
END. /* FOR EACH ttSubInvoice NO-LOCK: */
ASSIGN pcSubInvoiceList = TRIM(pcSubInvoiceList, ",")
       pcInvRowDetails  = TRIM(pcInvRowDetails, ",").

fFullCreditNote(
   piInvNum,
   pcSubInvoiceList,
   pcInvRowDetails,
   pcReasonGrp,
   pcReason,
   pcRemark,
   OUTPUT lcError).

IF lcerror ne "" THEN RETURN appl_err(lcerror).
add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   EMPTY TEMP-TABLE ttSubInvoice. 
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END FINALLY.
