/**
 * List some free ICC,ICC_TYPE (RETAILER stock). Used in staging.
 *
 * @input int;optional;number of ICCs (min 3, default 12, max 500)
 * @output array of string;
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR top_array AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO. 
DEF VAR piTotal AS INT NO-UNDO INIT 12.
DEF VAR liSIMCount AS INT NO-UNDO EXTENT 3.
DEF VAR lcTypes AS CHAR NO-UNDO init "micro,nano,plug_in". 
DEF VAR liType AS INT NO-UNDO.
DEF VAR liLimit AS INT NO-UNDO. 

top_array = validate_request(param_toplevel_id, "[int]").

IF NUM-ENTRIES(top_array) EQ 1 THEN
   piTotal = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 then RETURN.
 
if piTotal < 3 then piTotal = 3.
liLimit = int(piTotal / 3).

IF piTotal > 500 THEN RETURN appl_err("Max. 500 ICCs is allowed"). 

result_array = add_array(response_toplevel_id, "").

do liType =  1 to num-entries(lcTypes):
FOR EACH SIM NO-LOCK WHERE
         SIM.Brand = gcBrand AND
         SIM.Stock = "RETAILER" AND
         SIM.SimStat = 1 and
         Sim.simart = entry(liType,lcTypes):

   liSIMCount[liType] = liSIMCount[liType] + 1.
   if liSIMCount[liType] > liLimit then leave.

   add_string(result_array,"",sim.icc + "," + sim.simart).
END.
end.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
