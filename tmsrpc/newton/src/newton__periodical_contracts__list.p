/**
 * Get periodical contract type ids which are active and type Discount or Installlment.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of periodical contract type ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH DayCampaign NO-LOCK WHERE ' +
          'DayCampaign.Brand = ' + QUOTER(gcBrand) + ' AND ' +
          'LOOKUP(DayCampaign.DCType,"3,5") > 0 AND ' +
          'DayCampaign.ValidTo >= TODAY AND DayCampaign.ValidFrom <= TODAY'.

fListQuery(
   "DayCampaign",
   lcQuery,
   "DCEvent").
