/**
 * Get charge events  ids.
 *
 * @input conditions;struct;mandatory;supports 
 * @output struct;array of active charge events ids  
*/

{rpcmethods/flistrpc.i}


lcStruct = validate_struct(pcStruct, "paytype").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH FeeModel NO-LOCK WHERE FeeModel.Brand = "1" AND ' + 
                   'FeeModel.FMGroup = 1, ' +
              'FIRST FMItem OF FeeModel WHERE ' + 
                    'FMItem.ToDate >= TODAY '.

IF LOOKUP("paytype",lcStruct) > 0 THEN DO:

   DEFINE VARIABLE lcpricelist AS CHARACTER NO-UNDO. 
  
   lcpricelist = "FMItemPriceList" + get_string(pcStruct,"paytype").
 
   FIND TMSParam WHERE TMSParam.Brand = "1" AND
                       TMSParam.ParamGroup = "CCAdminTool" AND
                       TMSParam.ParamCode = lcpricelist NO-LOCK NO-ERROR. 

  
   lcQuery = lcQuery + ' AND FMItem.PriceList = ' + QUOTER(TMSParam.CharVal) .

   IF gi_xmlrpc_error NE 0 THEN RETURN.

END.

fListQuery(
   "FeeModel,FMItem",
   lcQuery,
   "FeeModel").



