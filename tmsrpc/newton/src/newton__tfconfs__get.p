/**
 * Get terminal financing configuration row.
 *
 * @input  id;array of string;mandatory;id list
 * @output array;array of config;list of active configurations
 * @config id;string;Unique Id
            rv_percentage;double;RV percentage
            commission_fee_percentage;double;
            tae;double;TAE
            tin;int;TIN (always 0)
            valid_from;date;
            valid_to;date;
 */

{rpcmethods/header_get.i}
DEF VAR liId AS INTEGER NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   liId = INT(pcId) NO-ERROR.
   
   FIND TFConf NO-LOCK WHERE 
        TFConf.TFConfId = liId NO-ERROR.

   IF NOT AVAIL TFConf THEN
      RETURN appl_err("Terminal financing configuation row not found: "+ pcId).

   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", STRING(tfconf.tfconfid)). 
   add_double(lcResultStruct, "rv_percentage", TFConf.RVPercentage).
   add_double(lcResultStruct, "commission_fee_percentage", TFConf.CommFeePerc).
   add_double(lcResultStruct, "tae", TFConf.tae).
   add_int(lcResultStruct, "tin", 0).
   add_datetime(lcResultStruct, "valid_from", TFConf.ValidFrom).
   add_datetime(lcResultStruct, "valid_to", TFConf.ValidTo).
END.
