FUNCTION fCreateTMSParam RETURNS LOGICAL
   ( icGroup AS CHARACTER,
     icCode  AS CHARACTER,
     icParamType AS CHARACTER,
     icParamValue AS CHARACTER ):

   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.Brand = "1" AND
      TMSParam.ParamGroup = icGroup AND
      TMSParam.ParamCode = icCode
   NO-ERROR.
   
   IF NOT AVAILABLE TMSParam
   THEN DO:
      CREATE TMSParam.
   END.
   
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.ParamGroup = icGroup
      TMSParam.ParamCode = icCode
      TMSParam.ParamType = icParamType
      .
  
   CASE IcParamType:
      WHEN "I"
      THEN TMSParam.IntVal = INTEGER(icParamValue).
      WHEN "C"
      THEN TMSParam.CharVal = icParamValue.
   END CASE.

   RETURN FALSE.

END FUNCTION.


/*  */
/*List of test customers*/
fCreateTMSParam("ESI_setting","ESI_TestCustomers","C","789,123,456").

/*Normal functionality = 0, Only for test customres = 1*/
fCreateTMSParam("ESI_setting","ESI_TestFilter","I","0"). /*0 OFF, 1 ON*/

/*Link that is used as base in building of ESI page link.*/
fCreateTMSParam("ESI_setting","ESILinkBase","C","www.qvantel.com/").

