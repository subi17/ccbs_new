/*This functionality does not create entry if it already exist. 
Just error code is returned. Changing needs another program/function.*/
{Syst/tmsconst.i}
FUNCTION fCreateEntryC RETURNS CHAR
   ( icParamCode AS CHAR,
     icParamGroup AS CHAR,
     icParamName AS CHAR,
     icParamCharVal AS CHAR
   ):


   FIND FIRST TMSParam NO-LOCK WHERE
              TMSParam.Brand EQ "1" AND
              TmsParam.ParamGroup EQ icParamGroup AND
              TMSPAram.ParamCode EQ icParamCode NO-ERROR.
   IF AVAIL TMSParam THEN RETURN "Parameter already found " + icParamCode.

   CREATE TMSParam.
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.ParamCode = icParamCode
      TMSParam.ParamGroup = icParamGroup 
      TMSParam.ParamType = "C"
      TMSParam.ParamName = icParamName
      TMSParam.Online = FALSE
      TMSParam.CharVal = icParamCharVal.

   RETURN "".

END.

FUNCTION fCreateEntryI RETURNS CHAR
   ( icParamCode AS CHAR,
     icParamGroup AS CHAR,
     icParamName AS CHAR,
     iiParamIntVal AS INT
   ):


   FIND FIRST TMSParam NO-LOCK WHERE
              TMSParam.Brand EQ "1" AND
              TmsParam.ParamGroup EQ icParamGroup AND
              TMSPAram.ParamCode EQ icParamCode NO-ERROR.
   IF AVAIL TMSParam THEN RETURN "Parameter already found " + icParamCode.

   CREATE TMSParam.
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.ParamCode = icParamCode
      TMSParam.ParamGroup = icParamGroup 
      TMSParam.ParamType = "I"
      TMSParam.ParamName = icParamName
      TMSParam.Online = FALSE
      TMSParam.IntVal = iiParamIntVal.

   RETURN "".

END.

DEF VAR lcRet AS CHAR NO-UNDO.
/*Directory for spooling*/
lcRet = fCreateEntryC("GBSpoolDir", /*param code*/
              "GBILLING",              /*param group*/
              "Spool dir for GB refund",  /*param name*/ 
              "/store/riftp/dumpfiles/gbilling/spool/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

 lcRet = fCreateEntryC("GBLogDir", /*param code*/
              "GBILLING",              /*param group*/
              "Log dir for GB refund",  /*param name*/ 
              "/store/riftp/dumpfiles/gbilling/logs/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fCreateEntryC("GBInDir", /*param code*/
              "GBILLING",              /*param group*/
              "Incoming dir for GB refund",  /*param name*/ 
              "/store/riftp/dumpfiles/gbilling/incoming/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fCreateEntryC("GBOutDir", /*param code*/
              "GBILLING",              /*param group*/
              "Outgoing dir for GB refund",  /*param name*/ 
              "/store/riftp/dumpfiles/gbilling/outgoing/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fCreateEntryC("GBProcDir", /*param code*/
              "GBILLING",              /*param group*/
              "Processed dir for GB refund",  /*param name*/ 
              "/store/riftp/dumpfiles/gbilling/processed/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.



