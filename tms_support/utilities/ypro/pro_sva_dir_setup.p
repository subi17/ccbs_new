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
/*Directory for incoming*/
lcRet = fCreateEntryC("YPRO_SVA_in_base_dir", /*param code*/
              "YPRO",              /*param group*/
              "Base level directory for Incoming SVA",  /*param name*/ 
              "/mnt/store/riftp/sva/incoming/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*Outgoing*/
lcRet = fCreateEntryC("YPRO_SVA_out_base_dir", /*param code*/
              "YPRO",              /*param group*/
              "Base level directory for Outgoing SVA",  /*param name*/ 
              "/mnt/store/riftp/sva/processed/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*Logs*/
lcRet = fCreateEntryC("YPRO_SVA_log_base_dir", /*param code*/
              "YPRO",              /*param group*/
              "Base level directory for SVA logs",  /*param name*/ 
              "/mnt/store/riftp/sva/logs/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.


/*Errors*/
lcRet = fCreateEntryC("YPRO_SVA_err_base_dir", /*param code*/
              "YPRO",              /*param group*/
              "Base level directory for SVA error logs",  /*param name*/ 
              "/mnt/store/riftp/sva/errors/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fCreateEntryC("YPRO_SVA_email_dir", /*param code*/
              "YPRO",              /*param group*/
              "Base level directory for SVA email files",  /*param name*/ 
              "/tmp/"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.






