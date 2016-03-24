/*This functionality does not create entry if it already exist. 
Just error code is returned. Changing needs another program/function.*/

FUNCTION fCreateEntryC RETURNS CHAR
   ( icParamCode AS CHAR,
     icParamGroup AS CHAR,
     icParamName AS CHAR,
     icParamCharVal AS CHAR
   ):


   FIND FIRST TMSParam NO-LOCK WHERE
              TMSParam.Brand EQ "1" AND
              TmsParam.ParamGroup EQ icParamGroup AND
              TMSPAram.ParamCode EQ icParamCode.
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

DEF VAR lcRet AS CHAR NO-UNDO.

lcRet = fCreateEntryC("hrlp_outgoing_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP file, TMS to IFS",  /*param name*/ 
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .


lcRet = fCreateEntryC("hrlp_incoming_list_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP FILE, IFS to TMS",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

 
lcRet = fCreateEntryC("hrlp_incoming_remove_redirection_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP FILE, IFS to TMS",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

 
lcRet = fCreateEntryC("hrlp_logs_outgoing_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP file, TMS to IFS",  /*param name*/ 
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .


lcRet = fCreateEntryC("hrlp_logs_incoming_list_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP FILE, IFS to TMS",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

 
lcRet = fCreateEntryC("hrlp_logs_incoming_remove_redirection_dir", /*param code*/
              "hrlp",              /*param group*/
              "Dir for HRLP FILE, IFS to TMS",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

 
