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
/*Directory for outgoing*/
lcRet = fCreateEntryC("HRLPOutDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for outgoing HRLP file",  /*param name*/ 
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .

/*Directory for incoming list*/
lcRet = fCreateEntryC("HRLPListInDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for incoming HRLP file",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*For removing redirection*/ 
lcRet = fCreateEntryC("HrlpRemRedirDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for HRLP redir removalfile",  /*param name*/
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*Directory for HRLP logs*/
lcRet = fCreateEntryC("HRLPLogDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for HRLP file, TMS to IFS",  /*param name*/ 
              "NOT_KNOWN_YET"). /*actual parameter value*/

if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .


 
