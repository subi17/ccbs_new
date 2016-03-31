/*This functionality does not create entry if it already exist. 
Just error code is returned. Changing needs another program/function.*/
{tmsconst.i}
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

FUNCTION fCreateEntryI RETURNS CHAR
   ( icParamCode AS CHAR,
     icParamGroup AS CHAR,
     icParamName AS CHAR,
     iiParamIntVal AS INT
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
      TMSParam.ParamType = "I"
      TMSParam.ParamName = icParamName
      TMSParam.Online = FALSE
      TMSParam.IntVal = iiParamIntVal.

   RETURN "".

END.

DEF VAR lcRet AS CHAR NO-UNDO.
/*Directory for spooling*/
lcRet = fCreateEntryC("HRLPSpoolDir", /*param code*/
              "HRLP",              /*param group*/
              "Spool ir for HRLP file",  /*param name*/ 
              "/store/riftp/dumpfiles/hrilp/spool"). /*actual parameter value*/

/*Directory for outgoing*/
lcRet = fCreateEntryC("HRLPOutDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for outgoing HRLP file",  /*param name*/ 
              "/store/riftp/dumpfiles/hrilp/outgoing"). /*actual parameter value*/


if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .

/*Directory for incoming list*/
lcRet = fCreateEntryC("HRLPListInDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for incoming HRLP file",  /*param name*/
              "/store/riftp/dumpfiles/hrilp/incoming"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*For removing redirection*/ 
lcRet = fCreateEntryC("HrlpRemRedirDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for HRLP redir removal file",  /*param name*/
              "/store/riftp/dumpfiles/hrilp/incoming"). /*actual parameter value*/

if LcRet NE "" THEN
   MESSAGE lcRet VIEW-AS ALERT-BOX.

/*Directory for HRLP logs*/
lcRet = fCreateEntryC("HRLPLogDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for HRLP related logs",  /*param name*/ 
              "/store/riftp/dumpfiles/hrilp/log"). /*actual parameter value*/

/*Directory for processed HRLP files*/
lcRet = fCreateEntryC("HRLProcDir", /*param code*/
              "HRLP",              /*param group*/
              "Dir for processed HRLP files",  /*param name*/ 
              "/store/riftp/dumpfiles/hrilp/log"). /*actual parameter value*/

lcRet = fCreateEntryI("HRLPTestLevel", /*param code*/
              "HRLP",              /*param group*/
              "HRLP feature test level",  /*param name*/ 
              {&Q25_HRLP_NO_TEST}). /*actual parameter value*/

/*List of subscriptions accepted in test*/
lcRet = fCreateEntryC("HRLPTestMSISDN", /*param code*/
              "HRLP",              /*param group*/
              "Test MSISDN list",  /*param name*/ 
              ""). /*actual parameter value*/



if LcRet NE "" THEN 
   MESSAGE lcRet VIEW-AS ALERT-BOX .


 
