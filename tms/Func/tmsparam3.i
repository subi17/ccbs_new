/* KooAa 01.07.1999
   Retuns TMSParam VALUE / 4 RepType - 4 functions 
   v3: use BOTH group & code

            05.09.03/aam brand 
            22.07.05/tk  global define
*/


&IF "{&TMSParam3Defined}" NE "YES" 
&THEN

&GLOBAL-DEFINE TMSParam3Defined YES



FUNCTION fCParamC RETURNS CHAR
  (INPUT inp1 AS CHAR,
   INPUT inp2 AS CHAR).

   DEF VAR ret AS c NO-UNDO init ?.

   RUN pGetParam(INPUT inp1, INPUT inp2).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.CharVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamI RETURNS INT 
  (INPUT inp1 AS CHAR,
   INPUT inp2 AS CHAR).


   DEF VAR ret AS i NO-UNDO init ?.

   RUN pGetParam(INPUT inp1, INPUT inp2).

   IF AVAIL TMSParam THEN DO:
      ret = tmsparam.intVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamDe RETURNS dec 
  (INPUT inp1 AS CHAR,
   INPUT inp2 AS CHAR).

   DEF VAR ret AS DE NO-UNDO init ?.

   RUN pGetParam(INPUT inp1, INPUT inp2).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DecVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamDa RETURNS Date
  (INPUT inp1 AS CHAR,
   INPUT inp2 AS CHAR).

   DEF VAR ret AS DA NO-UNDO init ?.

   RUN pGetParam(INPUT inp1, INPUT inp2).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DateVal.
      release TMSParam.
   END.

   RETURN ret.

END.

PROCEDURE pGetParam:

   DEF INPUT PARAM inp1 AS CHAR NO-UNDO.
   DEF INPUT PARAM inp2 AS CHAR NO-UNDO.

   FIND FIRST TMSParam where
              TMSParam.Brand      = gcBrand AND
              TMSParam.ParamGroup = inp1    AND
              TMSParam.ParamCode  = inp2
   no-lock no-error.

END PROCEDURE.

&ENDIF
