/* KooAa 01.07.1999
   Retuns TMSParam VALUE / 4 RepType - 4 functions 
   v3: use BOTH group & code

            05.09.03/aam brand 
*/

FUNCTION fGetParam4 RETURN LOGICAL
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER):

   FIND FIRST TMSParam WHERE
              TMSParam.Brand      = inp1 AND
              TMSParam.ParamGroup = inp2 AND
              TMSParam.ParamCode  = inp3
   NO-LOCK NO-ERROR.

END FUNCTION.


FUNCTION fCParamC4 RETURNS CHARACTER
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER):

   DEF VAR ret AS c NO-UNDO init ?.

   fGetParam4(inp1, inp2, inp3).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.CharVal.
      release TMSParam.
   END.

   RETURN ret.

END FUNCTION.

FUNCTION fCParamI4 RETURNS INTEGER
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER):

   DEF VAR ret AS i NO-UNDO init ?.

   fGetParam4(inp1, inp2, inp3).

   IF AVAIL TMSParam THEN DO:
      ret = tmsparam.intVal.
      release TMSParam.
   END.

   RETURN ret.

END FUNCTION.

FUNCTION fCParam4SetI RETURNS LOG
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER,
   INPUT iivalue AS INT):


   FIND FIRST TMSParam WHERE
              TMSParam.Brand      = inp1 AND
              TMSParam.ParamGroup = inp2 AND
              TMSParam.ParamCode  = inp3
              EXCLUSIVE-LOCK.

   IF AVAIL TMSParam THEN DO:
       TMSParam.IntVal = iiValue.
      release TMSParam.
      RETURN True.
   END.

   RETURN False.

END.



FUNCTION fCParamDe4 RETURNS DECIMAL
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER):

   DEF VAR ret AS DE NO-UNDO init ?.

   fGetParam4(inp1, inp2, inp3).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DecVal.
      release TMSParam.
   END.

   RETURN ret.

END FUNCTION.

FUNCTION fCParamDa4 RETURNS DATE
  (INPUT inp1 AS CHARACTER,
   INPUT inp2 AS CHARACTER,
   INPUT inp3 AS CHARACTER):

   DEF VAR ret AS DA NO-UNDO init ?.

   fGetParam4(inp1, inp2, inp3).

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DateVal.
      release TMSParam.
   END.

   RETURN ret.

END FUNCTION.
