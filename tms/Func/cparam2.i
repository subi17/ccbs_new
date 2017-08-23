/* KooAa 01.07.1999
   Retuns TMSParam VALUE / 4 RepType - 4 functions 

        03.09.03/aam brand, CParamFunctionDefined
*/

&IF "{&CParamFunctionDefined}" NE "YES" 
&THEN

&GLOBAL-DEFINE CParamFunctionDefined YES

FUNCTION fCParamC RETURNS CHAR
  (INPUT inp AS CHAR).

   DEF VAR ret AS c NO-UNDO init ?.

   FIND FIRST TMSParam where
              TMSParam.Brand      = Syst.Parameters:gcBrand AND 
              TMSParam.ParamCode  = inp
   no-lock no-error.

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.CharVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamI RETURNS INT 
  (INPUT inp AS CHAR).

   DEF VAR ret AS i NO-UNDO init ?.

   FIND FIRST TMSParam where
              TMSParam.Brand      = Syst.Parameters:gcBrand AND 
              TMSParam.ParamCode  = inp
   no-lock no-error.

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.IntVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamDe RETURNS dec 
  (INPUT inp AS CHAR).

   DEF VAR ret AS DE NO-UNDO init ?.

   FIND FIRST TMSParam where
              TMSParam.Brand      = Syst.Parameters:gcBrand AND 
              TMSParam.ParamCode  = inp
   no-lock no-error.

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DecVal.
      release TMSParam.
   END.

   RETURN ret.

END.

FUNCTION fCParamDa RETURNS Date
  (INPUT inp AS CHAR).

   DEF VAR ret AS DA NO-UNDO init ?.

   FIND FIRST TMSParam where
              TMSParam.Brand      = Syst.Parameters:gcBrand AND 
              TMSParam.ParamCode  = inp
   no-lock no-error.

   IF AVAIL TMSParam THEN DO:
      ret = TMSParam.DateVal.
      release TMSParam.
   END.

   RETURN ret.

END.


FUNCTION fCParam RETURNS CHAR 
  (INPUT icGroup AS CHAR,
   INPUT icCode  AS CHAR):
   
   DEF VAR lcRet AS CHAR NO-UNDO INIT ?.
   FIND FIRST TMSParam WHERE
              TMSParam.Brand      = Syst.Parameters:gcBrand AND
              TMSParam.ParamGroup = icGroup                 AND
              TMSParam.ParamCode  = icCode
   NO-LOCK NO-ERROR.
   
   IF AVAIL TMSParam THEN DO:
      lcRet = TMSParam.CharVal.
      RELEASE TMSParam.
   END.           

   RETURN lcRet.
END FUNCTION.   
&ENDIF
