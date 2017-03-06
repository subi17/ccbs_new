/* ----------------------------------------------------------------------
  MODULE .......: premiumnumber.i
  TASK .........: Get premium service number name
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.11.11
  Version ......: Yoigo
----------------------------------------------------------------------- */

&IF "{&PremiumNumber_I}" NE "YES"
&THEN
&GLOBAL-DEFINE PremiumNumber YES

{Syst/commali.i}

FUNCTION fGetPremiumServiceName RETURNS CHAR
   (icGsmBnr AS CHAR,
    idtDate AS DATE):

   DEF VAR lcGsmBnrPrefix AS CHAR NO-UNDO. 
   DEF VAR liDigits AS INT NO-UNDO. 
   DEF VAR liLength AS INT NO-UNDO. 

   DEF BUFFER PremiumNumber FOR PremiumNumber.
   
   liLength = MIN(9,LENGTH(icGsmBnr)).
   IF liLength < 5 THEN RETURN "".
   
   DO liDigits = liLength TO 5 BY -1:
      lcGsmBnrPrefix = SUBSTRING(icGsmBnr,1,liDigits).
      FIND FIRST PremiumNumber WHERE
                 PremiumNumber.Brand = gcBrand AND
                 PremiumNumber.BNumberPrefix = lcGsmBnrPrefix AND
                 PremiumNumber.ValidTo >= idtDate AND
                 PremiumNumber.ValidFrom <= idtDate 
      NO-LOCK NO-ERROR.
      IF AVAIL PremiumNumber THEN RETURN PremiumNumber.OperatorName.
   END.

   RETURN "".

END.

&ENDIF
