/* mobcdr_bdest.i    11.11.11/aam 
*/
{Syst/commali.i}

FUNCTION fGetMobCDRBType RETURNS INT
   (iiCallCase AS INT,
    icBDestination AS CHAR,
    iiBType AS INT):

   CASE iiCallCase:
   
      /* destination type is not used with roaming */
      WHEN 3  THEN iiBType = ?.
      WHEN 4  THEN iiBType = ?.
      WHEN 7  THEN iiBType = ?.
      WHEN 17 THEN iiBType = ?.

      WHEN 51 THEN DO:
         IF icBDestination = "500" OR icBDestination = "SMS" THEN 
            iiBType = 90.
         ELSE IF iiBType = 1 THEN iiBType = 1.
         ELSE IF iiBType = 4 THEN iiBType = 4.
         ELSE iiBType = 5.
      END.
      
      WHEN 53 THEN iiBType = 0.
      WHEN 54 THEN iiBType = 0.
   END CASE. 
     
   RETURN iiBType.

END FUNCTION.

FUNCTION fMobCDRBDestName RETURNS CHAR
   (iiCallCase AS INT,
    icBDestination AS CHAR,
    iiBType AS INT,
    idaDate AS DATE):
    
   DEF VAR liBType AS INT NO-UNDO.
   
   liBType = fGetMobCDRBType(iiCallCase,
                             icBDestination,
                             iiBType).
                         
   IF liBType = ? THEN 
   FIND FIRST BDest WHERE
      Bdest.Brand    = gcBrand      AND 
      BDest.BDest    = icBDestination AND
      BDest.ToDate  >= idaDate AND
      BDest.FromDate <= idaDate NO-LOCK NO-ERROR.
   ELSE FIND FIRST BDest WHERE
      Bdest.Brand    = gcBrand      AND 
      BDest.BDest    = icBDestination AND
      BDest.DestType = liBType AND
      BDest.ToDate  >= idaDate AND
      BDest.FromDate <= idaDate NO-LOCK NO-ERROR.

   IF AVAILABLE BDest THEN RETURN BDest.BDName.
   ELSE RETURN "".
    
END FUNCTION.

