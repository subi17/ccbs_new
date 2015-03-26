DEF BUFFER bDispCom FOR ServCom.

FUNCTION fGetServiceName RETURNS CHARACTER
(INPUT ServiceCodes AS CHAR):

   DEF VAR lcServiceNames AS C NO-UNDO.
   DEF VAR liLoop         AS I NO-UNDO.

   DO liloop = 1 TO  Num-entries(ServiceCodes,"."):
      FIND FIRST bDispCom WHERE
                 bDispCom.Brand   = gcBrand    AND 
                 bDispCom.servCom = ENTRY(liLoop,ServiceCodes,".") 
      NO-LOCK NO-ERROR.
      IF AVAIL bDispCom THEN
      ASSIGN
         lcServiceNames = lcServiceNames + bDispCom.scname + "+".
   END.
   lcServiceNames = SUBSTRING(lcServiceNames,1,LENGTH(lcServiceNames) - 1).

   RETURN lcServiceNames.
END.



FUNCTION fGetLocalName RETURNS CHARACTER
(INPUT ServiceCodes AS CHAR):

   DEF VAR lcServiceNames AS C NO-UNDO.
   DEF VAR liLoop         AS I NO-UNDO.

   DO liloop = 1 TO  Num-entries(ServiceCodes,"."):
      FIND FIRST bDispCom WHERE
                 bDispCom.Brand   = gcBrand    AND 
                 bDispCom.servCom = ENTRY(liLoop,ServiceCodes,".")
      NO-LOCK NO-ERROR.
      IF AVAIL bDispCom THEN
      ASSIGN
         lcServiceNames = lcServiceNames + bDispCom.ScLocalName + "+".
    END.
    lcServiceNames = SUBSTRING(lcServiceNames,1,LENGTH(lcServiceNames) - 1).

    RETURN lcServiceNames.
END.

