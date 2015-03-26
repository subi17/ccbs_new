/* fhdrtext.i     29.08.07/aam 

*/

FUNCTION fGetHdrText RETURNS CHARACTER
   (INPUT iiTextNbr  AS INT, 
    INPUT iiLanguage AS INT):

   FIND HdrText WHERE 
        HdrText.Brand  = gcBrand AND
        HdrText.te-nro = iiTextNbr    AND
        HdrText.te-kie = iiLanguage NO-LOCK NO-ERROR.
   IF AVAIL HdrText THEN RETURN HdrText.te-text.

   ELSE IF iiLanguage = 1 THEN RETURN "". 
    
   /* use default language */
   FIND HdrText WHERE 
        HdrText.Brand  = gcBrand AND
        HdrText.te-nro = iiTextNbr    AND  
        HdrText.te-kie = 1 NO-LOCK NO-ERROR.
   IF AVAIL HdrText THEN RETURN HdrText.te-text.

   ELSE RETURN "".

END FUNCTION.

