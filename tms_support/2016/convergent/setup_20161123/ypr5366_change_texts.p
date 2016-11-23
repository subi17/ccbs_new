def stream sOut.
def var lcFile AS CHAR NO-UNDO INIT "ypr5366.txt".

FUNCTION fCreateUpdateHeaTxt RETURNS CHAR
   ( iiLang AS INT,
     iiCode AS INT,
     icText AS CHAR
   ):

   FIND FIRST HdrText NO-LOCK WHERE
              HdrText.Brand EQ "1" AND
              HdrText.te-nro EQ iiCode AND
              HdrText.te-kie EQ iiLang NO-ERROR.
   IF AVAIL HdrText THEN DO:
      MESSAGE  "Text already found " + 
                STRING(iiCode) + " " + 
                STRING(iiLang) VIEW-AS ALERT-BOX.
      HdrText.te-text = icText.     
      RETURN "".
   END.

   message "creating hdrtext" VIEW-AS ALERT-BOX.
   CREATE HdrText.
   ASSIGN
      HdrText.Brand = "1"
      HdrText.te-nro = iiCode
      HdrText.te-kie = iiLang
      HdrText.te-text = icText.

   RETURN "".

END.

DEF VAR lcRet AS CHAR NO-UNDO.

/*Old 528 is copied to 573 and part of the old text is removed
The text is for premium services*/


OUTPUT STREAM sOut to VALUE(lcFile) APPEND.

FOR EACH hdrtext NO-LOCK where 
         hdrtext.te-nro EQ 532 or
         hdrtext.te-nro EQ 510 or
         hdrtext.te-nro EQ 509:
   put stream sOut UNFORMATTED 
   STRING(hdrtext.te-nro) + ";" + STRING(HdrText.te-kie) + ";" + HdrText.te-text SKIP.
END.

/*fixed line permanency text*/
lcRet = fCreateUpdateHeaTxt(1,
                      532,
                      "12 meses por el descuento de 100 euros en el coste de instalación, regulado en la condición particular de permanencia del las CCGGs de fijo.").

/*mobile permanency texts*/
lcRet = fCreateUpdateHeaTxt(1,
                      510,
                      "#YY meses en Yoigo y en tarifa por el descuento de #XXX euros ya incluido en el precio.").



FOR EACH hdrtext NO-LOCK where 
         hdrtext.te-nro EQ 532 or
         hdrtext.te-nro EQ 510 or
         hdrtext.te-nro EQ 509:
   put stream sOut UNFORMATTED 
   STRING(hdrtext.te-nro) + ";" + STRING(HdrText.te-kie) + ";" + HdrText.te-text SKIP.
END.



