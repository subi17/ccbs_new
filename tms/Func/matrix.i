&IF "{&matrix}" NE "YES"
&THEN
&GLOBAL-DEFINE matrix YES

FUNCTION fMatrixAnalyse RETURN INT
   (INPUT  icBrand    as CHAR, 
    INPUT  icKey      as CHAR, 
    INPUT  icname     AS CHAR,
    INPUT  icValue    AS CHAR,
    OUTPUT ocResponse AS CHAR).
   
   DEF VAR oiResult AS INT  NO-UNDO INIT 99.
   DEF VAR liLoop   AS INT  NO-UNDO.
   DEF VAR lcResponse AS CHAR NO-UNDO.

   IF NUM-ENTRIES(icValue,";") ne 
      NUM-ENTRIES(icName,";") THEN DO:
      
      oiResult = ?.
      RETURN oiResult.
   END.   

   ocResponse = "".
   
   MATRIX:
   FOR EACH Matrix WHERE 
            Matrix.Brand = icBrand AND 
            Matrix.MXKey = icKey   NO-LOCK 
   BREAK 
   BY Matrix.Prior:

      ASSIGN
      lcResponse = ""
      oiREsult   = 99.

      DO liLoop = 1 TO NUM-ENTRIES(icName,";"):

         IF NOT CAN-FIND(FIRST MXItem WHERE 
                               MXItem.MXseq  = Matrix.MXSeq  AND 
                               MXItem.MXName = ENTRY(liloop,icname,";"))
         THEN DO:
            lcResponse = lcResponse + STRING(Matrix.MXRes) + ";".
            NEXT.
         END.

         FIND FIRST MXItem WHERE
                    MXItem.MXseq   = Matrix.MXSeq  AND
                    MXItem.MXName  = ENTRY(liloop,icname,";") AND
                    MXITem.MXValue = ENTRY(liLoop,icvalue,";")
         NO-LOCK NO-ERROR.
                         
         IF NOT AVAILABLE MxItem THEN                     
         FIND FIRST MXItem WHERE
                    MXItem.MXseq   = Matrix.MXSeq  AND
                    MXItem.MXName  = ENTRY(liloop,icname,";") AND
                    ENTRY(liLoop,icvalue,";") MATCHES MxItem.MXValue
         NO-LOCK NO-ERROR.
 
         IF AVAIL MXItem THEN DO:
            lcResponse = lcResponse + STRING(Matrix.MXRes) + ";".      
            IF oiResult = 99 THEN oiResult = Matrix.MXres.
         END.
         ELSE DO:
            ASSIGN
               lcResponse = lcResponse + "?;"
               oiResult   = ?.
         END.
      END.

      IF ocResponse = "" OR oiResult NE ? OR ENTRY(1,lcResponse,";") NE "?"
      THEN ocResponse = lcResponse.
        
      /* perfect match */
      IF oiResult = Matrix.MXRes then LEAVE MATRIX.
      
   END.         

   IF ocResponse = "" THEN DO:
      oiResult = ?.
   END.
   ELSE IF oiResult ne ? AND ocresponse ne "" THEN DO:
      oiResult = Matrix.MXres.
   END.
        
   RETURN oiResult.           
              
END FUNCTION.


FUNCTION fListMatrix RETURN INT
   (INPUT  icBrand    as CHAR,
    INPUT  icKey      as CHAR,
    INPUT  icname     AS CHAR,
    INPUT  icValue    AS CHAR,
    OUTPUT ocResponse AS CHAR).

   DEF VAR oiResult  AS INT NO-UNDO.
   oiResult = ?.

   FOR EACH Matrix WHERE
            Matrix.Brand = icBrand AND
            Matrix.MXKey = icKey
      NO-LOCK BREAK BY Matrix.Prior:

      IF Matrix.MXRes <> 1 THEN NEXT.

      IF NOT CAN-FIND(FIRST MXItem WHERE
                            MXItem.MXseq   = Matrix.MXSeq  AND
                            MXItem.MXName  = ENTRY(1,icname,";") AND
                            LOOKUP(icvalue,MXITem.MXValue) > 0) THEN NEXT.

      FIND FIRST MXItem WHERE
                 MXItem.MXseq   = Matrix.MXSeq  AND
                 MXItem.MXName  = ENTRY(2,icname,";")
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MXItem THEN NEXT.

      ASSIGN ocResponse = ocResponse + "," + MXITem.MXValue
             ocResponse = TRIM(ocResponse,",").
             oiResult   = Matrix.MXres.
   END. /* FOR EACH Matrix WHERE */

   RETURN oiResult.

END FUNCTION.
  
&ENDIF
