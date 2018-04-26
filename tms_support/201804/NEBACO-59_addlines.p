/* Setting additional lines for NEBA tariffs.  */
/* https://kethor.qvantel.com/browse/NEBACO-59 */
/* 26.04.2018. jotorres                        */

DEF VAR lcAddLineCliTypes AS CHAR NO-UNDO INITIAL 
   "CONT10,CONT15,CONT25,CONT26,CONT33,CONT34".
   
DEF VAR lcNebaCliTypes AS CHAR NO-UNDO INITIAL
   "CONTFHNB45_300,CONTFHNB109_300,CONTFHNB2G_300,CONTFHNB3G_300,CONTFHNB58_300,CONTFHNB62_300,CONTFHNB69_300,CONTFHNB7G_300". 
 
DEF BUFFER bMXItem FOR MXItem.    
    
DEF VAR lii AS INT NO-UNDO.
DEF VAR lij AS INT NO-UNDO.
  
DO lii = 1 TO NUM-ENTRIES (lcAddLineClitypes):
   FIND FIRST Matrix NO-LOCK WHERE 
              Matrix.Brand  EQ "1"       AND 
              Matrix.MXKey  EQ "ADDLINE" AND
              Matrix.MXName EQ "Additional line " + ENTRY(lii, lcAddLineClitypes)
        NO-ERROR.
   IF AVAILABLE Matrix THEN 
   DO lij = 1 TO NUM-ENTRIES (lcNebaCliTypes):
      FIND FIRST MXItem NO-LOCK WHERE 
                 MXItem.MXSeq   EQ Matrix.MXSeq               AND
                 MXItem.MXName  EQ "SubsTypeFrom"             AND 
                 MXItem.MXValue EQ ENTRY(lij, lcNebaCliTypes)
            NO-ERROR.
      IF NOT AVAILABLE MXItem AND 
         CAN-FIND(FIRST bMXItem WHERE 
                        bMXItem.MXSeq   EQ Matrix.MXSeq               AND
                        bMXItem.MXName  EQ "SubsTypeFrom"             AND 
                        bMXItem.MXValue EQ REPLACE(ENTRY(lij, lcNebaCliTypes), "CONTFHNB","CONTFH"))
      THEN DO:
         CREATE MXItem.
         ASSIGN 
            MXItem.MXSeq   = Matrix.MXSeq
            MXItem.MXName  = "SubsTypeFrom"
            MXItem.MXValue = ENTRY(lij, lcNebaCliTypes).  
      END.          
   END.      
END.
