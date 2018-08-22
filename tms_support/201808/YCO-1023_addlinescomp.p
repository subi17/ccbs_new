/*---------------------------------------------------------------------- 
YCO-1023. Additional line compatibility for new Mobile Only PRO tariffs.
 /tms_support/201808/YCO-1023_addlinescompc.p 
jotorres 22.08.2018
-----------------------------------------------------------------------*/

DEFINE VARIABLE lcCliTypesList AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount        AS INTEGER   NO-UNDO.

/* Returns next MXSeq for Matrix */
FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
   
END FUNCTION.

/* -------- MAIN -------- */

/* Avoid more than one execution of the script. */
IF CAN-FIND(FIRST Matrix WHERE 
                  Matrix.Brand EQ  "1"      AND 
                  Matrix.MXKey EQ "ADDLINE" AND 
                  (Matrix.MXName EQ "Additional line CONT36" OR
                   Matrix.MXName EQ "Additional line CONT37" OR
                   Matrix.MXName EQ "Additional line CONT38" OR
                   Matrix.MXName EQ "Additional line CONT39")) THEN
   RETURN.

/* Creating Matrix for CONT36 */
CREATE Matrix.
ASSIGN 
   Matrix.Brand  = "1"  
   Matrix.MXName = "Additional line CONT36"
   Matrix.MXKey  = "ADDLINE" 
   Matrix.MXSeq  = fGetNextMXSeq()
   Matrix.MXRes  = 1
   Matrix.Prior  = 1.
 
/* Creating MXItem */
lcCliTypesList = 
   "CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL59,CONTFH59_50,"         +
   "CONTFH69_300,CONTFH89_1000,CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000,"    +
   "CONTDSL99,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONTDSLTB59,CONTFHTB59_50,"   +
   "CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300,CONTFHNB109_300,CONTFHNB58_300," +
   "CONTFHNB69_300,CONTFHNB62_300".

/* SubsTypeTo */
IF NOT CAN-FIND(FIRST MXItem WHERE 
                      MXItem.MXSeq   EQ Matrix.MXSeq AND
                      MXItem.MxName  EQ "SubsTypeTo" AND
                      MXItem.MXValue EQ "CONT36") THEN DO:
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT36".                          
END. 
/* SubsTypeFrom */                        
DO liCount = 1 TO NUM-ENTRIES(lcCliTypesList):
   IF NOT CAN-FIND(FIRST MXItem WHERE 
                         MXItem.MXSeq   EQ Matrix.MXSeq AND
                         MXItem.MxName  EQ "SubsTypeFrom" AND
                         MXItem.MXValue EQ ENTRY(liCount, lcCliTypesList)) THEN DO:
      CREATE MXItem.
      ASSIGN
         MXItem.MXSeq   = Matrix.MXSeq 
         MXItem.MxName  = "SubsTypeFrom"
         MXItem.MXValue = ENTRY(liCount, lcCliTypesList).
      RELEASE MXItem.   
   END.   
END.     
RELEASE Matrix.   
                
/* Creating Matrix for CONT37 */
CREATE Matrix.
ASSIGN 
   Matrix.Brand  = "1"  
   Matrix.MXName = "Additional line CONT37"
   Matrix.MXKey  = "ADDLINE" 
   Matrix.MXSeq  = fGetNextMXSeq()
   Matrix.MXRes  = 1
   Matrix.Prior  = 1.
 
/* Creating MXItem */
lcCliTypesList = 
   "CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL59,CONTFH59_50,"         +
   "CONTFH69_300,CONTFH89_1000,CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000,"    +
   "CONTDSL99,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONTDSLTB59,CONTFHTB59_50,"   +
   "CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300,CONTFHNB109_300,CONTFHNB58_300," +
   "CONTFHNB69_300,CONTFHNB62_300". 
 

/* SubsTypeTo */
IF NOT CAN-FIND(FIRST MXItem WHERE 
                      MXItem.MXSeq   EQ Matrix.MXSeq AND
                      MXItem.MxName  EQ "SubsTypeTo" AND
                      MXItem.MXValue EQ "CONT37") THEN DO:
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT37".                          
END. 
/* SubsTypeFrom */                        
DO liCount = 1 TO NUM-ENTRIES(lcCliTypesList):
   IF NOT CAN-FIND(FIRST MXItem WHERE 
                         MXItem.MXSeq   EQ Matrix.MXSeq AND
                         MXItem.MxName  EQ "SubsTypeFrom" AND
                         MXItem.MXValue EQ ENTRY(liCount, lcCliTypesList)) THEN DO:
      CREATE MXItem.
      ASSIGN
         MXItem.MXSeq   = Matrix.MXSeq 
         MXItem.MxName  = "SubsTypeFrom"
         MXItem.MXValue = ENTRY(liCount, lcCliTypesList).
      RELEASE MXItem.   
   END.   
END.     
RELEASE Matrix.    

/* Creating Matrix for CONT38 */
CREATE Matrix.
ASSIGN 
   Matrix.Brand  = "1"  
   Matrix.MXName = "Additional line CONT38"
   Matrix.MXKey  = "ADDLINE" 
   Matrix.MXSeq  = fGetNextMXSeq()
   Matrix.MXRes  = 1
   Matrix.Prior  = 1.
 
/* Creating MXItem */
lcCliTypesList = 
   "CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL59,"      +
   "CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTDSL99,CONTFH99_50,"    + 
   "CONTFH109_300,CONTFH129_1000,CONTDSLTB59,CONTFHTB59_50,"          +
   "CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300,CONTFHNB109_300," +
   "CONTFHNB69_300,CONTFHNB62_300".
   
/* SubsTypeTo */
IF NOT CAN-FIND(FIRST MXItem WHERE 
                      MXItem.MXSeq   EQ Matrix.MXSeq AND
                      MXItem.MxName  EQ "SubsTypeTo" AND
                      MXItem.MXValue EQ "CONT38") THEN DO:
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT38".                          
END. 
/* SubsTypeFrom */                        
DO liCount = 1 TO NUM-ENTRIES(lcCliTypesList):
   IF NOT CAN-FIND(FIRST MXItem WHERE 
                         MXItem.MXSeq   EQ Matrix.MXSeq AND
                         MXItem.MxName  EQ "SubsTypeFrom" AND
                         MXItem.MXValue EQ ENTRY(liCount, lcCliTypesList)) THEN DO:
      CREATE MXItem.
      ASSIGN
         MXItem.MXSeq   = Matrix.MXSeq 
         MXItem.MxName  = "SubsTypeFrom"
         MXItem.MXValue = ENTRY(liCount, lcCliTypesList).
      RELEASE MXItem.   
   END.   
END.     
RELEASE Matrix. 

/* Creating Matrix for CONT39 */
CREATE Matrix.
ASSIGN 
   Matrix.Brand  = "1"  
   Matrix.MXName = "Additional line CONT39"
   Matrix.MXKey  = "ADDLINE" 
   Matrix.MXSeq  = fGetNextMXSeq()
   Matrix.MXRes  = 1
   Matrix.Prior  = 1.
 
/* Creating MXItem */
lcCliTypesList = 
   "CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTDSL99,"    +
   "CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONTDSLTB59,"          +
   "CONTFHTB59_50,CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300," +
   "CONTFHNB109_300,CONTFHNB69_300".
   
/* SubsTypeTo */
IF NOT CAN-FIND(FIRST MXItem WHERE 
                      MXItem.MXSeq   EQ Matrix.MXSeq AND
                      MXItem.MxName  EQ "SubsTypeTo" AND
                      MXItem.MXValue EQ "CONT39") THEN DO:
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT39".                          
END. 
/* SubsTypeFrom */                        
DO liCount = 1 TO NUM-ENTRIES(lcCliTypesList):
   IF NOT CAN-FIND(FIRST MXItem WHERE 
                         MXItem.MXSeq   EQ Matrix.MXSeq AND
                         MXItem.MxName  EQ "SubsTypeFrom" AND
                         MXItem.MXValue EQ ENTRY(liCount, lcCliTypesList)) THEN DO:
      CREATE MXItem.
      ASSIGN
         MXItem.MXSeq   = Matrix.MXSeq 
         MXItem.MxName  = "SubsTypeFrom"
         MXItem.MXValue = ENTRY(liCount, lcCliTypesList).
      RELEASE MXItem.   
   END.   
END.     
RELEASE Matrix. 
