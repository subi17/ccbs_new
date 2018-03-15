/* Adding multiple compatibilities to Matrix */
DEF VAR liCont   AS INTEGER NO-UNDO.
DEF VAR lcList   AS CHARACTER NO-UNDO.
DEF VAR lSuccess AS LOGICAL NO-UNDO.


FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
END FUNCTION.

/* Validation to avoid more than one execution of this program */
IF CAN-FIND(FIRST Matrix WHERE Matrix.Brand  = "1" AND
                               Matrix.MXName = "Additional line CONT33" AND
                               Matrix.MXKey  = "ADDLINE") THEN
DO:
   MESSAGE "Program already executed" VIEW-AS ALERT-BOX.
   RETURN.
END.

/* Validation to avoid more than one execution of this program */
IF NOT CAN-FIND(FIRST Matrix WHERE Matrix.Brand  = "1" AND
                                   Matrix.MXName = "Additional line CONT25" AND
                                   Matrix.MXKey  = "ADDLINEHM") THEN
DO:
   MESSAGE "Program cannot be executed." SKIP 
           "There is no Additional line CONT25 - ADDLINEHM" VIEW-AS ALERT-BOX.
   RETURN.
END.


/* ******************** Main block ******************** */
blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:

   lSuccess = FALSE.

   /* Adding CONT33 - Tariff La Sinfin 7Gb as an available additional line for the below tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT33"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINE" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating TO record in MXItem for CONT33 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT33".
   
   /* Compatibility with additional line CONT33 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.
   
   DISPLAY "Additional line CONT33 - ADDLINE - Created MXSeq:" Matrix.MXSeq.
   
   
   /* Adding CONT34 - Tariff La Ciento 2Gb as an available additional line for the below tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT34"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINE" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating TO record in MXItem for CONT34 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeTo"
      MXItem.MXValue = "CONT34".
   
   /* Compatibility with additional line CONT34 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT34 - ADDLINE - Created MXSeq:" Matrix.MXSeq.
   
   
   /* Adding CONT10 - Tariff La cero 1,5Gb mobile only tariff has available as additional lines the below MOBILE ONLY tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONT10".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT10"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINEHM" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating FROM record in MXItem for CONT10 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeFrom"
      MXItem.MXValue = "CONT10".
   
   /* Adding compatibility with additional line CONT10 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT10 - ADDLINEHM - Created MXSeq:" Matrix.MXSeq.

   
   /* Adding CONT15 - Tariff La ciento 5Gb mobile only tariff has available as additional lines the below MOBILE ONLY tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONT10,CONT15,CONT34".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT15"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINEHM" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating FROM record in MXItem for CONT15 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeFrom"
      MXItem.MXValue = "CONT15".
   
   /* Adding compatibility with additional line CONT15 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT15 - ADDLINEHM - Created MXSeq:" Matrix.MXSeq.
   
   
   /* Adding CONT33 - Tariff La sinfín 7Gb mobile only tariff has available as additional lines the below MOBILE ONLY tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONT10,CONT15,CONT33,CONT34".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT33"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINEHM" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating FROM record in MXItem for CONT33 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeFrom"
      MXItem.MXValue = "CONT33".
   
   /* Adding compatibility with additional line CONT33 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT33 - ADDLINEHM - Created MXSeq:" Matrix.MXSeq.
   
   
   /* Adding CONT34 - Tariff La Ciento 2Gb mobile only tariff has available as additional lines the below MOBILE ONLY tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONT10,CONT34".

   /* Creating Additional line Matrix Header */
   CREATE Matrix.
   ASSIGN 
      Matrix.Brand  = "1"  
      Matrix.MXName = "Additional line CONT34"
      Matrix.Prior  = 1
      Matrix.MXKey  = "ADDLINEHM" 
      Matrix.MXSeq  = fGetNextMXSeq()
      Matrix.MXRes  = 1.
   
   /* Creating FROM record in MXItem for CONT34 */
   CREATE MXItem.
   ASSIGN
      MXItem.MXSeq   = Matrix.MXSeq 
      MXItem.MxName  = "SubsTypeFrom"
      MXItem.MXValue = "CONT34".
   
   /* Adding compatibility with additional line CONT34 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT34 - ADDLINEHM - Created MXSeq:" Matrix.MXSeq.


   /* Adding CONT25 - Tariff La sinfín 25Gb mobile only tariff has available as additional lines the below MOBILE ONLY tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONT33,CONT34". /* Note: already exists compatibility for CONT10, CONT15 and CONT25 */


   FIND FIRST Matrix WHERE Matrix.Brand  = "1" AND
                           Matrix.MXName = "Additional line CONT25" AND
                           Matrix.MXKey  = "ADDLINEHM"  NO-LOCK. /* Checked at the begining */
                  
   /* Adding compatibility with mobile only line CONT25 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT25 - ADDLINEHM - Updated MXSeq:" Matrix.MXSeq.


   /* Adding CONT33 - Tariff La sinfín 7Gb mobile is available as an additional line to the below tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000".

   FIND FIRST Matrix WHERE Matrix.Brand  = "1" AND
                           Matrix.MXName = "Additional line CONT33" AND
                           Matrix.MXKey  = "ADDLINE"  NO-LOCK. /* Added in this program */
                           
   /* Adding compatibility with mobile only line CONT33 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq 
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT33 - ADDLINE - Updated MXSeq:" Matrix.MXSeq.


   /* Adding CONT34 - Tariff La ciento 2Gb mobile is available as an additional line to the below tariffs */
   ASSIGN 
       liCont = 0
       lcList = "CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000,CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000".

                  
   FIND FIRST Matrix WHERE Matrix.Brand  = "1" AND
                           Matrix.MXName = "Additional line CONT34" AND
                           Matrix.MXKey  = "ADDLINE"  NO-LOCK. /* Added in this program */
                           
   /* Adding compatibility with mobile only line CONT34 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = Matrix.MXSeq AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = Matrix.MXSeq
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.

   DISPLAY "Additional line CONT34 - ADDLINE - Updated MXSeq:" Matrix.MXSeq.
   
   
   lSuccess = TRUE.
      
END.

MESSAGE "Execution successful? :" lSuccess VIEW-AS ALERT-BOX.

