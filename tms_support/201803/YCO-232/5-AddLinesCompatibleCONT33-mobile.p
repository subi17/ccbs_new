/* Adding CONT34 - Tariff La sinfín 7Gb mobile only tariff has available as additional lines the below mobile only tariffs */
DEF VAR liCont AS INTEGER NO-UNDO.
DEF VAR lcList AS CHARACTER NO-UNDO INITIAL
"CONT10,CONT15,CONT33,CONT34".


FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
END FUNCTION.

/* Validation to avoid more than one execution */
IF CAN-FIND(FIRST Matrix WHERE Matrix.Brand  = "1" AND
                               Matrix.MXName = "Additional line CONT33" AND
                               Matrix.MXKey  = "ADDLINEHM") THEN
DO:
   MESSAGE "Program already executed" VIEW-AS ALERT-BOX.
   RETURN.
END.

/* Main block */
blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:
                  
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
END.

DISPLAY "Created:" Matrix.MXSeq.