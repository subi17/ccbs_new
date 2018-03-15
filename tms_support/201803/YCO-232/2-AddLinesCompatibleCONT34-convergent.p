/* Adding CONT34 - Tariff La Ciento 2Gb as an available additional lines for the below tariffs */
DEF VAR liCont AS INTEGER NO-UNDO.
DEF VAR lcList AS CHARACTER NO-UNDO INITIAL
"CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000".


FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
END FUNCTION.

/* Validation to avoid more than one execution */
IF CAN-FIND(FIRST Matrix WHERE Matrix.Brand  = "1" AND
                               Matrix.MXName  = "Additional line CONT34" AND
                               Matrix.MXKey  = "ADDLINE") THEN
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
END.

DISPLAY "Created:" Matrix.MXSeq.