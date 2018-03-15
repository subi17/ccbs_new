/* Adding CONT25 - Tariff La sinfín 25Gb mobile only tariff has available as additional lines the below mobile only tariffs */
DEF VAR liCont AS INTEGER NO-UNDO.
DEF VAR lcList AS CHARACTER NO-UNDO INITIAL
"CONT33,CONT34". /* Note: already exists compatibility for CONT10, CONT15 and CONT25 */


/* Main block */
blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:
                  
   /* Adding compatibility with mobile only line CONT25 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = 150 AND
                                          MXItem.MxName  = "SubsTypeTo" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = 150 
             MXItem.MxName  = "SubsTypeTo"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.
END.

DISPLAY "Updated: 150".