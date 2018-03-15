/* Adding CONT33 - Tariff La sinfín 7Gb mobile is available as an additional line to the below tariffs */
DEF VAR liCont AS INTEGER NO-UNDO.
DEF VAR lcList AS CHARACTER NO-UNDO INITIAL
"CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000".


/* Main block */
blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:
                  
   /* Adding compatibility with mobile only line CONT33 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = 199 AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = 199 
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.
END.

DISPLAY "Updated: 199".