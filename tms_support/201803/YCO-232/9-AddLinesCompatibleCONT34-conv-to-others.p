/* Adding CONT34 - Tariff La ciento 2Gb mobile is available as an additional line to the below tariffs */
DEF VAR liCont AS INTEGER NO-UNDO.
DEF VAR lcList AS CHARACTER NO-UNDO INITIAL
"CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000,CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000".


/* Main block */
blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:
                  
   /* Adding compatibility with mobile only line CONT34 */
   DO liCont = 1 to NUM-ENTRIES(lcList):
       IF NOT CAN-FIND(FIRST MXItem WHERE MXItem.MXSeq   = 200 AND
                                          MXItem.MxName  = "SubsTypeFrom" AND
                                          MXItem.MXValue = ENTRY(liCont, lcList)) THEN
       DO:
          CREATE MXItem.
          ASSIGN
             MXItem.MXSeq   = 200
             MXItem.MxName  = "SubsTypeFrom"
             MXItem.MXValue = ENTRY(liCont, lcList).
       END.
   END.
END.

DISPLAY "Updated: 200".