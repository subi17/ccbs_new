FUNCTION fGetNextMatrixPriority RETURNS INTEGER 
   (icKey AS CHARACTER):

   DEFINE BUFFER bf_Matrix FOR Matrix.

   FIND LAST bf_Matrix WHERE bf_Matrix.mxkey = icKey NO-LOCK NO-ERROR.
   IF AVAILABLE bf_Matrix THEN 
      RETURN (bf_Matrix.Prior + 1).
   ELSE 
      RETURN 1.  

END FUNCTION.  

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

END FUNCTION.

FUNCTION fCreateMatrix RETURNS INTEGER
    (icName  AS CHARACTER):

    FIND FIRST Matrix NO-LOCK WHERE
        Matrix.Brand = "1" AND
        Matrix.MXKey = "EXTRALINE" AND
        Matrix.MXName = icName
    NO-ERROR.
    
    IF NOT AVAILABLE Matrix
    THEN DO:
        CREATE Matrix.
        ASSIGN
            Matrix.Brand  = "1"
            Matrix.MXName = icName
            Matrix.MXKey  = "EXTRALINE"
            Matrix.MXSeq  = fGetNextMXSeq()
            Matrix.Prior  = fGetNextMatrixPriority("EXTRALINE")
            Matrix.MXRes  = 1
            .
    END. 

    RETURN Matrix.MXSeq.

END FUNCTION.


FUNCTION fCreateMXItem RETURNS LOGICAL
    (iiMXSeq   AS INTEGER,
     icMXName  AS CHARACTER,
     icMXValue AS CHARACTER):

    FIND FIRST MXItem NO-LOCK WHERE
        MXItem.MXseq   = iiMXSeq  AND
        MXItem.MXName  = icMXName AND
        MXITem.MXValue = icMXValue
    NO-ERROR.
    
    IF NOT AVAILABLE MXItem
    THEN DO:
        CREATE MXItem.
        ASSIGN
            MXItem.MXseq   = iiMXSeq
            MXItem.MXName  = icMXName
            MXITem.MXValue = icMXValue
            .
    END. 

    RETURN FALSE.

END FUNCTION.

DEFINE VARIABLE liMXSeq AS INTEGER NO-UNDO.

liMXSeq = fCreateMatrix("CONT28").
fCreateMXItem(liMXSeq, "SubsTypeFrom", "CONTDSL3G").
fCreateMXItem(liMXSeq, "SubsTypeFrom", "CONTFH3G_50").
fCreateMXItem(liMXSeq, "SubsTypeFrom", "CONTFH3G_300").
fCreateMXItem(liMXSeq, "SubsTypeFrom", "CONTFH3G_1000").
