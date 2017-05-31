DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

END FUNCTION.

FUNCTION fCreateMatrix RETURNS LOGICAL
   ( icMXName  AS CHARACTER,
     icMXKey   AS CHARACTER,
     iiMXRes   AS INTEGER,
     iiPrior   AS INTEGER ):
   
   FIND FIRST Matrix EXCLUSIVE-LOCK WHERE
      Matrix.Brand  = "1"      AND
      Matrix.MXKey  = icMXKey  AND
      Matrix.Prior  = iiPrior  AND
      Matrix.MXName = icMXName
   NO-ERROR.
   
   IF NOT AVAILABLE Matrix
   THEN CREATE Matrix.

   IF Matrix.MXSeq = 0
   THEN Matrix.MXSeq = fGetNextMXSeq().

   ASSIGN
      giMXSeq       = Matrix.MXSeq
      Matrix.Brand  = "1"
      Matrix.MXKey  = icMXKey
      Matrix.Prior  = iiPrior
      Matrix.MXName = icMXName
      Matrix.MXSeq  = iiMXSeq
      Matrix.MXRes  = iiMXRes.

END FUNCTION.

FUNCTION fCreateMXItem RETURNS LOGICAL
   ( iiMXSeq   AS INTEGER,
     icMXName  AS CHARACTER,
     icMXValue AS CHARACTER):
   
   FIND FIRST MXItem EXCLUSIVE-LOCK WHERE
      MXItem.MXSeq  = iiMXSeq AND
      MXItem.MXName = icMXName
   NO-ERROR.
   
   IF NOT AVAILABLE MXItem
   THEN CREATE MXItem.

   ASSIGN
      MXItem.MXSeq   = iiMXSeq
      MXItem.MXName  = icMXName
      MXItem.MXValue = icMXValue.

END FUNCTION.

FUNCTION fCreateSLGAnalyse RETURNS LOGICAL
   ( icClitype AS CHARACTER,
     icBillCode AS CHARACTER,
     iiCCN AS INTEGER,
     icBDest AS CHARACTER,
     icServiceLimitGroup AS CHARACTER,
     iiSLGAType AS INTEGER ):
   
   FIND FIRST SLGAnalyse EXCLUSIVE-LOCK WHERE
      SLGAnalyse.Brand    = "1"        AND
      SLGAnalyse.BelongTo = TRUE       AND
      SLGAnalyse.Clitype  = icClitype  AND
      SLGAnalyse.BillCode = icBillCode AND
      SLGAnalyse.CCN      = iiCCN      AND
      SLGAnalyse.BDest    = icBDest    AND
      SLGAnalyse.Prior    = 0          AND
      SLGAnalyse.ValidTo  = DATE(12,31,2049) AND
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup AND
      SLGAnalyse.SLGAType = iiSLGAnalyse
   NO-ERROR.
   
   IF NOT AVAILABLE SLGAnalyse
   THEN CREATE SLGAnalyse.

   ASSIGN
      SLGAnalyse.Brand    = "1"
      SLGAnalyse.BelongTo = TRUE
      SLGAnalyse.Clitype  = icClitype
      SLGAnalyse.BillCode = icBillCode
      SLGAnalyse.CCN      = iiCCN
      SLGAnalyse.BDest    = icBDest
      SLGAnalyse.Prior    = 0
      SLGAnalyse.ValidFrom = DATE(5,1,2017).
      SLGAnalyse.ValidTo  = DATE(12,31,2049).
      
END FUNCTION.


/* CONTF8: giMXSeq should get a new value after the call */
fCreateMatrix("CONTF8", "PERCONTR", 1, 54).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateMXItem(giMXSeq, "SubsTypeTo", "CONTF8").
fCreateSLGAnalyse("CONTF", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "CFYOIGO", 30, "*", "VOICE100", 1).

/* CONT9: giMXSeq should be 93 after the call */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 28).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateSLGAnalyse("CONT9", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "CFYOIGO", 30, "*", "VOICE100", 1).

/* CONT10: giMXSeq should be 120 after the call */
fCreateMatrix("CONT10", "PERCONTR", 1, 43).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateSLGAnalyse("CONT10", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "CFYOIGO", 30, "*", "VOICE100", 1).

