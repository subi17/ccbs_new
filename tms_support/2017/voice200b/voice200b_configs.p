FUNCTION fCreateFee RETURNS CHAR
   (INPUT icDCName AS CHAR,
    INPUT icBillCode AS CHAR,
    INPUT ideAmount AS DECIMAL):

   DEF VAR lcFeeModel AS CHAR NO-UNDO.
   lcFeeModel = icBillCode + "MF".
   FIND FIRST FeeModel WHERE
               FeeModel.Brand = "1" AND
               FeeModel.FeeModel = lcFeeModel NO-LOCK NO-ERROR.
   IF NOT AVAIL FeeModel THEN
   DO:

       CREATE FeeModel.
       ASSIGN
           FeeModel.Brand    = "1"
           FeeModel.FeeModel = lcFeeModel
           FeeModel.FeeName  = icDCName
           FeeModel.FMGroup  = 0.

       FIND FIRST FMItem WHERE FMItem.Brand     = "1"         AND
                        FMItem.FeeModel  = lcFeeModel AND
                        FMItem.PriceList = "COMMON"     AND
                        FMItem.BillCode  = icBillCode AND
                        FMItem.ToDate    > TODAY      NO-LOCK NO-ERROR.
      IF NOT AVAIL FMItem THEN
      DO:
          CREATE FMItem.
          ASSIGN
              FMItem.Brand             = "1"
               FMItem.PriceList         = "COMMON"
               FMItem.BillCode          = icBillCode
               FMItem.FeeModel          = lcFeeModel
               FMItem.FromDate          = TODAY
               FMItem.ToDate            = DATE(12,31,2049)
               FMItem.BillType          = "MF"
               FMItem.Interval          = 1
               FMItem.BillCycle         = 2
               FMItem.FFItemQty         = 0
               FMItem.FFEndDate         = ?
               FMItem.Amount            = ideAmount
               FMItem.FirstMonthBR      = 0
               FMItem.BrokenRental      = 1
               FMItem.ServiceLimitGroup = "".
       END.
   END.

 RETURN "".

END.

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
      SLGAnalyse.SLGAType = iiSLGAType
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
      SLGAnalyse.Prior    = 5
      SLGAnalyse.ValidFrom = DATE(08,01,2017)
      SLGAnalyse.ValidTo  = DATE(12,31,2049)
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup
      SLGAnalyse.SLGAType = iiSLGAType
      .

END FUNCTION.


FUNCTION fcreateVoiceSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icgroup AS CHAR):
   fCreateSLGAnalyse(icClitype, "10100001", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100003", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100005", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFOTHER", 30, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFYOIGO", 30, "*", icGroup, 1).
END.

fcreateVoiceSLGAnalyses("CONTDSL39", "VOICE200B").
fcreateVoiceSLGAnalyses("CONTFH39_50", "VOICE200B").
fcreateVoiceSLGAnalyses("CONTFH49_300", "VOICE200B").
fcreateVoiceSLGAnalyses("CONT10", "VOICE200B").


fCreateFee("Voice 200 package",
           "VOICE200B",
            1.65).



