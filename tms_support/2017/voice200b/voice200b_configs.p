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



fCreateFee("Voice 200 package",
           "VOICE200B",
            1.65).



