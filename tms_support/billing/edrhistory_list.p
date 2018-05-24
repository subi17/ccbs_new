{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "qvantel".
{billing/edrhistory_changes.i}

DEF VAR i           AS INT  NO-UNDO.
DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR lcSource    AS CHAR NO-UNDO.
DEF VAR lcBNumber   AS CHAR NO-UNDO.
DEF VAR lcCLIType   AS CHAR NO-UNDO.
DEF VAR ldFrom      AS DEC  NO-UNDO.
DEF VAR ldTo        AS DEC  NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR lcSesNum    AS CHAR NO-UNDO.
DEF VAR llDetails   AS LOG  NO-UNDO.

DEF STREAM sLog.

DEF TEMP-TABLE ttAmtChanged NO-UNDO
   FIELD CLIType AS CHAR 
   FIELD BillCode AS CHAR
   FIELD Amount AS DEC
   INDEX CLIType CLIType BillCode.

DEF TEMP-TABLE ttErrorChanged NO-UNDO
   FIELD CLIType AS CHAR 
   FIELD ErrorCode AS INT
   FIELD BillCode AS CHAR
   FIELD Amount AS DEC 
   FIELD Qty AS INT
   INDEX CLIType CLIType ErrorCode BillCode.

DEF TEMP-TABLE ttDetail NO-UNDO
   FIELD CLIType AS CHAR 
   FIELD ErrorCode AS INT
   FIELD BillCode AS CHAR
   FIELD Amount AS DEC
   FIELD NewErrorCode AS INT
   FIELD NewBillCode AS CHAR
   FIELD NewAmount AS DEC
   FIELD Qty AS INT
   INDEX CLIType CLIType BillCode ErrorCode
   INDEX NewCLIType CLIType NewBillCode NewErrorCode.

FUNCTION fCollect RETURNS LOGIC
   (icCLIType AS CHAR,
    iiErrorCode AS INT,
    icBillCode AS CHAR,
    idAmount AS DEC,
    iiQty AS INT):

   FIND FIRST ttAmtChanged WHERE
              ttAmtChanged.CLIType = icCLIType AND
              ttAmtChanged.BillCode = icBillCode NO-ERROR.
   IF NOT AVAILABLE ttAmtChanged THEN DO:
      CREATE ttAmtChanged.
      ASSIGN 
         ttAmtChanged.CLIType = icCLIType
         ttAmtChanged.BillCode = icBillCode.
   END.
   ttAmtChanged.Amount = ttAmtChanged.Amount + idAmount.
    
   FIND FIRST ttErrorChanged WHERE
              ttErrorChanged.CLIType = icCLIType AND
              ttErrorChanged.ErrorCode = iiErrorCode AND
              ttErrorChanged.BillCode = icBillCode NO-ERROR.
   IF NOT AVAILABLE ttErrorChanged THEN DO:
      CREATE ttErrorChanged.
      ASSIGN 
         ttErrorChanged.CLIType = icCLIType
         ttErrorChanged.ErrorCode = iiErrorCode
         ttErrorChanged.BillCode = icBillCode.
   END.
   ASSIGN
      ttErrorChanged.Qty = ttErrorChanged.Qty + iiQty
      ttErrorChanged.Amount = ttErrorChanged.Amount + idAmount.

END FUNCTION.    

FUNCTION fDetails RETURNS LOGIC
   (icCLIType AS CHAR,
    iiErrorCode AS INT,
    icBillCode AS CHAR,
    idAmount AS DEC,
    iiNewErrorCode AS INT,
    icNewBillCode AS CHAR,
    idNewAmount AS DEC):

   FIND FIRST ttDetail WHERE
              ttDetail.CLIType = icCLIType AND
              ttDetail.BillCode = icBillCode AND
              ttDetail.ErrorCode = iiErrorCode AND
              ttDetail.NewBillCode = icNewBillCode AND
              ttDetail.NewErrorCode = iiNewErrorCode NO-ERROR.
   IF NOT AVAILABLE ttDetail THEN DO: 
      CREATE ttDetail.
      ASSIGN 
         ttDetail.CLIType = icCLIType
         ttDetail.ErrorCode = iiErrorCode
         ttDetail.BillCode = icBillCode
         ttDetail.NewErrorCode = iiNewErrorCode
         ttDetail.NewBillCode = icNewBillCode.
   END.

   ASSIGN
      ttDetail.Amount = ttDetail.Amount + idAmount
      ttDetail.NewAmount = ttDetail.NewAmount + idNewAmount
      ttDetail.Qty = ttDetail.Qty + 1.
    
END FUNCTION.    


ASSIGN
   lcFile = "/tmp/edrhistory_" +
            STRING(YEAR(TODAY),"9999") +
            STRING(MONTH(TODAY),"99") + 
            STRING(DAY(TODAY),"99") + "_" +
            REPLACE(STRING(TIME,"hh:mm:ss"),":","") +
            ".txt"   
   lcSesNum = SESSION:NUMERIC-FORMAT
   SESSION:NUMERIC-FORMAT = "EUROPEAN".
   
PAUSE 0.
UPDATE 
   SKIP(1)
   ldaFromDate COLON 15
      LABEL "Rerated"
      HELP "Rerated on"
      FORMAT "99-99-99"
   "-"
   ldaToDate
      NO-LABEL
      HELP "Rerated on"
      FORMAT "99-99-99"
      SKIP
   lcSource COLON 15
      LABEL "Source"
      HELP "Rerate source"
      FORMAT "X(30)"
      SKIP(1)
   llDetails COLON 15
      LABEL "Details"
      HELP "List details"
      FORMAT "Yes/No"
      SKIP
   lcFile COLON 15
      LABEL "File"
      HELP "Output file"
      FORMAT "X(50)"
      SKIP(1)
WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " RERATE LOG " FRAME fLog.

HIDE FRAME fLog NO-PAUSE.   
IF ldaFromDate = ? OR ldaToDate = ? OR lcFile = "" THEN RETURN.

ASSIGN
   ldFrom = Func.Common:mMake2DT(ldaFromDate,0)
   ldTo   = Func.Common:mMake2DT(ldaToDate,86399).
                            
FOR EACH EDRHistory NO-LOCK WHERE
         EDRHistory.Brand = "1" AND
         EDRHistory.UpdateDate >= ldaFromDate AND
         EDRHistory.UpdateDate <= ldaToDate AND
         (IF lcSource > "" THEN EDRHistory.UpdateSource = lcSource ELSE TRUE)
BREAK BY EDRHistory.DateSt
      BY EDRHistory.DtlSeq
      BY EDRHistory.UpdateDate
      BY EDRHistory.UpdateTime:
         
   /* handle one edr only once (the oldest change) */
   IF NOT FIRST-OF(EDRHistory.DtlSeq) THEN NEXT.
   
   RUN pInitHistory(EDRHistory.CLI,
                    EDRHistory.DateSt,
                    EDRHistory.TimeSt,
                    EDRHistory.DtlSeq,
                    OUTPUT lcBNumber).
                    
   lcCLIType = "".
   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.MsSeq = EDRHistory.MsSeq AND 
             MsOwner.TsEnd >= ldFrom AND
             MsOwner.TsBeg <= ldTo:
      lcCLIType = MsOwner.CLIType.       
   END.
   IF lcCLIType = "" AND EDRHistory.MsSeq = 0 THEN 
   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.CLI = EDRHistory.CLI AND 
             MsOwner.TsEnd >= ldFrom AND
             MsOwner.TsBeg <= ldTo:
      lcCLIType = MsOwner.CLIType.       
   END.
   IF lcCLIType = "" THEN 
   FOR FIRST MsOwner NO-LOCK WHERE
             MSowner.MsSeq = EDRHistory.MsSeq:
      lcCLIType = MsOwner.CLIType.
   END.
   IF lcCLIType = "" AND EDRHistory.MsSeq = 0 THEN 
   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.CLI = EDRHistory.CLI:
      lcCLIType = MsOwner.CLIType.
   END.
   
   /* deduct the logged one and add the current one */
   fCollect(lcCLIType,
            EDRHistory.ErrorCode,
            EDRHistory.BillCode,
            -1 * EDRHistory.Amount,
            -1).
            
   FOR FIRST ttHistory WHERE 
             ttHistory.Rated = "Current":
      fCollect(lcCLIType,
               ttHistory.ErrorCode,
               ttHistory.BillCode,
               ttHistory.Amount,
               1).
               
      IF llDetails THEN 
         fDetails(lcCLIType,
                  EDRHistory.ErrorCode,
                  EDRHistory.BillCode,
                  EDRHistory.Amount,
                  ttHistory.ErrorCode,
                  ttHistory.BillCode,
                  ttHistory.Amount).
   END.

   i = i + 1.
   PAUSE 0.
   DISP i LABEL "Qty"
   WITH 1 DOWN ROW 10 SIDE-LABELS CENTERED TITLE "COLLECTING" FRAME fQty.
         
END.

HIDE FRAME fQty NO-PAUSE.

IF NOT CAN-FIND(FIRST ttAmtChanged) THEN DO:
   MESSAGE "No changes were found"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

OUTPUT STREAM sLog TO VALUE(lcFile).

PUT STREAM sLog UNFORMATTED           
   "Rerated: " 
      STRING(ldaFromDate,"99.99.9999") "-" 
      STRING(ldaToDate,"99.99.9999") CHR(9)
   "Source: " (IF lcSource > "" THEN lcSource ELSE "ALL") SKIP(1)
   "AMOUNT CHANGES" SKIP
   "CLIType"  CHR(9)
   "Bill.Item" CHR(9)
   "BI Name"   CHR(9)
   "Amount Change" SKIP.

FOR EACH ttAmtChanged WHERE 
         ttAmtChanged.Amount NE 0:

   FIND FIRST BillItem WHERE 
              BillItem.Brand = Syst.Var:gcBrand AND
              BillItem.BillCode = ttAmtChanged.BillCode NO-LOCK NO-ERROR.
              
   PUT STREAM sLog UNFORMATTED           
      ttAmtChanged.CLIType CHR(9)
      ttAmtChanged.BillCode CHR(9)
      (IF AVAILABLE BillItem THEN BillItem.BIName ELSE "") CHR(9)
      ttAmtChanged.Amount SKIP.

   /* show always the amount that affected this billing item, 
      i.e. the amount that was deducted from this item or added to this item 
   */
   FOR EACH ttDetail WHERE
            ttDetail.CLIType = ttAmtChanged.CLIType AND
            ttDetail.NewBillCode = ttAmtChanged.BillCode AND
            ttDetail.BillCode NE ttAmtChanged.BillCode 
   BREAK BY ttDetail.BillCode:
            
      ACCUMULATE ttDetail.NewAmount (TOTAL BY ttDetail.BillCode)
                 ttDetail.Qty    (TOTAL BY ttDetail.BillCode).
      
      IF LAST-OF(ttDetail.BillCode) THEN 
      PUT STREAM sLog UNFORMATTED 
         FILL(CHR(9),4)
         "FROM"  CHR(9)
         ttDetail.BillCode  CHR(9)
         (ACCUM TOTAL BY ttDetail.BillCode ttDetail.Qty) CHR(9)
         (ACCUM TOTAL BY ttDetail.BillCode ttDetail.NewAmount) SKIP.
   END.
 
   FOR EACH ttDetail WHERE
            ttDetail.CLIType = ttAmtChanged.CLIType AND
            ttDetail.BillCode = ttAmtChanged.BillCode AND
            ttDetail.NewBillCode NE ttAmtChanged.BillCode
   BREAK BY ttDetail.NewBillCode:
   
      ACCUMULATE ttDetail.Amount (TOTAL BY ttDetail.NewBillCode)
                 ttDetail.Qty    (TOTAL BY ttDetail.NewBillCode).
      
      IF LAST-OF(ttDetail.NewBillCode) THEN
      PUT STREAM sLog UNFORMATTED 
         FILL(CHR(9),4)
         "TO"    CHR(9)
         ttDetail.NewBillCode  CHR(9)
         -1 * (ACCUM TOTAL BY ttDetail.NewBillCode ttDetail.Qty) CHR(9)
         -1 * (ACCUM TOTAL BY ttDetail.NewBillCode ttDetail.Amount) SKIP.
   END.
 
END.

PUT STREAM sLog UNFORMATTED 
   SKIP(2)
   "ERROR CODE / BILLING ITEM CHANGES" SKIP
   "CLIType"  CHR(9)
   "Error Code" CHR(9)
   "Error Desc." CHR(9)
   "Bill.Item" CHR(9)
   "BI Name"   CHR(9)
   "Qty Change" CHR(9)
   "Amount Change" SKIP.

FOR EACH ttErrorChanged WHERE 
         ttErrorChanged.Qty NE 0:

   FIND FIRST BillItem WHERE 
              BillItem.Brand = Syst.Var:gcBrand AND
              BillItem.BillCode = ttErrorChanged.BillCode NO-LOCK NO-ERROR.
              
   FIND FIRST MobError WHERE MobError.MobError = ttErrorChanged.ErrorCode
      NO-LOCK NO-ERROR.
      
   PUT STREAM sLog UNFORMATTED 
      ttErrorChanged.CLIType CHR(9)
      ttErrorChanged.ErrorCode CHR(9)
      (IF AVAILABLE MobError THEN MobError.MEName ELSE "") CHR(9)
      ttErrorChanged.BillCode CHR(9)
      (IF AVAILABLE BillItem THEN BillItem.BIName ELSE "") CHR(9)
      ttErrorChanged.Qty CHR(9)
      ttErrorChanged.Amount SKIP.
      
   FOR EACH ttDetail WHERE
            ttDetail.CLIType = ttErrorChanged.CLIType AND
            ttDetail.NewBillCode = ttErrorChanged.BillCode AND
            ttDetail.NewErrorCode = ttErrorChanged.ErrorCode AND
            (ttDetail.BillCode NE ttErrorChanged.BillCode OR
             ttDetail.ErrorCode NE ttErrorChanged.ErrorCode):

      PUT STREAM sLog UNFORMATTED 
         FILL(CHR(9),7)
         "FROM" CHR(9)
         ttDetail.ErrorCode CHR(9)
         ttDetail.BillCode  CHR(9)
         ttDetail.Qty CHR(9)
         ttDetail.NewAmount SKIP.
   END.
 
   FOR EACH ttDetail WHERE
            ttDetail.CLIType = ttErrorChanged.CLIType AND
            ttDetail.ErrorCode = ttErrorChanged.ErrorCode AND
            ttDetail.BillCode = ttErrorChanged.BillCode AND
            (ttDetail.NewBillCode NE ttErrorChanged.BillCode OR
             ttDetail.NewErrorCode NE ttErrorChanged.ErrorCode):
      PUT STREAM sLog UNFORMATTED 
         FILL(CHR(9),7)
         "TO"  CHR(9)
         ttDetail.NewErrorCode CHR(9)
         ttDetail.NewBillCode  CHR(9)
         -1 * ttDetail.Qty CHR(9)
         -1 * ttDetail.Amount SKIP.
   END.
   
END.

OUTPUT STREAM sLog CLOSE.

IF lcSesNum > "" THEN 
   SESSION:NUMERIC-FORMAT = lcSesNum.
                            
