{Func/timestamp.i}

DEFINE STREAM sTestIds.

OUTPUT STREAM sTestIds TO VALUE("/home/harrim/testids_newest_20080908.txt").

FUNCTION fGetMonthsAgo RETURN DATE (INPUT dtStartDate AS DATE, INPUT piMonthCount AS INTEGER):
   DEFINE VARIABLE iMonth AS INTEGER NO-UNDO. 
   REPEAT iMonth = 1 to piMonthCount:
       IF MONTH(dtStartDate) > 1 THEN
          dtStartDate = DATE(MONTH(dtStartDate) - 1, DAY(dtStartDate),  YEAR(dtStartDate)).
       ELSE 
          dtStartDate = DATE(12, DAY(dtStartDate), YEAR(dtStartDate) - 1).

   END. 
   RETURN dtStartDate.
END.


FUNCTION fOutputIds RETURN LOGICAL (INPUT pcText AS CHARACTER):
   PUT STREAM sTestIds UNFORMATTED "-----------------------------------------------" SKIP.
   PUT STREAM sTestIds UNFORMATTED "Header: " pcText SKIP.

   PUT STREAM sTestids UNFORMATTED "OrderId: " Order.OrderId SKIP.
   PUT STREAM sTestIds UNFORMATTED "MobSub.MsSeq:" MobSub.MsSeq SKIP.
   PUT STREAM sTestIds UNFORMATTED "Customer.OrgId (personal id): " Customer.OrgId SKIP.

   PUT STREAM sTestIds UNFORMATTED "CLI: " Order.CLI SKIP.
END.

FUNCTION fIsBarred RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE BUFFER xMobSub FOR MobSub.
   FIND xMobSub WHERE xMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF AVAIL xMobSub THEN
      IF xMobSub.MsStatus eq 8 THEN RETURN TRUE. ELSE RETURN FALSE.
   RETURN FALSE.

END.


FUNCTION fCheckIfBarringHistoryInMonths RETURN LOGICAL 
   (INPUT piMsSeq AS INTEGER, INPUT piDayCount AS INTEGER):
   
   DEFINE VARIABLE ldeTimeStamp AS INTEGER NO-UNDO. 
   ldeTimeStamp = fMake2Dt(TODAY - piDayCount, 0).
                
   FOR FIRST MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = piMsSeq AND
            MsRequest.ReqType = 35    AND
            MsRequest.ReqStat = 2     AND 
            MsRequest.DoneStamp > ldeTimeStamp AND 
            LOOKUP(MsRequest.ReqCParam1,"UNY_HURG,UNY_REST,UNY_SARC") > 0:
   
      RETURN TRUE.

   END. /* FOR EACH */
   
   RETURN FALSE.
END.


FUNCTION fGetOneLimitValues RETURN LOGICAL (
   INPUT dtCr AS DATE, 
   INPUT plBothEndLimited AS LOGICAL,
   INPUT dtLimitDate AS DATE,
   INPUT pcBeginText AS CHARACTER, 
   INPUT pcLimitText AS CHARACTER):

    IF dtLimitDate ne ? THEN
    DO:
       IF dtCr = dtLimitDate - 1 THEN
       DO:
          fOutputIds(pcBeginText + ", Day before " + pcLimitText + " limit").
          RETURN TRUE.
       END.
       ELSE IF dtCr = dtLimitDate THEN
       DO:
          fOutputIds(pcBeginText + ", Just on " + pcLimitText + " limit").
          RETURN TRUE.
       END.
       ELSE
       DO:
          fOutputIds(pcBeginText + ", other time before " + pcLimitText + " limit").
          RETURN TRUE.
       END.

       IF plBothEndLimited AND dtCr = dtLimitDate + 1 THEN
       DO:
          fOutputIds(pcBeginText + ", Day after " + pcLimitText + " limit").
          RETURN TRUE.
       END.
    END.
    ELSE
       fOutputIds(pcBeginText + ", " + pcLimitText + " limit").
  
    RETURN FALSE.
END.


FUNCTION fPeriodicalContractExist RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE lFound AS LOGICAL NO-UNDO. 
   lFound = FALSE.

   DEFINE BUFFER xMobSub FOR MobSub.
   FIND xMobSuB WHERE xMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF AVAIL xMobSub THEN
   DO:
      LoopDCCLI:
      FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq AND DCCLI.DCEvent eq "TERM18" AND DCCLI.ValidTo > TODAY NO-LOCK:
         lFound = TRUE. 
         LEAVE LoopDCCLI.
      END.
   END.
   
   RETURN lFound. 

END.


FUNCTION fIsPreactivated RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE BUFFER xMobSub FOR MobSub.
   FIND xMobSuB WHERE xMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF AVAIL xMobSub THEN
   DO:
       IF xMobSub.CLIType = "TARJ3" THEN RETURN TRUE.
       RETURN FALSE.
   END.
   RETURN FALSE.
END.


FUNCTION fGetValuesOnLimits RETURN LOGICAL (
      INPUT dtCr AS DATE, 
      INPUT dtPastLimit AS DATE, 
      INPUT dtLatestLimit AS DATE,
      INPUT pcBeginText AS CHARACTER,
      INPUT pcPastLimitText AS CHARACTER,
      INPUT pcLatestLimitText AS CHARACTER):

    DEFINE VARIABLE lBothEndLimited AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE lAccepted AS LOGICAL NO-UNDO. 
    lBothEndLimited = FALSE.
    lAccepted = FALSE.
 
    IF dtLatestLimit ne ? AND dtPastLimit ne ? THEN
       lBothEndLimited = TRUE.

    IF dtLatestLimit = ? AND dtPastLimit = ? THEN
       lAccepted = TRUE.
   
    IF dtPastLimit ne ? OR dtLatestLimit ne ? THEN
    DO:
       IF lBothEndLimited THEN 
       DO:
           IF dtCr >= dtPastLimit AND dtCr <= dtLatestLimit THEN lAccepted = TRUE.
       END. 
       ELSE
       DO:
           IF dtCr <= dtPastLimit THEN lAccepted = TRUE.
       END.
    END.
 
    IF lAccepted THEN
    DO:
       IF lBothEndLimited THEN
       DO:
          fGetOneLimitValues(dtCr, lBothEndLimited, dtPastLimit, pcBeginText, pcPastLimitText).
       END.
       ELSE
       DO:
          fGetOneLimitValues(dtCr, lBothEndLimited, dtPastLimit, pcBeginText, pcPastLimitText).
          fGetOneLimitValues(dtCr, lBothEndLimited, dtLatestLimit, pcBeginText, pcLatestLimitText).
       END.
   END.
   ELSE 
   DO:
      IF dtCr = dtPastLimit + 1 AND NOT lBothEndLimited THEN
      DO:
         fOutputIds(pcBeginText + ", Day after " + pcPastLimitText + " limit").
         RETURN TRUE.
      END.
   END.

   RETURN FALSE.

END.

FUNCTION fOutputIdType RETURN LOGICAL (INPUT pcIdType AS CHARACTER, INPUT piMsSeq AS INTEGER):
    DEFINE BUFFER xMobSub FOR MobSub.
    FIND xMobSub WHERE xMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
    IF AVAIL xMobSub THEN
    DO:
       FIND Customer WHERE Customer.Brand = "1" AND Customer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.
       IF AVAIL Customer THEN
       DO:
          IF Customer.CustIdType eq pcIdType THEN
          IF xMobSub.PayType eq FALSE THEN
            fOutputIds("IdTypes: " + pcIdType + ", Postpaid").
          ELSE
            fOutputIds("IdTypes: " + pcIdType + ", PrePaid").
       END.
    END.
    RETURN TRUE.
END.


FUNCTION fHandlePostPaid RETURN LOGICAL (INPUT pdCreation AS DATE):
    DEFINE VARIABLE dt18MonthsAgo AS DATE NO-UNDO. 
    DEFINE VARIABLE dt6MonthsAgo AS DATE NO-UNDO. 

    /* Order > 18 months ago */
    dt18MonthsAgo = fGetMonthsAgo(TODAY, 18).
    dt6MonthsAgo = fGetMonthsAgo(TODAY, 6).
    IF NOT fIsBarred(MobSub.MsSeq) THEN 
    DO:
       /* Postpaid, last order > 18 months ago. No ongoing barrings. No barring history > 90 days */
       IF NOT fCheckIfBarringHistoryInMonths(MobSub.MsSeq, 90) THEN
          fGetValuesOnLimits(pdCreation, dt18MonthsAgo, ?, 
             "PostPaid, No barrings and no barring history", "18 months", "").

       /* Postpaid, last order > 6 months ago < 18 months. No ongoing barrings. No barring history > 90 days */
       IF NOT fCheckIfBarringHistoryInMonths(MobSub.MsSeq, 90) THEN
          fGetValuesOnLimits(pdCreation, dt18MonthsAgo, dt6MonthsAgo, 
             "PostPaid, No barrings and no barring history", "18 months", "6 months").

       IF NOT fPeriodicalContractExist(MobSub.MsSeq) THEN
       DO:
          IF fCheckIfBarringHistoryInMonths(MobSub.MsSeq, 90) THEN
             fGetValuesOnLimits(pdCreation, ? , ?, 
                 "PostPaid, No barrings, barring history < 90 days", "-", "-").

          IF fCheckIfBarringHistoryInMonths(MobSub.MsSeq, 91) THEN
             fGetValuesOnLimits(pdCreation, ? , ?, 
                "PostPaid, No barrings, barring history < 91 days", "-", "-").

          IF fCheckIfBarringHistoryInMonths(MobSub.MsSeq, 89) THEN
             fGetValuesOnLimits(pdCreation, ? , ?, 
                "PostPaid, No barrings, barring history < 89 days", "-", "-").
       END.
    END.
    ELSE /* Ongoing barrings */
    DO:
       IF NOT fPeriodicalContractExist(MobSub.MsSeq) THEN
       DO:
          fGetValuesOnLimits(pdCreation, ?, ?, "Postpaid, ongoing barrings", "-", "-").
       END.
    END.

    IF fIsPreactivated(MobSub.MsSeq) THEN
    DO:

       fGetValuesOnLimits(pdCreation, ?, ?, "Postpaid, preactivated", "-", "-").
    END.

    RETURN TRUE.
END.


FUNCTION fHandlePrepaid RETURN LOGICAL:
    DEFINE VARIABLE dtCreation AS DATE NO-UNDO.
    DEFINE VARIABLE dtOrderCreation AS DATE NO-UNDO.
    DEFINE VARIABLE dt18MonthsAgo2 AS DATE NO-UNDO. 
    DEFINE VARIABLE diTIme2 AS INTEGER NO-UNDO. 

    fSplitTS(Order.CrStamp, output dtOrderCreation, output diTime2).
    fGetValuesOnLimits(dtOrderCreation, dt18MonthsAgo2, ?, "PrePaid", "18 months", "").
    IF fIsPreactivated(MobSub.MsSeq) THEN
    DO:

       fGetValuesOnLimits(dtOrderCreation, ?, ?, "Prepaid, preactivated", "-", "-").
    END.

    RETURN TRUE.
END.



DEFINE VARIABLE iMobSubSeqLimit AS INTEGER NO-UNDO. 
iMobSubSeqLimit = 2000.

FOR EACH MobSub WHERE MobSub.MsSeq >= iMobSubSeqLimit NO-LOCK:
   FOR EACH Order WHERE Order.Brand = "1" AND Order.MsSeq = MobSub.MsSeq NO-LOCK:
      FIND OrderCustomer WHERE OrderCustomer.Brand = "1" AND OrderCustomer.OrderId = Order.OrderId AND
           OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

      IF AVAIL OrderCustomer THEN
      DO:

          FIND FIRST Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
          IF AVAIL Customer THEN
          DO:

             DEFINE VARIABLE dtCreation AS DATE NO-UNDO.
             DEFINE VARIABLE diTime AS INTEGER NO-UNDO. 
             fSplitTS(Order.CrStamp, output dtCreation, output diTime).

             IF MobSub.PayType eq FALSE THEN /* PostPaid */
             DO:
                 fHandlePostPaid(dtCreation).
             END.
             ELSE /* Prepaid */
             DO:
                 fHandlePrepaid().    

             END.
             fOutputIdType("NIE", MobSub.MsSeq ).
             fOutputIdType("NIF", MobSub.MsSeq ).
             fOutputIdType("passport", MobSub.MsSeq ).
             fOutputIdType("CIF", MobSub.MsSeq ).
          END.
      END.
   END.
END.


OUTPUT STREAM sTestIds CLOSE.
