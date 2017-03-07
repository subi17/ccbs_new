{Syst/commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Func/date.i}
{Func/timestamp.i}
{Mm/requestaction_exec.i}

DEF VAR lcRequestTypes            AS CHAR NO-UNDO.
DEF VAR lcLogFile                 AS CHAR NO-UNDO.
DEF VAR liCount                   AS INT  NO-UNDO.
DEF VAR liRequestType             AS INT  NO-UNDO.
DEF VAR ldaActDate                AS DATE NO-UNDO.
DEF VAR ldaEndDate                AS DATE NO-UNDO.
DEF VAR ldeActStamp               AS DEC  NO-UNDO.
DEF VAR llMatch                   AS LOG  NO-UNDO.

DEF VAR liTotalSTC                AS INT  NO-UNDO.
DEF VAR liTotalBTC                AS INT  NO-UNDO.
DEF VAR liSTCWithPrepaid          AS INT  NO-UNDO.
DEF VAR liSTCPrepaidToPost        AS INT  NO-UNDO.
DEF VAR liSTCPostToPreErr         AS INT  NO-UNDO.
DEF VAR liSTCPostToPreOngoing     AS INT  NO-UNDO.
DEF VAR liBonoBTC                 AS INT  NO-UNDO.
DEF VAR liSTCWithOutPenaltyC      AS INT  NO-UNDO.
DEF VAR liBTCWithOutPenaltyC      AS INT  NO-UNDO.
DEF VAR liSTCWithPenaltyCMoreNew  AS INT  NO-UNDO.
DEF VAR liBTCWithPenaltyCMoreNew  AS INT  NO-UNDO.
DEF VAR liSTCWithPenaltyFee       AS INT  NO-UNDO.
DEF VAR liBTCWithPenaltyFee       AS INT  NO-UNDO.
DEF VAR liSTCWithOutPenaltyFee    AS INT  NO-UNDO.
DEF VAR liBTCWithOutPenaltyFee    AS INT  NO-UNDO.

DEF VAR lhRequest     AS HANDLE NO-UNDO.
DEF STREAM sin.

DEF BUFFER bMsRequest FOR MsRequest.

lcRequestTypes = "0,81".

IF DAY(TODAY) = 1 THEN
   ldaActDate = TODAY.
ELSE
   ldaActDate = fLastDayOfMonth(TODAY) + 1.

ASSIGN ldaEndDate  = (ldaActDate - 1)
       ldeActStamp = fMake2Dt(ldaActDate,0).

lcLogFile = "/apps/yoigo/tms_support/testing/pending_penalty_by_stc_" +
            STRING(YEAR(ldaActDate)) + STRING(MONTH(ldaActDate),"99") +
            STRING(DAY(ldaActDate),"99") + "_" +
            REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

OUTPUT STREAM sin TO VALUE(lcLogFile).

DEFINE VARIABLE liReqStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcReqStatuses AS CHARACTER NO-UNDO init "1,3,5,6,7,8".
DEFINE VARIABLE liLoop2 AS INTEGER NO-UNDO. 

DO liCount = 1 TO NUM-ENTRIES(lcRequestTypes):

   liRequestType = INT(ENTRY(liCount,lcRequestTypes)).

   do liLoop2 = 1 TO NUM-ENTRIES(lcReqStatuses):
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand = gcBrand AND
            MsRequest.ReqType = liRequestType AND
            MsRequest.ReqStatus = int(ENTRY(liLoop2,lcReqStatuses)) AND
            MsRequest.ActStamp = ldeActStamp:

      IF MsRequest.ReqType = 0 THEN
         liTotalSTC = liTotalSTC + 1.
      ELSE liTotalBTC = liTotalBTC + 1.
      
      STATUS DEFAULT "Checked STC: " + STRING(liTotalSTC) +
                     "|Checked BTC: " + STRING(liTotalBTC).

      /* Exclude pre=>pre STC */
      IF MsRequest.ReqType = 0 AND
         MsRequest.ReqCparam1 BEGINS "TARJ" AND
         MsRequest.ReqCparam2 BEGINS "TARJ" THEN DO:
         liSTCWithPrepaid = liSTCWithPrepaid + 1.
         NEXT.
      END.

      /* Exclude pre=>post STC */
      IF MsRequest.ReqType = 0 AND
         MsRequest.ReqCparam1 BEGINS "TARJ" AND
         MsRequest.ReqCparam2 BEGINS "CONT" THEN DO:
         liSTCPrepaidToPost = liSTCPrepaidToPost + 1.
         NEXT.
      END.

      /* Exclude Bono BTC */
      IF MsRequest.ReqType = 81 AND
         MsRequest.ReqCparam1 BEGINS "MDUB" THEN DO:
         liBonoBTC = liBonoBTC + 1.
         NEXT.
      END.

      IF MsRequest.ReqCparam2 BEGINS "TARJ" OR
         MsRequest.ReqCparam2 = "CONT8" THEN
         FIND FIRST DCCLI NO-LOCK WHERE
                    DCCLI.MsSeq = MsRequest.MsSeq AND
                    DCCLI.DCEvent BEGINS "TERM"   AND
                    DCCLI.ValidFrom <= ldaEndDate AND
                    DCCLI.ValidTo    > ldaEndDate AND
                    DCCLI.CreateFees = TRUE NO-ERROR.
      ELSE
         FIND FIRST DCCLI NO-LOCK WHERE
                    DCCLI.MsSeq = MsRequest.MsSeq AND
                    DCCLI.DCEvent BEGINS "TERM"   AND
             LOOKUP(DCCLI.DCEvent,"TERM18,TERM18-50,TERM24,TERM24-50") = 0 AND
                    DCCLI.ValidFrom <= ldaEndDate AND
                    DCCLI.ValidTo    > ldaEndDate AND
                    DCCLI.CreateFees = TRUE NO-ERROR.

      IF NOT AVAIL DCCLI THEN DO:
         IF MsRequest.ReqType = 0 THEN
            liSTCWithOutPenaltyC = liSTCWithOutPenaltyC + 1.
         ELSE liBTCWithOutPenaltyC = liBTCWithOutPenaltyC + 1.
         NEXT.
      END.

      IF MsRequest.ReqType = 0 AND
         MsRequest.ReqCparam1 BEGINS "CONT" AND
         MsRequest.ReqCparam2 BEGINS "TARJ" THEN DO:
         IF MsRequest.ReqStatus = 3 THEN
            liSTCPostToPreErr = liSTCPostToPreErr + 1.
         ELSE liSTCPostToPreOngoing = liSTCPostToPreOngoing + 1.
      END.

      lhRequest = BUFFER MsRequest:HANDLE.

      /* Contract will always be terminated to prepaid */
      IF MsRequest.ReqCparam2 BEGINS "TARJ" THEN
         llMatch = TRUE.
      ELSE
         RUN pFeeComparison(MsRequest.MsSeq,ldaActDate,DCCLI.DCEvent,
                            lhRequest,"ORIGINAL>NEW","",OUTPUT llMatch).

      IF NOT llMatch THEN DO:
         IF MsRequest.ReqType = 0 THEN
            liSTCWithPenaltyCMoreNew = liSTCWithPenaltyCMoreNew + 1.
         ELSE liBTCWithPenaltyCMoreNew = liBTCWithPenaltyCMoreNew + 1.
         NEXT.
      END.

      FIND FIRST bMsRequest WHERE
                 bMsRequest.MsSeq = MsRequest.MsSeq AND
                 bMsRequest.ReqType = 9 AND
                 bMsRequest.ReqCparam3 = DCCLI.DCEvent AND
                 bMsRequest.OrigRequest = MsRequest.MsRequest NO-LOCK NO-ERROR.
      IF AVAIL bMsRequest AND
         (bMsRequest.ReqStatus = 2 OR bMsRequest.ReqStatus = 8) THEN DO:
         IF MsRequest.ReqType = 0 THEN
            liSTCWithPenaltyFee = liSTCWithPenaltyFee + 1.
         ELSE liBTCWithPenaltyFee = liBTCWithPenaltyFee + 1.
      END.
      ELSE DO:
         IF MsRequest.ReqType = 0 THEN
            liSTCWithOutPenaltyFee = liSTCWithOutPenaltyFee + 1.
         ELSE liBTCWithOutPenaltyFee = liBTCWithOutPenaltyFee + 1.

         PUT STREAM sin UNFORMATTED STRING(MsRequest.MsSeq) + "|" +
             STRING(MsRequest.ReqStatus) + "|" + MsRequest.ReqCparam1 + "|" +
             MsRequest.ReqCparam2 + MsRequest.ReqCparam5 + "|" +
             DCCLI.DCEvent + "|" + STRING(DCCLI.ValidTo) skip.
      END.
   END.
   end.
END.

PUT STREAM sin UNFORMATTED "Total Ongoing STC: " + STRING(liTotalSTC) skip.
PUT STREAM sin UNFORMATTED "Total Ongoing BTC: " + STRING(liTotalBTC) skip.
PUT STREAM sin UNFORMATTED "Total STC Pre=>Pre: " + STRING(liSTCWithPrepaid) skip.
PUT STREAM sin UNFORMATTED "Total STC Pre=>Post: " + STRING(liSTCPrepaidToPost) skip.
PUT STREAM sin UNFORMATTED "Total STC Post=>Pre Error: " + STRING(liSTCPostToPreErr) skip.
PUT STREAM sin UNFORMATTED "Total STC Post=>Pre Ongoing: " + STRING(liSTCPostToPreOngoing) skip.
PUT STREAM sin UNFORMATTED "Total BTC Bono=>Bono: " + STRING(liBonoBTC) skip.
PUT STREAM sin UNFORMATTED "Total STC without penalty contract: " + STRING(liSTCWithOutPenaltyC) skip.
PUT STREAM sin UNFORMATTED "Total BTC without penalty contract: " + STRING(liBTCWithOutPenaltyC) skip.
PUT STREAM sin UNFORMATTED "Total STC to higer tariff: " + STRING(liSTCWithPenaltyCMoreNew) skip.
PUT STREAM sin UNFORMATTED "Total BTC to higer tariff: " + STRING(liBTCWithPenaltyCMoreNew) skip.
PUT STREAM sin UNFORMATTED "Total STC with penalty fee created: " + STRING(liSTCWithPenaltyFee) skip.
PUT STREAM sin UNFORMATTED "Total BTC with penalty fee created: " + STRING(liBTCWithPenaltyFee) skip.
PUT STREAM sin UNFORMATTED "Total STC without penalty fee created: " + STRING(liSTCWithOutPenaltyFee) skip.
PUT STREAM sin UNFORMATTED "Total BTC without penalty fee created: " + STRING(liBTCWithOutPenaltyFee) skip.

OUTPUT STREAM sin CLOSE.

