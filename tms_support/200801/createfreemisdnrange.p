{Func/timestamp.i}
{/apps/snet/200801/goldnumber.i}

DEFINE INPUT PARAMETER cBeginCLI AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER cEndCLI AS CHARACTER NO-UNDO. 

DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE curDate AS DATE NO-UNDO. 
DEFINE VARIABLE curDecTime AS DECIMAL NO-UNDO. 
DEFINE VARIABLE iBeginCLI AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEndCLI AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCurrentCLI AS INTEGER NO-UNDO. 
DEFINE VARIABLE lError AS LOGICAL NO-UNDO. 

iBeginCLI = INTEGER(cBeginCLI) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO:
   MESSAGE "Could not change BeginCLI to INTEGER" VIEW-AS ALERT-BOX.
   RETURN.
END.

iEndCLI = INTEGER(cEndCLI) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO:
   MESSAGE "Could not change EndCLI to INTEGER" VIEW-AS ALERT-BOX.
   RETURN.
END.



curDecTime = fmakeTS().
curDate = TODAY.
lError = FALSE.


MSISDNCreationLoop:
REPEAT iCurrentCLI = iBeginCLI TO iEndCLI:
   cCLI = STRING(iCurrentCLI).

   RUN CreateMSISDN(
      cCLI /* CLI */,
      "1" /*Brand*/, 
      curDate /* ActionDate, current date */,
      0 /* CustNum */, 
      0 /* LockedTo, default */, 
      0 /* StatusCode */,
      1 /* McCode = Class */, 
      FALSE /* MNP, default */, 
      0 /* MsisdnType, default */,
      0 /* MsSeq */, 
      0 /* OrderId */, 
      "" /* OutOperator */,
      FALSE /*PayType,  FALSE = PostPaid */, 
      ? /* PortingDate, default */, 
      0 /* PortingTime, 0 */,
      "" /* POS */, 
      0 /* PrevMuSeq, default */,
      curDecTime /* ValidFrom, current decimal time */,
      99999999.99999 /* ValidTo */, 
      OUTPUT lError).

   IF lError THEN
   DO:
      MESSAGE "MSISDN with CLI " + cCLI + 
         " already existed. Creation interrupted."
         VIEW-AS ALERT-BOX.

      LEAVE MSISDNCreationLoop.
      /* UNDO MSISDNCreationLoop, LEAVE MSISDNCreationLoop. */
   END.

   RUN CreateMSISDNNumber(
      cCLI /* CLI */,
      0 /* MSISDNType, voice */,
      0 /* Common Number */,
      OUTPUT lError ).

   IF lError THEN
   DO:
      MESSAGE "MSISDNNumber with CLI " + cCLI + 
         " already existed. Creation interrupted."
         VIEW-AS ALERT-BOX.
/*         UNDO MSISDNCreationLoop, LEAVE MSISDNCreationLoop. */
      LEAVE MSISDNCreationLoop.
   END.

   IF fGoldNumber(cCLI) = 1 THEN
   DO:
      FIND MSISDNNumber WHERE 
         MSISDNNumber.CLI = cCLI EXCLUSIVE-LOCK.
      MSISDNNumber.Rank = 1.
   END. /* IF fGoldNumber */
END. /* REPEAT */

PROCEDURE CreateMSISDN:
   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcBrand AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pdtActionDate AS DATE NO-UNDO. 
   DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER pdLockedTo AS DECIMAL NO-UNDO. 
   DEFINE INPUT PARAMETER piStatusCode AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER piMcCode AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER plMNP AS LOGICAL NO-UNDO. 
   DEFINE INPUT PARAMETER piMsisdnType AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER piMSSeq AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER piOrderId AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER pcOutOperator AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER plPayType AS LOGICAL NO-UNDO. 
   DEFINE INPUT PARAMETER pdtPortingDate AS DATE NO-UNDO. 
   DEFINE INPUT PARAMETER pdPortingTime AS DECIMAL NO-UNDO. 
   DEFINE INPUT PARAMETER pcPOS AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER piPrevMuSeq AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER pdValidFrom AS DECIMAL NO-UNDO. 
   DEFINE INPUT PARAMETER pdValidTo AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER plError AS LOGICAL NO-UNDO. 

   plError = FALSE.
   IF CAN-FIND(MSISDN WHERE MSISDN.CLI = pcCLI) THEN
   DO:
      plError = TRUE.
      RETURN.
   END.

   CREATE MSISDN.
   ASSIGN MSISDN.CLI = pcCLI
          MSISDN.ActionDate = pdtActionDate
          MSISDN.Brand = pcBrand
          MSISDN.CustNum = piCustNum
          MSISDN.StatusCode = piStatusCode
          MSISDN.McCode = piMcCode
          MSISDN.MNP = plMNP
          MSISDN.MsisdnType = piMsisdnType
          MSISDN.MSSeq = piMSSeq
          MSISDN.OrderId = piOrderId
          MSISDN.OutOperator = pcOutOperator
          MSISDN.PayType = plPayType
          MSISDN.PortingDate = pdtPortingDate
          MSISDN.PortingTime = pdPortingTime
          MSISDN.POS = pcPOS
          MSISDN.PrevMuSeq = piPrevMuSeq
          MSISDN.ValidFrom = pdValidFrom
          MSISDN.ValidTo = pdValidTo.
END.

PROCEDURE CreateMSISDNNumber:
   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER piMsisdnType AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER piRank AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER plError AS LOGICAL NO-UNDO. 
   
   plError = FALSE.
   IF CAN-FIND(MSISDNNumber WHERE CLI = pcCLI) THEN
   DO:
      plError = TRUE.
      RETURN.
   END.

   CREATE MSISDNNumber.
   ASSIGN MSISDNNumber.CLI = pcCLI
          MSISDNNumber.MsisdnType = piMsisdnType
          MSISDNNumber.Rank = piRank.
END.

