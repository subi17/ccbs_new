{testpaa.i}
{timestamp.i}
{msisdn.i}
DEFINE STREAM sLog.
DEFINE STREAM sELog.

DEFINE VARIABLE lTest AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dt3MonthsAgo AS DATE NO-UNDO. 
DEFINE VARIABLE de3MonthsAgoStamp AS DECIMAL NO-UNDO. 

lTest = FALSE.

DEFINE VARIABLE iCurrMonth AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCurrDay AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCurrYear AS INTEGER NO-UNDO. 

DEFINE VARIABLE i3monthsAgoMonth AS INTEGER NO-UNDO. 
DEFINE VARIABLE i3monthsAgoYear AS INTEGER NO-UNDO. 

DEFINE VARIABLE cDate3MonthsAgo AS CHARACTER NO-UNDO. 

iCurrMonth = MONTH(TODAY).
iCurrYear = YEAR(TODAY).
iCurrDay = DAY(TODAY).

FUNCTION fAddPrefixZeros RETURN CHARACTER 
   (INPUT pcInput AS CHARACTER, INPUT piLength AS INTEGER):

   DEFINE VARIABLE cRet AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iCurrLength AS INTEGER NO-UNDO.
   DEFINE VARIABLE iAdd AS INTEGER NO-UNDO. 
   iCurrLength = LENGTH(pcInput).
   cRet = pcInput.
   REPEAT iAdd = 1 TO piLength - iCurrLength:
      cRet = "0" + cRet.
   END.
   RETURN cRet.
END.

i3monthsAgoMonth = iCurrMonth - 3.
i3monthsAgoYear = iCurrYear.
IF i3monthsAgoMonth < 0 THEN
DO:
   i3monthsAgoYear = i3monthsAgoYear - 1.
   i3monthsAgoMonth = 13 + i3monthsAgoMonth.
END.

cDate3monthsAgo = fAddPrefixZeros(STRING(i3monthsAgoYear),4) + 
                  fAddPrefixZeros(STRING(i3monthsAgoMonth),2) + 
                  fAddPrefixZeros(STRING(iCurrDay),2).

/* MESSAGE cDate3monthsago VIEW-AS ALERT-BOX.
MESSAGE DECIMAL(cDate3monthsago) VIEW-AS ALERT-BOX.
 */

IF lTest THEN cTest = "_test". ELSE cTest = "".

OUTPUT STREAM sLog TO VALUE("/apps/snet/200812/hm_ycm1167.log").
OUTPUT STREAM sELog TO VALUE("/apps/snet/200812/hm_ycm1167_error.log").
PUT STREAM sLog UNFORMATTED "MSISDN records for which ValidFrom was at most " cDate3monthsAgo
                            "were changed" SKIP.

DEFINE BUFFER xMSISDN FOR MSISDN.

FOR EACH xMSISDN WHERE xMSISDN.Brand eq "1" USE-INDEX CLI EXCLUSIVE-LOCK BREAK BY CLI.
   IF FIRST-OF(xMSISDN.CLI) THEN
   DO:
      IF xMSISDN.StatusCode EQ 4 AND xMSISDN.ValidFrom <= DECIMAL(cDate3monthsAgo) THEN
      DO:
         FIND Mobsub WHERE Mobsub.CLI = xMSISDN.CLI NO-LOCK NO-ERROR.
         IF NOT AVAIL Mobsub THEN
         DO:
            PUT STREAM sLog UNFORMATTED "CLI = " xMSISDN.CLI 
               ",ValidFrom = " xMSISDN.ValidFrom " and ,ValidTo = " xMSISDN.ValidTo 
               "  Status 4 changed to 1. "
               "New MSISDN will have 0 for CustNum, OrderId and MsSeq and empty OutOperator" SKIP.
            IF NOT lTest THEN 
            DO:
               fMakeMsidnHistory(RECID(xMSISDN)).
               MSISDN.StatusCode = 1.
               MSISDN.CustNum = 0.
               MSISDN.OrderId = 0.
               MSISDN.MsSeq = 0.
               MSISDN.OutOperator = "".
            END.
         END.
         ELSE
            PUT STREAM sELog UNFORMATTED "CLI = " xMSISDN.CLI 
               ",ValidFrom = " xMSISDN.ValidFrom " and ,ValidTo = " xMSISDN.ValidTo 
               "  Status 4 not changed, because MSISDN was attached to subscription with"
               " MsSeq " Mobsub.MsSeq SKIP.
      END.
   END.
END.

OUTPUT STREAM sLog CLOSE.
OUTPUT STREAM sELog CLOSE.
