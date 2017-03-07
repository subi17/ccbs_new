/* ----------------------------------------------------------------------
  module .......: Mm/pmdub_bal_check.p
  task .........: Notify if customer does not have enough balance
                  before the end of the month and has active tbono8
  application ..: tms
  author .......: vikas
  created ......: 19.04.11
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fgettxt.i}
{Func/fmakesms.i}
{Func/detailvalue.i}
{Syst/tmsconst.i}
{Func/coinv.i}

DEFINE VARIABLE ldeMinBalance    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldeBalance       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldeCheckTS       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldeReqStamp      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lcBalance        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSMSText        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSMSTextKey     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSMSTextProfile AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liCurrPeriod     AS INTEGER    NO-UNDO.
DEFINE VARIABLE ldToDate         AS DATE       NO-UNDO.
DEF VAR ldeNow AS DEC NO-UNDO. 
DEF VAR liPos AS INT NO-UNDO. 
DEF VAR liTotal AS INTEGER NO-UNDO. 
DEF VAR liSMS AS INTEGER NO-UNDO. 
DEF VAR lcFormat AS CHAR NO-UNDO. 
DEF VAR liError AS INTEGER NO-UNDO. 
DEF VAR llCDR AS LOGICAL NO-UNDO. 
DEF VAR lcLogDir AS CHARACTER NO-UNDO. 
DEF VAR lcLogFile AS CHARACTER NO-UNDO.
DEF VAR ldLastDayOfMonth AS DATE NO-UNDO.
DEF VAR ldFirstDayOfMonth AS DATE NO-UNDO.
DEF VAR ldeFromTS AS DEC NO-UNDO.

ldFirstDayOfMonth = DATE(MONTH(TODAY),1,YEAR(TODAY)).
ldLastDayOfMonth = fLastDayOfMonth(TODAY).

ldeFromTS = fMake2Dt(ldFirstDayOfMonth,0).

/* Only Send the SMS last day of month or 3 days before last of month */
CASE (ldLastDayOfMonth - TODAY):
   WHEN 0 THEN lcSMSTextProfile = "PMDUBBalCheck2".
   WHEN 3 THEN lcSMSTextProfile = "PMDUBBalCheck1".
   OTHERWISE RETURN.
END.

ASSIGN
   ldeMinBalance = fCParamDe("PMDUBMinBalance")
   liCurrPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY)
   ldToDate      = (fInt2Date(liCurrPeriod,2) + 1)
   ldeNow        = fMakeTS()
   ldeCheckTS    = fMake2Dt(ldToDate, 00000)
   lcLogDir      = fCParam("PrepaidBundle","LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".
def stream sout.

lcLogFile = lcLogDir + "pmdub_bal_check_" +
   STRING(YEAR(TODAY)) + 
   STRING(MONTH(TODAY),"99")   +
   STRING(DAY(TODAY),"99") + ".txt".

output stream sout to value(lcLogFile).
PMDUB_LOOP:
FOR FIRST ServiceLimit WHERE
          ServiceLimit.GroupCode = {&PMDUB} NO-LOCK,
    EACH  MServiceLimit WHERE
          MServiceLimit.SLSeq    = ServiceLimit.SLSeq AND
          MServiceLimit.DialType = ServiceLimit.DialType AND
          MServiceLimit.EndTS    > ldeCheckTS NO-LOCK,
    FIRST MobSub WHERE MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK,
    FIRST Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK:

    liTotal = liTotal + 1.
    IF liTotal MOD 100 = 0 AND NOT SESSION:BATCH THEN DO:
       DISP liTotal liSMS liError. 
       PAUSE 0.
    END.

    /* If bundle termination request is already placed but */
    /* it is on-going then no need to notify customer      */
    IF CAN-FIND(FIRST MsRequest WHERE
                      MsRequest.MsSeq   = MobSub.MsSeq AND
                      MsRequest.ReqType = 9 AND
               LOOKUP(STRING(MsRequest.ReqStatus), "2,4,9,99") = 0 AND
                      MsRequest.ReqCParam3 = ServiceLimit.GroupCode
                      USE-INDEX MsSeq) THEN NEXT.

    ASSIGN llCDR = FALSE
           ldeBalance = 0.

    /* Look for latest Prepaid CDR to get the available balance */
    FOR LAST PrepCDR WHERE
             PrepCDR.CLI = MobSub.CLI AND
             PrepCDR.MsSeq = MobSub.MsSeq AND
             PrepCDR.ErrorCode = 0 AND
             PrepCDR.MSCID NE "CCGW" NO-LOCK USE-INDEX CLI,
        FIRST mcdrdtl2 WHERE
              mcdrdtl2.datest = PrepCDR.DateSt AND
              mcdrdtl2.dtlseq = PrepCDR.DtlSeq NO-LOCK:

       lcFormat = TRIM(ENTRY(4, mcdrdtl2.detail, "|")) NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
         put stream sout unformatted 
            MobSub.cli "||ERROR:Could not parse CDR Format" skip.
         liError = liError + 1.
         NEXT PMDUB_LOOP.
       END.

       liPos = fGetPosition(McdrDtl2.Version + lcFormat,
                            "Balance after") NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
         put stream sout unformatted 
            MobSub.cli "||ERROR:Could parse Balance After position" skip.
         liError = liError + 1.
         NEXT PMDUB_LOOP.
       END.

       IF liPos > 0 THEN ASSIGN
         lcBalance = REPLACE(ENTRY(liPos,MCDRDtl2.Detail,"|"),",", ".") 
         ldeBalance = DECIMAL(lcBalance) NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
         put stream sout unformatted 
            MobSub.cli "||ERROR:Could not parse balance" skip.
         liError = liError + 1.
         NEXT PMDUB_LOOP.
       END.
       llCDR = True.
    END. /* FOR LAST PrepCDR WHERE */

    IF llCDR EQ False then do:
      put stream sout unformatted MobSub.cli "||SKIPPED:No CDR" skip.
      NEXT PMDUB_LOOP.
    end.

    /* Notify customer if pre-paid balance is less */
    IF ldeBalance < ldeMinBalance THEN DO:
       /* different SMS text in first month then regular month */
       IF MServiceLimit.FromTS >= ldeFromTS THEN
          lcSMSTextKey = lcSMSTextProfile + "FM".
       ELSE lcSMSTextKey = lcSMSTextProfile.

       lcSMSText = fGetSMSTxt(lcSMSTextKey,
                              TODAY,
                              Customer.Language,
                              OUTPUT ldeReqStamp).
      
       IF lcSMSText > "" THEN DO:
          fMakeSchedSMS2(MobSub.CustNum,
                         MobSub.CLI,
                         10,
                         lcSMSText,
                         ldeReqStamp,
                         {&BONO8_SMS_SENDER},
                         ""). 
          liSMS = liSMS + 1.
          put stream sout unformatted MobSub.cli "|" ldeBalance "|SMS Sent" skip.
	END.
	ELSE DO:
         liError = liError + 1.
         put stream sout unformatted MobSub.cli "|" 
                  ldeBalance 
                  "|ERROR:SMS not defined" skip.
       END.
    END.
    ELSE PUT STREAM SOUT unformatted MobSub.cli "|" ldeBalance "|OK" skip.
END. /* FOR FIRST ServiceLimit WHERE */

IF NOT SESSION:BATCH THEN
   MESSAGE liTotal liSms liError VIEW-AS ALERT-BOX.
