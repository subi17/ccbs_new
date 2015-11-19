/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder.p
  task .........: Notify customer for Q25 ending
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{timestamp.i}
{cparam2.i}
{fgettxt.i}
{fmakesms.i}
{tmsconst.i}
{date.i}
{smsmessage.i}
{aes_encrypt.i}

DEF VAR ldaFromdate       AS DATE NO-UNDO.
DEF VAR liTime            AS INT  NO-UNDO.
DEF VAR ldeReqStamp       AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcSMSText         AS CHAR NO-UNDO.
DEF VAR liCount           AS INT  NO-UNDO.
DEF VAR lcGroupCodes      AS CHAR NO-UNDO.
DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR liMonth           AS INT  NO-UNDO.
DEF VAR ldaStartDate      AS DATE NO-UNDO.
DEF VAR ldaEndDate        AS DATE NO-UNDO.
DEF VAR lcSMSMessage        AS CHAR NO-UNDO.
DEF VAR ldReqStamp        AS DEC  NO-UNDO.

DEF STREAM Sout.

/* Function to check that calculated days exist in used month (if month 
   have 31 days, then 15th day should send SMS for days 29-31. February
   could contain 28 or 29 days and so on.) */
FUNCTION fCheckDates RETURNS LOGICAL
   (INPUT  iiMonths AS INT,
    INPUT  iiStartDay AS INT,
    INPUT  iiEndDay AS INT,
    OUTPUT odaStartDate AS DATE,
    OUTPUT odaEndDate AS DATE).
   DEF VAR  ldaCountDate       AS DATE NO-UNDO.
   ldaCountDate = ADD-INTERVAL(TODAY, iiMonths, 'months':U).
   IF iiStartDay > DAY(fLastDayOfMonth(ldaCountDate)) THEN
      RETURN FALSE.
   ELSE IF iiEndDay > DAY(fLastDayOfMonth(ldaCountDate))
      THEN iiEndDay = DAY(fLastDayOfMonth(ldaCountDate)).
   ELSE IF iiEndDay = 30 and DAY(fLastDayOfMonth(ldaCountDate)) = 31 THEN
      iiEndDay = 31.
   odaStartDate = DATE(MONTH(ldaCountDate),iiStartDay,YEAR(ldaCountDate)).
   odaEndDate = DATE(MONTH(ldaCountDate),iiEndDay,YEAR(ldaCountDate)).
   RETURN TRUE.
END FUNCTION.

FUNCTION fSendQ25SMSMessages RETURNS LOGICAL
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT).

   DEF VAR lcPeriod AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR liNotSendCount AS INT NO-UNDO.
   DEF VAR liBilledCount AS INT NO-UNDO.
   DEF VAR liNotDCCLICount AS INT NO-UNDO.
   DEF VAR liReturnedDevices AS INT NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
  
   ASSIGN lcLogDir     = fCParam("Q25","Q25_reminder_LogDir").

   IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

   lcLogFile = lcLogDir + "Q25_reminder_" +
               STRING(YEAR(TODAY)) +
               STRING(MONTH(TODAY),"99") +
               /* STRING(DAY(TODAY),"99") + */ ".txt".
   
   OUTPUT STREAM Sout TO VALUE(lcLogFile) APPEND. 

   lcPeriod = STRING(YEAR(idaStartDate)) + (IF(MONTH(idaStartDate) < 10) THEN
              "0" ELSE "") + STRING(MONTH(idaStartDate)).
   FOR EACH SingleFee USE-INDEX BillCode WHERE
            SingleFee.Brand       = gcBrand AND
            SingleFee.Billcode BEGINS "RVTERM" AND
            SingleFee.HostTable   = "Mobsub" AND
            SingleFee.SourceTable = "DCCLI" AND
            SingleFee.CalcObj     = "RVTERM" AND 
            SingleFee.BillPeriod  = INT(lcPeriod) NO-LOCK:

      IF SingleFee.Billed AND
         NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                            Invoice.Invnum = SingleFee.InvNum aND
                            Invoice.InvType = 99) THEN DO:
         liBilledCount = liBilledCount + 1.
         NEXT. /* "Residual fee billed". */
      END.
      FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
              DCCLI.PerContractId = INT(SingleFee.sourcekey) AND
              DCCLI.Brand   = gcBrand AND
              DCCLI.DCEvent BEGINS "PAYTERM" AND
              DCCLI.MsSeq   = INT(SingleFee.KeyValue) AND
              DCCLI.ValidTo < idaStartDate AND
              DCCLI.ValidTo > idaEndDate NO-ERROR.

      IF NOT AVAIL DCCLI THEN DO:
         /* No DCCLI for example between start and end date, singlefee is for 
            whole month or no DCCLI for some error case (?). */
         liNotDCCLICount = liNotDCCLICount + 1.
         NEXT.
      END.
      ELSE IF DCCLI.TermDate NE ? OR DCCLI.RenewalDate NE ? THEN DO:
         liNotSendCount = liNotSendCount + 1.
         NEXT. /* Installment contract not found, terminated or renewal done 
                  SMS should not be send. */
      END.
      ELSE DO:
         FIND FIRST TermReturn WHERE 
                    TermReturn.OrderId = SingleFee.OrderId AND
                    TermReturn.ReturnTS > fHMS2TS(DCCLI.ValidFrom, "0").
         IF AVAIL TermReturn AND TermReturn.deviceScreen AND
                  TermReturn.deviceStart THEN DO:
            /* Accepted return of device */
            liReturnedDevices = liReturnedDevices + 1.
            NEXT.
         END.

      END.
      liCount = liCount + 1. /* Full count in Month */
      IF iiPhase = 1 OR iiPhase = 2 THEN DO: /* Q25 month 22 or 23 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth22and23",
                                 TODAY,
                                 1, 
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DATE","20" + "/" +
                                         STRING(MONTH(DCCLI.ValidTo))).
      END.
      ELSE IF iiPhase = 3 THEN DO: /* Q25 month 24 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth24",
                                 TODAY,
                                 1,
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DD","20").
      END.
      
      /* Encrypt MSISDN */
      lcEncryptedMSISDN = encrypt_data("123456789",
                     "AES_CFB_256", "YoigoQ25EncryptionPassword").
      lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
 
      lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", encrypt_data(DCCLI.Cli,
                   "AES_CFB_256", "YoigoQ25EncryptionPassword")).

      fCreateSMS(SingleFee.Custnum,
                 SingleFee.Cli,
                 DCCLI.MsSeq,
                 SingleFee.OrderId,
                 lcSMSMessage,
                 "622",
                 {&SMS_TYPE_OFFER}).


   END.
   PUT STREAM Sout UNFORMATTED
      STRING(iiPhase) + "|" +
      STRING(idaStartDate) + "|" STRING(idaEnddate) + "|" +
      STRING(liCount) + "|" + STRING(liNotSendCount) + "|" +
      STRING(liBilledCount) + "|" + STRING(liNotDCCLICount) + "|" +
      STRING(liReturnedDevices) + "|" + STRING(etime / 1000) SKIP. 
   OUTPUT STREAM Sout CLOSE.
   RETURN TRUE.
END FUNCTION.


/* Handling of sms sending is different at January 2016 */
IF DAY(TODAY) > 15 OR TODAY < 1/13/16 THEN
   RETURN.
ELSE IF TODAY = 1/13/16 THEN DO:
   liStartDay = 1.
   liEndDay = 10.
END.
ELSE IF TODAY = 1/14/16 THEN DO:
   liStartDay = 11.
   liEndDay = 20.
END.
ELSE IF TODAY = 1/15/16 THEN DO:
   liStartDay = 21.
   liEndDay = 30.
END.

ELSE DO:
   /* works when executed during 1. - 15. of each month as planned */
   liStartDay = (DAY(TODAY) * 2) - 1. 
   liEndDay = (DAY(TODAY) * 2).
END.

/* Month 22 */
IF fCheckDates(2, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 1).
END.

/* Month 23 */
IF fCheckDates(1, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 2).
END.

/* Month 24 */
IF fCheckDates(0, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 3).
END.
