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

DEF VAR ldaFromdate       AS DATE NO-UNDO.
DEF VAR liTime            AS INT  NO-UNDO.
DEF VAR ldeReqStamp       AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcSMSText         AS CHAR NO-UNDO.
DEF VAR liCount               AS INT  NO-UNDO.
DEF VAR lcGroupCodes          AS CHAR NO-UNDO.
DEF VAR liStartDay        AS INT NO-UNDO.
DEF VAR liEndDay          AS INT NO-UNDO.
DEF VAR liMonth           AS INT NO-UNDO.
DEF VAR ldaStartDate      AS DATE NO-UNDO.
DEF VAR ldaEndDate        AS DATE NO-UNDO.

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
   (INPUT iiStartDate AS DATE,
    INPUT iiEndDate AS DATE).
  
   DEF VAR lcPeriod AS CHAR NO-UNDO.
   lcPeriod = STRING(YEAR(iiStartDate)) + STRING(MONTH(iiStartDate)).
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
                            Invoice.InvType = 99) THEN
         NEXT. /* "Residual fee billed". */

      FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
              DCCLI.PerContractId = INT(SingleFee.sourcekey) AND
              DCCLI.Brand   = gcBrand AND
              DCCLI.DCEvent BEGINS "PAYTERM" AND
              DCCLI.MsSeq   = INT(SingleFee.KeyValue) AND
              DCCLI.ValidTo < iiStartDate AND
              DCCLI.ValidTo > iiEndDate NO-ERROR.

      IF NOT AVAIL DCCLI OR DCCLI.TermDate NE ? OR 
         DCCLI.RenewalDate NE ? THEN
         NEXT. /* Installment contract not found, terminated or renewal done */

   END.
   RETURN TRUE.
END FUNCTION.

ASSIGN lcLogDir     = fCParam("Q25","Q25_reminder_LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "Q25_reminder_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

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

OUTPUT STREAM Sout TO VALUE(lcLogFile).

/* Month 22 */
IF fCheckDates(2, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:

END.

/* Month 23 */
IF fCheckDates(1, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:

END.

/* Month 24 */
IF fCheckDates(0, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:

END.
OUTPUT STREAM Sout CLOSE.
