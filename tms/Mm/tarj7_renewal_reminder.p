/* ----------------------------------------------------------------------
  module .......: Mm/tarj7_renewal_reminder.p
  task .........: Notify customer for TARJ7 automatic renewal
  application ..: tms
  author .......: vikas
  created ......: 15.10.13
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

DEF VAR ldaFromdate       AS DATE NO-UNDO.
DEF VAR liTime            AS INT  NO-UNDO.
DEF VAR ldeReqStamp       AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcSMSText         AS CHAR NO-UNDO.

DEF STREAM Sout.

lcLogDir = fCParam("PrepaidBundle","TARJ7_LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "tarj7_renewal_reminder_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM Sout TO VALUE(lcLogFile).

FOR FIRST ServiceLimit WHERE
          ServiceLimit.GroupCode = "TARJ7" NO-LOCK,
     EACH MServiceLimit WHERE
          MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
          MServiceLimit.DialType = ServiceLimit.DialType AND
          MServiceLimit.EndTS    = 99999999.99999 NO-LOCK:

   fSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).

   IF ldaFromdate >= TODAY THEN NEXT.

   /* Skip the case if the current day of the month is not day of the month 
      before the activation date. */
   IF DAY(ldaFromdate) EQ 1 THEN DO:
      IF fLastDayOfMonth(TODAY) NE TODAY THEN NEXT.
   END.
   ELSE DO:
      IF DAY(fLastDayOfMonth(TODAY)) >= DAY(ldaFromdate) THEN DO:
         IF DAY(TODAY) NE DAY(ldaFromdate - 1) THEN NEXT.
      END.
      ELSE IF TODAY NE fLastDayOfMonth(TODAY) - 1 THEN NEXT.      
   END.

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|MobSub not found" SKIP.
      NEXT.
   END.

   FIND FIRST Customer WHERE
              Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|Customer not found" SKIP.
      NEXT.
   END.

   lcSMSText = fGetSMSTxt("TARJ7RenewalReminder",
                          TODAY,
                          Customer.Language,
                          OUTPUT ldeReqStamp).

    
   IF lcSMSText > "" THEN DO:
       
      lcSMSText = REPLACE(lcSMSText,"#DATE",
                          STRING(DAY(TODAY + 1),"99") + "/" +
                          STRING(MONTH(TODAY + 1),"99")).
        
      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     9,
                     lcSMSText,
                     ldeReqStamp,
                     "22622",
                     "").
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) "|SMS Sent" SKIP.
   END. /* IF lcSMSText > "" THEN DO: */
END.

OUTPUT STREAM Sout CLOSE.
