/* ----------------------------------------------------------------------
  module .......: Mm/additional_term_reminder.p
  task .........: Notify if customer has pending termination request for
                  additional line and last primary line was ported out.
  application ..: tms
  author .......: vikas
  created ......: 27.02.13
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fgettxt.i}
{Func/fmakesms.i}
{Syst/tmsconst.i}
{Func/coinv.i}

DEF VAR ldFirstDayOfMonth AS DATE NO-UNDO.
DEF VAR ldLastDayOfMonth  AS DATE NO-UNDO.
DEF VAR ldaTermdate       AS DATE NO-UNDO.
DEF VAR liTermTime        AS INT  NO-UNDO.
DEF VAR ldeFromTS         AS DEC  NO-UNDO.
DEF VAR ldeReqStamp       AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcSMSTextProfile  AS CHAR NO-UNDO.
DEF VAR lcSMSText         AS CHAR NO-UNDO.

DEF BUFFER bMsRequest     FOR MsRequest.

DEF STREAM Sout.

ASSIGN ldFirstDayOfMonth = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldLastDayOfMonth  = fLastDayOfMonth(ldFirstDayOfMonth)
       ldeFromTS         = fMake2Dt(ldLastDayOfMonth,86399).

/* Only Send the SMS 20 or 2 days before of termination date */
CASE (ldLastDayOfMonth - TODAY):
   WHEN 20 OR WHEN 2 THEN .
   OTHERWISE RETURN.
END.

lcLogDir = fCParam("MultiSim","LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "additional_term_reminder_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM Sout TO VALUE(lcLogFile).

TERM_LOOP:
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand      = gcBrand AND
         MsRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
         MsRequest.ReqStatus  = {&REQUEST_STATUS_NEW} AND
         MsRequest.ActStamp   = ldeFromTS AND
         MsRequest.ReqCparam3 = STRING({&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM}),
   FIRST MobSub WHERE
         MobSub.MsSeq = MsRequest.MsSeq NO-LOCK,
   FIRST Customer WHERE
         Customer.CustNum = MobSub.CustNum NO-LOCK:

   fSplitTS(MsRequest.ActStamp,OUTPUT ldaTermdate,OUTPUT liTermTime).

   /* Only Send the SMS 20 or 2 days before of termination date */
   CASE (ldaTermdate - TODAY):
      WHEN 20 THEN lcSMSTextProfile = "AdditionalSIMTermRem2".
      WHEN 2  THEN lcSMSTextProfile = "AdditionalSIMTermRem3".
      OTHERWISE NEXT TERM_LOOP.
   END.

   FIND FIRST bMsRequest WHERE
              bMsRequest.MsRequest  = MsRequest.OrigRequest AND
              bMsRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
              bMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
              bMsRequest.ReqCparam3 = "2" NO-LOCK NO-ERROR.
   IF NOT AVAIL bMsRequest THEN NEXT TERM_LOOP.

   lcSMSText = fGetSMSTxt(lcSMSTextProfile,
                          TODAY,
                          Customer.Language,
                          OUTPUT ldeReqStamp).

   IF lcSMSText > "" THEN DO:
      lcSMSText = REPLACE(lcSMSText,"#DATE",STRING(ldaTermdate)).
      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     9,
                     lcSMSText,
                     ldeReqStamp,
                     "22622",
                     "").
      PUT STREAM Sout UNFORMATTED MobSub.CLI "|SMS Sent" skip.
   END. /* IF lcSMSText > "" THEN DO: */
END.

OUTPUT STREAM Sout CLOSE.


