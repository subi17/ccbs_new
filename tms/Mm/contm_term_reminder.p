/* ----------------------------------------------------------------------
  module .......: Mm/contm_term_reminder.p
  task .........: Notify if customer has pending termination request and
                  primary line was ported out.
  application ..: tms
  author .......: vikas
  created ......: 27.02.13
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
{coinv.i}

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
DEF VAR liTotal           AS INT  NO-UNDO.
DEF VAR liSMS             AS INT  NO-UNDO.
DEF VAR liError           AS INT  NO-UNDO.

DEF BUFFER lbMobSub       FOR MobSub.
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

lcLogFile = lcLogDir + "contm_term_reminder_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM Sout TO VALUE(lcLogFile).

TERM_LOOP:
FOR EACH MsRequest WHERE
         MsRequest.Brand     = gcBrand AND
         MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
         MsRequest.ReqStatus = {&REQUEST_STATUS_NEW} AND
         MsRequest.ActStamp  = ldeFromTS NO-LOCK,
   FIRST MobSub WHERE
         MobSub.MsSeq = MsRequest.MsSeq AND
         MobSub.MultiSIMId > 0 AND
         MobSub.MultiSIMType = {&MULTISIMTYPE_SECONDARY} NO-LOCK,
   FIRST Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK:

   /* If termination request reason is not MultiSIM */
   IF MsRequest.ReqCparam3 <> STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM})
   THEN NEXT TERM_LOOP.

   fSplitTS(MsRequest.ActStamp,OUTPUT ldaTermdate,OUTPUT liTermTime).

   /* Only Send the SMS 20 or 2 days before of termination date */
   CASE (ldaTermdate - TODAY):
      WHEN 20 THEN lcSMSTextProfile = "MultiSIMSecondaryTermRem2".
      WHEN 2  THEN lcSMSTextProfile = "MultiSIMSecondaryTermRem3".
      OTHERWISE NEXT TERM_LOOP.
   END.

   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand        = gcBrand AND
              lbMobSub.MultiSimID   = MobSub.MultiSimID AND
              lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
              lbMobSub.Custnum      = MobSub.Custnum NO-ERROR.
   IF AVAIL lbMobSub THEN DO:
      PUT STREAM Sout UNFORMATTED
          MobSub.CLI "|Warning:Primary line is already active" skip.
      liError = liError + 1.
      NEXT TERM_LOOP.
   END. /* IF AVAIL lbMobSub THEN DO: */

   FIND FIRST TermMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              TermMobSub.Brand        = gcBrand AND
              TermMobSub.MultiSimID   = MobSub.MultiSimID AND
              TermMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
              TermMobSub.Custnum      = MobSub.Custnum NO-ERROR.
   IF NOT AVAIL TermMobSub THEN DO:
      PUT STREAM Sout UNFORMATTED
          MobSub.CLI "|Error:Terminated Primary line not found" skip.
      liError = liError + 1.
      NEXT TERM_LOOP.
   END. /* IF NOT AVAIL TermMobSub THEN DO: */

   FOR EACH bMsRequest WHERE
            bMsRequest.MsSeq     = TermMobSub.MsSeq AND
            bMsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
            bMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-LOCK
            USE-INDEX MsSeq BY UpdateStamp DESC:

      /* If not MNP Outporting */
      IF bMsRequest.ReqCparam3 <> "2" THEN NEXT TERM_LOOP.
      LEAVE.
   END. /* FOR EACH bMsRequest WHERE */

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
      liSMS = liSMS + 1.
      PUT STREAM Sout UNFORMATTED MobSub.CLI "|SMS Sent" skip.
   END. /* IF lcSMSText > "" THEN DO: */
   ELSE DO:
      liError = liError + 1.
      PUT STREAM Sout UNFORMATTED MobSub.CLI "|ERROR:SMS not defined" skip.
   END.

   liTotal = liTotal + 1.
END.

OUTPUT STREAM Sout CLOSE.

IF NOT SESSION:BATCH THEN
   MESSAGE liTotal liSms liError VIEW-AS ALERT-BOX.

