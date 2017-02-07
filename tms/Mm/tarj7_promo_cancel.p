/* ----------------------------------------------------------------------
  module .......: Mm/tarj7_promo_cancel.p
  task .........: Daily script to cancel TARJ7 promo (1.2 GB=>600 MB). YPR-1747
  application ..: tms
  author .......: anttis
  created ......: 17.10.2014
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

DEF VAR ldaFromdate       AS DATE NO-UNDO.
DEF VAR liTime            AS INT  NO-UNDO.
DEF VAR ldeReqStamp       AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.

DEF VAR liRequest AS INT NO-UNDO. 
DEF VAR lcResult AS CHAR NO-UNDO. 
DEF VAR ldeNow AS DEC NO-UNDO. 
DEF VAR ldeToday AS DEC NO-UNDO. 

DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 

DEF STREAM Sout.

lcLogDir = fCParam("PrepaidBundle","TARJ7_LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "tarj7_promo_cancel_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM Sout TO VALUE(lcLogFile) append.

def buffer bmservicelimit for mservicelimit.
def buffer bOrigrequest for msrequest.

ASSIGN
   ldeNow = fmakets()
   ldeToday = YEAR(TODAY) * 10000 +
              MONTH(TODAY) * 100 +
              DAY(TODAY).

FOR FIRST ServiceLimit WHERE
          ServiceLimit.GroupCode = "TARJ7" NO-LOCK,
     EACH MServiceLimit WHERE
          MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
          MServiceLimit.DialType = ServiceLimit.DialType AND
          MServiceLimit.EndTS    = 99999999.99999 NO-LOCK:

   fSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).

   IF ldaFromdate >= TODAY THEN NEXT.

   i = i + 1.

   if not session:batch and i mod 100 = 0 then do:
      disp i j with frame a.
      pause 0.
   end.

   /* Skip the case if the current day of the month is not day of the month 
      before the activation date. */
   IF DAY(ldaFromdate) EQ 1 THEN DO:
      IF DAY(TODAY) NE 1 THEN NEXT.
   END.
   ELSE DO:
      IF DAY(fLastDayOfMonth(TODAY)) >= DAY(ldaFromdate) THEN DO:
         IF DAY(TODAY) NE DAY(ldaFromdate) THEN NEXT.
      END.
      ELSE IF TODAY NE fLastDayOfMonth(TODAY) THEN NEXT.      
   END.
 
   IF MServiceLimit.InclAmt NE 1228 THEN NEXT.

   j = j + 1.

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|MobSub not found" skip.
      NEXT.
   END.

   FIND FIRST Customer WHERE
              Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|Customer not found" skip.
      NEXT.
   END.
   
   IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.ReqType = 1 AND
                     MsRequest.ReqCParam1 EQ "SHAPER" AND
                     MsRequest.ReqCParam2 EQ "TARJ7_PROMO" AND
                     MsRequest.ActStamp >= ldeToday) THEN DO:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|Skipped, promotion was activated on the same day" skip.
      NEXT.
   END.

   find first bOrigrequest NO-LOCK where
              bOrigrequest.msseq = MobSub.msseq and
              bOrigrequest.reqtype = 8 and
              bOrigrequest.reqstatus = 2 and
              bOrigrequest.reqcparam3 = "TARJ7" and
              bOrigrequest.actstamp = MServiceLimit.fromts no-error.

   IF NOT AVAIL bOrigrequest then do:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|ERROR:original activation request not found" skip.
      NEXT.
   end.

   liRequest = fServiceRequest(MobSub.MsSeq,
                               "SHAPER",
                               1, /* activate */
                               "TARJ7",
                               ldeNow,
                               "",         /* salesman */
                               FALSE,      /* fees */
                               FALSE,      /* sms */
                               "",
                               "5",
                               bOrigrequest.msrequest,
                               FALSE,
                               OUTPUT lcResult).

   if liRequest eq 0 then do:
      PUT STREAM Sout UNFORMATTED STRING(MServiceLimit.MsSeq) +
                 "|ERROR:request creation failed:" lcResult skip.
      NEXT.

   end.

   find bmservicelimit EXCLUSIVE-LOCK  where
        rowid(bmservicelimit) = rowid(MServiceLimit).

   ASSIGN
      bmservicelimit.inclamt = 600.

   DEFINE VARIABLE lcOrdertype AS CHARACTER NO-UNDO.
   lcOrdertype = "".
   FOR EACH  order NO-LOCK where
             order.msseq = MobSub.msseq,
        first orderaction of order NO-LOCK where
              orderaction.itemtype = "promotion":
         lcOrdertype = string(order.ordertype).
   end.

   PUT STREAM Sout UNFORMATTED
      bMServiceLimit.MsSeq "|"
      MobSub.cli "|"
      lcOrdertype "|"
      recid(bMServiceLimit) "|"
      bMServiceLimit.fromTs "|"
      "OK:" liRequest skip.

   release bMServiceLimit.

END.

OUTPUT STREAM Sout CLOSE.
