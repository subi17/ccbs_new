/*------------------------------------------------------------
  MODULE .......: DBQUEUE.P
  FUNCTION .....: RUN queued procedures
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 24.02.2001 kl
  MODIFIED .....: 26.02.2001 kl: use BUFFER, main loop AS NO-LOCK
                  07.09.2001 kl: pCleanCdrQ added
                  13.09.2001 kl: ALL CDRs TO trancall in pCleanCdrQ
                  07.03.2003 kl: MASTER 1.0
                  07.03.2003/aam customer.balance[2] -> CreditLimit
                  07.06.2003 kl: run pCleanCdrQ commented
                  10.06.2003 kl: eventlog

  VERSION ......: M15
--------------------------------------------------------------*/

{Syst/commpaa.i}
{Func/lib/eventlog.i}

FUNCTION fAmIUp RETURNS INT
  (INPUT type AS INT, INPUT val AS INT).

   DEF VAR ret AS i NO-UNDO.

   case type.
      when 1 then ret = fCParamI("DBQueue").
      when 2 THEN DO:
         FIND FIRST TMSParam where
                    TMSParam.ParamCode = "DBQueue"
         exclusive-lock.
         ASSIGN TMSParam.IntVal = val NO-ERROR.
         release TMSParam.
      END.
   END.

   RETURN ret.

END.

DEF VAR lAmtUpd AS INT NO-UNDO.

fELog("CRON","DBQueue Started").

/* check IF I'm running already */
IF fAmIUp(1,0) = 0 THEN fAmIUp(2,1).
ELSE DO:
   fELog("CRON","DBQueue was already running").
   RETURN.
END.
if not session:batch then message "Cleaning queues ....".

/* clean MTHCall queue records */
RUN pCleanMthQ.

fELog("CRON","DBQueue Ended: " + string(lAmtUpd) + " records.").


/* set DBQueue TMSParam.IntVal into 0, I'm NOT running */
fAmIUp(2,0).
PROCEDURE pCleanMthQ:

   DEF BUFFER updqueue FOR TMSQueue.

   FOR EACH TMSQueue NO-LOCK.

      /* check IF record is free */
      FIND FIRST updqueue where
           recid(updqueue) = recid(TMSQueue)
      exclusive-lock NO-ERROR no-wait.
      /* IF NOT free jump TO NEXT one, this one on NEXT RUN */
      IF locked(updqueue) THEN NEXT.

      FIND FIRST MTHCall where
                 MTHCall.CustNum = updqueue.CustNum AND
                 MTHCall.Month   = updqueue.month
      exclusive-lock NO-ERROR.
      IF NOT AVAIL MTHCall THEN DO:
         FIND FIRST Customer where
                    Customer.CustNum = updqueue.CustNum
         NO-LOCK NO-ERROR.

         IF NOT AVAIL Customer THEN NEXT.

         CREATE MTHCall.
         ASSIGN
            MTHCall.CustNum   = updqueue.CustNum
            MTHCall.Month     = updqueue.Month
            MTHCall.Limit     = Customer.CreditLimit
            MTHCall.CloseDate = ?.
      END.

      ASSIGN 
         MTHCall.called = MTHCall.called + updqueue.Queued
         lAmtUpd        = lAmtUpd        + 1.

      /* monthly call alarm actions TO daily files ... */
      IF MTHCall.limit  > 0             AND
         MTHCall.called > MTHCall.limit AND
         MTHCall.alarm  = FALSE THEN DO:

         /* exceeded customer */
         FIND FIRST Customer where
                    Customer.CustNum = MTHCall.CustNum
         NO-LOCK NO-ERROR.
         ASSIGN MTHCall.alarm = TRUE.

      END.

      /* remove updated record */
      DELETE updqueue.

   END.

END.
