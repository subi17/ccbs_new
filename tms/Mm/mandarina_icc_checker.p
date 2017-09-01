/* ----------------------------------------------------------------------
  MODULE .......: mandarina_icc_checker.p
  TASK .........: Program removes Mandarina redirection if it is active
                  when ICC is done.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 08/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/*----------------------------------------------------------------------
https://kethor.qvantel.com/browse/MANDLP-10
---------------------------------------------------------------------- */

/* includes */
{Syst/commpaa.i}
gcbrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/lpfunctions.i}

/* Lock and seek time hadling parameters */
DEF VAR lcTableName            AS CHAR NO-UNDO.
DEF VAR lcActionID             AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS        AS DEC  NO-UNDO.
DEF VAR ldCollPeriodStartTS    AS DEC  NO-UNDO.
DEF VAR ldCollPeriodEndTS      AS DEC  NO-UNDO.
DEF VAR lcError                AS CHAR NO-UNDO.
DEF VAR llgReqDone             AS LOGICAL NO-UNDO.

DEF BUFFER bLP_MsRequest FOR MsRequest.

/* Check if process ir running */
ASSIGN
   lcTableName = "MANDARINA"
   lcActionID  = "Mandarina_ICC_Checker"
   ldCurrentTimeTS = fMakeTS().

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      QUIT. /*No reporting in first time.*/
   END.
   ELSE DO:
      /*Set collection period start time to match the previous period end*/
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS
         ldCollPeriodStartTS    = ActionLog.ActionTS.
      RELEASE Actionlog.
   END.
END.


ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60). /*Now - 1 minute */

/* ICC lookup part */
/* Find ICC requests */
/* Find information if LP is active. If yes, switch it off */
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand EQ gcBrand AND
         MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} AND
         MsRequest.UpdateStamp > ldCollPeriodStartTS AND
         MsRequest.UpdateStamp < ldCollPeriodEndTS AND
         MsRequest.ReqType EQ {&REQTYPE_ICC_CHANGE} /*15*/ AND
         MsRequest.UpdateStamp <= MsRequest.DoneStamp:
   FIND FIRST bLP_MsRequest NO-LOCK WHERE 
              bLP_MsRequest.MsSeq EQ MsRequest.MsSeq AND
              bLP_MsRequest.ActStamp < MsRequest.DoneStamp AND
              bLP_MsRequest.ReqCparam1 EQ "LP" 
              USE-INDEX MsActStamp NO-ERROR.
   IF AVAIL bLP_MsRequest THEN DO:
      IF (bLP_MsRequest.ReqCparam2 EQ "REDIRECTION_OTAFAILED1" OR
          bLP_MsRequest.ReqCparam2 EQ "REDIRECTION_OTAFAILED2") THEN DO:
         /*The last LP command was Mandarina LP redirection setting
           -> this must be cancelled*/
         llgReqDone = fMakeLPCommandRequest(MsRequest.MsSeq,
                                            "REMOVE",
                                            MsRequest.CustNum,
                                            "Redirection removed",
                                            "Redirection removed, reason: ICC",
                                            "ICC_Checker",
                                            "5", /* Automatic */ 
                                            INPUT-OUTPUT lcError).
         /* TODO: Log writing and error handling.*/
         /* TODO: Source setting for DUMP purposes. */
              
      END.
      ELSE NEXT. /*do not remove other commands*/

   END.
   ELSE NEXT.
END.



/*End of ICC lookup */

/*Update cunrent collection period end time to actionlog*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionTS = ldCollPeriodEndTS.
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.



