/* ----------------------------------------------------------------------
  MODULE .......: ifs_q25hrlp_response_reader.p
  TASK .........: Handle Q25 HRLP response file (sent by IFS)
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & kariaika
  CREATED ......: 29.3.2016
  Version ......: yoigo
---------------------------------------------------------------------- */
{tmsconst.i}
{q25functions.i}


lcTableName = "HRLP".
lcActionID = {&Q25_HRLP_RESP_READER}.
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
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.

      RELEASE Actionlog.
   END.
END.

/*Kari, add file reading functionality here*/


DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.


