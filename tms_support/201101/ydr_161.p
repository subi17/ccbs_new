{testpaa.i}
{tmsconst.i}

DEFINE VARIABLE lcErrorCodes AS CHARACTER NO-UNDO. 

lcErrorCodes = "ACCS NXSES".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def buffer bMNPOperation FOR MNPOperation.

def stream sout.
output stream sout to /apps/yoigo/tms_support/201101/ydr_161.txt.

MNPPROCESS_LOOP:
FOR EACH MNPOperation WHERE
   MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO} AND
   LOOKUP(MNPOperation.ErrorCode,lcErrorCodes) > 0 AND
   MNPOperation.Sender = {&MNP_SENDER_TMS} AND
   MNPOperation.StatusCode > 10 NO-LOCK,
   FIRST MNPProcess NO-LOCK WHERE
         MNPProcess.MNPSeq = MNPOperation.MNPSeq AND
         MNPProcess.StatusCode = 0:

   FIND bMNPOperation EXCLUSIVE-LOCK WHERE
        RECID(bMNPOperation) = RECID(MNPOperation).
   
   put stream sout unformatted 
      bMNPOperation.mnpoperationid "|"
      bMNPOperation.errorcode "|"
      bMNPOperation.errordesc skip.
   
   ASSIGN
      bMNPOperation.ErrorHandled = 0
      bMNPOperation.ErrorCode = ""
      bMNPOperation.ErrorDesc = ""
      bMNPOperation.MsgTurn = MNPOperation.MsgTurn + 1
      bMNPOperation.StatusCode = {&MNP_MSG_WAITING_SEND}.

   RELEASE bMNPOperation. 
   
   i = i + 1.
end.

disp i.
