{Syst/testpaa.i}
{Syst/tmsconst.i}

DEFINE VARIABLE lcErrorCodes AS CHARACTER NO-UNDO. 

lcErrorCodes = "MNP_PROCE".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def buffer bMNPOperation FOR MNPOperation.

def stream sout.
output stream sout to /apps/yoigo/tms_support/201101/ydr_161_3.txt.

MNPPROCESS_LOOP:
FOR EACH MNPOperation WHERE
   MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO} AND
   LOOKUP(MNPOperation.ErrorCode,lcErrorCodes) > 0 AND
   MNPOperation.Sender = 2 AND
   MNPOperation.StatusCode = 902 and
   MNPOperation.errordesc = "MNP process already exists" NO-LOCK,

   FIRST MNPProcess NO-LOCK WHERE
         MNPProcess.MNPSeq = MNPOperation.MNPSeq AND
         MNPProcess.MNPType = 2:


      FIND bMNPOperation EXCLUSIVE-LOCK  WHERE
           RECID(bMNPOperation) = RECID(MNPOperation).
      
      put stream sout unformatted 
         bMNPOperation.mnpoperationid "|"
         bMNPOperation.errorcode "|"
         bMNPOperation.errordesc skip.
      
      ASSIGN
         bMNPOperation.ErrorHandled = 2.
      
      RELEASE bMNPOperation. 
      i = i + 1.
   
end.

disp i.
