/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 28.01.10
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/tmsconst.i}
{Func/timestamp.i}

DEF BUFFER bMNPOperation FOR MNPOperation.
DEF BUFFER bMNPProcessResend FOR MNPProcess.

FUNCTION fResendMNPMessage RETURNS INT
   (BUFFER ibMNPOperation FOR MNPOperation,
    OUTPUT ocError AS CHARACTER):

   DEFINE VARIABLE liMNPOperSeq AS INTEGER NO-UNDO.
   
   IF ibMNPOperation.Sender NE {&MNP_SENDER_TMS} THEN DO:
      ocError = "Only message from TMS can be resend".
      RETURN 0.
   END.

   IF ibMNPOperation.StatusCode <= 10 THEN DO:
      ocError = "Cannot resend ongoing or handled message".
      RETURN 0.
   END.

   IF NOT CAN-FIND(FIRST bMNPProcessResend WHERE
        bMNPProcessResend.MNPSeq = ibMNPOperation.MNPSeq AND
        LOOKUP(STRING(bMNPProcessResend.StatusCode),"0,1,2,5") > 0) THEN DO:
      ocError = "Resending is allowed only for active process".
      RETURN 0.
   END.

   liMNPOperSeq = NEXT-VALUE(MNPOperSeq).

   CREATE bMNPOperation.
   bMNPOperation.MNPOperationId = liMNPOperSeq.

   ASSIGN
      bMNPOperation.CreatedTS = fMakeTS()
      bMNPOperation.MessageType = ibMNPOperation.MessageType
      bMNPOperation.MNPSeq = ibMNPOperation.MNPSeq
      bMNPOperation.MsgTurn = ibMNPOperation.MsgTurn + 1
      bMNPOperation.Sender = ibMNPOperation.Sender
      bMNPOperation.StatusCode = {&MNP_MSG_WAITING_SEND}
      bMNPOperation.XMLSeq = ibMNPOperation.XMLSeq.
      
   COPY-LOB ibMNPOperation.XMLRequest TO bMNPOperation.XMLRequest.

   RELEASE bMNPOperation.

   RETURN liMNPOperSeq.
              
END FUNCTION. 
