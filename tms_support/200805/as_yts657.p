{testpaa.i}
{msreqfunc.i}

def stream sout.
output stream sout to "/apps/snet/200805/as_yts657_msrequest.bak" append.
DEFINE VARIABLE liNewStatus AS INTEGER NO-UNDO.

liNewStatus = 9.

FOR EACH msrequest where
   msrequest.msrequest = 353480 NO-LOCK:
   put stream sout unformatted msrequest.msrequest " " msrequest.reqstatus 
   " => " liNewStatus skip.
   fReqStatus(liNewStatus,"Tech support (yts-657)"). 
END.

output stream sout close.
