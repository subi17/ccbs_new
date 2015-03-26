{commpaa.i}
katun = "anttis".
gcBrand = "1".
{fmakemsreq.i}

def stream sin.
input stream sin from /apps/snet/200901/as_yts1248.log.

def stream sout.
output stream sout to /apps/snet/200901/as_yts1248.fix append.

def stream serr.
output stream serr to /apps/snet/200901/as_yts1248.err append.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liReq AS INTEGER NO-UNDO.
DEFINE VARIABLE imsseq AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcInfo AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def buffer br for msrequest.
repeat:
   import stream sin unformatted lcLine.
  
   i = i + 1.

   if i <= 10 then next.
/*
if i > 10 then leave.
*/
   imsseq = int(entry(1,lcLine,"|")).
   
   find mobsub where
      mobsub.msseq = imsseq NO-LOCK NO-ERROR.

   IF NOT AVAIL mobsub then do:
      put stream serr unformatted imsseq "|" " NOT FOUND" SKIP.
      next.
   end.
   
   FIND FIRST subser WHERE
      subser.msseq = mobsub.msseq and
      subser.servcom = "obr" NO-LOCK use-index servcom NO-ERROR.

   IF AVAIL subser and 
      length(subser.servcom) = 4 and
      subser.ssstat = 0 then do:
      
      liReq = fServiceRequest(MobSub.MsSeq,
                              TRIM(SubSer.ServCom),
                              SubSer.SSStat,
                              SubSer.SSParam,
                              fMakeTS(),
                              "",
                              FALSE,      /* fees */
                              FALSE,      /* sms */
                              "",
                              OUTPUT lcInfo).

      put stream sout unformatted mobsub.msseq "|" liReq "|" lcInfo skip.
   
   END.
   ELSE DO:
      put stream serr unformatted imsseq "|" " OK ALREADY" SKIP.
      next.
   END.
   

end.

input stream sin close.
output stream sout close.

