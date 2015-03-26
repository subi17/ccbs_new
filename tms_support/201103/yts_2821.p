input from yts_2821.input.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMsisdn AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to yts_2821.log.

repeat:
   import unformatted lcLine.
   lcMsisdn = entry(2,lcLine,";").
   liMsSeq = int(entry(3,lcLine,";")).

   find mobsub where
      mobsub.brand = "1" and
      mobsub.cli = lcMsisdn and
      mobsub.msseq = liMsSeq NO-LOCK.

   FOR EACH fatime where
      fatime.brand = "1" and 
      fatime.msseq = mobsub.msseq and
      fatime.cli = mobsub.cli and
      fatime.ftgrp = "bono8cp" and
      fatime.period >= 201103 and
      fatime.period <= 201105 use-index mobsub:
      export stream sout fatime.
      delete fatime.
   end.
end.
