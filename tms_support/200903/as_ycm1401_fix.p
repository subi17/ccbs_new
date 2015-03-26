input from /apps/snet/200903/as_ycm1401.log.
output to /apps/snet/200903/as_ycm1401_fixed.log.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttDup
FIELD filu AS CHAR
FIELD cli AS CHAR
FIELD gsmbnr AS CHAR
FIELD datest AS DATE
FIELD timestart AS INT
FIELD counted AS INT
FIELD billdur AS INT
INDEX i IS PRIMARY UNIQUE cli filu gsmbnr datest timestart.

repeat:
   import unformatted lcLine.
   find mobcdr where
      recid(mobcdr) = int(entry(1,lcLine,"|")) NO-LOCK NO-ERROR.
   
   find mcdrdtl2 where
        mcdrdtl2.datest = mobcdr.datest and
        mcdrdtl2.dtlseq = mobcdr.dtlseq NO-LOCK.
   
   find ttDup where
     ttDup.filu = entry(6,mcdrdtl2.detail,"|") and
     ttDup.cli = mobcdr.cli and
     ttDup.gsmbnr = mobcdr.gsmbnr and
     ttDup.datest = mobcdr.datest and
     ttDup.timestart = mobcdr.timestart and
     ttDup.billdur = mobcdr.billdur EXCLUSIVE-LOCK  NO-ERROR.
   
   IF NOT AVAIL ttDup THEN DO:
      create ttDup.
      assign 
         ttDup.filu = entry(6,mcdrdtl2.detail,"|") 
         ttDup.cli = mobcdr.cli
         ttDup.gsmbnr = mobcdr.gsmbnr
         ttDup.datest = mobcdr.datest
         ttDup.timestart = mobcdr.timestart
         ttDup.billdur = mobcdr.billdur
         ttDup.counted = 1.
      put unformatted recid(mobcdr) "|" 
         mobcdr.errorcode "->" entry(2,lcLine,"|") "|" 
         mobcdr.invseq "->" entry(3,lcLine,"|") skip.
      find current mobcdr EXCLUSIVE-LOCK.
      assign
         mobcdr.errorcode = int(entry(2,lcLine,"|"))
         mobcdr.invseq = int(entry(3,lcLine,"|")).
      release mobcdr.
   END.
   else do:
      put unformatted lcLine skip.
   end.
end.
