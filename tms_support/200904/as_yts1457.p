DEFINE VARIABLE filut AS CHARACTER NO-UNDO. 

filut = "CGW_CDR_200904080915_206458 CCGW_CDR_200904101005_207042 CCGW_CDR_200904101020_207045 CCGW_CDR_200904111820_207429". 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttFile
FIELD filu AS CHAR
INDEX i IS PRIMARY UNIQUE filu. 

do i = 1 to num-entries(filut, " "):
   create ttFile.
   assign ttFile.filu = entry(i, filut, " ").
end.

def stream sout.
output stream sout to /apps/snet/200904/as_ycm1457.log.

def buffer bmobcdr for mobcdr.

DEFINE TEMP-TABLE ttDup
FIELD filu AS CHAR
FIELD cli AS CHAR
FIELD gsmbnr AS CHAR
FIELD datest AS DATE
FIELD timestart AS INT
FIELD billdur AS INT
INDEX i IS PRIMARY UNIQUE cli filu gsmbnr datest timestart.

FOR EACH mobcdr WHERE
   mobcdr.datest >= 4/1/2009 and
   mobcdr.datest <= 4/14/2009 NO-LOCK:
   
   IF mobcdr.rateccn = 72 then do: 

      find mcdrdtl2 where
           mcdrdtl2.datest = mobcdr.datest and
           mcdrdtl2.dtlseq = mobcdr.dtlseq NO-LOCK.
      
      find ttFile where ttFile.filu = entry(6,mcdrdtl2.detail,"|") NO-LOCK NO-ERROR.
      IF AVAIL ttFile THEN DO:
      
         find ttDup where
           ttDup.filu = entry(6,mcdrdtl2.detail,"|") and
           ttDup.cli = mobcdr.cli and
           ttDup.gsmbnr = mobcdr.gsmbnr and
           ttDup.datest = mobcdr.datest and
           ttDup.timestart = mobcdr.timestart and
           ttDup.billdur = mobcdr.billdur NO-LOCK  NO-ERROR.
         
         IF NOT AVAIL ttDup THEN DO:
            create ttDup.
            assign 
               ttDup.filu = entry(6,mcdrdtl2.detail,"|") 
               ttDup.cli = mobcdr.cli
               ttDup.gsmbnr = mobcdr.gsmbnr
               ttDup.datest = mobcdr.datest
               ttDup.timestart = mobcdr.timestart
               ttDup.billdur = mobcdr.billdur.
               
               put stream sout unformatted recid(mobcdr) 
                  "|" mobcdr.errorcode 
                  "|" mobcdr.invseq "|SKIPPED" skip. 
            
         END.
         ELSE DO:
            do trans:
               find bmobcdr EXCLUSIVE-LOCK where
                  recid(bmobcdr) = recid(mobcdr).
               
               put stream sout unformatted recid(bmobcdr) 
                  "|" bmobcdr.errorcode 
                  "|" bmobcdr.invseq skip. 
               
               assign
                  bmobcdr.errorcode = 9100 
                  bmobcdr.invseq = 0.
               release bmobcdr.
            end.
         END.
      
      END.
   
   END.   
   
ENd.

output stream sout close.
