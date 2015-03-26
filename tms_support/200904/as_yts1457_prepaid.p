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
output stream sout to /apps/snet/200904/as_ycm1457_prepaid.log.

def buffer bprepcdr for prepcdr.

DEFINE TEMP-TABLE ttDup
FIELD filu AS CHAR
FIELD cli AS CHAR
FIELD gsmbnr AS CHAR
FIELD datest AS DATE
FIELD timestart AS INT
FIELD billdur AS INT
INDEX i IS PRIMARY UNIQUE cli filu gsmbnr datest timestart.

FOR EACH prepcdr WHERE
   prepcdr.datest >= 4/8/2009 and
   prepcdr.datest <= 4/11/2009 NO-LOCK:
   
   IF prepcdr.rateccn = 72 then do: 

      find mcdrdtl2 where
           mcdrdtl2.datest = prepcdr.datest and
           mcdrdtl2.dtlseq = prepcdr.dtlseq NO-LOCK.
      
      find ttFile where ttFile.filu = entry(6,mcdrdtl2.detail,"|") NO-LOCK NO-ERROR.
      IF AVAIL ttFile THEN DO:
      
         find ttDup where
           ttDup.filu = entry(6,mcdrdtl2.detail,"|") and
           ttDup.cli = prepcdr.cli and
           ttDup.gsmbnr = prepcdr.gsmbnr and
           ttDup.datest = prepcdr.datest and
           ttDup.timestart = prepcdr.timestart and
           ttDup.billdur = prepcdr.billdur NO-LOCK  NO-ERROR.
         
         IF NOT AVAIL ttDup THEN DO:
            create ttDup.
            assign 
               ttDup.filu = entry(6,mcdrdtl2.detail,"|") 
               ttDup.cli = prepcdr.cli
               ttDup.gsmbnr = prepcdr.gsmbnr
               ttDup.datest = prepcdr.datest
               ttDup.timestart = prepcdr.timestart
               ttDup.billdur = prepcdr.billdur.
               
               put stream sout unformatted recid(prepcdr) 
                  "|" prepcdr.errorcode 
                  "|" prepcdr.invseq "|SKIPPED" skip. 
            
         END.
         ELSE DO:
            do trans:
               find bprepcdr EXCLUSIVE-LOCK where
                  recid(bprepcdr) = recid(prepcdr).
               
               put stream sout unformatted recid(bprepcdr) 
                  "|" bprepcdr.errorcode 
                  "|" bprepcdr.invseq skip. 
               
               assign
                  bprepcdr.errorcode = 9100 
                  bprepcdr.invseq = 0.
               release bprepcdr.
            end.
         END.
      
      END.
   
   END.   
   
ENd.

output stream sout close.
