DEFINE VARIABLE filut AS CHARACTER NO-UNDO. 

filut = "CDEGYMSESPXF01006 CDINDSPESPXF02050 CDHUNH1ESPXF00774 CDFRAF3ESPXF01641 CDINDWBESPXF00910 CDITAWIESPXF00002 CDMNEMTESPXF00403 CDSWEIQESPXF01333 CDPRTTMESPXF00270 CDUKRASESPXF00596 CDLTU03ESPXF00946 CDBGRVAESPXF02697 CDFRAF1ESPXF02746 CDNZLNHESPXF00091 CDPHLSRESPXF01396 CDPRTOPESPXF01575 CDTURTCESPXF01152 CDUSAW6ESPXF00282 CDINDA5ESPXF00030 CDINDBLESPXF00030 CDRUSNWESPXF02223 CDRUSNWESPXF02224 CDSWETRESPXF02509 CDUSACGESPXF00342 CDUZB05ESPXF00692 CDINDATESPXF00041 CDINDATESPXF00042 CDINDA3ESPXF00030 CDINDA6ESPXF00035 CDINDA7ESPXF00030 CDINDH1ESPXF00035 CDINDMTESPXF00031 CDMYSMTESPXF00699 CDNLDPTESPXF02518 CDINDA1ESPXF00036 CDINDA8ESPXF00030 CDINDA9ESPXF00030 CDINDJBESPXF00035 CDINDJHESPXF00039 CDRUS01ESPXF00825 CDSLVTPESPXF00484 CDSLVTPESPXF00485 CDZAFCCESPXF00373 CDINDSCESPXF00032 CDSWEHUESPXF02459 CDUZBDUESPXF00765 CDZAFCCESPXF00371 CDZAFCCESPXF00372 CDDNKHUESPXF02461 CDFINTFESPXF02826 CDFRAF1ESPXF02747 CDDZAWTESPXF00351 CDFRAF1ESPXF02748 CDGEOGCESPXF04501 CDNZLNHESPXF00092 CDSVNMTESPXF00381 CDSVNMTESPXF00382 CDDNKIAESPXF02646 CDHRVCNESPXF01153 CDPERTMESPXF00424 CDBOLTEESPXF00105 CDDOM01ESPXF01295 CDDOMCLESPXF00067 CDFRAF1ESPXF02749 CDINDCCESPXF02659 CDITASIESPXF00958 CDPAKWAESPXF00646 CDRUSBDESPXF01303 CDECUPGESPXF00182 CDIRLH3ESPXF00749 CDUSACGESPXF00343 CDARGTPESPXF00111 CDDEUD1ESPXF01107 CDFRAF1ESPXF02750 CDPOLP4ESPXF01054 CDRUSNWESPXF02225".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttFile
FIELD filu AS CHAR
INDEX i IS PRIMARY UNIQUE filu. 

do i = 1 to num-entries(filut, " "):
   create ttFile.
   assign ttFile.filu = entry(i, filut, " ").
end.

def stream sout.
output stream sout to /apps/snet/200903/as_ycm1401_2.log.

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
   mobcdr.datest >= 3/1/2009 and
   mobcdr.datest <= 3/18/2009 NO-LOCK:
   
   IF (mobcdr.rateccn ne mobcdr.ccn) and
      (mobcdr.rateccn = 3 or 
      mobcdr.rateccn = 4 or
      mobcdr.rateccn = 7) then do: 

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
           ttDup.billdur = mobcdr.billdur EXCLUSIVE-LOCK  NO-ERROR.
         
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
                  bmobcdr.errorcode = 8041
                  bmobcdr.invseq = 0.
               release bmobcdr.
            end.
         END.
      
      END.
   
   END.   
   
ENd.

output stream sout close.
