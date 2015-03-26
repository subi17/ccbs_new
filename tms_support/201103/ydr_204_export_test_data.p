session:numeric-format = "american".
session:year-offset = 1980.

def stream smobcdr.
output stream smobcdr to mobcdr.d.

def stream smcdrdtl2.
output stream smcdrdtl2 to mcdrdtl2.d.

FOR EACH mobcdr NO-LOCK where
         mobcdr.cli = "633000107" and
         mobcdr.errorcode = 8040
         use-index cli:

   find mcdrdtl2 where
        mcdrdtl2.datest = mobcdr.datest and
        mcdrdtl2.dtlseq = mobcdr.dtlseq NO-LOCK.

   export stream smobcdr mobcdr.
   export stream smcdrdtl2 mcdrdtl2.
  
END.

