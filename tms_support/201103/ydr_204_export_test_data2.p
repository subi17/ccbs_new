session:numeric-format = "american".
session:year-offset = 1980.

def stream smobcdr.
output stream smobcdr to prepcdr.d.

def stream smcdrdtl2.
output stream smcdrdtl2 to mcdrdtl2_prepcdr.d.

FOR EACH prepcdr NO-LOCK where
         prepcdr.cli = "649060042" and
         prepcdr.errorcode = 8040
         use-index cli:

   find mcdrdtl2 where
        mcdrdtl2.datest = prepcdr.datest and
        mcdrdtl2.dtlseq = prepcdr.dtlseq NO-LOCK.

   export stream smobcdr prepcdr.
   export stream smcdrdtl2 mcdrdtl2.
  
END.

