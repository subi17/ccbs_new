def stream sout.
def stream sout2.
def stream sout3.

DEF VAR lcTargetFolder AS CHAR NO-UNDO. 

lcTargetFolder = "../tms_support/2018/acc-quickwin/".

output stream sout to value(lcTargetFolder + "dumpfile.d").
output stream sout2 to value(lcTargetFolder + "dumphpd.d").
output stream sout3 to value(lcTargetFolder + "dffield.d").

FOR EACH dumpfile NO-LOCK where
   index(dumpfile.dumpname,"orderproduct") > 0 :

   export stream sout dumpfile.

   FIND dumphpd NO-LOCK where
        dumphpd.dumpid = dumpfile.dumpid.
   export stream sout2 dumphpd.

   FOR EACH dffield NO-LOCK where
            dffield.dumpid = dumpfile.dumpid :
      export stream sout3 dffield.
   END.
end.
