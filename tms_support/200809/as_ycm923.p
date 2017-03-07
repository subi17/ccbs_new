{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/msisdn.i}
DEFINE VARIABLE secs AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldDate AS DATE NO-UNDO. 

def stream sout.
output stream sout to /apps/snet/200809/as_ycm923_2.txt.

def buffer m2 for msisdn.
FOR EACH m2 WHERE
   m2.brand = "1" NO-LOCK USE-INDEX CLI
   BREAK BY m2.cli:
   IF FIRST-OF(m2.cli) AND m2.statuscode = 4 and
      m2.validto < 99999999 AND
      lookup(m2.pos,"CC,POS,TELEM,Webshop") > 0 then do:
      
      fSplitTS(m2.validto, output ldDate, output secs). 
      
      
      IF TODAY - ldDate > 90 THEN DO:
         
         fMakeMsidnHistory(recid(m2)).

         assign
            msisdn.custnum = 1001
            msisdn.statuscode = 1
            msisdn.outoperator = "".
         
         put stream sout unformatted msisdn.cli " " msisdn.pos " " msisdn.validto " " TODAY - ldDate SKIP.
      
      END.
   END.  
END.   
output stream sout close.

/*
fMakeMsidnHistory(recid(msisdn)).

assign
msisdn.custnum = 1001
msisdn.statuscode = 1
msisdn.outoperator = "".
*/
