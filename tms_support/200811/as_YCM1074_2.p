{commpaa.i}
katun = "anttis".
gcBrand = "1".
{date.i}
{msisdn.i}

DEFINE VARIABLE ldDate AS DATE NO-UNDO.
DEFINE VARIABLE secs AS INTEGER NO-UNDO.

def stream sout.

output stream sout to /home/anttis/guu.txt.

def buffer m2 for msisdn.
FOR EACH m2 WHERE
   m2.brand = "1" NO-LOCK USE-INDEX CLI
   BREAK BY m2.cli:
   
   IF FIRST-OF(m2.cli) AND m2.statuscode = 4 and
      m2.validto < 99999999 AND
      m2.pos = "GIFT" then do:

      fSplitTS(m2.validto, output ldDate, output secs).

      fMakeMsidnHistory(recid(m2)).

      assign
         msisdn.outoperator = ""
         msisdn.pos = "POS"
         msisdn.statuscode = 1
         msisdn.orderid = 0 
         msisdn.msseq = ?. 

      put stream sout unformatted m2.cli " " m2.pos " " m2.validto " " TODAY - ldDate SKIP.

   END.
END.
output stream sout close.
