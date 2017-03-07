{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/msisdn.i}

input from /apps/snet/200812/Yoigo_Preactivated_Terminations_July.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

repeat:
   
   import unformatted lcLine.

   FIND FIRST msisdn WHERE
      msisdn.brand = "1" and
      msisdn.cli   = lcLine use-index CLI  NO-LOCK NO-ERROR.    
   
   fMakeMSIDNHistory(recid(msisdn)).

   ASSIGN
      msisdn.pos = "POS" 
      msisdn.statuscode = 1 
      msisdn.msseq = ?
      msisdn.orderid = 0.

end.
