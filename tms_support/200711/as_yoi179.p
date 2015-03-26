DEFINE VARIABLE lcPoss AS CHARACTER NO-UNDO init "JANNE,JAAKKO".
DEFINE VARIABLE lcPos AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaTo AS DATE NO-UNDO.
DEFINE VARIABLE crecid AS RECID.
DEFINE VARIABLE ldeMax AS DECIMAL NO-UNDO init 0.

def buffer msisdn2 for msisdn.
def buffer msisdn3 for msisdn.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO.
def stream excel.
def stream excel2.
OUTPUT STREAM excel TO "/apps/snet/200711/as_yoi179.report".
OUTPUT STREAM excel2 TO "/apps/snet/200711/as_yoi179.msisdn".
output to /apps/snet/200711/as_yoi179.bak.
PUT STREAM excel UNFORMATTED "CLI|POS|ValidFrom:"  SKIP.
do i = 1 to num-entries(lcPoss,","):
   lcPos = entry(i,lcPoss,",").
   FOR EACH msisdn no-lock where
      pos = lcPos break
      by msisdn.cli:
      if first-of(msisdn.cli) then do: 
         find first msisdn2 where msisdn2.brand = "1" and
            msisdn2.cli = msisdn.cli use-index CLI.   
         if  msisdn2.orderid eq 0 and msisdn2.msseq eq ? and msisdn2.statuscode ne 0
         then do:
            PUT STREAM excel2 UNFORMATTED msisdn2.cli + " "
        string(msisdn2.orderid) + " " string(msisdn2.msseq) + " " +
        string(msisdn2.statuscode) skip.
            for each msisdn3 EXCLUSIVE-LOCK where 
            msisdn3.cli = msisdn2.cli break 
            by msisdn3.cli
            by msisdn3.validfrom: 
            export msisdn3.   
            if first-of(msisdn3.cli) then do:
               PUT STREAM excel UNFORMATTED "Status 0: " + msisdn3.cli + " "
               + msisdn3.pos + " " + string(msisdn3.validfrom) SKIP.
               msisdn3.statuscode = 0. 
               next.
            END.
               PUT STREAM excel UNFORMATTED "DELETED : " +  msisdn3.cli + " " 
               + msisdn3.pos + " " + string(msisdn3.validfrom) SKIP.
               delete msisdn3.
            end.
            PUT STREAM excel UNFORMATTED SKIP(1). 
         end.
      end.
   END.
end.
output close.

FOR EACH msisdn where pos = "JAAKKO" OR pos = "JANNE" EXCLUSIVE-LOCK:
   PUT STREAM excel "POS changed to SNSTEST" msisdn.cli + " " + string(msisdn.validto) +  " " + msisdn.pos SKIP.
   msisdn.pos = "SNSTEST".
END.

OUTPUT STREAM excel CLOSE.
OUTPUT STREAM excel2 CLOSE.
