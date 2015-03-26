{date.i}

DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaFrom AS DATE NO-UNDO. 

IF SESSION:BATCH THEN DO:
   lcParam = SESSION:PARAMETER.

   IF lcParam NE "" THEN DO:
      ldaFrom = DATE(lcParam).
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Parameter is not date".
         RETURN.
      END.
   END.
END.

IF ldaFrom = ? THEN ldaFrom = TODAY - 1.

def stream slog.
output stream slog to value("/scratch/log/roamcdr_delays/roamcdr_delays_" 
   + string(year(ldaFrom),"9999") + string(month(ldaFrom),"99") + 
     string(day(ldaFrom),"99") + ".txt").

FOR EACH roamcdr where
   roamcdr.datestart >= ldaFrom
   and roamcdr.datestart <= ldaFrom  NO-LOCK:
  if roamcdr.dateread > roamcdr.datestart then do:
      if roamcdr.dateread > roamcdr.datestart + 1  or
         roamcdr.tsread - trunc(roamcdr.tsread,0) > 0.04500 then 
      put stream slog unformatted 
        roamcdr.plmn "|"
        roamcdr.eventtype "|"
        fTS2HMS(fDate2TS(roamcdr.datestart) + (roamcdr.timestart / 100000)) "|"
        fTS2HMS(roamcdr.tsread) "|"
        units skip.
   end. 
END.

FOR EACH roamgprs where
   roamgprs.datestart >= ldaFrom 
   and roamgprs.datestart <= ldaFrom NO-LOCK:
  if roamgprs.dateread > roamgprs.datestart then do:
      if roamgprs.dateread > roamgprs.datestart + 1  or
         roamgprs.tsread - trunc(roamgprs.tsread,0) > 0.04500 then 
      put stream slog unformatted 
        roamgprs.plmn "|"
        roamgprs.eventtype "|"
        fTS2HMS(fDate2TS(roamgprs.datestart) + (roamgprs.timestart / 100000)) "|"
        fTS2HMS(roamgprs.tsread) "|"
        units skip.
   end. 
END.

output stream slog close.
