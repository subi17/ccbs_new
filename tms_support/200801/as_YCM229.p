def stream sout.
OUTPUT stream sout TO /apps/snet/200801/roamreport.txt.
DEFINE VARIABLE p AS CHARACTER NO-UNDO.
for each roamoper no-lock.
case roamoper.production:
when 0 then p = "-".
when 1 then p = "V".
when 2 then p = "G".
when 3 then p = "A".
end.
/*
message "\"" + roamoper.imsi + "\"" "\"" + roamoper.plmn + "\"" "\"" + roamoper.country + "\"" "\"" + roamoper.commname + "\"" "\"" + roamoper.roamgroup + "\"" "\"" + p + "\"" "\"" + STRING(roamoper.fileseq) + "\"" "\"" + STRING(roamoper.testfileseq) + "\"".

pause 0.
*/
/*
 export roamoper.imsi roamoper.plmn roamoper.country roamoper.commname
        roamoper.roamgroup p roamoper.fileseq roamoper.testfileseq.
*/
   put stream sout unformatted 
      roamoper.imsi chr(9)
      roamoper.plmn chr(9)
      roamoper.country chr(9)
      roamoper.commname chr(9)
      roamoper.roamgroup chr(9)
      p chr(9)
      roamoper.fileseq chr(9)
      roamoper.testfileseq skip.
end.
output stream sout close.
