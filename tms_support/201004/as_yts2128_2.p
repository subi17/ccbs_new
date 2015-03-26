input from _valid_charged_CDRs_to_resend_sorted_unique_without_filename.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLinePrev AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineNow AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liFileSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcRoamoper AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLines AS INTEGER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
def stream sfileseq.

output stream sfileseq to fileseq.txt.

find first roamoper NO-LOCK.
repeat:
   import unformatted lcLine.
   
   i = i + 1.
   
   lcLineNow = substring(lcLine,1,17).

   if lcLinePrev NE lcLineNow then do:

      output stream sout close.
      
      if i ne 1 then do:
         put stream sfileseq unformatted roamoper.plmn "|" recid(roamoper) "|" roamoper.fileseq "|" liFileSeq skip.
        roamoper.fileseq = liFileSeq.
      end.

      lcRoamoper = entry(3,lcLineNow).
      find roamoper where
           roamoper.plmn = lcRoamoper and
           production > 0 EXCLUSIVE-LOCK.
      
      liFileSeq = roamoper.fileseq + 1.
      liLines = 0.

      lcFile = "CDESPXF" + lcRoamoper + STRING(liFileSeq,"99999") + ".asc".
      output stream sout to value(lcFile).
   end.
     

   IF liLines >= 4000 THEN DO:
      liFileSeq = liFileSeq + 1.
      liLines = 0.
      output stream sout close.
      lcFile = "CDESPXF" + lcRoamoper + STRING(liFileSeq,"99999") + ".asc".
      output stream sout to value(lcFile).
   END.

   lcLine = substring(lcLine,1,17) + STRING(liFileSeq,"99999") + substring(lcLine,23).
   put stream sout unformatted lcline skip.
   liLines = liLines + 1.

   lcLinePrev = lcLineNow.
end.


put stream sfileseq unformatted roamoper.plmn "|" recid(roamoper) "|" roamoper.fileseq "|" liFileSeq skip.
roamoper.fileseq = liFileSeq.

disp i.
