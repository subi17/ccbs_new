
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: ansavola 
  CREATED ......: 24.08.17
  CHANGED ......:
  Version ......: ccbs
----------------------------------------------------------------------- */
input from yts_11346.input.

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcNumber AS CHAR NO-UNDO. 
DEF VAR liMin AS INT NO-UNDO. 
DEF VAR liMax AS INT NO-UNDO. 

def stream sout.
output stream sout to yts_11346.log.

repeat trans:

   import unformatted lcLine.

   if trim(lcLine) eq "" then next.

   assign
      liMin = int(entry(1,lcLine," "))
      liMax = int(entry(2,lcLine," ")) 
      lcNumber = trim(entry(3,lcLine, " ")) no-error.

   if error-status:error then do:
      put stream sout unformatte
         lcLine ";ERROR syntax" skip.

      next.
   end.

   FIND  bdesttrans EXCLUSIVE-LOCK where
         bdesttrans.translatenumber = lcNumber and
         bdesttrans.ratingzone = "SHORT" no-error.
   IF NOT AVAIL bdesttrans then do:
      put stream sout unformatte
         lcLine ";ERROR not found" skip.

      next.
   end.

   if bdesttrans.minlength ne 0 or
      bdesttrans.maxlength ne 0 then do:
      put stream sout unformatte
         lcLine ";ERROR already set" skip.
   end.

   assign
      bdesttrans.minlength = liMin
      bdesttrans.maxlength = liMax.

   put stream sout unformatted
      lcLine ";OK" skip.
   
end.
