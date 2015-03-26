
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 08.01.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}

def stream sin.
input stream sin from /home/anttis/virhe_solog_mod.txt.
def var lcLine as char no-undo.

output to /home/anttis/reqstatus_to_6.txt.

repeat with frame a:
   
   import stream sin unformatted lcLine.
   find mobsub where mobsub.cli = entry(1, lcLine, " ") no-lock no-error.
   
   FIND FIRST msrequest where
      msrequest.msseq   = mobsub.msseq and
      msrequest.reqtype = 0 and
      msrequest.reqstatus = 3 and
      msrequest.actstamp >= 20090101 use-index msseq NO-LOCK NO-ERROR.

    IF NOT AVAIL msrequest then do:
      put unformatted mobsub.cli " ERROR: request not found" skip.
      next.
    END.
  
   fReqstatus(6,"").

      put unformatted mobsub.cli " OK" skip.
/* 
   find solog where solog.solog = int(entry(2,lcLine,"|")) no-lock no-error.
/*  if index(solog.response, "error") > 0 then */
   put unformatted entry(1, lcLine, "|") format "x(12)" solog.solog solog.stat solog.response skip.
*/

END.
input stream sin close.
