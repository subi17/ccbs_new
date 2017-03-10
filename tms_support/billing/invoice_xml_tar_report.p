{Func/timestamp.i}
def var lcsep as char no-undo.
def var lcfile as char no-undo.
def var ldainvdate as date no-undo.
def var ldfrom as dec no-undo.
def var ldto as dec no-undo.

form
   skip(1)
   ldainvdate format "99-99-9999" colon 12 
      label "Billing run"
      help "When FuncRun process was done"
   lcfile format "x(50)" colon 12
      label "File"
      help "File name"
   skip(1)   
   with overlay row 10 centered title " XML Tar Report "
      side-labels frame ftar.
      
pause 0.
update 
   ldainvdate
   lcfile with frame ftar.
hide frame ftar no-pause.
   
if ldainvdate = ? or lcfile = "" then return.
    
assign 
   ldfrom = fmake2dt(ldainvdate,0)
   ldto   = fmake2dt(ldainvdate,86399).
   
def stream slog.
output stream slog to value(lcfile).

lcsep = ";".

put stream slog unformatted
   "File"  lcsep
   "Size"  lcsep
   "XML Qty" lcsep
   "MD5"  skip.

for first funcrunqschedule no-lock where
          funcrunqschedule.frqueueid = 3 and
          funcrunqschedule.startts >= ldfrom and
          funcrunqschedule.startts <= ldto and
          funcrunqschedule.runstate ne "cancelled",
    first funcrunexec no-lock where
          funcrunexec.frqscheduleid = funcrunqschedule.frqscheduleid and
          funcrunexec.frconfigid = 6,
     each funcrunprocess no-lock where
          funcrunprocess.frexecid = funcrunexec.frexecid,
    each funcrunresult of funcrunprocess no-lock 
by funcrunprocess.frprocessid
by funcrunresult.frresultseq
by funcrunresult.resultorder:

   /*
   disp funcrunexec.frexecid
        funcrunprocess.frprocessid
        funcrunresult.frresultseq
        funcrunresult.resultorder
        funcrunresult.charparam format "x(40)".
   */

   put stream slog unformatted
      entry(1,funcrunresult.charparam," ") lcsep
      funcrunresult.decparam lcsep
      funcrunresult.intparam lcsep
      entry(2,entry(2,funcrunresult.charparam," "),":") skip.

end.

output stream slog close.

