{Func/timestamp.i}

def var ldaeventdate as date no-undo.
def var lidelay as int no-undo.
def var lcoldtype as char no-undo.
def var lcnewtype as char no-undo.
def var ldactivated as dec extent 2 no-undo.
def var lddone as dec no-undo.
def var lcfile as char no-undo.
def var i as int no-undo.
def var ldtactivated as datetime no-undo.
def var ldtdone as datetime no-undo.

def stream slog.


pause 0.
update 
   ldaeventdate colon 20
      format "99-99-9999"
      label "Activation Date"
   lidelay colon 20
      format ">>9"
      label "Delay (hours)"
   lcoldtype colon 20
      format "x(20)"
      label "Old Subs.Type"
   lcnewtype colon 20
      format "x(20)"
      label "New Subs.Type" skip(1)
   lcfile colon 20
      format "x(50)"
      label "Output File" 
   with side-labels overlay row 10 centered title " Find STC Requests "
      frame fstc.
      
hide frame fstc.

if ldaeventdate = ? or lcfile = "" then return.

assign
   ldactivated[1] = year(ldaeventdate) * 10000 + 
                    month(ldaeventdate) * 100 + 
                    day(ldaeventdate)
   ldactivated[2] = ldactivated[1] + 0.86399.

output stream slog to value(lcfile).

put stream slog unformatted
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "Old Type" chr(9)
   "New Type" chr(9)
   "Activation" chr(9)
   "Done"       skip.
   
for each msrequest no-lock use-index reqtype where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstat = 2 and
         msrequest.actstamp >= ldactivated[1] and
         msrequest.actstamp <= ldactivated[2]:
         
   if lcoldtype > "" and msrequest.reqcparam1 ne lcoldtype then next.
   if lcnewtype > "" and msrequest.reqcparam2 ne lcnewtype then next.
  
   if lidelay > 0 then do:
      assign 
         ldtactivated = fTimeStamp2DateTime(msrequest.actstamp)  
         ldtdone      = fTimeStamp2DateTime(msrequest.donestamp).
         
      if interval(ldtdone,ldtactivated,"hours") < lidelay then next.
   end.  
   
   i = i + 1.
   
   pause 0.
   disp i label "Found" 
   with side-labels overlay row 10 centered frame fqty.
      
         
   put stream slog unformatted
      msrequest.cli   chr(9)
      msrequest.msseq chr(9)
      msrequest.reqcparam1 chr(9)
      msrequest.reqcparam2 chr(9)
      fts2hms(msrequest.actstamp)  chr(9)
      fts2hms(msrequest.donestamp)  skip.
end.

hide frame fqty no-pause.

output stream slog close.

message i "requests found"
view-as alert-box title "DONE".

