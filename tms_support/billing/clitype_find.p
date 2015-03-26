def var lcclitype as char no-undo.
def var ldaact    as date no-undo.
def var liqty     as int  no-undo.
def var lcfile    as char no-undo.
def var liccn     as int  no-undo.
def var limindur  as int  no-undo.
def var limindata as int  no-undo.
def var lisec     as int  no-undo.
def var lidata    as int64 no-undo.
def var i         as int  no-undo.
def var j         as int  no-undo.
def var llany     as log  no-undo.
def var ldafrom   as date no-undo.
def var ldato     as date no-undo.
def var llpick    as log  no-undo.

def stream slog.

pause 0.
update
   lcclitype format "x(12)"      label "Subsc.Type"
      help "Subscription type"
   ldaact    format "99-99-9999" label "Activated"
      help "Activation date" skip(1)
   liccn     format ">>>9"       label "Call Case" 
      help "Call case (spocmt), 0=not checked"
   ldafrom    format "99-99-9999" label "From"
      help "Events from"
   ldato   format "99-99-9999" label "To"
      help "Events to"
   limindur  format ">>>>>>>>>9" label "Min. Duration"
      help "Minimum duration (seconds), 0=not checked"
   limindata format ">>>>>>>>>9" label "Min. Data"
      help "Minimum data amount (Mb), 0=not checked" skip(1)
   liqty     format ">>>9"       label "Quantity"
      help "Quantity of cases to pick"
   lcfile    format "x(50)"      label "File"
      help "Output file"
with overlay side-labels 1 column row 4 centered title " FIND CLITYPE "
   frame fclitype.
hide frame fclitype no-pause.
   
if lcclitype = "" or ldaact = ? or liqty = 0 or lcfile = "" or
   (liccn > 0 and (ldafrom = ? or ldato = ?))
then return.

output stream slog to value(lcfile).

put stream slog unformatted
   "MSISDN"  chr(9)
   "Subscr.ID" chr(9)
   "Activated" chr(9)
   "Type" skip.
   
pause 0.
disp i format ">>>>>>9" label "Browse" 
     j format ">>>>>>9" label "Found"
with overlay row 10 centered side-labels frame fqty.
   
for each mobsub no-lock where
         mobsub.brand = "1" and
         mobsub.clitype = lcclitype and
         mobsub.activationdate >= ldaact:

   i = i + 1.
   pause 0.
   disp i j with frame fqty.

   if liccn > 0 then do:
      assign
         llpick = false
         lisec  = 0
         lidata = 0
         llany  = (limindur = 0 and limindata = 0).
    
      for each mobcdr no-lock use-index cli where
               mobcdr.cli = mobsub.cli and
               mobcdr.datest >= ldafrom and
               mobcdr.datest <= ldato and
               mobcdr.spocmt = liccn:

         /* one event is enough */
         if llany then do:
            llpick = true.
            leave.
         end.
         
         assign 
            lisec = lisec + mobcdr.billdur
            lidata = lidata + mobcdr.datain + mobcdr.dataout. 
      end.

      /* min limits should be exceeded */
      if not llany and not llpick then do:
         lidata = lidata / (1024 * 1024).
         
         if (limindur > 0 and lisec > limindur) or
            (limindata > 0 and lidata > limindata) then llpick = true.
      end.
      
      if not llpick then next.
   end.
      
   put stream slog unformatted
       mobsub.cli   chr(9)
       mobsub.msseq chr(9)
       mobsub.activationdate chr(9)
       mobsub.clitype skip.

   j = j + 1.
   if j >= liqty then leave. 
end.

hide frame fqty no-pause.

output stream slog close.
   
message j "subscriptions were found"
view-as alert-box title "DONE".


