{timestamp.i}

def stream slog.

def var ldafromdate as date no-undo.
def var ldatodate   as date no-undo.
def var lclogfile  as char no-undo.
def var lcclitype  as char no-undo.
def var lcdcevent  as char no-undo.
def var liqty      as int  no-undo.
def var ldfromper  as dec  no-undo.
def var ldtoper    as dec  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var lctype     as char no-undo.
def var lccli      as char no-undo.
def var limsseq    as int  no-undo.
def var llactivation as log no-undo.
def var lireqtype  as int  no-undo.

assign
   lclogfile = "/tmp/find_pc_request.txt"
   liqty     = 50
   llactivation = true.

pause 0.
update ldafromdate  label "Activation Date" colon 18 format "99-99-99"
       "-"
       ldatodate no-label format "99-99-99" 
          validate(input ldatodate >= input ldafromdate,"Invalid dates") skip
       lcclitype    label "CLI Type"     colon 18 format "x(16)"
       lcdcevent    label "Per.Contract" colon 18 format "x(16)"
       llactivation label "Request Type" colon 18 
          format "Activation/Termination" help "(A)ctivation or (T)ermination"
       liqty        label "Pick"         colon 18 format ">>>>>>9" skip(1)
       lclogfile    label "Result File"  colon 18 format "x(40)"
with overlay side-labels row 10 centered title " FIND PC REQUESTS "
     frame ffind.

hide frame ffind no-pause.
     
if ldafromdate = ? or ldatodate = ? or lclogfile = "" then return.

assign
   lireqtype = if llactivation then 8 else 9
   ldfromper = fmake2dt(ldafromdate,0)
   ldtoper   = fmake2dt(ldatodate,86399).

output stream slog to value(lclogfile).

put stream slog unformatted
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "CLI Type" chr(9)
   "Per.Contract" chr(9)
   "Type" chr(9)
   "Activation Time" skip.

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = lireqtype and
         msrequest.reqstat = 2 and
         msrequest.actstamp >= ldfromper and
         msrequest.actstamp <= ldtoper and
         (if lcdcevent > "" 
          then msrequest.reqcparam3 = lcdcevent
          else true),
   first msowner no-lock where
         msowner.msseq = msrequest.msseq and
         msowner.tsend >= msrequest.actstamp and
         msowner.tsbeg <= msrequest.actstamp and
         (if lcclitype > "" 
          then msowner.clitype = lcclitype
          else true):
         
    i = i + 1.

    if i < 100 or i mod 100 = 0 then do:
       pause 0.
       disp i label "Found" 
       with overlay row 10 centered side-labels frame fqty.
    end.

    put stream slog unformatted
       msrequest.cli    chr(9)
       msrequest.msseq  chr(9)
       msowner.clitype  chr(9)
       msrequest.reqcparam3 chr(9)
       msrequest.reqtype chr(9)
       fts2hms(msrequest.actstamp) skip.
          
    if i >= liqty then leave. 
end.

hide frame fqty no-pause.

output stream slog close.



   


