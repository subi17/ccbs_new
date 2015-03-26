{timestamp.i}

def stream slog.

def var ldafromdate as date no-undo.
def var ldatodate  as date no-undo.
def var lclogfile  as char no-undo.
def var lcclitype  as char no-undo.
def var lcbillcode as char no-undo.
def var liqty      as int  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var lcevent    as char no-undo.
def var liperiodfrom as int  no-undo.
def var liperiodto   as int  no-undo.
def var lhmob     as handle no-undo.
def var lhterm    as handle no-undo.
def var lipicked  as int  no-undo.
def var llfound   as log  no-undo.
def var ldperiodfrom as dec no-undo.
def var ldperiodto   as dec no-undo.

assign
   lclogfile = "/tmp/find_clitype_billitem.txt"
   liqty     = 9999999
   ldafromdate = date(month(today),1,year(today)).
if month(today) = 12 then ldatodate = date(12,31,year(today)).
else ldatodate = date(month(today) + 1,1,year(today)) - 1.


pause 0.
update ldafromdate label "Period From"  colon 15 format "99-99-99"
       ldatodate   label "Period To"    colon 15 format "99-99-99"
       lcclitype   label "CLI Type"     colon 15 format "x(16)"
       lcbillcode  label "Billing Item" colon 15 format "x(16)"
       liqty       label "Pick"         colon 15 format ">>>>>>9" skip(1)
       lclogfile   label "Result File"  colon 15 format "x(40)"
with overlay side-labels row 10 centered title " FIND SUBSCR.TYPE/BILLITEM "
     frame ffind.

hide frame ffind no-pause.
     
if ldafromdate = ? or ldatodate = ? or lclogfile = "" or lcclitype = "" or
   lcbillcode = "" then return.

assign
   liperiodfrom = year(ldafromdate) * 100 + month(ldafromdate)
   liperiodto   = year(ldatodate) * 100 + month(ldatodate)
   ldperiodfrom = fmake2dt(ldafromdate,0)
   ldperiodto   = fmake2dt(ldatodate,86399)
   lhmob        = buffer mobsub:handle
   lhterm       = buffer termmobsub:handle.

   
output stream slog to value(lclogfile).

put stream slog unformatted
   "Cust"     chr(9)
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "Active"   chr(9)
   "Subscr. Type" chr(9)
   "Billing Item" chr(9)
   "Event"    skip.

for each mobsub no-lock where
         mobsub.brand = "1",
   first msowner no-lock where
         msowner.msseq = mobsub.msseq and
         msowner.tsend >= ldperiodfrom and
         msowner.tsbeg <= ldperiodto and
         msowner.clitype = lcclitype:
         
   i = i + 1.

   pause 0.
   disp i label "Checked" 
        j label "Found" 
   with overlay row 8 centered frame fmobqty title " Active ".

   run pfindevent(lhmob,
                  output llfound).
          
   if llfound then do:
      j = j + 1.
      lipicked = lipicked + 1.
   end.
    
   if lipicked >= liqty then leave. 
end.

i = 0.
j = 0.

if lipicked < liqty then 
for each termmobsub no-lock where
         termmobsub.brand = "1",
   first msowner no-lock where
         msowner.msseq = termmobsub.msseq and
         msowner.tsend >= ldperiodfrom and
         msowner.tsbeg <= ldperiodto and
         msowner.clitype = lcclitype:
          
   i = i + 1.

   pause 0.
   disp i label "Checked" 
        j label "Found" 
   with overlay row 14 centered frame ftermqty title " Terminated ".

   run pfindevent(lhterm,
                  output llfound).
          
   if llfound then do:
      j = j + 1.
      lipicked = lipicked + 1.
   end.
    
   if lipicked >= liqty then leave. 
end.

hide frame fmobqty no-pause.
hide frame ftermqty no-pause.

output stream slog close.


procedure pfindevent:

   def input parameter ihsubs as handle no-undo.
   def output parameter olfound as log no-undo.

   assign
      lcevent = ""
      olfound = false.
       
   for each fixedfee no-lock where
            fixedfee.brand = "1" and
            fixedfee.hosttable = "mobsub" and
            fixedfee.keyvalue = string(ihsubs::msseq) and
            fixedfee.billcode = lcbillcode,
      first ffitem of fixedfee no-lock where
            ffitem.billper >= liperiodfrom and
            ffitem.billper <= liperiodto:
      lcevent = "Fixed Fee".
      leave.
   end.

   if lcevent = "" then 
   for first singlefee no-lock where
             singlefee.brand = "1" and
             singlefee.hosttable = "mobsub" and
             singlefee.keyvalue = string(ihsubs::msseq) and
             singlefee.billcode = lcbillcode and
             singlefee.billper >= liperiodfrom and
             singlefee.billper <= liperiodto:
      lcevent = "Single Fee".       
   end.

   if lcevent = "" then 
   for each mobcdr no-lock use-index cli where
            mobcdr.cli = ihsubs::cli and
            mobcdr.datest >= ldafromdate and
            mobcdr.datest <= ldatodate and
            mobcdr.billcode = lcbillcode:
      lcevent = "CDR".
      leave.
   end.
        
   if lcevent = "" then next.

   put stream slog unformatted
      ihsubs::invcust   chr(9)
      ihsubs::cli       chr(9)
      ihsubs::msseq     chr(9)
      string((ihsubs:name = "mobsub"),"Yes/No") chr(9)
      ihsubs::clitype   chr(9)
      lcbillcode       chr(9)
      lcevent          skip.

   olfound = true.
   
end procedure.




