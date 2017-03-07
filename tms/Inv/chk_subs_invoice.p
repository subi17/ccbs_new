define input parameter icOutputFile as char no-undo.
def input param ldtinv  as date no-undo.
def input param ldtfrom as date no-undo.
def input param ldtto   as date no-undo.

DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST chk_subs_invoice.p" skip(1).

{Func/timestamp.i}
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

def var lisubs as int no-undo.
def var libill as int no-undo.
def var lideny as int no-undo.
def var linew  as int no-undo.
def var lilist as int no-undo.
def var literm as int no-undo.
def var lldeny as log no-undo.
def var llbill as log no-undo.
def var llnew  as log no-undo.
def var llend  as log no-undo.

def var ldfirst as dec  no-undo.
def var ldend   as dec  no-undo.

def var ldperiodfrom as dec no-undo.
def var ldperiodto   as dec no-undo.
def var liqty        as int no-undo.

if ldtfrom = ? or ldtto = ? then return.

assign
   ldperiodfrom  = fmake2dt(ldtfrom,0)
   ldperiodto    = fmake2dt(ldtto,86399).
   
if ldtinv = ? then do:
   if month(ldtto) = 12 
   then ldtinv = date(1,1,year(ldtto) + 1).
   else ldtinv = date(month(ldtto) + 1,1,year(ldtto)).
end.

def buffer bowner for msowner.


def temp-table ttseq no-undo
   field msseq as int
   field cli   as char
   index msseq msseq
   index cli cli.

def temp-table ttinv no-undo
   field invnum as int
   field subinvnum as int
   index invnum invnum subinvnum.

def stream slog.
output stream slog to /tmp/subs_unclear.log.   

pause 0.
disp lisubs column-label "Subscr.Qty"
     lideny column-label "Denied"
     libill column-label "Billed"
     linew  column-label "New"
     literm column-label "Terminated on 1."
     lilist column-label "Unclear"
with 1 down frame fstat 
   title " Active subscriptions on period " +   
         string(ldtfrom,"99-99-99") + " - " + string(ldtto,"99-99-99") + " ".

for each msowner no-lock where 
         msowner.brand = "1" and
         msowner.tsbeg <= ldperiodto  and
         msowner.tsend >= ldperiodfrom and
         msowner.paytype = false and
         msowner.cli > ""
         j = 1 to 500:

   if can-find(first ttseq where ttseq.msseq = msowner.msseq and
                                 ttseq.cli   = msowner.cli)
   then next.
   
   lisubs = lisubs + 1.
   
   assign 
      lldeny  = false
      llbill  = false
      llnew   = false
      llend   = false
      ldfirst = msowner.tsbeg
      ldend   = msowner.tsend.

   for each subinvoice no-lock where
            subinvoice.msseq = msowner.msseq and
            subinvoice.cli   = msowner.cli,
      first invoice of subinvoice no-lock where 
             invoice.invtype = 1 and
             invoice.invdate = ldtinv:
      assign 
         llbill = true
         libill = libill + 1.
         
      create ttinv.
      assign 
         ttinv.invnum = invoice.invnum
         ttinv.subinvnum = subinvoice.subinvnum.
   end.   
 
   if not llbill then 
   for first actionlog no-lock use-index tablename where
             actionlog.brand     = "1" and
             actionlog.tablename = "mobsub" and
             actionlog.keyvalue  = string(msowner.msseq) and
             actionlog.actionid  = "denybill" and
             actionlog.actionstatus = 0 and 
             actionlog.fromdate <= ldtinv and
             actionlog.todate   >= ldtinv:
      assign         
         lldeny = true
         lideny = lideny + 1.
   end.

   if not lldeny and not llbill and msowner.tsbeg > ldperiodfrom then do:

      for each bowner no-lock where
               bowner.msseq = msowner.msseq and
               bowner.paytype = false:
         ldfirst = min(ldfirst,bowner.tsbeg).
      end.   
    
      if ldfirst > ldperiodfrom then do:
         llnew = true.
        
         for each invseq no-lock where
                  invseq.msseq = msowner.msseq and
                  invseq.fromdate <= ldtto and
                  invseq.billed = false,
            first mobcdr no-lock where
                  mobcdr.invcust = invseq.custnum and
                  mobcdr.invseq  = invseq.invseq and
                  round(mobcdr.amount,2) > 0 and
                  mobcdr.readints < ldperiodto:
                  
            llnew = false.
            leave.
         end.
        
         if llnew then 
            linew = linew + 1.
      end.
   end.
   
   /* terminated on first day */
   else if not lldeny and not llbill and  
       truncate(ldend,0) = truncate(ldperiodfrom,0) 
   then do:

      for each bowner no-lock where
               bowner.msseq = msowner.msseq and
               paytype = false:
         ldend = max(ldend,msowner.tsend).
      end.   
       
      if truncate(ldend,0) = truncate(ldperiodfrom,0) then do:

         llend = true.
         
         for each invseq no-lock where
                  invseq.msseq = msowner.msseq and
                  invseq.fromdate <= ldtto and
                  invseq.billed = false,
            first mobcdr no-lock where
                  mobcdr.invcust = invseq.custnum and
                  mobcdr.invseq  = invseq.invseq and
                  round(mobcdr.amount,2) > 0 and
                  mobcdr.readints < ldperiodto:
                  
            llend = false.
            leave.
         end.
        
         if llend then 
            literm = literm + 1.
      end.    
   end.
   
   if not lldeny and not llbill and not llnew and not llend then do:
      put stream slog unformatted
         msowner.cli     chr(9)
         msowner.msseq   chr(9)
         msowner.invcust skip.
      lilist = lilist + 1.
   end.
   
   if lisubs mod 1000 = 0 then do:
      pause 0.
      disp lisubs 
           lideny 
           libill 
           linew  
           literm 
           lilist 
      with frame fstat.
   end.
       
   create ttseq.
   assign
      ttseq.msseq = msowner.msseq
      ttseq.cli   = msowner.cli.

end.

disp stream sout lisubs 
     lideny 
     libill 
     linew  
     literm 
     lilist 
     with frame fstat.

for each invoice no-lock use-index invdate where
         invoice.brand   = "1" and 
         invoice.invdate = ldtinv and
         invoice.invtype = 1,
    each subinvoice of invoice no-lock:

   if not can-find(first ttinv where 
                         ttinv.invnum = invoice.invnum and
                         ttinv.subinvnum = subinvoice.subinvnum) then do:
      liqty = liqty + 1.
      if ok then ok = false.

      disp stream sout liqty format ">>>>>>9"
           invoice.invnum 
           subinvoice.subinvnum
           invoice.extinvid
           invoice.custnum 
           subinvoice.cli
           subinvoice.msseq column-label "MsSeq"
           subinvoice.invamt
           with frame fbills 
              title " Unclear invoices " + string(ldtinv,"99-99-99") + " "
              10 down.
   end.      
end.
 
if ok then put stream sout "OK". 
put stream sout unformatted skip(2).
output stream sout close.
