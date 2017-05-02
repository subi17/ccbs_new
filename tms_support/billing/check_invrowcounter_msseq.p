{Func/date.i}
{Func/istc.i}

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.
def var l as int no-undo. 
def var ldatodate as date no-undo.
def var ldafromdate as date no-undo.
def var liMsSeq as int no-undo.
def var llmatch as log no-undo.
def var llpause as log no-undo.
def var llistcchecked AS LOGICAL NO-UNDO.
def var ldaistcdate AS DATE NO-UNDO. 
def var ldaCounterToDate AS DATE NO-UNDO.
def var ldaMonth AS DATE NO-UNDO.

def temp-table ttcounter no-undo
   like invrowcounter.

assign 
  ldafromdate = date(month(today),1,year(today))
  ldatodate = flastdayofmonth(ldafromdate).

pause 0.
update liMsSeq format ">>>>>>>9" label "Subscr.ID" skip
       ldafromdate format "99-99-9999" label "Period"
       "-"
       ldatodate no-label format "99-99-9999" 
   with overlay row 10 centered title " CHECK COUNTER " 
        side-labels frame fCheck.
hide frame fCheck no-pause.
   
if liMsSeq = 0 then return.

for first msowner no-lock use-index MsSeq where          
          msowner.msseq = liMsSeq:
 
   pause 0.
   disp msowner.cli msowner.msseq.

   llpause = false.
   ldaMonth = ?.
   
   empty temp-table ttcounter.
         
   for each mobcdr no-lock use-index cli where
            mobcdr.cli = msowner.cli and
            mobcdr.datest >= ldafromdate and
            mobcdr.datest <= ldatodate and
            mobcdr.errorcode = 0 and
            mobcdr.msseq = msowner.msseq,
      first invseq no-lock where
            invseq.invseq = mobcdr.invseq:
   
      if month(mobcdr.datest) ne month(ldaMonth) THEN DO:
         ldaMonth = mobcdr.datest. 
         ldaISTCDate = fGetiSTCDate(mobcdr.MsSeq,
                                    mobcdr.Custnum,
                                    mobcdr.datest).
      end.

      if ldaISTCDate ne ? AND ldaISTCDate > mobcdr.datest THEN
         ldaCounterToDate = ldaISTCDate - 1.
      else ldaCounterToDate = ldaToDate.
                          
      find first ttcounter where 
              ttcounter.InvCust     = mobcdr.InvCust AND
              ttcounter.InvSeq      = mobcdr.InvSeq AND
              ttcounter.BillCode    = mobcdr.BillCode AND
              ttcounter.CCN         = mobcdr.CCN AND
              ttcounter.MsSeq       = mobcdr.MsSeq AND
              ttcounter.CLI         = mobcdr.CLI AND
              ttcounter.TariffNum   = mobcdr.TariffNum AND
              ttcounter.VatIncl     = mobcdr.VatIncl AND
              ttcounter.ReportingID = "," and
              ttcounter.DCEvent     = mobcdr.DCEvent AND
              ttcounter.todate      = ldaCounterToDate no-error.
      if not available ttcounter then do:
         create ttcounter.
         assign 
              ttcounter.InvCust     = mobcdr.InvCust
              ttcounter.InvSeq      = mobcdr.InvSeq 
              ttcounter.BillCode    = mobcdr.BillCode
              ttcounter.CCN         = mobcdr.CCN
              ttcounter.MsSeq       = mobcdr.MsSeq
              ttcounter.CLI         = mobcdr.CLI
              ttcounter.TariffNum   = mobcdr.TariffNum
              ttcounter.VatIncl     = mobcdr.VatIncl
              ttcounter.ReportingID = "," 
              ttcounter.DCEvent     = mobcdr.DCEvent
              ttcounter.todate      = ldaCounterToDate.
      end.

      assign
         ttcounter.Quantity = ttcounter.Quantity + 1
         ttcounter.Duration = ttcounter.Duration + mobcdr.BillDur
         ttcounter.Amount   = ttcounter.Amount + mobcdr.Amount 
         ttcounter.DataAmt  = ttcounter.DataAmt + 
                                 mobcdr.DataIn + mobcdr.DataOut
         ttcounter.RefPrice = ttcounter.RefPrice + mobcdr.RefPrice.
   end.
   
   llmatch = true.
   
   for each ttcounter
   by ttcounter.billcode
   by ttcounter.ccn:
   
      llmatch = true.
      j = j + 1.
       
      find first invrowcounter where 
              invrowcounter.InvCust     = ttcounter.InvCust AND
              invrowcounter.InvSeq      = ttcounter.InvSeq AND
              invrowcounter.BillCode    = ttcounter.BillCode AND
              invrowcounter.CCN         = ttcounter.CCN AND
              invrowcounter.MsSeq       = ttcounter.MsSeq AND
              invrowcounter.CLI         = ttcounter.CLI AND
              invrowcounter.TariffNum   = ttcounter.TariffNum AND
              invrowcounter.VatIncl     = ttcounter.VatIncl AND
              invrowcounter.ReportingID = "," and
              invrowcounter.DCEvent     = ttcounter.DCEvent AND
              invrowcounter.todate      = ttcounter.todate no-lock no-error.
      if not available invrowcounter then llmatch = false.
      
      else if 
         invrowcounter.quantity ne ttcounter.quantity or
         invrowcounter.duration ne ttcounter.duration or
         invrowcounter.amount ne ttcounter.amount or
         invrowcounter.dataamt ne ttcounter.dataamt then llmatch = false.
       
      if llmatch then next.
  
      disp ttcounter.billcode format "x(10)"
           ttcounter.ccn
           ttcounter.tariffnum format ">>>>9"
           ttcounter.vatincl
           ttcounter.quantity  format ">>>>>9"
           ttcounter.duration
           ttcounter.amount
           ttcounter.dataamt 
           ttcounter.invseq with 4 down.
      if not available invrowcounter then disp "no irc".
   
      if available invrowcounter then 
        disp invrowcounter.billcode format "x(10)" column-label "ir bitem"
             invrowcounter.ccn
             invrowcounter.tariffnum format ">>>>9"
             invrowcounter.vatincl
             invrowcounter.quantity  format ">>>>>9"
             invrowcounter.duration
             invrowcounter.amount
             invrowcounter.dataamt 
             invrowcounter.fromdate
             invrowcounter.todate 
             with 4 down.
             
             
      llpause = true.
      k = k + 1.
      /*
      for each mobcdr no-lock use-index cli where
            mobcdr.cli = msowner.cli and
            mobcdr.datest >= ldafromdate and
            mobcdr.datest <= ldatodate and
            mobcdr.billcode = ttcounter.billcode and
            mobcdr.ccn = ttcounter.ccn,
      first invseq no-lock where
            invseq.invseq = mobcdr.invseq /* and
            invseq.billed = false */:

           disp string(mobcdr.timest,"hh:mm:ss")
                readints
                mobcdr.ccn
                mobcdr.tariffnum
                mobcdr.errorcode
                mobcdr.billdur
                mobcdr.amount
                mobcdr.invseq.
      end.
      */
   end.        

   for each invrowcounter no-lock where
            invrowcounter.msseq = msowner.msseq and
            invrowcounter.todate = ldatodate and
            invrowcounter.invseq > 0 and
            invrowcounter.cli = msowner.cli and
            invrowcounter.quantity ne 0:
            
      find first ttcounter where 
              ttcounter.InvCust     = invrowcounter.InvCust AND
              ttcounter.InvSeq      = invrowcounter.InvSeq AND
              ttcounter.BillCode    = invrowcounter.BillCode AND
              ttcounter.CCN         = invrowcounter.CCN AND
              ttcounter.MsSeq       = invrowcounter.MsSeq AND
              ttcounter.CLI         = invrowcounter.CLI AND
              ttcounter.TariffNum   = invrowcounter.TariffNum AND
              ttcounter.VatIncl     = invrowcounter.VatIncl AND
              ttcounter.ReportingID = "," and
              ttcounter.DCEvent     = invrowcounter.DCEvent AND
              ttcounter.todate      = invrowcounter.todate no-lock no-error.
      if not available ttcounter then do:
        disp invrowcounter.billcode format "x(10)"
             invrowcounter.ccn
             invrowcounter.tariffnum format ">>>>9"
             invrowcounter.vatincl
             invrowcounter.quantity  format "->>>>>9"
             invrowcounter.duration  format "->>>>>>9"
             invrowcounter.amount
             invrowcounter.dataamt
             invrowcounter.dcevent
             invrowcounter.invseq
             invrowcounter.invcust
             "no ttc" 
             can-find(first invseq where invseq.invseq = invrowcounter.invseq)
                format "/No invseq"
             with 5 down.
        llpause = true. 
        k = k + 1.
        /*
        for each mobcdr no-lock where
                 mobcdr.cli = msowner.cli and
                 mobcdr.datest >= ldafromdate and
                 mobcdr.datest <= ldatodate and
                 mobcdr.msseq = invrowcounter.msseq and
                 mobcdr.billcode = invrowcounter.billcode,
           first invseq no-lock where
                 invseq.invseq = mobcdr.invseq /* and
                 invseq.billed = false */:

           disp string(mobcdr.timest,"hh:mm:ss")
                readints
                mobcdr.ccn
                mobcdr.tariffnum
                mobcdr.errorcode
                mobcdr.billdur
                mobcdr.amount
                mobcdr.invseq
                mobcdr.dcevent.
        end.
        */
      end.  
   end.   

   if llpause then pause.
end.   

empty temp-table ttcounter.

if k = 0 then 
message "No differencies"
view-as alert-box.

      

