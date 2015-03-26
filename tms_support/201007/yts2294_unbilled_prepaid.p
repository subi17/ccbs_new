output to yts2294_unbilled_prepaid.txt.

FOR EACH mobcdr where
   mobcdr.datest  < 2/1/2010 and
   mobcdr.billcode begins "PRE" NO-LOCK:
   
   find invseq where
        invseq.invseq = mobcdr.invseq NO-LOCK.

   if invseq.billed then find invoice where
      invoice.invnum = invseq.invnum NO-LOCK.
      if avail invoice then 
      put unformatted invoice.extinvid "|" invoice.invdate "|" mobcdr.cli "|" mobcdr.datest "|" billcode "|" mobcdr.amount "|" errorcode "|" invseq.billed skip.
      else put unformatted "||" mobcdr.cli "|" mobcdr.datest "|" billcode "|" mobcdr.amount "|" errorcode "|" invseq.billed skip.
end. 

