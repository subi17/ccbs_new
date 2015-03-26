for each msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 0 and
   reqstatus = 2 and
   actstamp > 20090101 NO-LOCK:

   IF msrequest.reqcparam1 begins "TARJ" THEN NEXT.
   IF msrequest.reqcparam2 begins "CONT" THEN NEXT.

   find dccli where 
      dccli.msseq = msrequest.msseq and
      dccli.dcevent = "TERM18II" NO-LOCK NO-ERROR.

   if avail dccli and dccli.termdate ne ? then do:
      find first singlefee where
         singlefee.brand = "1" and
         singlefee.billcode = "termperiod" and
         singlefee.custnum = msrequest.custnum and
         singlefee.hosttable = "mobsub" and
         singlefee.keyvalue = string(msrequest.msseq) NO-LOCK no-error.
      IF NOT AVAIL singlefee then do:
         disp msrequest.cli.
      end.
   end.
END.
