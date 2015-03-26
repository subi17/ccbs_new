{commpaa.i}
katun = "anttis".
gcBrand = "1".
{fmakemsreq.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

FOR EACH msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 9 and
   msrequest.reqstatus = 2 and
   msrequest.actstamp = 20090318.00000 and
   msrequest.usercode = "anttis" NO-LOCK:
   find dccli where 
        dccli.msseq    = msrequest.msseq and
        dccli.dcevent  = msrequest.reqcparam3 and
        dccli.validto  = 3/18/2009 and
        dccli.termdate = 3/18/2009 EXCLUSIVE-LOCK.

   assign dccli.validto = 3/17/2009
          dccli.termdate = 3/17/2009.

   release dccli.
   i = i + 1.
END.

disp i.
