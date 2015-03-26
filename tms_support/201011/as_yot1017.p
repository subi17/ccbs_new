DEFINE TEMP-TABLE ttErrorCode
   FIELD errorcode AS INT
   FIELD qty AS INT
   FIELD revenue LIKE prepcdr.charge
INDEX errorcode IS PRIMARY UNIQUE errorcode. 

output to as_yot1017.log. 

FOR EACH prepcdr NO-LOCK where
   prepcdr.datest >= 11/8/2010 and
   prepcdr.datest <= 11/14/2010 and
   prepcdr.errorcode ne 0 use-index Date:

   find first ttErrorCode where
              ttErrorCode.errorcode = prepcdr.errorcode
   EXCLUSIVE-LOCK no-error.
   IF NOT AVAIL ttErrorCode then do:
      create ttErrorCode.
      assign
         ttErrorCode.errorcode = prepcdr.errorcode.
   end.
   assign
      ttErrorCode.qty = ttErrorCode.qty + 1
      ttErrorCode.revenue = ttErrorCode.revenue + prepcdr.charge.
end.

FOR EACH ttErrorCode NO-LOCK:
   disp ttErrorCode.
end.
