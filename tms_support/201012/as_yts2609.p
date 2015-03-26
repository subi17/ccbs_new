DEFINE TEMP-TABLE ttReport
   FIELD billcode AS char label "BillItem"
   FIELD i AS INT label "Count"
   FIELD a like Mobcdr.Amount label "Amount"
INDEX billcode IS PRIMARY UNIQUE billcode.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
FOR EACH mobcdr where
   mobcdr.datest = 12/5/2010 and
   mobcdr.errorcode = 0 and
   mobcdr.eventtype = "SMS" NO-LOCK use-index date:
      find ttReport where
           ttReport.billcode = mobcdr.billcode EXCLUSIVE-LOCK no-error.

      IF NOT AVAIL ttReport then do:
         create ttReport.
         assign ttReport.billcode = mobcdr.billcode.
      end.
      assign
        ttReport.i = ttReport.i + 1 
        ttReport.a = ttReport.a + mobcdr.amount. 
   i = i + 1.
   if i mod 10000 = 0 then disp i.
   pause 0.
END.

FOR EACH ttReport NO-LOCK:
   disp ttReport.
end.
