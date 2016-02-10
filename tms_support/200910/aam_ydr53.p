{Syst/testpaa.i}
katun = "ari".

&GLOBAL-DEFINE STAR_EVENT_USER katun

{Func/lib/eventlog.i}

DEFINE VARIABLE lhBillItem AS HANDLE NO-UNDO.
lhBillItem = BUFFER BillItem:HANDLE.
RUN StarEventInitialize(lhBillItem).


for each billitem where
         billitem.accnum = 70518109:

      disp billcode biname .
     
      RUN StarEventSetOldBuffer(lhBillItem).
      billitem.costcentre = "SL".
      RUN StarEventMakeModifyEvent(lhBillItem).
end.

fcleaneventobjects().

