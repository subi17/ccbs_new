{Syst/commpaa.i}
katun  = "talammin".
gcBrand = "1".
{Syst/tmsconst.i}
{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
      {Func/lib/eventlog.i}
END.

IF llDoEvent THEN DO:
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.


def var lii as int no-undo.

for each order exclusive-lock use-index stamp where brand = "1" and crstamp > 20161130 and deliverysecure = 1 and deliverytype = 6:

   order.deliverysecure = 2.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

   lii = lii + 1.
end.

fCleanEventObjects().

message lii view-as alert-box.
