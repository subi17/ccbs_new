{Syst/commpaa.i}
gcbrand = "1".
{Func/q25functions.i}
DEF VAR lcLogtext AS CHAR NO-UNDO.

FOR EACH SingleFee USE-INDEX BillCode WHERE
               SingleFee.Brand       = "1" AND
               SingleFee.Billcode BEGINS "RVTERM" AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.SourceTable = "DCCLI" AND
               SingleFee.CalcObj     = "RVTERM" AND
               SingleFee.BillPeriod  = 201609 NO-LOCK:
            IF SingleFee.OrderId <= 0 THEN NEXT.

      IF NOT fisQ25ExtensionAllowed(BUFFER SingleFee,
                                   lcLogText) THEN NEXT.

    DISP Singlefee.orderid.

END.
