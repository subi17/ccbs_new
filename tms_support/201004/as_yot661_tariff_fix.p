def stream sroamtariff.
output stream sroamtariff to roamtariff.d.

def stream srtitem.
output stream srtitem to rtitem.d.

FOR EACH roamtariff where
   roamtariff.pricelist = "turtc" and
   validfrom = 4/9/2010 and
   validto = 4/12/2010 NO-LOCK:
   export stream sroamtariff roamtariff.
   FOR EACH rtitem where
            rtitem.tariffnum = roamtariff.tariffnum EXCLUSIVE-LOCK:
      export stream srtitem rtitem.
      rtitem.tariff = 0.0133333.
   end.
end.

FOR EACH roamtariff where
   roamtariff.pricelist = "turtc" and
   validfrom = 4/13/2010 NO-LOCK:
   export stream sroamtariff roamtariff.
   FOR EACH rtitem where
            rtitem.tariffnum = roamtariff.tariffnum NO-LOCK:
      export stream srtitem rtitem.
   end.
end.
