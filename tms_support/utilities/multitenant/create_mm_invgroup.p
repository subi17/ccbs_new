DEFINE VARIABLE lcTZ AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE lcType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPrefix AS CHARACTER NO-UNDO. 

FOR EACH invgroup NO-LOCK:
   FIND FIRST iginvnum NO-LOCK where
         iginvnum.brand = invgroup.brand and
         iginvnum.invgroup = invgroup.invgroup no-error.
   IF AVAIL iginvnum then next. 
   disp invgroup.invgroup avail(iginvnum).


   do i = 1 to 99.
      if lookup(string(i),"1,5,6,7,8,9,99") = 0 then next.

      case invgroup.invgroup:
         when "VAT2" then lcTZ = "06".
         when "IGIC2" then lcTZ = "07".
         when "IPSIC2" then lcTZ = "08".
         when "IPSIM2" then lcTZ = "09".
         when "MASMOVIL" then lcTZ = "10".
         otherwise do:
            next.
         end.
      end.

      case i:
         when 1 or when 6 or when 7 then lcType = "D".
         when 5 or when 8 or when 9 then lcType = "C".
         when 99 then .
         otherwise do:
            MESSAGE invgroup.invgroup i VIEW-AS ALERT-BOX. 
            next.
         end.
      end.

      if i eq 99 then do:
         case invgroup.invgroup:
            when "VAT2" then lcPrefix = "MIVA".
            when "IGIC2" then lcPrefix = "MIGI".
            when "IPSIC2" then lcPrefix = "MIPC".
            when "IPSIM2" then lcPrefix = "MIPM".
            otherwise next.
         end.
      end.
      else lcPrefix = lcTZ + "17" + string(i) + lcType. 

      create iginvnum.
      assign
         iginvnum.Brand   = "1"
         iginvnum.FromDate = 1/1/2017
         iginvnum.InvGroup = InvGroup.InvGroup
         iginvnum.InvNum   = 0
         iginvnum.InvType  = i
         iginvnum.SeqPrefix = lcPrefix. 

   end.

end.

