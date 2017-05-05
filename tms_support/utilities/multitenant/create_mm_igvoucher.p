DEFINE VARIABLE lcTZ AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE lcType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPrefix AS CHARACTER NO-UNDO. 

FOR EACH invgroup NO-LOCK:

   FIND FIRST igvoucher NO-LOCK where
              igvoucher.brand = invgroup.brand and
              igvoucher.invgroup = invgroup.invgroup no-error.

   IF AVAIL igvoucher then next. 
   disp invgroup.invgroup avail(igvoucher).


   do i = 0 to 12:

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

      if lookup(string(i),"0,1,2,3,4,5,8") > 0 then lcType = "P".
      else if lookup(string(i),"6,7,9,10") > 0 then lcType = "A".
      else if lookup(string(i),"11") > 0 then lcType = "UA".
      else if lookup(string(i),"12") > 0 then lcType = "MA".
      else MESSAGE invgroup.invgroup i VIEW-AS ALERT-BOX. 

      if i < 10 then lcType = string(i) + lcType.
      else if i eq 10 then lcType = "0" + lcType.

      lcPrefix = lcTZ + "17" + lcType. 

      create igvoucher.
      assign
         igvoucher.Brand   = "1"
         igvoucher.FromDate = 1/1/2017
         igvoucher.InvGroup = InvGroup.InvGroup
         igvoucher.PaymType  = i
         igvoucher.SeqPrefix = lcPrefix
         igvoucher.voucher = 0.

   end.

end.
