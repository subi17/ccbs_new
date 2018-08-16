{Syst/tmsconst.i}
DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 

FOR EACH Customer NO-LOCK WHERE 
         Customer.Brand EQ Syst.Var:gcBrand:   
   
   i = i + 1.
   if i mod 100 eq 0 THEN DO:
      disp i j with frame a.
      pause 0.
   END.

   IF CAN-FIND(FIRST Address WHERE
       Address.HostTable = "Customer" AND
       Address.Keyvalue = STRING(Customer.CustNum) AND
       Address.AddressType = {&ADDRESS_TYPE_BILLING}) THEN NEXT.

   FIND FIRST CustomerReport WHERE
              CustomerReport.Custnum = Customer.Custnum
   NO-LOCK NO-ERROR.

   CREATE Address.
   ASSIGN
      Address.AddressID = NEXT-VALUE(AddressID)
      Address.HostTable = "Customer"
      Address.KeyValue = STRING(Customer.CustNum)
      Address.AddressType = {&ADDRESS_TYPE_BILLING}
      Address.Address = Customer.Address
      Address.City = Customer.PostOffice
      Address.ZipCode = Customer.ZipCode
      Address.Region = Customer.Region
      Address.Country = Customer.Country
      Address.StreetCode = CustomerReport.StreetCode WHEN AVAIL CustomerReport
      Address.CityCode = CustomerReport.CityCode WHEN AVAIL CustomerReport
      Address.TownCode = CustomerReport.TownCode WHEN AVAIL CustomerReport.

   j = i + 1.
END.
