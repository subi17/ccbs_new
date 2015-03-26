def buffer m for msisdn.
FIND FIRST msisdn where brand = "1" and cli = "622201500" NO-LOCK NO-ERROR.
create m.
buffer-copy msisdn except validfrom validto to m. 
assign
   m.validfrom = msisdn.validto + 0.00001
   m.validto = 99999999.99999
   m.actiondate = today
   m.statuscode = 3.
