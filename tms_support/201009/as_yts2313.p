find postcode where
   country eq "es" and
   zipcode = "00000" and
   region = "00" and
   postoffice = "GANDIA, DE (VER CALLEJERO DE GANDIA)" EXCLUSIVE-LOCK.

assign
   postcode.zipcode = "46701"
   postcode.region = "46".
release postcode.

find postcode where
   country eq "es" and
   zipcode = "00000" and
   region = "00" and
   postoffice = "GRANJILLA, LA (VER CALLEJERO DE SAN SEBASTIAN DE LOS REYES)" EXCLUSIVE-LOCK.

assign
   postcode.zipcode = "28702"
   postcode.region = "28".
release postcode.

find postcode where
   country eq "es" and
   zipcode = "00000" and
   region = "00" and
   postoffice = "TRUBIA, DE (CORVERA)(VER CALLEJERO DE AVILÉS)" EXCLUSIVE-LOCK.

assign
   postcode.zipcode = "33404"
   postcode.region  = "33".
release postcode.
