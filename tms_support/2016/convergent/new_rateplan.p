DEF VAR ldafrom AS DATE INIT 09/07/16.

/*FIND FIRST RatePlan WHERE rateplan.rateplan EQ "CONTRATOFUS".
DISP RatePlan.

FOR EACH PListConf WHERE Plistconf.rateplan EQ "CONTRATOFUS":
   DISP PListConf.
END.
*/
/*
CREATE RatePlan.
Assign
   Rateplan.Rateplan = "CONTRATOCONVF"
   RatePlan.RpName = "Contrato convergent (Post paid)"
   RatePlan.Brand = "1".

*/
CREATE RatePlan.
Assign
   Rateplan.Rateplan = "CONTRATOCONVS"
   RatePlan.RpName = "Contrato convergent (Post paid)"
   RatePlan.Brand = "1".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 5
   PListConf.rateplan = "CONTRATOCONVS"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON3".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 30
   PListConf.rateplan = "CONTRATOCONVS"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATOFIXED".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 50
   PListConf.rateplan = "CONTRATOCONVS"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATO".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 60
   PListConf.rateplan = "CONTRATOCONVS"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON".

/*
CREATE PListConf.
ASSIGN
   PListConf.dfrom = 08/01/16
   PlistConf.dto = 12/31/49
   PListConf.prior = 5
   PListConf.rateplan = "CONTRATOCONVF"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON3".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = 08/01/16
   PlistConf.dto = 12/31/49
   PListConf.prior = 30
   PListConf.rateplan = "CONTRATOCONVF"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATOFIXED".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = 08/01/16
   PlistConf.dto = 12/31/49
   PListConf.prior = 50
   PListConf.rateplan = "CONTRATOCONVF"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATO".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = 08/01/16
   PlistConf.dto = 12/31/49
   PListConf.prior = 60
   PListConf.rateplan = "CONTRATOCONVF"
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON".
*/
