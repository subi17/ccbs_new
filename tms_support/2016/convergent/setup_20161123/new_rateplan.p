DEF VAR ldafrom AS DATE INIT 11/01/16.
DEF VAR lcRatePlan AS CHAR NO-UNDO.

lcRatePlan = "CONTRATOCONVC".

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
   Rateplan.Rateplan = lcRatePlan
   RatePlan.RpName = "Contrato convergent (Post paid)"
   RatePlan.Brand = "1".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 5
   PListConf.rateplan = lcRatePlan
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON3".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 30
   PListConf.rateplan = lcRatePlan
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATOFIXED".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 20
   PListConf.rateplan = lcRatePlan
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "CONTRATO8".

CREATE PListConf.
ASSIGN
   PListConf.dfrom = ldaFrom
   PlistConf.dto = 12/31/49
   PListConf.prior = 60
   PListConf.rateplan = lcRatePlan
   PListConf.startcharge = TRUE
   PListConf.brand = "1"
   PListConf.pricelist = "COMMON".

