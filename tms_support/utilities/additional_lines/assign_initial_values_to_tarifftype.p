/* ----------------------------------------------------------------------
  MODULE .......: assign_initial_values_to_tarifftype.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 13.03.17
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "ADDITIONAL_LINES".
gcBrand = "1".

DEF VAR lcConvergent AS CHAR NO-UNDO. 
DEF VAR lcFixedOnly  AS CHAR NO-UNDO. 
DEF VAR lcFusion     AS CHAR NO-UNDO. 

ASSIGN lcConvergent = "CONTDSL39,CONTDSL40,CONTDSL45,CONTDSL48,CONTDSL52,CONTDSL58,CONTDSL59,CONTFH39_50,CONTFH40_50,CONTFH45_50,CONTFH48_50,CONTFH49_300,CONTFH50_300,CONTFH52_50,CONTFH55_300,CONTFH58_300,CONTFH58_50,CONTFH59_50,CONTFH62_300,CONTFH68_300,CONTFH69_300"
       lcFixedOnly  = "CONTDSL35,CONTFH35_50,CONTFH45_300"
       lcFusion     = "CONTFF,CONTSF,CONTSF10,CONTSF14".

/* Adding new TariffType value to CLITytpe configurations, AND 
   for Convergent tariff's (3P) change LineType value from Entry Line to Main Line */

FOR EACH CLIType EXCLUSIVE-LOCK WHERE 
         CLIType.Brand = gcBrand:
   
   IF LOOKUP(CLIType.CLIType,lcConvergent) > 0 THEN  
      ASSIGN CLIType.TariffType = 1
             CLIType.LineType   = 1.
   ELSE IF LOOKUP(CLIType.CLIType,lcFixedOnly) > 0 THEN 
      CLIType.TariffType = 2.
   ELSE IF LOOKUP(CLIType.CLIType,lcFusion) > 0 THEN 
      CLIType.TariffType = 3.
   ELSE 
      CLIType.TariffType = 0. /* Mobile Only */

END.
