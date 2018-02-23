/*
   RES-538 - Digital Signature
 
  Add data to DB Common, table Country, field CountryISO3 
  
  Run first in simulate mode and check possible errors in .log
*/

DEF STREAM sin.
INPUT STREAM sin FROM "res_538_country_codes.input".
DEF STREAM sout.
OUTPUT STREAM sout TO "res_538_country_codes.log".

DEF VAR lcline AS CHAR NO-UNDO. 
DEF VAR lcCountryName AS CHAR NO-UNDO. 
DEF VAR lcIsoCode AS CHAR NO-UNDO.
DEF VAR lcReport AS CHAR NO-UNDO.
DEF VAR llSimulate AS LOG NO-UNDO INIT FALSE.

/* Title for output log */
PUT STREAM sout UNFORMATTED
   "Country;ISOCode;Code" SKIP.

/* SIMULATION FLAG:
  FALSE = ACTION
  TRUE  = SIMULATION */
llSimulate = TRUE.

IMPORT STREAM sin UNFORMATTED lcline.  /* SKIP title from input file */

REPEAT:
   IMPORT STREAM sin UNFORMATTED lcline.
   ASSIGN
      lcReport = ""
      lcCountryName = ENTRY(1,lcline,";")
      lcIsoCode = ENTRY(2,lcline,";").

   FIND FIRST Country EXCLUSIVE-LOCK WHERE
      Country.CoName EQ lcCountryName USE-INDEX Country NO-ERROR.
   IF AVAIL Country THEN DO:
      IF NOT llSimulate THEN DO:
         ASSIGN
            Country.CountryISO3 = lcIsoCode.
      END.
      lcReport = lcCountryName + ";" + lcIsoCode + ";" + Country.Country.
   END.
   ELSE DO:
      lcReport = "****NOT FOUND Country name: " + lcCountryName.
      /* MESSAGE "Country name not found!!" lcCountryName VIEW-AS ALERT-BOX.*/
   END.
   /* Final report action */
   PUT STREAM sout UNFORMATTED
      lcReport SKIP.
END.

 /* apostrophe sign cannot handle. Make own script for
    COTE D'IVOIRE (IVORY COAST);CI  --> ISO-3 = CIV
 */

FIND FIRST Country EXCLUSIVE-LOCK WHERE
   Country.CoName BEGINS "COTE D".
IF AVAIL Country THEN DO:  
   IF NOT llSimulate THEN DO:
      ASSIGN
         Country.CountryISO3 = "CIV". /* COSTA DE MARFIL;CIV */
   END.
   MESSAGE "Added COTE D'IVOIRE = CIV!" VIEW-AS ALERT-BOX.
END.
OUTPUT STREAM sout CLOSE.
