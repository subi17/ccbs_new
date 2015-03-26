/* ftaxdata.i      28.11.06/aam

   tax handling
   
   changes:        22.05.07/aam fTaxPerc

*/

&IF "{&fTaxData}" NE "YES" 
&THEN

&GLOBAL-DEFINE fTaxData YES

/* get tax% using taxzone and tax class */
FUNCTION fTaxPerc RETURNS DECIMAL
   (icTaxZone  AS CHAR,
    icTaxClass AS CHAR, /* 1=usage, 2=terminals */
    idaDate    AS DATE):

   /* tax% using both taxzone and taxclass */
   FIND FIRST VatCode WHERE
              VatCode.TaxClass = icTaxClass AND
              VatCode.TaxZone  = icTaxZone AND
              VatCode.Todate   >= idaDate AND
              VatCode.FromDate <= idaDate NO-LOCK NO-ERROR.
   IF AVAILABLE VatCode THEN RETURN VatCode.VatPerc.

   ELSE RETURN 0.0.
   
END FUNCTION.


/* get taxzone using region */
FUNCTION fRegionTaxZone RETURNS CHARACTER
   (icRegion AS CHAR):

   /* taxzone from region */
   FIND Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
   IF AVAILABLE Region THEN RETURN Region.TaxZone.

   /* if region is unknown then use zone 1 */
   ELSE RETURN "1".
    
END FUNCTION.

/* get taxcode using region and tax class */
FUNCTION fRegionTaxCode RETURNS INTEGER
   (icRegion   AS CHAR,
    icTaxClass AS CHAR, /* 1=usage, 2=terminals */
    idaDate AS DATE):    

   DEF VAR lcTaxZone AS CHAR NO-UNDO.

   /* taxzone from region */
   lcTaxZone = fRegionTaxZone(icRegion).
   
   /* actual taxcode using both taxzone and taxclass */
   FIND FIRST VatCode WHERE
              VatCode.TaxClass = icTaxClass AND
              VatCode.TaxZone  = lcTaxZone AND
              VatCode.ToDate   >= idaDate AND
              VatCode.FromDate <= idaDate NO-LOCK NO-ERROR.
   IF AVAILABLE VatCode THEN RETURN VatCode.VatCode.

   ELSE RETURN 0.
   
END FUNCTION.

/* get tax% using region and tax class */
FUNCTION fRegionTaxPerc RETURNS DECIMAL
   (icRegion   AS CHAR,
    icTaxClass AS CHAR, /* 1=usage, 2=terminals */
    idaDate    AS DATE):

   DEF VAR lcTaxZone AS CHAR NO-UNDO.
   
   /* taxzone from region */
   lcTaxZone = fRegionTaxZone(icRegion).
   
   /* actual tax% using both taxzone and taxclass */
   RETURN fTaxPerc(lcTaxZone,
                   icTaxClass,
                   idaDate).
   
END FUNCTION.

/* get taxzone using postcode */
FUNCTION fZipCodeTaxZone RETURNS CHARACTER
  (icZipCode AS CHARACTER):

   DEFINE VARIABLE lcTaxZone AS CHARACTER NO-UNDO.
   
   FIND FIRST PostCode WHERE
              PostCode.Country = "ES" AND
              PostCode.ZipCode = icZipCode
   NO-LOCK NO-ERROR.

   IF AVAIL PostCode 
   THEN lcTaxZone = fRegionTaxZone(PostCode.Region).
   ELSE lcTaxZone = fRegionTaxZone(SUBSTRING(icZipCode,1,2)).

   RETURN lcTaxZone.
   
END FUNCTION.

/* get tax% using zipcode and tax class */ 
FUNCTION fZipCodeTaxPerc RETURNS DECIMAL
   (icZipCode  AS CHAR,
    icTaxClass AS CHAR, /* 1=usage, 2=terminals */
    idaDate    AS DATE):

   DEF VAR lcRegion AS CHAR NO-UNDO.
      
   FIND FIRST PostCode WHERE
              PostCode.Country = "ES" AND 
              PostCode.ZipCode = icZipCode NO-LOCK NO-ERROR.
   IF AVAILABLE PostCode 
   THEN lcRegion = PostCode.Region.
   ELSE lcRegion = SUBSTRING(icZipCode,1,2).
   
   /* tax% from region */
   RETURN fRegionTaxPerc(lcRegion,
                         icTaxClass,
                         idaDate).
   
END FUNCTION.

&ENDIF

