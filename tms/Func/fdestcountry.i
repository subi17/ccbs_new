/* fdestcountry.i    30.11.09/aam 
*/

{Func/transname.i}

&IF "{&DestCountry}" NE "YES" 
&THEN
&GLOBAL-DEFINE DestCountry YES

FUNCTION fGetDestCountry RETURNS CHARACTER
   (icValue AS CHARACTER):

   DEFINE VARIABLE lii AS INTEGER NO-UNDO.

   DO lii = MIN(4,LENGTH(icValue)) TO 1 BY -1:
      FIND FIRST PLMN NO-LOCK WHERE
         PLMN.CountryPrefix = SUBSTRING(icValue,1,lii)
      NO-ERROR.

      IF AVAILABLE PLMN
      THEN RETURN PLMN.Country.
   END.

   RETURN "".

END FUNCTION.

FUNCTION fDestCountry RETURNS CHAR
   (icBrand     AS CHAR,
    iiCallCase  AS INT,
    idaCallDate AS DATE,
    iiDtlSeq    AS INT,
    icBNumber   AS CHAR,
    iiBType     AS INT,
    icMSCID     AS CHAR,
    icServiceName AS CHAR):

   DEF VAR lcDestCountry AS CHAR NO-UNDO.
   DEF VAR lcNetwork     AS CHAR NO-UNDO.
   DEF VAR lcMSRN        AS CHAR NO-UNDO.
   
   /* for voice MT use network owner or MSRN */
   IF iiCallCase = 7 THEN DO:
      /* in TAP format we get network owner, in POST not, so use MSRN there */
      IF LOOKUP(icMSCID,"TAP3,NRTRDE") > 0 THEN DO:
         IF icServiceName > ""
         THEN lcNetwork = icServiceName.
         ELSE RUN Mm/cdr_detail_value.p("MobCDR",
                                     idaCallDate,
                                     iiDtlSeq,
                                     "Network owner",
                                     OUTPUT lcNetwork).
         IF lcNetwork > ""  THEN DO:
            FIND FIRST PLMN WHERE PLMN.PLMN = lcNetwork NO-LOCK NO-ERROR.
            IF AVAILABLE PLMN THEN lcDestCountry = PLMN.Country. 
         END.
      END.
       
      ELSE DO:
         IF icServiceName > ""
         THEN lcMSRN = icServiceName.
         ELSE RUN Mm/cdr_detail_value.p("MobCDR",
                                     idaCallDate,
                                     iiDtlSeq,
                                     "MSRN",
                                     OUTPUT lcMSRN).

         IF lcMSRN > ""
         THEN lcDestCountry = fGetDestCountry(LEFT-TRIM(lcMSRN,"0")).
      END.
   END.

   ELSE DO:
      IF iiBType = 1 /* international */
      THEN lcDestCountry = fGetDestCountry(icBNumber).
      ELSE lcDestCountry = "ES". /* national */
   END.

   RETURN lcDestCountry.

END FUNCTION.

FUNCTION fDestCountryName RETURNS CHAR
   (icBrand     AS CHAR,
    iiLanguage  AS INT,
    iiCallCase  AS INT,
    idaCallDate AS DATE,
    iiDtlSeq    AS INT,
    icBNumber   AS CHAR,
    iiBType     AS INT,
    icMSCID     AS CHAR,
    icServiceName AS CHAR):

   DEF VAR lcCountry AS CHAR NO-UNDO.
   
   lcCountry = fDestCountry(icBrand,
                            iiCallCase,
                            idaCallDate,
                            iiDtlSeq,
                            icBNumber,
                            iiBType,
                            icMSCID,
                            icServiceName).
                            
   IF lcCountry = "" THEN RETURN "".
   
   ELSE RETURN fGetItemName(icBrand,
                            "country",
                            lcCountry,
                            iiLanguage,
                            idaCallDate).
   
END FUNCTION.

&ENDIF
