/* fdestcountry.i    30.11.09/aam 
*/

{Func/transname.i}

&IF "{&DestCountry}" NE "YES" 
&THEN
&GLOBAL-DEFINE DestCountry YES

FUNCTION fDestCountry RETURNS CHAR
   (icBrand     AS CHAR,
    iiCallCase  AS INT,
    idaCallDate AS DATE,
    iiDtlSeq    AS INT,
    icBNumber   AS CHAR,
    iiBType     AS INT,
    icMSCID     AS CHAR):

   DEF VAR lcDestCountry AS CHAR NO-UNDO.
   DEF VAR liBPos        AS INT  NO-UNDO.
   DEF VAR lcNetwork     AS CHAR NO-UNDO.
   DEF VAR lcMSRN        AS CHAR NO-UNDO.
   
   /* for voice MT use network owner or MSRN */
   IF iiCallCase = 7 THEN DO:

      /* in TAP format we get network owner, in POST not, so use MSRN there */
      IF LOOKUP(icMSCID,"TAP3,NRTRDE") > 0 THEN DO:
         RUN Mm/cdr_detail_value.p("MobCDR",
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
         RUN Mm/cdr_detail_value.p("MobCDR",
                                idaCallDate,
                                iiDtlSeq,
                                "MSRN",
                                OUTPUT lcMSRN).
 
         IF lcMSRN > ""  THEN DO:
            lcMSRN = LEFT-TRIM(lcMSRN,"0").
            
            DO liBPos = MIN(4,LENGTH(lcMSRN)) TO 1 BY -1:
               FIND FIRST PLMN WHERE
                          PLMN.CountryPrefix = SUBSTRING(lcMSRN,1,liBPos)
               NO-LOCK NO-ERROR.
               IF AVAILABLE PLMN THEN DO:
                  lcDestCountry = PLMN.Country.
                  LEAVE.
               END.
            END.
         END.
         
      END.   
   END.

   ELSE DO:
      /* international */
      IF iiBType = 1 THEN DO:
         DO liBPos = MIN(4,LENGTH(icBNumber)) TO 1 BY -1:
            FIND FIRST PLMN WHERE 
                       PLMN.CountryPrefix = SUBSTRING(icBNumber,1,liBPos)
               NO-LOCK NO-ERROR.
            IF AVAILABLE PLMN THEN DO:
               lcDestCountry = PLMN.Country. 
               LEAVE.
            END.
         END.
      END.
      /* national */
      ELSE lcDestCountry = "ES".
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
    icMSCID     AS CHAR):

   DEF VAR lcCountry AS CHAR NO-UNDO.
   
   lcCountry = fDestCountry(icBrand,
                            iiCallCase,
                            idaCallDate,
                            iiDtlSeq,
                            icBNumber,
                            iiBType,
                            icMSCID).
                            
   IF lcCountry = "" THEN RETURN "".
   
   ELSE RETURN fGetItemName(icBrand,
                            "country",
                            lcCountry,
                            iiLanguage,
                            idaCallDate).
   
END FUNCTION.

&ENDIF
