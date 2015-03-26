/* fotint.i     12.03.2002/aam
   get the overtime percent according TO customer's category

                09.09.03/aam brand
*/

DEF VAR xIntMargin AS DEC NO-UNDO. 
DEF VAR xDefInt    AS DEC NO-UNDO. 

/* note: this requires tmsparam2.i in the calling module */
ASSIGN xIntMargin = fCParamDe("OTIntMargin")
       xDefInt    = fCParamDe("OverTimeInt"). 

FUNCTION fOtIntPerc RETURNS DECIMAL
    (iCategory AS CHAR,
     iDate     AS Date). 

    /* FIRST get the category */
    FIND CustCat WHERE 
         CustCat.Brand    = gcBrand AND
         CustCat.Category = iCategory NO-LOCK NO-ERROR.

    /* general percent FOR ALL */
    IF NOT AVAILABLE CustCat THEN RETURN xDefInt.

    /* THEN according TO the Interest definition of category 
       get the valid percent 
    */
    FIND FIRST Interest WHERE 
        Interest.Brand      = gcBrand AND
        Interest.ValidFrom <= iDate   AND
        Interest.IntType    = CustCat.IntType
    NO-LOCK NO-ERROR.                      

    /* IF category related percent NOT AVAILABLE THEN use general percent */
    IF NOT AVAILABLE Interest THEN 
    FIND FIRST Interest WHERE
        Interest.Brand      = gcBrand AND
        Interest.ValidFrom <= iDate   AND
        Interest.IntType    = 0
    NO-LOCK NO-ERROR. 

    IF AVAIL Interest THEN DO:

        /* FOR RepType 2 a defined margin must be added */
        IF Interest.IntType = 2 THEN 
             RETURN Interest.IntPerc + xIntMargin.

        /* fixed percent */
        ELSE RETURN Interest.IntPerc.
    END.

    /* general percent FOR ALL */
    ELSE RETURN xDefInt. 


END FUNCTION.         

