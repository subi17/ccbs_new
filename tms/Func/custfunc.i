/* ----------------------------------------------------------------------
  MODULE .......: profunc.i
  TASK .........: Functions for handling Yoigo PRO related functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
&IF "{&YOIGOCUSTFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOCUSTFUNC_I YES

/* Find correct customer segment */
FUNCTION fgetCustSegment RETURNS CHAR
   (INPUT icIdType AS CHAR,
    INPUT ilSelfemployed AS LOG,
    INPUT ilProCust AS LOG,
    OUTPUT ocCategory AS CHAR):

   DEF VAR lcSegment AS CHAR NO-UNDO.

   DEF BUFFER CustCat FOR CustCat.

   IF ilProCust THEN DO:
      FIND FIRST CustCat NO-LOCK WHERE
                 Custcat.brand EQ Syst.Parameters:gcBrand AND
                 Custcat.custidtype EQ icIdType AND
                 CustCat.selfemployed EQ ilSelfEmployed AND
                 CustCat.pro EQ ilProCust NO-ERROR.
      IF AVAIL CustCat THEN DO:
         lcSegment = CustCat.Segment.
         ocCategory = CustCat.category.
      END.
   END.
   ELSE DO:
      IF icIDType EQ "CIF" THEN
         ocCategory = "23".
      ELSE IF icIDType EQ "NIF" AND ilSelfEmployed THEN 
         ocCategory = "44".         
      ELSE IF icIDType EQ "NIF" AND NOT ilSelfEmployed THEN
         ocCategory = "10". 
      ELSE IF icIDType EQ "NIE" AND ilSelfEmployed THEN
         ocCategory = "45".         
      ELSE IF icIDType EQ "NIE" AND NOT ilSelfEmployed THEN
         ocCategory = "11". 
      IF ocCategory > "" THEN DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    Custcat.brand EQ Syst.Parameters:gcBrand AND
                    Custcat.category EQ ocCategory NO-ERROR.

         IF AVAIL CustCat THEN
            lcSegment = CustCat.Segment.
      END.
      ELSE DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    Custcat.brand EQ Syst.Parameters:gcBrand AND
                    CustCat.custidtype EQ icIdType AND
                    CustCat.selfemployed EQ ilSelfemployed AND
                    CustCat.pro EQ ilProCust.
         IF AVAIL CustCat THEN DO:
            lcSegment = CustCat.Segment.
            ocCategory = CustCat.category.
         END.
      END.
   END.
   RETURN lcSegment.
END.

&ENDIF


