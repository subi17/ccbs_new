/* brand.i            04.09.03/aam 
*/               

/* 'sel' should be the frame name in all browsers, but this gives 
   the option to use other frame names */
&IF "{&BrFrame}" = ""
&THEN
&GLOBAL-DEFINE BrFrame sel
&ENDIF

DEF VAR lcBrand  AS CHAR   NO-UNDO. 

/* set default brand from global variable */
lcBrand = gcBrand.

FUNCTION fChkBrand RETURNS LOG
   (icBrand  AS CHAR).

   IF icBrand = "*" THEN RETURN TRUE.

   ELSE IF CAN-FIND(Brand WHERE Brand.Brand = icBrand)
   THEN RETURN TRUE.

   ELSE RETURN FALSE.                    

END FUNCTION.

/* set brand specific propath */
FUNCTION fBrandPath RETURNS LOGICAL
   (icNewBrand AS CHAR).

   DEF VAR lcNewPath AS CHAR NO-UNDO.

   /* 
   basic propath is set in the unix script (tms) that starts the session,
   brand spesific paths are added from brand.progpath
   */

   /* get the basic propath */
   lcNewPath = OS-GETENV("PROPATH").

   /* add brand's path as primary */
   FIND Brand WHERE Brand.Brand = icNewBrand NO-LOCK NO-ERROR.
   IF AVAILABLE Brand AND Brand.ProgPath > ""
   THEN lcNewPath = Brand.ProgPath +
                 (IF SUBSTRING(Brand.ProgPath,LENGTH(Brand.ProgPath),1) NE ","
                  THEN ","
                  ELSE "") +
                  lcNewPath. 

   IF lcNewPath > "" AND lcNewPath NE ?
   THEN PROPATH = lcNewPath.

   RETURN TRUE. 

END.

/* change brand (main user can do this basically anywhere) */
FUNCTION fChgBrand RETURNS LOGICAL
   (icNewBrand AS CHAR,
    ihFrame    AS HANDLE).

   DEF VAR lcNewComp AS CHAR NO-UNDO.
   DEF VAR liBCnt    AS INT  NO-UNDO. 

   /* not changed */
   IF icNewBrand = gcBrand THEN DO:
      lcBrand = icNewBrand.
      RETURN FALSE.
   END.

   /* valid brand */
   IF NOT CAN-FIND(Brand WHERE Brand.Brand = icNewBrand) 
   THEN RETURN FALSE. 

   /* change company name */
   FIND FIRST Company WHERE 
              Company.Brand = icNewBrand NO-LOCK NO-ERROR.
   lcNewComp = IF AVAILABLE Company 
               THEN Company.CompName 
               ELSE "Unknown". 

   /* change current frame title */
   IF VALID-HANDLE(ihFrame) THEN 
      ihFrame:TITLE = REPLACE(ihFrame:TITLE,ynimi,lcNewComp).

   /* set propath */
   fBrandPath(icNewBrand).

   /* set global variables */
   ASSIGN ynimi   = lcNewComp
          gcBrand = icNewBrand
          lcBrand = icNewBrand.

   RETURN TRUE. 

END FUNCTION.   

/* general behaviour for find-routines */
FUNCTION fRecFound RETURNS LOGICAL
   (iiOrder AS INT).

   IF NOT AVAILABLE {&BrTable} THEN DO:
      BELL.
      MESSAGE "NOT FOUND !".
      PAUSE 1 NO-MESSAGE.
      lcBrand = gcBrand.
      RETURN FALSE.
   END.

   /* brand was changed */
   IF gcAllBrand THEN DO:
      fChgBrand({&BrTable}.Brand,
                FRAME {&BrFrame}:HANDLE).
   END.                            

   &IF "{&GenBrowseVariables}" NE "NO"
   &THEN
   /* record was found */
   ASSIGN order      = iiOrder 
          Memory     = recid({&BrTable}) 
          must-print = TRUE.
   &ENDIF

   RETURN TRUE.        

END FUNCTION.

