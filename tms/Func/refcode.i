/* ===========================================================================
 MODULE ........: refcode.i
 APPLICATION ...: tms
 TASK ..........: reference code functions
 CREATED .......: 09.11.2001
 CHANGED .......: 09.09.03/aam brand
 Version .......: M15
 ============================================================================*/


/*********** SHORTED REFERENCE number *******/
FUNCTION fViite RETURNS CHARACTER (INPUT lref AS CHARACTER).
   DEF VAR lilength    AS DECIMAL   NO-UNDO.
   DEF VAR linumber    AS INTEGER   NO-UNDO.
   DEF VAR ldenumber   AS DECIMAL   NO-UNDO.
   DEF VAR lcrefstring AS CHARACTER NO-UNDO.
   DEF VAR lii         AS INT       NO-UNDO.
   DEF VAR lii2        AS INT       NO-UNDO.

   ASSIGN
   lilength  = LENGTH(lref) / 5
   linumber  = TRUNCATE(lilength,0)
   ldenumber = lilength - linumber
   lii2      = INT(ldenumber * 5).

   lcrefstring  = SUBSTR(lref,1,lii2).

   DO lii = 1 TO linumber:
      lcrefstring = lcrefstring + " " + SUBSTR(lref,(lii2 + 1),5).
      lii2 = lii2 + 5. 
   END.

   RETURN TRIM(lcrefstring).
   
END FUNCTION.

/*********** RETURNS TEXT ****************************/

FUNCTION fTeksti RETURNS CHARACTER
   (INPUT inro AS INTEGER, INPUT ikieli AS INTEGER).

   /* FIND TEXT WITH CUSTOMERS LANGUAGE */
   FIND FIRST HdrText WHERE 
              HdrText.Brand  = gcBrand AND
              HdrText.te-nro = inro    AND
              HdrText.te-kie = ikieli NO-LOCK NO-ERROR.
   IF AVAIL HdrText THEN RETURN HdrText.te-text.

   /* IF CAN'T FIND THEN USE DEFAULT LANGUAGE */
   ELSE DO:
      FIND FIRST HdrText WHERE 
                 HdrText.Brand  = gcBrand AND
                 HdrText.te-nro = inro    AND  
                 HdrText.te-kie = 1 NO-LOCK NO-ERROR.
      IF AVAIL HdrText THEN RETURN te-text.

      /* IF CAN'T FIND ANY LANGUAGE  */
      ELSE RETURN "".
   END.

END FUNCTION.


/*********** REFERENCENUMBER LAST number **********
   Version:     FIN                                                     */
FUNCTION fChkNbrFIN RETURNS CHARACTER (INPUT ilanro AS CHAR).

   DEF VAR laskuno  AS INT  FORMAT "zzzzzzz9" NO-UNDO.
   DEF VAR viite    AS CHAR FORMAT "x(25)"    NO-UNDO.
   DEF VAR apulasno AS CHAR                   NO-UNDO.
   DEF VAR ii       AS INT                    NO-UNDO.
   DEF VAR viyht    AS INT                    NO-UNDO.
   DEF VAR viitei   AS INT  FORMAT "9"        NO-UNDO.
   DEF VAR vistri   AS INT                    NO-UNDO.
   DEF VAR vija     AS INT                    NO-UNDO.
   DEF VAR ker      AS INT                    NO-UNDO.

   ASSIGN 
   apulasno = ilanro
   vistri = 0
   ker    = 7
   viyht  = 0
   viitei = 0
   vija   = 0
   ii     = LENGTH(apulasno).

   REPEAT WHILE ii > 0:
      ASSIGN
      vistri = INTEGER(SUBSTRING(apulasno,ii,1))
      viyht  = viyht + ker * vistri
      ker    = ker - 4
      ii     = ii - 1.

      IF ii = 0 THEN LEAVE.
      ASSIGN
      vistri = INTEGER(SUBSTRING(apulasno,ii,1))
      viyht  = viyht + ker * vistri
      ker    = ker - 2
      ii     = ii - 1.

      IF ii = 0 THEN LEAVE.
      ASSIGN
      vistri = INTEGER(SUBSTRING(apulasno,ii,1))
      viyht  = viyht + ker * vistri
      ker    = 7
      ii     = ii - 1.

      IF ii = 0 THEN LEAVE.
   END.

   ASSIGN vija = viyht MODULO 10.
   IF vija <> 0 THEN ASSIGN viitei = 10 - vija.

   RETURN STRING(viitei).

END FUNCTION.


