/* tariffd.i      26.07.02/aam 
   general definitions for procedures that use tariff.i*

                  17.09.02/aam function fTZNames
*/

DEF VAR llDayType  AS LOG                NO-UNDO EXTENT 3.
DEF VAR llDayFrom  AS LOG                NO-UNDO EXTENT 3.
DEF VAR llDayTo    AS LOG                NO-UNDO EXTENT 3.
DEF VAR liCount    AS INT                NO-UNDO.

DEF VAR lcTZName   AS CHAR               NO-UNDO EXTENT 6.


/* default names for time zones */
FOR EACH TMSCodes NO-LOCK WHERE
    TMSCodes.TableName = "Tariff" AND
    TMSCodes.FieldName = "TZName":

   IF INTEGER(TMSCodes.CodeValue) >= 1 AND
      INTEGER(TMSCodes.CodeValue) <= 6
   THEN lcTZName[INTEGER(TMSCodes.CodeValue)] = TMSCodes.CodeName.
END. 

/* get the default names for tariff */
FUNCTION fGetTZNames RETURNS LOGICAL.

   DO liCount = 1 TO 6:
      Tariff.TZName[liCount] = lcTZName[liCount].
   END. 

   RETURN TRUE.

END FUNCTION.

/* save the latest names that user entered as default for this session */
FUNCTION fSaveTZNames RETURNS LOGICAL.

   IF NOT AVAILABLE Tariff THEN RETURN FALSE. 

   DO liCount = 1 TO 6:
      lcTZName[liCount] = Tariff.TZName[liCount]. 
   END. 

   RETURN TRUE. 

END FUNCTION.

