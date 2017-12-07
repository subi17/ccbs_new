&IF "{&EXTRALINEFUNC_I}" NE "YES"
&THEN

&GLOBAL-DEFINE EXTRALINEFUNC_I YES

{Syst/tmsconst.i}

/* Returns comma delimited character list of extraline clitypes (tariffs) */ 
FUNCTION fExtraLineCLITypes RETURNS CHARACTER:

   DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeTo":

      lcReturnValue = lcReturnValue + "," + MXItem.MXValue.
   END.

   RETURN LEFT-TRIM(lcReturnValue, ",").

END FUNCTION.

/* Check if a mainline clitype is allowed for an extraline clitype  */ 
FUNCTION fCLITypeAllowedForExtraLine RETURNS LOGICAL
   (icCLIType          AS CHARACTER,
    icExtraLineCLIType AS CHARACTER):

   DEFINE BUFFER MXItemExtra FOR MXItem.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItemExtra NO-LOCK WHERE
             MXItemExtra.MXSeq   = Matrix.MXSeq   AND
             MXItemExtra.MXName  = "SubsTypeTo" AND
             MXItemExtra.MXValue = icExtraLineCLIType,       
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeFrom" AND
             MXItem.MXValue = icCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.


/* Check which extraline clitype a mainline clitype is using.
   If the given clitype is not mainline clitype the function returns
   empty string */ 
FUNCTION fExtraLineForMainLine RETURNS CHARACTER
   (icMainLineCLIType  AS CHARACTER):

   DEFINE BUFFER MXItemMain FOR MXItem.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItemMain NO-LOCK WHERE
             MXItemMain.MXSeq   = Matrix.MXSeq AND
             MXItemMain.MXName  = "SubsTypeFrom" AND
             MXItemMain.MXValue = icMainLineCLIType,
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = MXItemMain.MXSeq AND
             MXItem.MXName  = "SubsTypeTo":
                
       RETURN MXItem.MXValue.
   END.
   
   RETURN "".

END FUNCTION.

/* Check if the clitype is extraline clitype */
FUNCTION fCLITypeIsExtraLine RETURNS LOGICAL
   (icExtraLineCLIType AS CHARACTER):

   DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq   AND
             MXItem.MXName  = "SubsTypeTo" AND
             MXItem.MXValue = icExtraLineCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Check if the clitype is mainline clitype
   (i.e. it is part of any of extraline clitype mainline list) */ 
FUNCTION fCLITypeIsMainLine RETURNS LOGICAL
   (icCLIType  AS CHARACTER):

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeFrom" AND
             MXItem.MXValue = icCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

&ENDIF