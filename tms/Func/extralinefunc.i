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

/* Function checks if STC is possible when the new clitype is
   extraline. It is possible when the customer has a free
   mainline and the mainline is suitable for the extraline */
FUNCTION fSTCPossible RETURNS LOGICAL
   (iiCustNum    AS INTEGER,
    icNewCLIType AS CHARACTER):

   IF NOT fCLITypeIsExtraLine(icNewCLIType)
   THEN RETURN TRUE.

   DEFINE BUFFER MobSub FOR MobSub.

   /* Find suitable mainline mobsub from the customer */
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.CustNum      EQ iiCustNum      AND
            MobSub.MultiSimId   EQ 0                   AND
            MobSub.MultiSimtype EQ 0                   AND
            (MobSub.MsStatus    EQ {&MSSTATUS_ACTIVE}  OR
             MobSub.MsStatus    EQ {&MSSTATUS_BARRED}):

      IF NOT fCLITypeAllowedForExtraLine(MobSub.CLIType, icNewCLIType)
      THEN NEXT.

      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

&ENDIF