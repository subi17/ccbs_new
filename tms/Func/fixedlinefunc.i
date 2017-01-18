/* ----------------------------------------------------------------------
  MODULE .......: fixedlinefunc.i
  TASK .........: Functions for handling fixed line related functionality
                  Reference Convergent offer project YPR-4729.                 
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 19.11.15
  CHANGED ......:
  ------------------------------------------------------------------------*/
&IF "{&FIXEDLINEFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE FIXEDLINEFUNC_I YES
{tmsconst.i}
{timestamp.i}

/* Function makes new MSOwner when subscription is partially
   terminated or mobile part order closed */
FUNCTION fUpdatePartialMSOwner RETURNS LOGICAL
   (iiMsSeq AS INT,
    icFixedNumber AS CHAR):
   DEF VAR ldUpdateTS AS DEC NO-UNDO.
   DEF BUFFER MsOwner FOR MsOwner.
   DEF BUFFER bNewMsowner FOR Msowner.

   ldUpdateTS = fMakeTS().
   FIND FIRST MSOwner WHERE 
              MSOwner.MsSeq  = iiMsSeq AND
              MSOwner.TsEnd >= ldUpdateTS
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MSOwner THEN RETURN FALSE.

   MSOwner.TsEnd = ldUpdateTS.
   CREATE bNewMsowner.
   BUFFER-COPY MSOwner EXCEPT TsEnd tsbegin TO bNewMsowner.
   ASSIGN
      bNewMsowner.CLI = icFixedNumber
      bNewMsowner.imsi = ""
      bNewMsowner.CliEvent = "F"
      bNewMsowner.tsbegin = fSecOffSet(ldUpdateTS,1)
      bNewMsowner.TsEnd = 99999999.99999.
   RELEASE MSOwner.
   RELEASE bNewMsowner.
   RETURN TRUE.

END.   

/*Function returns True if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIsConvergenceTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Parameters:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
      CliType.FixedLineDownload NE ? AND 
      CliType.FixedLineDownload NE "" THEN RETURN TRUE.
   
   RETURN FALSE.
END.   


/*Used when there is no clitype available directly.*/
/*Function returns True if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fHasConvergenceTariff RETURNS LOGICAL
   (iiMsSeq AS INT):
   DEF BUFFER bCLIType FOR CLIType.
   DEF BUFFER bMobsub FOR MobSub.
   DEF BUFFER bTermMS FOR TermMobSub.
   DEF VAR lcCliType AS CHAR.

   FIND FIRST bMobsub NO-LOCK WHERE
              bMobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      FIND FIRST bTermMS NO-LOCK WHERE 
                 bTermMS.MsSeq EQ iiMsSeq NO-ERROR.
      IF NOT AVAIL bTermMS THEN RETURN FALSE.
      ELSE lcCliType = bTermMS.CLIType.
   END.
   ELSE lcCLIType = bMobSub.CliType.

   RETURN fIsConvergenceTariff(lcCLIType).
END.   

FUNCTION fCanTerminateConvergenceTariff RETURNS LOGICAL
   (iiMsSeq AS INT,
    iiTerminationReason AS INT,
    OUTPUT ocError AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderFusion FOR OrderFusion.

   IF iiTerminationReason EQ ? OR
      iiTerminationReason EQ {&SUBSCRIPTION_TERM_REASON_MNP}
      THEN RETURN TRUE.

   FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = iiMsSeq AND
            LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST OrderFusion NO-LOCK WHERE
            OrderFusion.Brand = Syst.Parameters:gcBrand AND
            OrderFusion.OrderID = Order.OrderID:

      IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:

         IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:
            ocError = "Ongoing Convergent order".
            RETURN FALSE.
         END.
         ELSE ocError = "Warning: Ongoing convergent order".
   
      END.
      ELSE DO:
      
         IF Order.StatusCode NE {&ORDER_STATUS_PENDING_MOBILE_LINE} AND
            Order.StatusCode NE {&ORDER_STATUS_MNP_REJECTED} THEN DO:
            ocError = "Mobile line order is ongoing".
            RETURN FALSE.
         END.
         ELSE ocError = "Warning: Mobile line order is ongoing".
   
      END.
   END.

   RETURN TRUE.

END.

/* Check if convergent contract based by subscription name.
   All convergent subscription name starts CONTDSL (ADSL) or 
   CONTFH (fiber)*/
FUNCTION fIsConvergentFixedContract RETURNS LOGICAL
   (icContract AS CHAR):
   IF icContract BEGINS "CONTDSL" OR
      icContract BEGINS "CONTFH" THEN 
      RETURN TRUE.
   RETURN FALSE.
END.   

/* Check convergent STC compability. Special handling that allows convergent
   STC between subscription types which have same fixed line part.
   Convergent ADSL subscription can be changed to other ADSL
   Convergent fiber 50MB can be changed to other convergent 50MB fiber.
   Convergent fiber 300MB can be changed to other convergent 300MB fiber.
*/
FUNCTION fCheckConvergentSTCCompability RETURNS LOGICAL
   (INPUT icOldCliType AS CHAR,
    INPUT icNewCliType AS CHAR):
   DEF BUFFER bOldClitype FOR Clitype.

   /* compatible if both have same download speed for fixedline */
   FIND FIRST bOldClitype WHERE
              bOldClitype.brand EQ Syst.Parameters:gcBrand AND
              bOldClitype.clitype EQ icOldCliType NO-ERROR.
   IF AVAIL bOldClitype THEN
      IF CAN-FIND (FIRST Clitype WHERE
                         Clitype.brand EQ Syst.Parameters:gcBrand AND
                         Clitype.clitype EQ icNewCliType AND
                         Clitype.FixedLineDownload EQ 
                            bOldClitype.FixedLineDownload) THEN
         RETURN TRUE.
   
   /* otherwise is not compatible */
   RETURN FALSE.
END.                                         

&ENDIF
