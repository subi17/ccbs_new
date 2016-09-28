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
&IF "{&fixedlinefunc}" NE "YES"
&THEN
&GLOBAL-DEFINE fixedlinefunc YES

/*Function returns Trie if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIsConvergenceTariff RETURNS LOGICAL
   (icCliType AS CHAR):
   DEF BUFFER bCLIType FOR CLIType.

   FIND FIRST bCLIType NO-LOCK WHERE
              bCLIType.Brand EQ Syst.Parameters:gcBrand AND
              bCLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL bCliType THEN DO:
      IF bCliType.FixedLineDownload NE ? AND 
         bCliType.FixedLineDownload NE "" THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
END.   


/*Used when there is no clitype available directly.*/
/*Function returns Trie if a tariff can be defined as convergent tariff.
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




&ENDIF
