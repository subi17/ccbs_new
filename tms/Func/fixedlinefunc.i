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
{Syst/commali.i}
{Func/cparam2.i}

/*Function returns Trie if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fHasConvergenceTariff RETURNS LOGICAL
   (iiMsSeq AS INT):
   DEF BUFFER bCLIType FOR CLIType.
   DEF BUFFER bMobsub FOR MobSub.

   FIND FIRST bMobsub NO-LOCK WHERE
              bMobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF NOT AVAIL bMobSub THEN RETURN FALSE.           

   FIND FIRST bCLIType NO-LOCK WHERE
              bCLIType.Brand EQ gcBrand AND
              bCLIType.CliType EQ bMobsub.CLIType NO-ERROR.
   IF AVAIL CliType THEN DO:
      IF bCliType.FixedLineDownload NE ? AND 
         bCliType.FixedLineDownload NE "" THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
END.   


&ENDIF
