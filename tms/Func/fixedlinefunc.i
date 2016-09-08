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
{commali.i}
{cparam2.i}

/*Function returns Trie if a tariff can be defined as convergent tariff.
NOTE: if tariff list is missing or reading fails FALSE is returned*/
FUNCTION fIsConvergenceTariff RETURNS LOGICAL
   (icCLIType AS CHAR):
   DEF VAR lcTariffList AS CHAR NO-UNDO.
   lcTariffList = fCparamC("AllConvergentTariffs").
   IF lcTariffList EQ ? THEN RETURN FALSE.
   IF LOOKUP(icCLIType, lcTariffList) > 0 THEN RETURN TRUE.
      
   RETURN FALSE.
END.   

&ENDIF
