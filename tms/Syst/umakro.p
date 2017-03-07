/* --------------------------------------------------
  MODULE .......: UMAKRO.p
  FUNCTION .....: Invoice layout module
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: ??.??.??
  MODIFIED .....: 13.03.98 kl automatic selection
                  08.01.98 kl no layout IF Printed into PaymFile
                  12.01.00 jp cparams
                  17.10.01/aam tele1-references removed
                  21.11.01/aam another invoice makro
                  10.01.02 ht  JG version
                  21.02.03/aam take macros from parameters if not set
                  28.03.03/tk  check if device is email
                  28.05.03/aam MacroDeposit
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAM lcMacros  AS C NO-UNDO.

DEF VAR lcMacFile  AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.
DEF VAR lcMacParam AS CHAR NO-UNDO. 
DEF VAR lcMacDir   AS CHAR NO-UNDO. 

IF lcMacros = "" OR lcMacros = ? THEN DO:

   lcMacParam = "MacroInvoice,MacroDeposit,MacroSpec".

   ASSIGN lcMacDir = fCParamC("MacroDir")   
          lcMacros = "". 

   DO i = 1 TO NUM-ENTRIES(lcMacParam):

      lcMacFile = fCParamC(ENTRY(i,lcMacParam)). 

      IF lcMacFile NE "" AND lcMacFile NE ?
      THEN lcMacros = lcMacros + 
                      (IF lcMacros NE ""
                       THEN ","
                       ELSE "") +
                      lcMacDir + 
                      lcMacFile.
   END.                
END. 

IF lcMacros = "" OR lcMacros = ? THEN RETURN. 

/* get printer's physical name */
FIND FIRST TMSPrinter where
           TMSPrinter.PrinterId = TMSPrinter
NO-LOCK NO-ERROR.
IF NOT AVAIL TMSPrinter THEN 
MESSAGE "SYSTEM ERROR: printer record" TMSPrinter "does not exist"
VIEW-AS ALERT-BOX ERROR.

/* IF NOT into PaymFile or email*/
if TMSPrinter.Device ne "-" AND TMSPrinter.Device NE "EMail" THEN DO:

   DO i = 1 TO NUM-ENTRIES(lcMacros):
      UNIX SILENT value(TMSPrinter.Device + " " + ENTRY(i,lcMacros)).
   END.

END.

HIDE MESSAGE no-pause.


