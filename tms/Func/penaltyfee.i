&IF "{&PENALTYFEE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE PENALTYFEE_I YES
/* penaltyfee.i     18.06.08/aam
   
   penalty fee calculation formula
*/   

FUNCTION fCalculateFactor RETURNS DEC
   (idtBegContract AS DATE,     /* contract begins */
    idtRenewalDate AS DATE,     /* extension date */
    idtEndContract AS DATE,     /* contract should have ended */
    idtEndContractOrig AS DATE, /* original contract end date */
    idtCalcDate    AS DATE,     /* when calculated */
    iiCalcMethod   AS INT):     /* full, proportional */
    
   DEF VAR ldFactor    AS DEC  NO-UNDO.
   DEF VAR liDaysLeft  AS INT  NO-UNDO.
   DEF VAR liTotalDays AS INT  NO-UNDO.

   IF idtEndContractOrig = ? THEN idtEndContractOrig = idtEndContract.
   IF idtRenewalDate NE ? THEN idtBegContract = idtRenewalDate.
      
   IF idtBegContract = ? OR idtEndContract = ? OR idtCalcDate = ? OR
      idtEndContract < idtBegContract OR
      idtCalcDate > idtEndContract OR
      iiCalcMethod = 0 
   THEN ldFactor = 0.
   
   /* proportional fee, based on time left on contract */   
   ELSE DO:
      IF iiCalcMethod = 2 THEN ASSIGN
         liDaysLeft  = idtEndContract + 1 - idtCalcDate
         liTotalDays = idtEndContractOrig - idtBegContract + 1
         ldFactor = MAX(0,liDaysLeft / liTotalDays).
      
      ELSE ldFactor = 1. 
   END.
   
   /* penalty factor cannot be greater than 1 
     (can happen if termination date is before begin date) */
   IF ldFactor > 1 THEN ldFactor = 1.

   RETURN ldFactor.
   
END FUNCTION.

&ENDIF
