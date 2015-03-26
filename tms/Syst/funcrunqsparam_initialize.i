/* funcrunqsparam_initialize.i  14.03.12/aam
*/

FUNCTION fDateValue RETURNS DATE
   (icDefaultValue AS CHAR,
    idaRunDate AS DATE):

   DEF VAR ldaDate AS DATE NO-UNDO.
   
   CASE icDefaultValue:
   WHEN "#RUNDATE" THEN ldaDate = idaRunDate.
   WHEN "#RUNBEGIN" THEN
      ldaDate = DATE(MONTH(idaRunDate),1,YEAR(idaRunDate)).
   WHEN "#RUNEND" THEN 
      ldaDate = IF MONTH(idaRunDate) = 12 
                THEN DATE(12,31,YEAR(idaRunDate))
                ELSE DATE(MONTH(idaRunDate) + 1,1,YEAR(idaRunDate)) - 1.
   WHEN "#PREVBEGIN" THEN
      ldaDate = ADD-INTERVAL(DATE(MONTH(idaRunDate),1,YEAR(idaRunDate)),
                             -1,"month").
   WHEN "#PREVEND" THEN 
      ldaDate = DATE(MONTH(idaRunDate),1,YEAR(idaRunDate)) - 1.
   WHEN "#FUSIONDUEDATE" THEN
      ldaDate = DATE(MONTH(idaRunDate),16,YEAR(idaRunDate)).
   OTHERWISE
      ldaDate = DATE(icDefaultValue) NO-ERROR.
   END CASE.   

   RETURN ldaDate.

END FUNCTION.

FUNCTION fIntegerValue RETURNS INT
   (icDefaultValue AS CHAR,
    idaRunDate AS DATE):

   DEF VAR liValue    AS INT  NO-UNDO.
   DEF VAR ldaPrevEnd AS DATE NO-UNDO.

   CASE icDefaultValue:
   WHEN "#RUNMONTH" THEN 
      liValue = YEAR(idaRunDate) * 100 + MONTH(idaRunDate).
   WHEN "#PREVMONTH" THEN ASSIGN 
      ldaPrevEnd = DATE(MONTH(idaRunDate),1,YEAR(idaRunDate)) - 1
      liValue = YEAR(ldaPrevEnd) * 100 + MONTH(ldaPrevEnd).
   OTHERWISE
      liValue = INT(icDefaultValue) NO-ERROR.
   END CASE.    
   
   RETURN liValue.

END FUNCTION.

   