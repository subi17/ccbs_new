/* old_unbilled_events.i      03.04.12/aam
*/

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}

FUNCTION fOldUnbilledEventLimit RETURNS DATE
   (INPUT iiAddMonths AS INT):

   DEF VAR liOldMonth    AS INT  NO-UNDO.
   DEF VAR ldaEventDate  AS DATE NO-UNDO.
   
   liOldMonth = fCParamI("OldUnbilledEventLimit").
   IF liOldMonth = ? THEN liOldMonth = 4.   

   ASSIGN
      liOldMonth = liOldMonth + iiAddMonths
      ldaEventDate = ADD-INTERVAL(TODAY,-1 * liOldMonth,"months")
      ldaEventDate = fLastDayOfMonth(ldaEventDate).

   RETURN ldaEventDate.

END FUNCTION.


