/* orderstat.p

   changes:     16.04.07/aam new parameters to tmscodesbr
*/
   
{Syst/commali.i}   

DEF VAR lcValue       AS CHAR NO-UNDO.

run tmscodesbr.p(INPUT   "ORDER",
               INPUT   "StatusCode",
               INPUT   "",
               INPUT   "Order Statuses",
               INPUT   "",
               OUTPUT  lcValue).


IF lcValue > "" THEN run order(1,8,lcValue,0).
