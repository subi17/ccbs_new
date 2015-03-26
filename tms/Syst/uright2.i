/* uright2.i

   check  IF user has write right   */

IF NOT qupd THEN DO:
   MESSAGE
   "You do NOT have right to change anything in" SKIP
   "this program"
   VIEW-AS ALERT-BOX error
   title " CHANGES  ARE NOT  ALLOWED ".

   NEXT.

END.
