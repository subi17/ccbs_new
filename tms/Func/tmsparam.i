/* tmsparam.i   - checks whether a system PARAMETER exists 

   {1}  = PARAMETER code
   {2}  = Action IF NOT found


----------------------------------------------------------------- */

FIND first TMSParam WHERE 
     tmsparam.brand = Syst.Var:gcBrand AND 
     tmsparam.ParamCode = "{1}" NO-LOCK NO-ERROR.
IF NOT AVAIL TMSParam THEN DO:
   MESSAGE
   "System Parameter '{1}' does not exist" SKIP
   "in the Parameter File !"
   VIEW-AS ALERT-BOX ERROR TITLE
   " CHECK THE PARAMETER FILE ".

   {2}.

END.   

