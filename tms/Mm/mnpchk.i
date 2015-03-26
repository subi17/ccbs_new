/* mnpchk.i     07/2003 jp

   
*/   
        
      FIND FIRST TeleCompany WHERE 
                 TeleCompany.TCCode = INT(substr(ttCall.routingnumber,1,2))
      NO-LOCK NO-ERROR.

      IF AVAIL TeleCompany THEN lcbnet = Telecompany.bdest.
 
