/* fwidlst.i        16.04.04/aam

   get a list of user ids and passwords for web 
   
   changes:         27.01.05/aam TeleF version
                    18.01.06/aam new structure for user accounts    
*/

{Func/fwebuser.i}

DEF BUFFER bWebCustomer FOR Customer.

FUNCTION fWebUserList RETURNS CHAR
   (iiCustNum AS INT,
    iiMargin  AS INT).  
   
   DEF VAR lcHUser   AS CHAR NO-UNDO. 
   DEF VAR lcHPass   AS CHAR NO-UNDO. 
   DEF VAR lcWList   AS CHAR NO-UNDO. 
   DEF VAR lcUID     AS CHAR NO-UNDO.
   DEF VAR lcUPwd    AS CHAR NO-UNDO.
   
   FIND bWebCustomer WHERE
        bWebCustomer.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bWebCustomer THEN RETURN "".
   
   /* get customer's own account */
   IF NOT get_account_data(iiCustNum,
                           OUTPUT lcUID,  
                           OUTPUT lcUPwd)
   THEN RETURN "".                           

          /* labels */
   ASSIGN lcHUser = fTeksti(190,Customer.Language)
          lcHPass = fTeksti(191,Customer.Language).

   lcWList =  FILL(" ",iiMargin) +

              lcHUser + " " +
              lcUID   + FILL(" ",5) + 
              
              lcHPass + " " + 
              lCUPwd  + CHR(10).
              
   RETURN lcWList.
    
   
END FUNCTION.

FUNCTION fWebUserIDs RETURNS LOGIC
   (INPUT        iiCustNum AS INT,
    INPUT-OUTPUT icText    AS CHAR).
   
   DEF VAR lcUID     AS CHAR NO-UNDO.
   DEF VAR lcUPwd    AS CHAR NO-UNDO.
   
   FIND bWebCustomer WHERE
        bWebCustomer.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bWebCustomer THEN RETURN FALSE.
   
   /* get customer's own account */
   IF NOT get_account_data(iiCustNum,
                           OUTPUT lcUID,  
                           OUTPUT lcUPwd)
   THEN RETURN FALSE.                           

   ELSE DO:
      ASSIGN icText = REPLACE(icText,"#WEBUSERID",lcUID)
             icText = REPLACE(icText,"#WEBUSERPWD",lcUPWD).
             
      RETURN TRUE.       
   END.  
   
END FUNCTION.


