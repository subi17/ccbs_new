/* ----------------------------------------------------------------------------
  MODULE .......: useraccountreq.p
  FUNCTION .....: user account handling
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.09.07 (separated from msrequest.i)
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}
{Func/fwebuser.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 5 THEN RETURN "ERROR".

RUN pUserAccount.

RETURN RETURN-VALUE.



PROCEDURE pUserAccount:

   DEF VAR llPrintLetter AS LOG NO-UNDO.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   IF LOOKUP(MsRequest.ReqCParam1,"passwd,loginpwd,new,chglogin") = 0 THEN DO:
      fReqError("Nothing to do").
      RETURN. 
   END.
 
   llPrintLetter = TRUE.
 
   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   CASE MsRequest.ReqCParam1:
   /* change password to an existing account */
   WHEN "passwd" THEN DO:
      IF NOT update_account(MsRequest.CustNum,?,?) THEN DO:
         fReqError("Password could not be set").
         RETURN.
      END.
   END.

   /* create a new account */
   WHEN "new" THEN DO:
      /* if a new customer -> create returns true and letter will be printed */
      IF NOT create_account(MsRequest.CustNum,?,?) THEN DO:
       
         /* for old customers nothing is done */
         llPrintLetter = FALSE.          
         
         fReqError("Customer already has a user account").
         RETURN.
      END. 
   END.

   /* change both loginid and passwd */
   WHEN "loginpwd" OR 
   WHEN "chglogin" THEN DO:

      /* delete old account */
      remove_account(MsRequest.CustNum).
      
      /* create a new one */
      IF NOT create_account(MsRequest.CustNum,?,?) THEN DO:
         fReqError("New account could not be created").
         RETURN.
      END. 
      
      /* no letter wanted */
      IF MsRequest.ReqCParam1 = "chglogin" THEN llPrintLetter = FALSE.
   END. 
   
   END CASE. 
   
   /* print a letter */
   IF llPrintLetter THEN DO:

      RUN Mc/prinuser(MsRequest.CustNum,
                   MsRequest.ReqCParam1, 
                   OUTPUT lcReqChar).

      IF lcReqChar > "" THEN DO:
         fReqLog("User account letter print failed: " + lcReqChar).
      END. 
   END.
   
   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      lcSMSText = fGetTxt("SMS",
                          "UserAccountChg",
                          TODAY,
                          Customer.Language).

      IF lcSMSText > "" THEN DO:                    
         /*
         lcSMSText = REPLACE(lcSMSText,"#NewUser",lcNewUser).
         */
              
         /* don't send messages before 8 am. */
         ldReqStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
         IF ldReqStamp = ? THEN ldReqStamp = fMakeTS().

         fMakeSchedSMS(Customer.CustNum,
                       Customer.SMSNumber,
                       16,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.
 
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

