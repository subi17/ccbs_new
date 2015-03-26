DEFINE INPUT PARAMETER pcUser       AS CHARACTER NO-UNDO.  
DEFINE INPUT PARAMETER pcInputFile  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulate   AS LOGICAL NO-UNDO. 

/* Delimiter is defined as ; because input file is taken from Excel
   with Save As, CSV format and then the delimiter is automatically ; */
DEFINE VARIABLE lcDelim                 AS CHARACTER NO-UNDO INIT ";".
DEFINE VARIABLE liPasswordHistoryLength AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcLine                  AS CHARACTER NO-UNDO. 

DEFINE BUFFER bTMSPass FOR TMSPass.

{commpaa.i}
katun = pcUser.
{cparam2.i}
gcBrand = "1".
liPasswordHistoryLength = fCParamI("PassWdHistory").
{log.i}
{date.i}

fSetLogFileName(pcLogFile).
fSetLogEntryTypes(fGetValidLogEntryTypes()).
fSetGlobalLoggingLevel(1).
fClearLog().


FUNCTION fDelete RETURNS LOGICAL
(icLine AS CHAR):

   fLog(icLine,"fDelete").   
   FIND FIRST tmsuser EXCLUSIVE-LOCK WHERE 
              tmsuser.usercode = ENTRY(3,icLine,lcDelim)
              NO-ERROR.
   IF NOT AVAIL tmsuser THEN DO:
      fLog("TMSUser not available","ERR").
      RETURN FALSE.
   END.
   IF TMSUser.usergroup EQ "NOTinUSE" THEN DO:
      fLog("User already deactivated","Warning").
      RETURN TRUE.
   END.
   IF NOT plSimulate THEN DO:
      tmsuser.todate = TODAY.
      tmsuser.usergroup = "NOTinUSE".
   END.
   RELEASE tmsuser.
   fLog("User deactivated","fDelete").
   RETURN TRUE.

END FUNCTION. 



FUNCTION fNew RETURNS LOGICAL (icLine AS CHAR):
   DEFINE VARIABLE lcUsercode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcGroup AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPassword AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEmail AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaValidFrom AS DATE NO-UNDO. 
   DEFINE VARIABLE ldaValidTo AS DATE NO-UNDO. 
   DEFINE VARIABLE liTopupLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liCreditLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldLimitAmt AS DECIMAL NO-UNDO EXTENT 8.
   DEFINE VARIABLE licount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldeTS AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE liUsernum AS INTEGER NO-UNDO. 
   ldeTS = fMakeTS().

   fLog(icLine,"fAdd").   
   
   lcUsercode = ENTRY(3,icLine,lcDelim).
   IF TRIM(lcUsercode) = "" THEN DO:
      fLog("Username empty","ERR").
      RETURN FALSE.
   END.

   FIND FIRST tmsuser NO-LOCK WHERE 
              tmsuser.usercode = ENTRY(3,icLine,lcDelim)
              NO-ERROR.
   IF AVAIL tmsuser THEN DO:
      fLog("Usercode already exists","ERR").
      RETURN FALSE.
   END.
   
   lcPassword = lcUsercode. /* use default */

   lcGroup = ENTRY(4,icLine,lcDelim).
   if TRIM(lcGroup) = "" THEN DO:
      fLog("Usergroup empty","ERR").
      RETURN FALSE.
   END.
   FIND FIRST usergrp NO-LOCK WHERE 
              usergrp.usergroup = lcGroup NO-ERROR.
   IF NOT AVAIL usergrp THEN DO:
      fLog("Undefined usergroup","ERR").
      RETURN FALSE.
   END.
      
   lcName = ENTRY(2,icLine,lcDelim).

   /* validFrom */
   ldaValidFrom = TODAY. /* default */

   ldaValidTo = 12/31/2054. /* default */

   /* TopUp Limit is not used anymore (setup to 0) */
   liTopupLimit  = 0.
   
   /* Credit Limit is not updated anymore in tmsuser table (setup to 0)  */
   liCreditLimit = 0.

   lcTemp = TRIM(ENTRY(5,icLine,lcDelim)).
   IF lcTemp NE "" THEN DO:
      liCreditLimit = INT(lcTemp) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fLog("Credit limit is incorrect format","ERR").
         RETURN FALSE.
      END.
   END.

   FIND LAST tmsuser NO-LOCK USE-INDEX usernum.
   liUsernum = tmsuser.usernum + 1.
   
   IF NOT plSimulate THEN DO:
      CREATE tmsuser.
      ASSIGN
         tmsuser.brand = "1"
         tmsuser.usernum = liUsernum
         tmsuser.usercode = lcUsercode
         tmsuser.usergroup = lcGroup
         tmsuser.username = lcName
         tmsuser.email = lcEmail
         tmsuser.fromdate = ldaValidFrom
         tmsuser.todate = ldaValidTo
         tmsuser.topuplimit = 0 
         tmsuser.creditlimit = 0.
      CREATE tmspass.
      ASSIGN
         tmspass.usercode = lcUsercode
         tmspass.Password = lcPassword 
         tmspass.creator  = katun
         tmspass.createts = ldeTS.

      IF lcTemp NE "" THEN DO:

          IF NOT plSimulate THEN DO:
             /*create limit for this user */
             CREATE UserLimit.
             ASSIGN UserLimit.Brand      = gcBrand
                    UserLimit.LimitType  = 9
                    UserLimit.LimitTarget = "TMSUser"
                    UserLimit.LimitTargetID = TMSUser.UserCode
                    UserLimit.LimitAmt = liCreditLimit.
             RELEASE UserLimit.
          END.
      END.

   END.

   fLog("User added, password created","fAdd").
   RETURN TRUE.

END FUNCTION. 


FUNCTION fModify RETURNS LOGICAL (icLine AS CHAR):
   DEFINE VARIABLE lcUsercode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcGroup AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPassword AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEmail AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaValidFrom AS DATE NO-UNDO. 
   DEFINE VARIABLE ldaValidTo AS DATE NO-UNDO. 
   DEFINE VARIABLE liTopupLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liCreditLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldLimitAmt AS DECIMAL NO-UNDO.
   DEFINE VARIABLE licount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldeTS AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DEFINE VARIABLE llModified AS LOGICAL NO-UNDO INIT FALSE. 
   ldeTS = fMakeTS().
   fLog(icLine,"fModify").   

   lcUsercode = ENTRY(3,icLine,lcDelim).
   IF TRIM(lcUsercode) = "" THEN DO:
      fLog("Username empty","ERR").
      RETURN FALSE.
   END.

   update-block:
   DO TRANSACTION:
      FIND FIRST tmsuser EXCLUSIVE-LOCK WHERE 
                 tmsuser.usercode = ENTRY(3,icLine,lcDelim)
                 NO-ERROR.
      IF NOT AVAIL tmsuser THEN DO:
         fLog("Usercode doesn't exist","ERR").
         UNDO, RETURN FALSE.
      END.
      
      lcTemp = TRIM(ENTRY(5,icLine,lcDelim)).
      IF lcTemp NE "" THEN DO:
         liCreditLimit = INT(lcTemp) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            fLog("Credit limit is incorrect format","ERR").
            UNDO, RETURN FALSE.
         END.
      END.
      
      lcGroup = ENTRY(4,icLine,lcDelim).
      if TRIM(lcGroup) NE "" THEN DO:
         IF lcGroup BEGINS "Change to" THEN
            lcGroup = TRIM(ENTRY(3,lcGroup," ")).
         if lcGroup ne "Update Top-up/Credit Limits" then do:
         FIND FIRST usergrp NO-LOCK WHERE 
                    usergrp.usergroup = lcGroup NO-ERROR.
         IF NOT AVAIL usergrp THEN DO:
            fLog("Undefined usergroup","ERR").
            UNDO, RETURN FALSE.
         END.
         end.
         IF NOT plSimulate and lcGroup NE "Update Top-up/Credit Limits" THEN tmsuser.usergroup = lcGroup.
         llModified = TRUE.
      END.

      lcName = ENTRY(2,icLine,lcDelim).
      IF TRIM(lcName) NE "" THEN DO:
         IF NOT plSimulate THEN tmsuser.username = lcName.
         llModified = TRUE.
      END.
      
      IF lcTemp NE "" THEN DO:

          FIND UserLimit WHERE
               UserLimit.Brand = gcBrand AND
               UserLimit.LimitType = 9 AND
               UserLimit.LimitTarget = "TMSUser" AND
               UserLimit.LimitTargetID = TMSUser.UserCode NO-LOCK NO-ERROR.

          IF NOT AVAIL UserLimit THEN DO:
             /*create limit for this user */
             IF NOT plSimulate THEN DO:
                CREATE UserLimit.
                ASSIGN UserLimit.Brand      = gcBrand
                       UserLimit.LimitType  = 9
                       UserLimit.LimitTarget = "TMSUser"
                       UserLimit.LimitTargetID = TMSUser.UserCode
                       UserLimit.LimitAmt = liCreditLimit.
                RELEASE UserLimit.
             END.
             llModified = TRUE.
          END.
          ELSE DO:
            IF UserLimit.LimitAmt NE liCreditLimit THEN DO:
               
               IF NOT plSimulate THEN DO:
                  FIND CURRENT UserLimit EXCLUSIVE-LOCK.
                  UserLimit.LimitAmt = liCreditLimit.
                  RELEASE UserLimit.
               END.
               llModified = TRUE.
            END.

          END.
      END.

      IF llModified THEN
         fLog("User modified","fModify").   
      ELSE 
         fLog("Nothing to change","Warning").
      RETURN TRUE.
   END.   

END FUNCTION. 


INPUT FROM VALUE(pcInputFile).
DEFINE VARIABLE liChanges AS INTEGER NO-UNDO. 
DEFINE VARIABLE llOk AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liLines AS INTEGER NO-UNDO. 

fLog("Starting user update run","MAIN").
fLog("Reading input from: " + pcInputFile,"MAIN").
REPEAT:
   IMPORT UNFORMATTED lcLine.
   liLines = liLines + 1.
   llOk = DYNAMIC-FUNCTION("f" + ENTRY(1,lcLine,lcDelim),lcLine) NO-ERROR.   
   IF llOk THEN liChanges = liChanges + 1.

END.

MESSAGE "Parsed " liLines " lines" SKIP
        liChanges " modifications" VIEW-AS ALERT-BOX.


