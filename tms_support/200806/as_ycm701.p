DEFINE VARIABLE lcDelim AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE liPasswordHistoryLength AS INTEGER NO-UNDO. 
DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.
DEFINE BUFFER bTMSPass FOR TMSPass.

{commpaa.i}
{cparam.i2}
gcBrand = "1".
liPasswordHistoryLength = fCParamI("PassWdHistory").
llSimulate = FALSE. 

{log.i}
{date.i}
katun = "anttis".

fSetLogFileName("/apps/snet/200806/user-update_" + 
   STRING(YEAR(TODAY),"9999") +
   STRING(MONTH(TODAY),"99")  +
   STRING(DAY(TODAY),"99") + "_" +
   REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".log").
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
   IF NOT llSimulate THEN DO:
      tmsuser.todate = TODAY.
      tmsuser.usergroup = "NOTinUSE".
   END.
   RELEASE tmsuser.
   fLog("User deactivated","fDelete").
   RETURN TRUE.

END FUNCTION. 



FUNCTION fAdd RETURNS LOGICAL
(icLine AS CHAR):
   DEFINE VARIABLE lcUsercode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcGroup AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPassword AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEmail AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaValidFrom AS DATE NO-UNDO. 
   DEFINE VARIABLE ldaValidTo AS DATE NO-UNDO. 
   DEFINE VARIABLE liTopupLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liCreditLimit AS INTEGER NO-UNDO. 
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
   
   lcTemp = ENTRY(5,icLine,lcDelim).
   IF TRIM(lcTemp) EQ "" THEN DO:
      liTopupLimit = 0.
   END.
   ELSE DO:
      liTopupLimit = INT(lcTemp) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fLog("Invalid TopUp Limit","ERR").
         RETURN FALSE.
      END.
   END.

   lcTemp = ENTRY(5,icLine,lcDelim).
   IF TRIM(lcTemp) EQ "" THEN DO:
      liCreditLimit = 0.
   END.
   ELSE DO:
      liCreditLimit = INT(lcTemp) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fLog("Invalid Credit Limit","ERR").
         RETURN FALSE.
      END.
   END.

   FIND LAST tmsuser NO-LOCK USE-INDEX usernum.
   liUsernum = tmsuser.usernum + 1.
   
   IF NOT llSimulate THEN DO:
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
         tmsuser.topuplimit = liTopupLimit
         tmsuser.creditlimit = liCreditLimit.
      CREATE tmspass.
      ASSIGN
         tmspass.usercode = lcUsercode
         tmspass.Password = lcPassword 
         tmspass.creator  = katun
         tmspass.createts = ldeTS.
   END.

   fLog("User added, password created","fAdd").
   RETURN TRUE.

END FUNCTION. 

FUNCTION fModify RETURNS LOGICAL
(icLine AS CHAR):

   DEFINE VARIABLE lcUsercode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcGroup AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPassword AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEmail AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaValidFrom AS DATE NO-UNDO. 
   DEFINE VARIABLE ldaValidTo AS DATE NO-UNDO. 
   DEFINE VARIABLE liTopupLimit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liCreditLimit AS INTEGER NO-UNDO. 
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
         UNDO.
         RETURN FALSE.
      END.
      
      lcGroup = ENTRY(4,icLine,lcDelim).
      if TRIM(lcGroup) NE "" THEN DO:
         FIND FIRST usergrp NO-LOCK WHERE 
                    usergrp.usergroup = lcGroup NO-ERROR.
         IF NOT AVAIL usergrp THEN DO:
            fLog("Undefined usergroup","ERR").
            UNDO.
            RETURN FALSE.
         END.
         IF NOT llSimulate THEN tmsuser.usergroup = lcGroup.
         llModified = TRUE.
      END.

      lcName = ENTRY(2,icLine,lcDelim).
      IF TRIM(lcName) NE "" THEN DO:
         IF NOT llSimulate THEN tmsuser.username = lcName.
         llModified = TRUE.
      END.

      lcTemp = ENTRY(5,icLine,lcDelim).
      IF TRIM(lcTemp) NE "" THEN DO:
         liTopupLimit = INT(lcTemp) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            fLog("Invalid TopUp Limit","ERR").
            UNDO.
            RETURN FALSE.
         END.
         IF NOT llSimulate THEN tmsuser.topuplimit = liTopupLimit.
         llModified = TRUE.
      END.

      lcTemp = ENTRY(5,icLine,lcDelim).
      IF TRIM(lcTemp) NE "" THEN DO:
         liCreditLimit = INT(lcTemp) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            fLog("Invalid Credit Limit","ERR").
            UNDO.
            RETURN FALSE.
         END.
         IF NOT llSimulate THEN tmsuser.creditlimit = liCreditLimit.
         llModified = TRUE.
      END.

      IF llModified THEN
         fLog("User modified","fModify").   
      ELSE 
         fLog("Nothing to change","Warning").
      RETURN TRUE.
   END.   

END FUNCTION. 

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.

lcInputFile = "/apps/snet/200806/TMS_user_update_20080606.csv".

INPUT FROM VALUE(lcInputFile).
DEFINE VARIABLE liChanges AS INTEGER NO-UNDO. 
DEFINE VARIABLE llOk AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liLines AS INTEGER NO-UNDO. 

fLog("Starting user update run","MAIN").
fLog("Reading input from: " + lcInputFile,"MAIN").
REPEAT:
   IMPORT UNFORMATTED lcLine.
   liLines = liLines + 1.
   llOk = DYNAMIC-FUNCTION("f" + ENTRY(1,lcLine,lcDelim),lcLine) NO-ERROR.   
   IF llOk THEN liChanges = liChanges + 1.

END.

MESSAGE "Parsed " liLines " lines" SKIP
        liChanges " modifications" VIEW-AS ALERT-BOX.



