/* fuserright.i     16.11.07/aam


*/
&IF "{&FUSERRIGHT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE FUSERRIGHT_I YES
{Syst/commali.i}

FUNCTION fTokenRights RETURNS CHAR
   (icUserCode AS CHAR,
    icTokens   AS CHAR):
   
   DEF VAR lcRights   AS CHAR NO-UNDO.
   DEF VAR liTokenCnt AS INT  NO-UNDO.

   lcRights = "".
   
   FOR FIRST TMSUser NO-LOCK WHERE
             TMSUser.UserCode = icUserCode,
       FIRST UserGrp OF TMSUser NO-LOCK:
             
      DO liTokenCnt = 1 TO NUM-ENTRIES(icTokens):
      
        /* if full rights then no need to look further */ 
        IF LOOKUP(ENTRY(liTokenCnt,icTokens),UserGrp.ModifyTokens) > 0 
        THEN RETURN "RW".
           
        ELSE IF LOOKUP(ENTRY(liTokenCnt,icTokens),UserGrp.ShowTokens) > 0 
        THEN lcRights = 'R'.

      END.

   END.
             
   RETURN lcRights.
   
END FUNCTION.

FUNCTION fIsAdminUser RETURNS LOGIC
   (icUserCode AS CHAR):
   
   FIND FIRST TMSUser WHERE TMSUser.UserCode = icUserCode NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSUser THEN RETURN FALSE.
   
   RETURN TMSUser.AdminUser.
   
END FUNCTION.

FUNCTION fIsAdminToken RETURNS LOGIC
   (icTokens AS CHAR):

   DEF VAR liTokenCnt AS INT NO-UNDO.
   
   DO liTokenCnt = 1 TO NUM-ENTRIES(icTokens):
      FIND FIRST Token WHERE Token.TokenCode = ENTRY(liTokenCnt,icTokens)
         NO-LOCK NO-ERROR.
      IF AVAILABLE Token AND Token.AdminToken THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
   
END FUNCTION.

 
FUNCTION fUserLimitAmt RETURNS DECIMAL
   (icUserCode AS CHAR,
    ilimitType AS INT):
 
   DEF VAR ldlimitAmt AS DECIMAL NO-UNDO.
   
   FIND TMSUser WHERE TMSUser.UserCode = icUserCode NO-LOCK NO-ERROR .
   IF AVAIL TMSUser THEN DO:
      /* check if limits amounts is defined individualy  */
      FIND FIRST UserLimit WHERE 
                 UserLimit.Brand = gcBrand AND
                 UserLimit.LimitTarget = "TMSUser" AND 
                 UserLimit.LimitType = ilimitType AND 
                 UserLimit.LimitTargetID = TMSUser.UserCode NO-LOCK NO-ERROR.
      IF AVAIL UserLimit THEN ldlimitAmt = Userlimit.limitAmt.
      ELSE DO:
          /* extract limit amount defined at group level  */
          FIND FIRST UserLimit WHERE 
                     UserLimit.Brand = gcBrand AND 
                     UserLimit.LimitTarget = "UserGroup" AND
                     UserLimit.LimitType = ilimitType AND
                     UserLimit.LimitTargetID = TMSUser.UserGroup 
                     NO-LOCK NO-ERROR.
          IF AVAIL UserLimit THEN ldlimitAmt = UserLimit.LimitAmt.
          ELSE RETURN -1. /* error value */
      END.
   END.
             
   RETURN ldlimitAmt.
   
END FUNCTION.

&ENDIF
