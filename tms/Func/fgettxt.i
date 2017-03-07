&IF "{&fGetTxt}" NE "YES"
&THEN

&GLOBAL-DEFINE fGetTxt YES
{Syst/commali.i}
{Func/timestamp.i}
{Func/date.i}
{Syst/tmsconst.i}

FUNCTION fGetSmsTS RETURNS DECIMAL.

   DEF VAR ldeCurrentTS AS DE NO-UNDO FORMAT "99999999.99999".
   DEF VAR ldeSeconds   AS DE NO-UNDO FORMAT "9.999999".
   DEF VAR liCompare    AS I  NO-UNDO.                  
   DEF VAR ldeNewStamp  AS DE NO-UNDO FORMAT "99999999.99999".  

   ASSIGN
      ldeCurrentTS = fmakets()
      ldeSeconds   = ldeCurrentTS - (INT(SUBSTRING(STRING(ldeCurrentTS),1,8)))
      liCompare    = ldeSeconds * 100000.

   IF    liCompare < 10  * 3600 THEN DO:
      ldeNewStamp = fmake2Dt(today, 10 * 3600).
   END.
   ELSE  IF liCompare > 21 * 3600 THEN DO:
      ldeNewStamp = fmake2Dt(today + 1, 10 * 3600).
   END.   
   ELSE DO:
      ldeNewstamp = ldeCurrentTS.
   END.

   RETURN ldeNewStamp.

END. 

FUNCTION _fGetTxt RETURNS CHAR
    (iTarget AS CHAR,
     iKey    AS CHAR,
     iDate   AS DATE,
     iLang   AS INT,
     OUTPUT ocSendRule AS CHAR,
     OUTPUT ocSender AS CHAR):

   DEF VAR oText AS CHAR NO-UNDO.   

   oText = "".

   /* with desired language */ 
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand AND 
             InvText.Target    = iTarget AND
             InvText.KeyValue  = iKey    AND
             InvText.FromDate <= iDate   AND
             InvText.ToDate   >= iDate   AND
             InvText.Language  = iLang:
      ASSIGN
         oText = invtext.invtext
         ocSendRule = invtext.sendrule
         ocSender = invtext.sender.
   END.

   /* if not available then get text with default language */
   IF oText = "" AND iLang NE 1 THEN 
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand AND 
             InvText.Target    = iTarget AND
             InvText.KeyValue  = iKey    AND
             InvText.FromDate <= iDate   AND
             InvText.ToDate   >= iDate   AND
             InvText.Language  = 1:

      ASSIGN
         oText = invtext.invtext
         ocSendRule = invtext.sendrule
         ocSender = invtext.sender.

      IF InvText.Target  = "SMS" THEN
      FOR FIRST RepText NO-LOCK WHERE
                RepText.Brand      = gcBrand AND 
                RepText.TextType   = 32 AND
                RepText.LinkCode   = STRING(InvText.ITNum) AND
                RepText.FromDate  <= iDate   AND
                RepText.ToDate    >= iDate   AND
                RepText.Language   = iLang:
         oText = RepText.RepText.
      END.
   END.


   RETURN otext.

END.

FUNCTION fGetTxt RETURNS CHAR
    (iTarget AS CHAR,
     iKey    AS CHAR,
     iDate   AS DATE,
     iLang   AS INT):

   DEF VAR lcText AS CHAR NO-UNDO. 
   DEF VAR lcSendRule AS CHAR NO-UNDO. 
   DEF VAR lcSender AS CHAR NO-UNDO. 

   lcText = _fGetTxt(iTarget,
                    iKey,
                    iDate,
                    iLang,
                    OUTPUT lcSendRule,
                    OUTPUT lcSender).
   RETURN lcText.

END.

FUNCTION fGetSMSSendRule RETURNS CHAR
    (iKey    AS CHAR,
     iDate   AS DATE,
     iLang   AS INT,
     OUTPUT ocSender AS CHAR):
   
   DEF VAR lcSendRule AS CHAR NO-UNDO.
   
   _fGetTxt("SMS",
            iKey,
            iDate,
            iLang,
            OUTPUT lcSendRule,
            OUTPUT ocSender).

   RETURN lcSendRule.
END.

FUNCTION fGetSMSTxt RETURNS CHAR
    (iKey    AS CHAR,
     iDate   AS DATE,
     iLang   AS INT,
     OUTPUT odeSendTime AS DEC):
   
   DEF VAR lcText AS CHAR NO-UNDO. 
   DEF VAR lcSendRule AS CHAR NO-UNDO.
   DEF VAR lcSender AS CHAR NO-UNDO. 
   
   lcText = _fGetTxt("SMS",
                    iKey,
                    iDate,
                    iLang,
                    OUTPUT lcSendRule,
                    OUTPUT lcSender).
   
   IF lcText > "" THEN CASE lcSendRule:
      WHEN {&SMS_SENDRULE_24H} OR WHEN {&SMS_SENDRULE_24H_EXCEPT_LAST_DAY} 
         THEN odeSendTime = fmakets().
      WHEN {&SMS_SENDRULE_OFFICEH} OR WHEN {&SMS_SENDRULE_OFFICEH_EXCEPT_LAST_DAY}
         THEN odeSendTime = fGetSMSTS().
      OTHERWISE odeSendTime = fGetSMSTS().
   END.
   
   RETURN lcText.

END.

&ENDIF
