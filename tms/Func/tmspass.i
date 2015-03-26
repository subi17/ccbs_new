&IF "{&TMSPASS_I}" NE "YES" &THEN
&GLOBAL-DEFINE TMSPASS_I YES

/* ----------------------------------------------------------------------
  MODULE .......: tmspass.i
  TASK .........: functions to generate and check tmspass
  APPLICATION ..: TMS 
  AUTHOR .......: mvi
  CREATED ......: 10.01.07
  CHANGED ......: 02.06.07 mvi added comments, Yoigo version
                  

  Version ......: Yoigo
---------------------------------------------------------------------- */



/** Check if password complies to the password rules.
 * 1. Must be at least 8 chars
 * 2. Must be at maximum 16 chars
 * 3. Must contain an upper-case letter
 * 4. Must contain a number
 *
 * @param icPass password to check for validity
 * @returns 0 when ok, 1 wrong length, 2 no number or uppercase letter
 */ 
FUNCTION fCheckPassword RETURN INT
(icPass AS CHAR):
   
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE c AS INTEGER NO-UNDO. 
   DEFINE VARIABLE hasUpperCase AS LOGICAL NO-UNDO INIT FALSE.
   DEFINE VARIABLE hasNumber AS LOGICAL NO-UNDO INIT FALSE. 
  
   IF LENGTH(icPass) < 8 OR
      LENGTH(icPass) > 16 THEN RETURN 1.

   /* go thru all chars in password */
   DO i = 1 TO LENGTH(icPass):
      c = ASC(SUBSTRING(icPass,i,1)).
      /* check valid chars */
      IF NOT ((c >= ASC("a") AND c <= ASC("z")) OR 
              (c >= ASC("A") AND c <= ASC("Z")) OR
              (c >= ASC("0") AND c <= ASC("9")) OR
              (c EQ ASC("_"))) THEN RETURN 1.
      
      IF c >= ASC("A") AND c <= ASC("Z") THEN DO:
         hasUpperCase = TRUE.
      END.
      ELSE IF c >= ASC("0") AND c <= ASC("9") THEN DO:
         hasNumber = TRUE.
      END.
      
      /* one char can exists only 3 times in a password */
      IF NUM-ENTRIES(icPass,CHR(c)) > 4 THEN RETURN 2.
   END.

   /* 0 = valid password */ 
   IF hasNumber AND hasUpperCase THEN RETURN 0.
   
   RETURN 2. 
END FUNCTION.


/** Generate a valid random password 
 * 
 * @param iiLength how many chars in password
 * @param ilSafe, should similar looking chars like "l,1,o,O,0" be avoided
 * @returns char; valid password
 * @author mvi
 */
FUNCTION fGeneratePassword RETURN CHAR
(iiLength AS INTEGER,
 ilSafe AS LOGICAL):
   DEFINE VARIABLE lcPass AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liCount AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DEFINE VARIABLE c AS INTEGER NO-UNDO. 
   DEFINE VARIABLE a AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcRandom AS CHARACTER NO-UNDO.
  
   /* try to mess up random generator a bit */
   INPUT THROUGH "echo $RANDOM".
   IMPORT lcRandom.
   DO i = 1 TO INT(lcRandom).         
      RANDOM(1,100).
   END.
  
   /* generate the password */
   REPEAT:
      a = a + 1.
      lcPass = "".
      DO liCount = 1 TO iiLength:
         INPUT THROUGH "echo $RANDOM".
         IMPORT lcRandom.
         i = INT(lcRandom).         
         IF i > 16000 THEN c = RANDOM(48,57).
         ELSE IF i > 8000 THEN c = RANDOM(65,90).
         ELSE c = RANDOM(97,122).
          
         lcPass = lcPass + CHR(c). 
      END.

      /* give option to generate invalid, shorter or longer sequences */
      IF iiLength < 8 OR iiLength > 16 THEN RETURN lcPass.
     
      /* safe mode replaces chars that look similar and cause typos */
      IF ilSafe THEN DO:
         lcPass = REPLACE(lcPass,"I","k"). /* uppercase "i" */
         lcPass = REPLACE(lcPass,"S","q"). /* uppercase "s" */
         lcPass = REPLACE(lcPass,"i","R"). /* lowercase "i" */
         lcPass = REPLACE(lcPass,"O","T"). /* uppercase "o" */
         lcPass = REPLACE(lcPass,"l","L"). /* lowercase  "L" */
         lcPass = REPLACE(lcPass,"0","3"). /* zero */
         lcPass = REPLACE(lcPass,"1","X"). /* one */
         lcPass = REPLACE(lcPass,"5","H"). /* five */
      END.
     
      /* but otherwise be sure it's valid password */
      IF fCheckPassword(lcPass) = 0 THEN  
         RETURN lcPass.
      
      /* if we fail to generate the password in reasonable time */
      IF a > 100000 THEN RETURN ?.
      
   END.   
END FUNCTION.

&ENDIF

