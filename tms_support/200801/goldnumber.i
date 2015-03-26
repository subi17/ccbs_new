/* ----------------------------------------------------------------------------
  MODULE .......: GOLDNUMBER.P
  FUNCTION .....: Set Gold Number flag
  APPLICATION ..: TMS
  CREATED ......: 25.10.2006 KL
  CHANGED ......:

  Version ......: XFERA
  --------------------------------------------------------------------------- */

DEFINE VARIABLE lcMSISDN AS CHARACTER NO-UNDO.

DEF VAR ld AS DEC NO-UNDO.

FUNCTION fGoldNumber RETURNS INTEGER
  (INPUT pcMSISDN AS CHARACTER):

   DEFINE VARIABLE liGoldNumber AS INTEGER NO-UNDO.
   
   /* 1. Same number from up and down, groups of two */
   IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,5,1) AND
      SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,7,1) AND
      SUBSTR(pcMSISDN,8,1) = SUBSTR(pcMSISDN,9,1) THEN 
      liGoldNumber = 1.

   /* 2.1 Same three numbers at the beginning and at the end */
   ELSE IF SUBSTR(pcMSISDN,1,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,2,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,3,1) = SUBSTR(pcMSISDN,7,1) THEN
      liGoldNumber = 1.

   /* 2.2 Same three numbers at the beginning and at the end */
   ELSE IF SUBSTR(pcMSISDN,1,1) = SUBSTR(pcMSISDN,7,1) AND
           SUBSTR(pcMSISDN,2,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,3,1) = SUBSTR(pcMSISDN,9,1) THEN
      liGoldNumber = 1.

   /* 3 Same numbers from up and down, groups of three */
   ELSE IF (SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,5,1)  AND
            SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,6,1)) AND
           (SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,8,1)  AND
            SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,9,1)) THEN
      liGoldNumber = 1.

   /* 4. 4 last numbers are the same, groups of four */
   ELSE IF SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,8,1) = SUBSTR(pcMSISDN,9,1) THEN
      liGoldNumber = 1.

   /* 5. 4 digit numbers after 622 or 633 are the same, groups of four */
   ELSE IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,7,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,7,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,7,1) THEN
      liGoldNumber = 1.
   
   /* 6. 4 digit numbers in the middle of free six digit */
   ELSE IF SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,8,1) THEN
      liGoldNumber = 1.

   /* 7. 5 last number are the same, groups of five */
   ELSE IF SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,8,1) = SUBSTR(pcMSISDN,9,1) THEN
      liGoldNumber = 1.

   /* 8. 5  digit numbers after 622 or 633 are the same, groups of five */
   ELSE IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,8,1) THEN
      liGoldNumber = 1.
   
   /* 9. 6 last numbers are the same */
   ELSE IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,7,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,8,1) = SUBSTR(pcMSISDN,9,1) THEN
      liGoldNumber = 1.
   
   /* 10.1. 6 digits with proportional increase */
   ELSE IF INT(SUBSTR(pcMSISDN,5,1)) = INT(SUBSTR(pcMSISDN,4,1)) + 1 AND
           INT(SUBSTR(pcMSISDN,6,1)) = INT(SUBSTR(pcMSISDN,5,1)) + 1 AND
           INT(SUBSTR(pcMSISDN,7,1)) = INT(SUBSTR(pcMSISDN,6,1)) + 1 AND
           INT(SUBSTR(pcMSISDN,8,1)) = INT(SUBSTR(pcMSISDN,7,1)) + 1 AND
           INT(SUBSTR(pcMSISDN,9,1)) = INT(SUBSTR(pcMSISDN,8,1)) + 1 THEN
      liGoldNumber = 1.

   /* 10.2. 6 digits with proportional decrease */
   ELSE IF INT(SUBSTR(pcMSISDN,5,1)) = INT(SUBSTR(pcMSISDN,4,1)) - 1 AND
           INT(SUBSTR(pcMSISDN,6,1)) = INT(SUBSTR(pcMSISDN,5,1)) - 1 AND
           INT(SUBSTR(pcMSISDN,7,1)) = INT(SUBSTR(pcMSISDN,6,1)) - 1 AND
           INT(SUBSTR(pcMSISDN,8,1)) = INT(SUBSTR(pcMSISDN,7,1)) - 1 AND
           INT(SUBSTR(pcMSISDN,9,1)) = INT(SUBSTR(pcMSISDN,8,1)) - 1 THEN
      liGoldNumber = 1.

   /* 10.3. 6 digits with proportional increase, 8th digit 9, 9th digit 0 */
   ELSE IF SUBSTR(pcMSISDN,4,6)      = "567890" THEN liGoldNumber = 1.

   /* 10.4. 6 digits with proportional decrease, 4th digit 0, 5th digit 9 */
   ELSE IF SUBSTR(pcMSISDN,4,6)      = "098765" THEN liGoldNumber = 1.

   /* 11.1 Others, multiple combinations: 010 101 */
   ELSE IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,6,1) AND
           SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,7,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,9,1) THEN
      liGoldNumber = 1.

   /* 11.2 Others, multiple combinations: 989 989 */
   ELSE IF SUBSTR(pcMSISDN,4,3) = SUBSTR(pcMSISDN,7,3) THEN
      liGoldNumber = 1.
   
   /* 11.3 Others, multiple combinations: 147 741 */
   ELSE IF SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,9,1) AND
           SUBSTR(pcMSISDN,5,1) = SUBSTR(pcMSISDN,8,1) AND
           SUBSTR(pcMSISDN,6,1) = SUBSTR(pcMSISDN,7,1) THEN
      liGoldNumber = 1.

   /* !!! ANALYSED EARLIER !!!
   /* 11.4 Others, multiple combinations: 6222 X 2226 */
   ELSE IF SUBSTR(pcMSISDN,1,4) = "6222" AND
           SUBSTR(pcMSISDN,6,4) = "2226" THEN liGoldNumber = 1.

   /* 11.5 Others, multiple combinations: 6333 X 3336 */
   ELSE IF SUBSTR(pcMSISDN,1,4) = "6333" AND
           SUBSTR(pcMSISDN,6,4) = "3336" THEN liGoldNumber = 1.

   /* 11.6 Others, multiple combinations: 622 XXX 226 */
   ELSE IF (SUBSTR(pcMSISDN,1,3) = "622"                 AND
            SUBSTR(pcMSISDN,7,3) = "226")                AND
           (SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,4,1)  AND
            SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,5,1)) THEN
      liGoldNumber = 1.

   /* 11.7 Others, multiple combinations: 633 XXX 336 */
   ELSE IF (SUBSTR(pcMSISDN,1,3) = "633"                 AND
            SUBSTR(pcMSISDN,7,3) = "336")                AND
           (SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,4,1)  AND
            SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,5,1)) THEN
      liGoldNumber = 1.
   */

   RETURN liGoldNumber.

END FUNCTION.
/*
etime(yes).

OUTPUT TO ../KooAa/gn.txt.
DO ld = 622000000 TO 622999999:
   lcMSISDN = STRING(ld).
   PUT UNFORMATTED lcMSISDN " " fGoldMember(lcMSISDN) CHR(10).
END.
OUTPUT CLOSE.

message etime. */ 


