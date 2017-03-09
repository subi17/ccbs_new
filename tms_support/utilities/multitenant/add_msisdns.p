/* Script to add new MSISDN numbers
   Change liBegin and liEnd input values to needed ones 
   MasMovil range specified to 72260 */
{Syst/commpaa.i}
katun = "qvantel".
gcbrand = "1".
{Func/timestamp.i}
{Func/msisdn.i}
{Func/multitenantfunc.i}
/* Create free MSISDNs starting from begin to end. */

DEFINE VARIABLE liBegin AS INT NO-UNDO INIT 722600000.
DEFINE VARIABLE liEnd   AS INT NO-UNDO INIT 722600100.


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

   /* !!! ANALYSED EARLIER !!! */
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

   /* 11.8 Others, multiple combinations: 722 XXX 227 */
   ELSE IF (SUBSTR(pcMSISDN,1,3) = "722"                 AND
            SUBSTR(pcMSISDN,7,3) = "227")                AND
           (SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,4,1)  AND
            SUBSTR(pcMSISDN,4,1) = SUBSTR(pcMSISDN,5,1)) THEN
      liGoldNumber = 1.
   ELSE IF SUBSTR(pcMSISDN,1,4) = "7228" AND
           SUBSTR(pcMSISDN,6,4) = "8227" THEN liGoldNumber = 1.


   RETURN liGoldNumber.

END FUNCTION.

define variable ldenow    as decimal no-undo.

define stream slog.
output stream slog to "added_msisdns.log" append.

ldenow = fmakets().
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ld AS INTEGER NO-UNDO.
DEFINE VARIABLE lcMSISDN AS CHARACTER NO-UNDO.
DEFINE VARIABLE liGold AS INTEGER NO-UNDO.

/* Check inputs */
IF liEnd < liBegin THEN
   MESSAGE "ERROR: " liBegin " is bigger than " liEnd VIEW-AS ALERT-BOX.
IF liBegin LT 722600000 AND liEnd GE 722600000 THEN
   MESSAGE "ERROR: " liBegin " and " liEnd " in different tenant areas" 
   VIEW-AS ALERT-BOX.
IF liBegin GE 722600000 AND liEnd GE 722610000 THEN
   MESSAGE "ERROR: " liBegin " and " liEnd " in different tenant areas"
   VIEW-AS ALERT-BOX.

/* Set tenant based on MB-93 description */
IF STRING(liBegin) BEGINS "72260" THEN DO:
   IF NOT fsetEffectiveTenantForAllDB("TMasMovil") THEN DO:
      MESSAGE "Tenant change to Masvmovil failed" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   MESSAGE "Add MasMovil MSISDNs " liBegin " to " liEnd VIEW-AS ALERT-BOX.
END.
ELSE DO:
   IF NOT fsetEffectiveTenantForAllDB("Default") THEN DO:
      MESSAGE "Tenant change to Default failed" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   MESSAGE "Add Yoigo MSISDNs " liBegin " to " liEnd VIEW-AS ALERT-BOX.
END.

each_loop:

DO ld = liBegin TO liEnd TRANS:

   i = i + 1.
/*   if i <= 20 then next.
   if i > 20 then leave.   */

   if i mod 100 = 0 then do:
      disp i with frame a.
      pause 0.
   end.

   lcMSISDN = STRING(ld).

   liGold = fGoldNumber(lcMSISDN).
   if liGold eq 1 then
      put stream slog unformatted lcMSISDN skip.
  
   find msisdnnumber where
        msisdnnumber.cli = lcMSISDN NO-LOCK no-error.
   IF AVAIL msisdnnumber then next.

   find first msisdn where
              msisdn.cli = lcMSISDN NO-LOCK no-error.
   IF AVAIL msisdn then next.

   find first mobsub where
              mobsub.cli = lcMSISDN NO-LOCK no-error.
   IF AVAIL mobsub then next.

   liGold = fGoldNumber(lcMSISDN).

   CREATE MSISDNNumber.
   ASSIGN
      MSISDNNumber.CLI = lcMSISDN
      MSISDNNumber.Rank = liGold.

   CREATE MSISDN.
   ASSIGN
      MSISDN.Brand      = gcBrand
      MSISDN.CLI        = msisdnnumber.cli
      MSISDN.ValidFrom  = ldeNow
      MSISDN.POS        = "ONLINE"
      MSISDN.Statuscode = (IF liGold eq 0 THEN 1 ELSE 0)
      MSISDN.ValidTo    = 99999999.99999
      MSISDN.ActionDate = Today.

   put stream slog unformatted

      MSISDN.cli "|"
      MSISDN.pos "|"
      MSISDNnumber.rank

      lcMSISDN "|"
      liGold
 
      skip.

   release msisdn.
   release msisdnnumber.

  /*  put stream slog unformatted lcMSISDN skip. */
end.
disp i.

output stream slog close.
