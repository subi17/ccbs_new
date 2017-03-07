/* ----------------------------------------------------------------------
  MODULE .......: load_premium_numbers.p
  TASK .........: Load Premium Numbers
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 17.11.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
katun   = "Qvantel".

DEF VAR lcInputFile     AS CHAR NO-UNDO FORMAT "x(45)".
DEF VAR lcOutputFile    AS CHAR NO-UNDO FORMAT "x(45)".
DEF VAR lcRead          AS CHAR NO-UNDO.
DEF VAR lcBNumberPreFix AS CHAR NO-UNDO.
DEF VAR ldValidFrom     AS DATE NO-UNDO FORMAT "99-99-99".
DEF VAR ldValidTo       AS DATE NO-UNDO FORMAT "99-99-99".
DEF VAR liLineCount     AS INT  NO-UNDO.

DEF STREAM sin.
DEF STREAM sout.

FORM
   SKIP
   "Enter Input File Path :" lcInputFile SKIP
   "Enter Output File Path:" lcOutputFile
   WITH OVERLAY CENTERED ROW 10 TITLE " Load Premium Numbers " NO-LABELS
   FRAME finvrow.

UPDATE lcInputFile
       lcOutputFile with FRAME finvrow.

IF lcInputFile = "" THEN DO:
   MESSAGE "Please Enter Input File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcOutputFile = "" THEN DO: */

IF lcOutputFile = "" THEN DO:
   MESSAGE "Please Enter Output File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcOutputFile = "" THEN DO: */

ldValidTo = 12/31/49.

OUTPUT STREAM sout TO VALUE(lcOutputFile).

INPUT STREAM sin FROM VALUE(lcInputFile).

REPEAT TRANSACTION:

   IMPORT STREAM sin UNFORMATTED lcRead.

   IF lcRead = "" THEN NEXT.

   liLineCount = liLineCount + 1.

   IF NUM-ENTRIES(lcRead,"#") < 4 THEN DO:
      PUT STREAM sout UNFORMATTED
          lcRead CHR(9)
          "Invalid file format" SKIP.
      NEXT.
   END. /* IF NUM-ENTRIES(lcRead,"#") < 4 THEN DO: */

   ASSIGN lcBNumberPreFix = ENTRY(1,lcRead,"#") + ENTRY(2,lcRead,"#")
          ldValidFrom     = DATE(ENTRY(4,lcRead,"#")) NO-ERROR.
   
   IF ERROR-STATUS:ERROR OR ldValidFrom = ? THEN
      ldValidFrom = TODAY.

   IF CAN-FIND(FIRST PremiumNumber WHERE
                     PremiumNumber.Brand = gcBrand AND
                     PremiumNumber.BNumberPreFix = lcBNumberPreFix) THEN DO:
      PUT STREAM sout UNFORMATTED
          lcRead CHR(9)
          "This BNumberPreFix record already exists in system." SKIP.
      NEXT.
   END. /* IF CAN-FIND(FIRST PremiumNumber WHERE */

   CREATE PremiumNumber.
   ASSIGN PremiumNumber.Brand = gcBrand
          PremiumNumber.BNumberPreFix = lcBNumberPreFix
          PremiumNumber.OperatorName  = ENTRY(3,lcRead,"#")
          PremiumNumber.ValidFrom     = ldValidFrom
          PremiumNumber.ValidTo       = ldValidTo.

   PUT STREAM sout UNFORMATTED
       lcRead CHR(9)
       "Successfully loaded." SKIP.

END. /* REPEAT: */

PUT STREAM sout UNFORMATTED "Total records have been readed: " STRING(liLineCount).

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.
