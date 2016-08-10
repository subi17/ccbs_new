/*Program moves google billing related logs and output files to target 
directories.*/
{commpaa.i}
{gbilling.i}
{ftransdir.i}
{eventlog.i}
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF STREAM sFile.

gcBrand = "1".
fInitGBParameters().

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcGBSpoolDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcGBSpoolDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      /*Accept only activation files*/
      IF NOT lcFileName BEGINS "es_yoigo-" THEN NEXT.
      /*do not move file that is generated today*/
      IF lcFilename MATCHES("*" + STRING(INT(fMakeTS())) + "*") THEN NEXT.
      /*check date*/

   END.
   ELSE NEXT.

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).
   
   IF INDEX(lcInputFile, "result") > 0 THEN
      fMove2TransDir(lcInputFile, "", lcGBOutDir). 
   ELSE
      fMove2TransDir(lcInputFile, "", lcGBLogDir).

   IF SESSION:BATCH AND lcInputFile NE "" THEN
      fBatchLog("FINISH", lcInputFile).
END.


