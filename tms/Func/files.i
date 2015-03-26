/* files.i     04.09.08/aam
*/

DEF STREAM sGetFiles.

FUNCTION fHasDirectoryFiles RETURNS LOGICAL
   (INPUT icDir   AS CHAR,
    INPUT icFiles AS CHAR):

   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR llFiles    AS LOG  NO-UNDO.
 
   llFiles = FALSE.

   INPUT STREAM sGetFiles THROUGH VALUE("ls -1 " + icDir + "/" + icFiles).

   REPEAT:
      IMPORT STREAM sGetFiles UNFORMATTED lcFileName.

      IF lcFileName = "" OR
         lcFileName MATCHES ("*o such file or*")
      THEN NEXT.

      llFiles = TRUE.
      LEAVE.
   END.

   INPUT STREAM sGetFiles CLOSE.
   
   RETURN llFiles.

END FUNCTION.
