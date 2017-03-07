/* ddtrans.i      30.05.05/aam move processed file to archive
*/

{Func/ftransdir.i}

DEF VAR lcArchive  AS CHAR  NO-UNDO.

lcArchive = fCParamC("DDArchive").

PROCEDURE pDDAuthTrans:

   DEF INPUT PARAMETER icFile AS CHAR NO-UNDO.
   
   /* move the file to processed */
   IF lcArchive NE "" AND lcArchive NE ? AND SEARCH(icFile) NE ? THEN DO:
      fTransDir(icFile,
                "",
                lcArchive). 
   END.
                
END PROCEDURE.

