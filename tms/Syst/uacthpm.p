/* -----------------------------------------------------
  MODULE .......: UACTHPM.P
  FUNCTION .....: Activate a HP Macro on HP Style Laser
  AUTHOR .......: PT
  CREATED ......: 23.05.1999
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

DEF INPUT PARAMETER PrintDevice  AS c NO-UNDO.
DEF INPUT PARAMETER MacroFile    AS c NO-UNDO.

DEF VAR i        AS i NO-UNDO.
DEF VAR pdev     AS c NO-UNDO.


IF search(MacroFile) = ? THEN DO:
   BELL.
   MESSAGE 
   "System Error: HP-Laser Macro File" SKIP
   MacroFile "was not found !"         SKIP
   VIEW-AS ALERT-BOX error
   title " UNKNOWN FORM / MACRO ".
END.
ELSE DO:

   if opsys = "unix" THEN DO:

      /* use spooler. extract device's name from STRING 'lp{tr} -Pxxxx' */
      i = index(PrintDevice,"-P").
      IF i > 0 THEN pdev = substr(PrintDevice,i).

      /* IF spooler is NOT used, assume that PrintDevice can be used 'as is' */
      ELSE pdev = PrintDevice.

      unix silent value("lpr " + pdev + " " + MacroFile).                           END.

   ELSE DO:

      i = index(PrintDevice," ").
      IF i = 0 THEN 
      i = index(PrintDevice,"-P").

      IF i > 0 THEN DO:
         MESSAGE 
         "Warning: The printer You have choosen" SKIP
         "seems not to have a DOS/WIN compatible"  SKIP
         "device name: '" + PrintDevice + "' -"  
         VIEW-AS ALERT-BOX error TITLE
         "Form (macro) activation is omitted".

      END.
      ELSE DO:
         /* on msdos  AND Windows environment use PrintDevice AS is */
         dos silent value("copy/b " + MacroFile + " " + PrintDevice). 
      END.   
   END.

END.

