/*------------------------------------------------------
  MODULE .......: NNRUN.P
  TASK .........: RUN a module
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 11.10.1997
  MODIFIED .....: 22.10.1998 kl - save into swe-r
                  04.02.1999 kl - save into ../r
                  03.08.2004/aam  longer module name
  Version ......: M15
--------------------------------------------------------------------------- */

{commali.i}

def var module as c  format "x(30)"   NO-UNDO.
def var ok     as lo format "Yes/No" NO-UNDO.
form
   module    label "Module ...."
             help  "Name of the program module You want to run"                          validate( input module = ""
             or search(input module + ".r") NE ?
             or search(input module + ".p") NE ?,
             "Unknown module !")
WITH
    overlay row 6 centered side-labels title " RUN " FRAME modu.


PAUSE 0.
ehto = 9. RUN ufkey.
UPDATE module WITH FRAME modu.
if module ne "" THEN DO:

   if search(module + ".p") NE ? THEN DO:
      BELL.
      message "Do you want to compile this module before run (Y/N) ?"
      UPDATE ok.

      if ok then compile value(search(module + ".p")) save into ../r.
   END.
   HIDE FRAME modu.
   RUN value(module).
END.
HIDE FRAME modu.

