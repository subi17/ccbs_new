/* -----------------------------------------------------------------------
  MODULE .......: ushell.p
  FUNCTION .....: Escape TO the operating system during a Progress session
  APPLICATION ..: nn
  CREATED ......: 21.01.99 
  changePVM ....: 
  Version ......: M15
  ----------------------------------------------------------------------- */

{commali.i}

DEF VAR defdir AS c NO-UNDO.
DO FOR TMSUser:
  FIND TMSUser where TMSUser.UserCode = katun no-lock.
  defdir = TMSUser.RepDir.
END.

if opsys = "unix" then unix value("(cd " + defdir + "; tcsh )").
ELSE dos.


PAUSE 0.
HIDE FRAME frm no-pause.
PAUSE 0 no-message.
HIDE MESSAGE.
