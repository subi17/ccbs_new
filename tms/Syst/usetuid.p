/* ----------------------------------------------------------------------
  MODULE .......: usetuid.p
  TASK .........: Sets default user id FOR TMS login
  APPLICATION ..: nn
  CREATED ......: 18.12.2001 pt
  CHANGED ......: 
  VERSION ......: M15
 ---------------------------------------------------------------------- */

DEF OUTPUT PARAMETER katun AS C  NO-UNDO.

DEF VAR whoami AS C NO-UNDO.


IF OPSYS = "unix" THEN DO:
   INPUT THRU "who am i".
   IMPORT UNFORMATTED whoami.
   INPUT CLOSE.

   /*  modify certain userid FORMAT */
   IF INDEX(whoami,"!") > 0 THEN whoami = TRIM(ENTRY(2,whoami,"!")).
   /* cut away other fields after userid */
   whoami = ENTRY(1,whoami," ").

   /* special case: susbtitute UNIX login IF root OR starnet */
   IF LOOKUP(whoami,"root,starnet") > 0 THEN katun = "eka".
   ELSE katun = whoami.
END.
ELSE katun = "".   


