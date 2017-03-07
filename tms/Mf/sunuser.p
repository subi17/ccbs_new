
{Syst/testpaa.i}

FUNCTION fChkUser RETURNS logical
  (INPUT  login AS CHAR, INPUT  comment AS CHAR,
   OUTPUT dbll  AS CHAR, OUTPUT dbln    AS CHAR).

   DEF VAR ret    AS lo NO-UNDO.
   DEF VAR whoami AS c  NO-UNDO.

   input through "cat /etc/passwd".
   LOOP:
   repeat:
      IMPORT UNFORMATTED whoami.
      if entry(1,whoami,":") = login   OR  
         entry(5,whoami,":") = comment THEN
            ASSIGN
               ret  = TRUE
               dbll = entry(1,whoami,":").
               dbln = entry(5,whoami,":").
      IF ret THEN LEAVE LOOP.
   END.
   INPUT CLOSE.

   RETURN ret.

END. /* FUNCTION fChkUser */

def var comment as c  no-undo format "x(30)".
DEF VAR login   AS c  NO-UNDO.
DEF VAR homedir AS c  NO-UNDO.
DEF VAR shell   AS c  NO-UNDO.
DEF VAR skele   AS c  NO-UNDO.
DEF VAR useradd AS c  NO-UNDO.
DEF VAR whoami  AS c  NO-UNDO.
DEF VAR dbln    AS c  NO-UNDO.
DEF VAR dbll    AS c  NO-UNDO.

def var bOk     as lo no-undo format "Yes/No".

form                                   skip(1)
   "   LogIn Name ..:"   login         SKIP
   "   Full Name ...:"   comment  "  " skip(1)
with centered row 5 no-labels title " Add a SUN user " FRAME adduser.

/* WhoAmI ? */
INPUT THROUGH whoami.
IMPORT UNFORMATTED whoami.
INPUT CLOSE.

/* IF I'm NOT root ... */
if whoami ne "root" THEN DO:
   message " Only authorized users can use this feature ! "
      view-as alert-box title "  NOTICE  ".
   RETURN.
END.

/* default values */
ASSIGN
   homedir = "/usr/users/"
   skele   = homedir + "default"
   shell   = "/usr/local/bin/tcsh".

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   UPDATE 
      login
      comment
   WITH FRAME adduser EDITING:

      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "login" THEN DO:
            if input login = "" THEN RETURN.
         END.
      END.

      APPLY LASTKEY.

   END.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         bOk = FALSE.
         message "Are you SURE you want to add a user (Y/N) ?" UPDATE bOk.
         IF bOk THEN LEAVE task.
      END.
   END.


   IF fChkUser(INPUT  login, INPUT  comment,
               OUTPUT dbll,  OUTPUT dbln)    THEN DO:
      message " Login:" dbll "-" dbln SKIP
              " already exists !" 
         view-as alert-box title " ERROR ".
   END.
   ELSE DO:
      ASSIGN 
         homedir = homedir + login
         useradd = "useradd"         +
                   " -c '" + comment +
                   "' -s " + shell   +
                   " -d "  + homedir +
                   " -m "  +
                   " -k "  + skele   +
                   " "     + login.

      OUTPUT TO /dev/null.
      UNIX SILENT value(useradd).
      OUTPUT CLOSE.

   END.

   IF NOT fChkUser(INPUT  login, INPUT  comment,
                   OUTPUT dbll,  OUTPUT dbln)    THEN DO:

      message " For some reason user was not added ..."
         view-as alert-box title " ERROR ".

   END.
   ELSE DO:
      message " User" comment "was added." SKIP 
              "Use now the 'passwd' Command for the login !" SKIP
              "It has to be used in UNIX prompt."
         view-as alert-box title " LOGIN " + login + " ".
   END.

END.



