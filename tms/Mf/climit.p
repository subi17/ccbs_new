{Syst/commali.i}
{Func/tmsparam2.i}
{Func/function.i}

DEF VAR cfile AS c  NO-UNDO.
DEF VAR subj  AS c  NO-UNDO.
DEF VAR EMail AS c  NO-UNDO.
DEF VAR eadd  AS c  NO-UNDO.
DEF VAR Name  AS c  NO-UNDO.
DEF VAR zz    AS c  NO-UNDO.
DEF VAR zday  AS DA NO-UNDO.
DEF VAR i     AS i  NO-UNDO.
DEF VAR errd  AS c  NO-UNDO.

ASSIGN
   errd  = fChkPath(fCParamC("ErrLogDir"))
   EMail = fCParamC("CreditEmail")
   zday  = TODAY - 1.

IF EMail = ? OR errd = ? THEN RETURN.

FOR EACH InvGroup no-lock.

   ASSIGN
      Name = lc(errd                     + 
                InvGroup.InvGroup         +
                string(month(zday),"99") +
                string(day(zday),"99")   +
                ".log").

   IF search(Name) NE ? THEN DO:

      subj = "\"Credit Limit: " + InvGroup.InvGroup + " " +
                string(zday,"99-99-99") + " - Customer numbers" + "\"".

      DO i = 1 TO num-entries(EMail):
         eadd = entry(i,email).
         /* parameters FOR sending the EMail */
         zz = "/usr/bin/mailx -s " + subj  +        /* email's subject */
                               " " + eadd  +        /* EMail address   */
                             " < " + Name.          /* email's content */

         /* send EMail AS user hotb TO get the right sender */
         unix silent value("/usr/bin/su hotb -c '" + zz + "'").
      END.

   END.

END.
