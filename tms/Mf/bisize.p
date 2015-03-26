{commali.i}
{excel.i}
{cparam2.i}

OUTPUT TO /dev/null.

DEF STREAM email.

DEF VAR line   AS c NO-UNDO.
def var sep    as c no-undo init " ".
DEF VAR i      AS i NO-UNDO.
DEF VAR fsize  AS i NO-UNDO.
DEF VAR epath  AS c NO-UNDO.
DEF VAR eadd   AS c NO-UNDO.
DEF VAR msg    AS c NO-UNDO.
DEF VAR subj   AS c NO-UNDO.
DEF VAR email  AS c NO-UNDO.
DEF VAR zz     AS c NO-UNDO.
DEF VAR error  AS i NO-UNDO.
DEF VAR bifile AS c NO-UNDO.
DEF VAR bisize AS i NO-UNDO.
DEF VAR g%     AS i NO-UNDO.

DEF STREAM bifile.

ASSIGN
   bifile = fCParamC("BiFile")
   bisize = fCParamI("BiSize")
   email  = fCParamC("OnLineEmail").

input  stream bifile through value("ls -l " + bifile).
IMPORT STREAM bifile UNFORMATTED line.

DO i = 1 TO num-entries(line,sep).
   if substr(entry(i,line,sep),1,1) >= "0" AND
      substr(entry(i,line,sep),1,1) <= "9" AND
      length(entry(i,line,sep)) >= 5 THEN fsize = int(entry(i,line,sep)).
   IF fsize NE 0 THEN LEAVE.
END.

/* FOR AUTOMATIC WHEN READY
IF      fsize >= 2000000000 THEN ASSIGN msg =
   "Before image file (" + bifile + ") was too big: "       +
   string(fsize,"9999999999") + " bytes."                   + my-nl +
   "Database service function is being run automatically."  + my-nl +
   my-nl + "CHECK IF OnLine READING HAS RESTARTED SUCCESFULLY "     +
   "WITHIN 15 MINUTES !"
   error = 2.
ELSE IF fsize >= 1800000000 THEN ASSIGN msg =
   "Before image file (" + bifile + ") is growing too big: "        +
   string(fsize,"9999999999") + " bytes."                           + my-nl +
   "Database service function should be run as soon as possible"    + my-nl +
   "Contact StarNet Systems to do the work !"
   error = 1.
   ----------- */   

IF      fsize >= 2000000000 THEN ASSIGN msg =
   "Before image file (" + bifile + ") was too big: "         +
   string(fsize,"9999999999") + " bytes."                     + my-nl +
   "OnLine reader has to be stopped NOW !"                    + my-nl + 
   "Database service function has to be run "                 +
   "before starting it again."                                + my-nl +
   "Contact StarNet Systems to do the work !"
   error = 3.
ELSE IF fsize >= 1800000000 THEN ASSIGN msg =
   "Before image file (" + bifile + ") is growing too big: "      +
   string(fsize,"9999999999") + " bytes."                         + my-nl +
   "Database service function should be run as soon as possible"  + my-nl +
   "Contact StarNet Systems to do the work !"
   error = 3.
ELSE IF fsize >= 1500000000 THEN ASSIGN msg =
   "Before image file (" + bifile + ") is growing too big: "      +
   string(fsize,"9999999999") + " bytes."                         + my-nl +
   "Please follow the growth of the file to avoid a crash !"      + my-nl +
   "Maximum size is 2.1 GB"
   error = 1.

/* BI file hasn't grown, no messages */
IF bisize = fsize THEN error = 0.

IF error > 0 THEN DO:

   epath = "/home/nn/cron/email2.tmp".      

   /*
   msg = "Before image file (/tempo/nndb/nnm.b1) was too big: "   +
         string(fsize,"999999999") + " bytes."                    + my-nl +
         "Database service function is being run automatically."  + my-nl +
         my-nl + "Restart OnLine reading after 15 minutes !".
   */

   OUTPUT STREAM email TO value(epath).
   PUT STREAM email UNFORMATTED msg my-nl my-nl.
   OUTPUT STREAM email CLOSE.
   unix silent value("nnmusers >> " + epath).

   case error:
      when 1 then subj = "\"MESSAGE: BeforeImageFile Getting Big !\"".
      when 2 then subj = "\"WARNING: BeforeImageFile GROWING BIG !\"".
      when 3 then subj = "\"ERROR: BeforeImageFile IS TOO BIG !\"".
   END.

   DO i = 1 TO num-entries(email):

      eadd = entry(i,email).

      /* parameters FOR sending the email */
      zz = "mailx -s " + subj  +        /* email's subject */
                   " " + eadd  +        /* email address   */
                 " < " + epath.         /* email's content */

      /* send email AS user hotb TO get the right sender */
      unix silent value("su hotb -c '" + zz + "'").

   END.
   /*
   IF error = 2 THEN DO:
      /* CLOSE DOWN the database immediatelly */
      zz = "proshut /home2/nndb/nnm -by > /dev/null".
      UNIX SILENT value(zz).

      /* truncate (empty) THEN before image file */
      zz = "proutil /home2/nndb/nnm -C truncate -G 10 > /dev/null".
      UNIX SILENT value(zz).

      /* restart the HotBilling */
      UNIX SILENT cdronline.
   END.
   */

END.


