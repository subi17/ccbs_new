DEF VAR emsg AS c NO-UNDO.
DEF VAR esub AS c NO-UNDO.

FUNCTION fMailx RETURNS LOG
  (INPUT emsg AS CHAR,
   INPUT esub AS CHAR).

   unix silent value("mailx -s " + esub + " " + emsg + 
                     " < /dev/null > /dev/null").

END.

