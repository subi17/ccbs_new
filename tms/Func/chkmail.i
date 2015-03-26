
/* Is this e-mail line erroneous 
   11.04.2000 jp                          */

FUNCTION fIsEmail RETURNS INTEGER
(INPUT line AS CHAR, OUTPUT ret AS INTEGER):

DEF VAR i    AS i NO-UNDO.
DEF VAR hit  AS i NO-UNDO.
DEF VAR was AS i NO-UNDO.
   i = 1.

   DO i = i TO length(line):
      IF      substring(line,i,1) = "@" THEN hit = hit + 1.
      ELSE IF substring(line,i,1) = "," THEN was = was + 1.
   END.

   ret = hit - was.
   RETURN ret.

END.



