DEF STREAM sIn.

DEF VAR lcICC AS CHAR NO-UNDO.

INPUT STREAM sIn FROM "ydr-2342-simart.txt".

REPEAT:
   lcICC = "".

   IMPORT STREAM sIn UNFORMATTED lcICC.

   IF lcICC = "" THEN NEXT.

   FIND FIRST Sim EXCLUSIVE-LOCK WHERE
              Sim.Brand = "1" AND
              Sim.ICC   = lcICC NO-ERROR.
   IF AVAIL Sim THEN DO:
      ASSIGN Sim.SimArt = "universal_orange".
   END.
END.

INPUT STREAM sIn CLOSE.
