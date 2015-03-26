def input param icFile as char no-undo.
def input param icLog as char no-undo.
def input param icOldStock as CHAR no-undo.
def input param icNewStock as CHAR no-undo.
def input param ilSimulate as log no-undo.

DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO.

def stream sin.
input stream sin from value(icFile).

def stream slog.
output stream slog to value(icLog).

icc_loop:
repeat:

   import stream sin unformatted lcFile.

   lcFile = TRIM(lcFile).

   FIND sim where sim.icc = lcFile NO-LOCK NO-ERROR.
 
   IF LENGTH(lcFile) NE 19 THEN 
   DO:
      MESSAGE "Could not trim input line " lcFile 
        " to contain only 19 chars as ICC"
        VIEW-AS ALERT-BOX.
      RETURN.
   END.

   IF not avail sim THEN DO:
      put stream slog unformatted lcFile " NOT FOUND" SKIP.
      next icc_loop.
   END.
         
   IF sim.Stock eq icOldStock THEN DO:
         put stream slog unformatted 
            sim.icc " " sim.simstat " " sim.stock "->" icNewStock skip.

         IF NOT ilSimulate THEN
         DO:
            find current sim EXCLUSIVE-LOCK.
            assign sim.Stock = icNewStock.
         END.
   END.
   ELSE  
       put stream slog unformatted "sim with icc " sim.icc " was not in stock " 
             icOldStock " as expected, but in stock " sim.stock SKIP.
END.
output stream slog close.
input stream sin close. 
