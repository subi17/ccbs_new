/* eventlog.i

   changes:      13.12.05/aam date to each line
*/
   
&IF "{&eventlogDef}" NE "YES"
&THEN

&GLOBAL-DEFINE eventlogDef YES
   
{commali.i}
{excel.i}
{cparam2.i}

def var fpath as c no-undo.
fpath = fCparamC("EventlogDir").
 
if fpath = ? then fpath = "/tmp/".
 
FUNCTION fELog RETURN LOG
  (cUser AS CHAR,cLine AS CHAR).

   DEF VAR i     AS i NO-UNDO.

   OUTPUT STREAM excel TO 
      value(fpath + 
            string(day(today),"99")    + 
            string(month(today),"99")  +
            string(year(today),"9999") + ".log") append.

   i = index(cLine," "). 
   DO WHILE i > 0.
      substr(cLine,i,1) = "".
      i = index(cLine," ").
   END.

   PUT STREAM excel UNFORMATTED 
      cUser "," 
      STRING(YEAR(TODAY),"9999")
      STRING(MONTH(TODAY),"99")
      STRING(DAY(TODAY),"99") ","
      string(time,"hh:mm:ss") ","
      cLine my-nl.

   OUTPUT STREAM excel CLOSE.

END.


FUNCTION fMELog RETURN LOG
  (cUser AS CHAR,cLine AS CHAR).

   DEF VAR i     AS i NO-UNDO.

   OUTPUT STREAM excel TO 
      value(fpath +
            string(day(today),"99")    + 
            string(month(today),"99")  +
            string(year(today),"9999") + ".log") append.

   i = index(cLine," "). 
   DO WHILE i > 0.
      substr(cLine,i,1) = "".
      i = index(cLine," ").
   END.

   PUT STREAM excel UNFORMATTED 
      cUser "," 
      STRING(YEAR(TODAY),"9999")
      STRING(MONTH(TODAY),"99")
      STRING(DAY(TODAY),"99") ","
      string(time,"hh:mm:ss") ","
      cLine my-nl.

   OUTPUT STREAM excel CLOSE.

END.

/*
Prints timestamp, action and filename to standard output 
Used for "Yoigo Dashboard"
*/
FUNCTION fBatchLog RETURNS LOGICAL
(icAction AS CHAR, icFileName AS CHAR):
   DEFINE VARIABLE lcDateTime AS CHARACTER NO-UNDO.
   lcDateTime = ISO-DATE(DATETIME(TODAY, MTIME)).
   lcDateTime = SUBSTRING(lcDateTime, 1, INDEX(lcDateTime,".") - 1).
   MESSAGE lcDateTime icAction icFileName.
END FUNCTION.

&ENDIF
