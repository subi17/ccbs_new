{Func/date.i}
{Func/heartbeat.i}

DEFINE VARIABLE lcDelivFile AS CHARACTER NO-UNDO FORMAT "X(23)".
DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFiles     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNagios    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhHandle    AS HANDLE    NO-UNDO.

FORM
   ldaDate     COLUMN-LABEL "Date"
   lcTime      COLUMN-LABEL "Time"
   liFiles     COLUMN-LABEL "Files sent"
   lcDelivFile COLUMN-LABEL "Latest file"
WITH ROW 8 CENTERED TITLE " ICC File To Dextra " FRAME frm.

lcNagios = "iccout:ICC File Sender".

fKeepAlive(lcNagios).

RUN icc_procedures PERSISTENT SET lhHandle.

LOOP:
DO WHILE TRUE:
   
   PUT SCREEN ROW 22 "Creating new file..".

   lcDelivFile = "ccbs_" + fDateFMT(TODAY,"ddmmyyyy") + 
                  REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

   RUN pOneDelivery IN lhHandle (lcDelivFile).

   ASSIGN
      ldaDate = TODAY
      lcTime  = STRING(TIME,"HH:MM:SS")
      liFiles = liFiles + 1.

   DISP
      ldaDate
      lcTime
      liFiles
      lcDelivFile
   WITH FRAME frm.

   PUT SCREEN ROW 20 "Latest file: " + lcDelivFile.
   PUT SCREEN ROW 22 "F8 to QUIT         ".
   
   READKEY PAUSE 360.

   fKeepAlive(lcNagios).
   
   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN LEAVE LOOP.

END.

DELETE OBJECT lhHandle NO-ERROR.
