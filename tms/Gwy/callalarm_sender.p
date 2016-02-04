{Func/heartbeat.i}

DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNagios    AS CHARACTER NO-UNDO.

FORM
   ldaDate     COLUMN-LABEL "Date" AT 4
   lcTime      COLUMN-LABEL "Time"
WITH ROW 8 CENTERED TITLE " CallAlarm SMSC Sender " FRAME frm.

lcNagios = "callalarm:CallAlarm SMS".

fKeepAlive(lcNagios).

LOOP:
DO WHILE TRUE:
   
   PUT SCREEN ROW 22 "Sending SMSs ......".

   RUN Gwy/smscgwy("CallAlarm","","").

   ASSIGN
      ldaDate = TODAY
      lcTime  = STRING(TIME,"HH:MM:SS").

   DISP
      ldaDate
      lcTime
   WITH FRAME frm.

   PUT SCREEN ROW 22 "F8 to QUIT         ".
   
   READKEY PAUSE 5.

   fKeepAlive(lcNagios).
   
   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN LEAVE LOOP.

END.

QUIT.
