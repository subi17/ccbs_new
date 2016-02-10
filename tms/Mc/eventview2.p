/*-----------------------------------------------------------------------------
  MODULE....: eventview2.p
  TASK......: show events FOR a FIELD
  AUTHOR....: tk 
  CREATED...: 07.05.2002
  MODIFIED..: 20.05.2002/tk EXTENT bug fixed
              23.29.2002/jr old value to browser
----------------------------------------------------------------------------- */
{Syst/commali.i}

&GLOBAL-DEFINE STAR_EVENT_USER katun

{Func/lib/eventlog.i}

DEFINE VAR itable  LIKE Eventlog.TableName NO-UNDO.
DEFINE VAR ikey    LIKE Eventlog.Key       NO-UNDO.
DEFINE VAR ifield  AS CHAR                 NO-UNDO. 
DEFINE VAR gcLabel AS CHAR                 NO-UNDO.
DEFINE VAR ghField AS HANDLE               NO-UNDO.

DEFINE INPUT PARAMETER ihBuffer AS HANDLE NO-UNDO.

itable = ihBuffer:TABLE.
ikey   = getStarEventKey(ihBuffer).
ifield = focus:name
       + ( IF focus:index > 0
           then "[" + string(focus:index) + "]"
           else ""
         ). 
ghField = ihBuffer:BUFFER-FIELD(focus:name).
gcLabel = ghField:LABEL.


DEFINE VAR i      AS INTEGER                          NO-UNDO.
DEFINE VAR ok     AS Log      FORMAT "Yes/No"         NO-UNDO.
DEFINE VAR tlabel AS CHAR                             NO-UNDO.


DEFINE TEMP-TABLE tlog
  FIELD EventDate  LIKE Eventlog.EventDate
  FIELD tTime  LIKE Eventlog.EventTime
  FIELD tUser  LIKE Eventlog.UserCode FORMAT "X(6)"
  FIELD tEvent LIKE Eventlog.Action FORMAT "X(6)"
  FIELD tValue AS CHAR format "x(20)"
  FIELD oValue AS CHAR FORMAT "X(20)"
INDEX EventDate is PRIMARY
  EventDate DESCENDING
  ttime DESCENDING.

tlabel = gcLabel.

FORM
   tlog.EventDate
   tlog.tTime
   tlog.tUser
   tlog.tEvent
   tlog.tValue  COLUMN-LABEL "New value"
   tlog.oValue  COLUMN-LABEL "Old value"
WITH
     width 75 OVERLAY
     CENTERED
     ROW 4
     TITLE " History of field " + tlabel + " "
     10  DOWN
     FRAME sel.


/* Gather ALL events where this particular FIELD has been modified */
FOR 
EACH  Eventlog NO-LOCK WHERE
      Eventlog.TableName = itable  AND
      Eventlog.Key       = ikey    AND
      LOOKUP(ifield,datavalues,CHR(255)) > 0 .


   i = LOOKUP(ifield,datavalues,CHR(255)).
   CREATE tlog.
   ASSIGN
     tlog.EventDate  = Eventlog.EventDate
     tlog.tTime  = Eventlog.EventTime
     tlog.tUser  = Eventlog.UserCode
     tlog.tEvent = Eventlog.Action
     tlog.tValue = ENTRY(i + 2, Eventlog.DataValues, CHR(255))
     tlog.oValue = ENTRY(i + 1, Eventlog.DataValues, CHR(255)).
END.

PAUSE 0.


/* DISPLAY ALL events */
IF CAN-FIND(FIRST tlog) THEN DO:
   ok = TRUE.
SHOW:
   FOR 
   EACH tlog
   WITH FRAME sel:

      DISP tlog WITH FRAME sel.

      IF FRAME-LINE < FRAME-DOWN THEN DOWN.
      ELSE DO:
         ok = TRUE.
         MESSAGE "MORE events (Y/N) ? " UPDATE ok.
         IF NOT ok THEN  LEAVE show.
         CLEAR FRAME sel ALL.
         UP FRAME-LINE - 1.
      END.   

   END. /* show */

   IF ok /* show NOT interrupted BY user ... */ THEN DO:
      /* ALL events have been shown now */
      MESSAGE "HIT ENTER TO RETURN !".
      PAUSE NO-MESSAGE.
   END.

   HIDE FRAME sel.

END.
ELSE DO:
   MESSAGE "NO Event history available for this field"
   VIEW-AS ALERT-BOX ERROR.
END.


