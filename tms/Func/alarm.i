/* This file contains TMS Alarm System functions
   to handle alarm creations and status changes */

{timestamp.i}

/* Constants for alarm severity */
&GLOBAL-DEFINE ALARM_SEVERITY_INFO   0
&GLOBAL-DEFINE ALARM_SEVERITY_MINOR  1
&GLOBAL-DEFINE ALARM_SEVERITY_NORMAL 2
&GLOBAL-DEFINE ALARM_SEVERITY_MAJOR  3

/* Constants for alarm status */
&GLOBAL-DEFINE ALARM_STATUS_ACTIVE     1
&GLOBAL-DEFINE ALARM_STATUS_CLEARED    2
&GLOBAL-DEFINE ALARM_STATUS_RESETED    3
&GLOBAL-DEFINE ALARM_STATUS_CANCELLED  4

DEFINE VARIABLE lcAlarmText  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAlarmTexts AS CHARACTER NO-UNDO.
lcAlarmTexts = "First informative alarm,Minor alarm,Normal error in system,Serious Error in System".

/* This function check if alarm is already active */
FUNCTION fIsAlarmActive RETURNS LOGICAL
   (iiAlarmNumber AS INT):

   FIND FIRST Alarm NO-LOCK WHERE
              Alarm.AlarmID EQ iiAlarmNumber AND
              Alarm.CurrentStatus EQ {&ALARM_STATUS_ACTIVE} OR
              Alarm.CurrentStatus EQ {&ALARM_STATUS_CLEARED} NO-ERROR.

   IF AVAIL Alarm THEN RETURN TRUE.
   RETURN FALSE.

END.

/* This function reset active alarm */
FUNCTION fResetAlarm RETURNS LOGICAL
   (iiAlarmNumber AS INT):

   FIND FIRST Alarm NO-LOCK WHERE
              Alarm.AlarmID EQ iiAlarmNumber AND
              Alarm.CurrentStatus EQ {&ALARM_STATUS_ACTIVE} OR
              Alarm.CurrentStatus EQ {&ALARM_STATUS_CLEARED} NO-ERROR.

   IF AVAIL Alarm THEN DO:
      ASSIGN 
         Alarm.CurrentStatus = {&ALARM_STATUS_RESETED}
         Alarm.ResetTime = fMakeTS()
         Alarm.ActionLog = Alarm.ActionLog + "QvaltelOC" + STRING(Alarm.ResetTime).
      RETURN TRUE.
   END.

   RETURN FALSE.

END.


/* This function cancel active alarm */
FUNCTION fCancelAlarm RETURNS LOGICAL
   (iiAlarmNumber AS INT):

   FIND FIRST Alarm NO-LOCK WHERE
              Alarm.AlarmID EQ iiAlarmNumber AND
              Alarm.CurrentStatus EQ {&ALARM_STATUS_ACTIVE} OR
              Alarm.CurrentStatus EQ {&ALARM_STATUS_CLEARED} NO-ERROR.

   IF AVAIL Alarm THEN DO:
      ASSIGN 
         Alarm.CurrentStatus = {&ALARM_STATUS_CANCELLED}
         Alarm.CancelTime = fMakeTS()
         Alarm.ActionLog = Alarm.ActionLog + "QvaltelOC" + STRING(Alarm.CancelTime).
      RETURN TRUE.
   END.

   RETURN FALSE.

END.

/* This function clear active alarm */
FUNCTION fClearAlarm RETURNS LOGICAL
   (iiAlarmNumber AS INT):

   FIND FIRST Alarm NO-LOCK WHERE
              Alarm.AlarmID EQ iiAlarmNumber AND
              Alarm.CurrentStatus EQ {&ALARM_STATUS_ACTIVE} NO-ERROR.

   IF AVAIL Alarm THEN DO:
      ASSIGN 
         Alarm.CurrentStatus = {&ALARM_STATUS_CLEARED}
         Alarm.ClearTime = fMakeTS()
         Alarm.ActionLog = Alarm.ActionLog + "QvaltelOC" + STRING(Alarm.ClearTime).
      RETURN TRUE.
   END.

   RETURN FALSE.

END.


/* This function set new alarm if it is not already active */
FUNCTION fSetAlarm RETURNS LOGICAL
   (iiAlarmNumber AS INT,
    icParameters  AS CHAR):

   IF fIsAlarmActive(iiAlarmNumber) THEN RETURN FALSE.

   lcAlarmText = ENTRY(iiAlarmNumber,lcAlarmTexts).
   CREATE Alarm.
   ASSIGN
      Alarm.AlarmID       = iiAlarmNumber
      Alarm.AlarmText     = lcAlarmText
      Alarm.ProgramBlock  = PROGRAM-NAME(2)
      Alarm.Severity      = iiAlarmNumber /* Fake */
      Alarm.CurrentStatus = {&ALARM_STATUS_ACTIVE}
      Alarm.SettingTime   = fMakeTS().

   RETURN TRUE.

END.

