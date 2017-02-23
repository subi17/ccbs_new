/* This file contains TMS Alarm System functions
   to handle alarm creations and status changes */

/* Constants for alarm severity */
&GLOBAL-DEFINE ALARM_SEVERITY_INFO   0
&GLOBAL-DEFINE ALARM_SEVERITY_MINOR  1
&GLOBAL-DEFINE ALARM_SEVERITY_NORMAL 2
&GLOBAL-DEFINE ALARM_SEVERITY_MAJOR  3

/* Constants for alarm status */
&GLOBAL-DEFINE ALARM_STATUS_ACTIVE     1
&GLOBAL-DEFINE ALARM_STATUS_CANCELLED  2
&GLOBAL-DEFINE ALARM_STATUS_RESETED    3
&GLOBAL-DEFINE ALARM_STATUS_CLEARED    4

/* Define here all alrms description text */
&GLOBAL-DEFINE 0001_ALARM_TEXT "Example alarm"

/* This function check if alarm is already active */
FUNCTION fIsAlarmActive RETURNS LOGICAL
   (iiAlarmNumber AS INT):

   RETURN TRUE.

END.


/* This function set new alarm if it is not already active */
FUNCTION fSetAlarm RETURNS LOGICAL
   (iiAlarmNumber AS INT,
    icParameters  AS CHAR):

   RETURN TRUE.

END.


