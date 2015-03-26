/* eventval.i
   Checks, IF Eventlog is done 
*/

&IF "{&EVENTVAL_I}" NE "YES"
&THEN

&GLOBAL-DEFINE EVENTVAL_I YES

DEF VAR llDoEvent AS LO NO-UNDO INIT TRUE.

&ENDIF
