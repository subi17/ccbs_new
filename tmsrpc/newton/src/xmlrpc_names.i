/* ----------------------------------------------------------------------
  MODULE .......: xmlrpc_names.i 
  TASK .........: Name conversions between TMS and Newton
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 17.03.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttNamePairs NO-UNDO
   FIELD tms AS CHAR
   FIELD web AS CHAR
   INDEX tms IS UNIQUE tms
   INDEX web IS UNIQUE web.

FUNCTION fInitNamePairs RETURNS LOGICAL
(  icTMSName AS CHAR,
   icWebName AS CHAR):
   
   CREATE ttNamePairs.
   ASSIGN
      ttNamePairs.tms = icTMSName
      ttNamePairs.web = icWebName.

END FUNCTION. 

fInitNamePairs("FATime","FATScheme").
fInitNamePairs("BillItem","BillingItem").
fInitNamePairs("PerContract","PeriodicalContract").
fInitNamePairs("TopUp","TopupScheme").

FUNCTION fConvertToWebName RETURNS CHARACTER
   (icTMSName AS CHAR):

   FIND ttNamePairs NO-LOCK WHERE
        ttNamePairs.tms = icTMSName NO-ERROR.
   IF AVAIL ttNamePairs THEN RETURN ttNamePairs.web. 
   ELSE RETURN icTMSName.

END FUNCTION. 

FUNCTION fConvertToTMSName RETURNS CHARACTER
   (icWebName AS CHAR):
   
   FIND ttNamePairs NO-LOCK WHERE
        ttNamePairs.web = icWebName NO-ERROR.
   IF AVAIL ttNamePairs THEN RETURN ttNamePairs.tms. 
   ELSE RETURN icWebName.

END FUNCTION. 
