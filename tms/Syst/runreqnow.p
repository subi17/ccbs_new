
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 18.09.07
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

DEFINE INPUT PARAMETER iiMsRequest AS INTEGER.
DEFINE INPUT PARAMETER iiStatNow   AS INTEGER.
DEFINE INPUT PARAMETER iIParam     AS INTEGER.
DEFINE INPUT PARAMETER iCParam     AS CHARACTER.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiMsRequest AND
     MsRequest.Brand     = gcBrand
NO-LOCK NO-ERROR.

DEF VAR ldtActivate AS DATE      NO-UNDO.
DEF VAR liStampTime AS INTEGER   NO-UNDO.
DEF VAR Ok          AS LOGICAL   NO-UNDO.
DEF VAR lcInfo      AS CHARACTER NO-UNDO.

fSplitTS(MsRequest.ActStamp,
         OUTPUT ldtActivate,
         OUTPUT liStampTime).


IF ldtActivate > TODAY OR
  (ldtActivate = TODAY AND liStampTime > TIME)
  THEN MESSAGE "Request is scheduled to be run on"
               STRING(ldtActivate,"99-99-99")
               STRING(liStampTime,"hh:mm:ss") "." SKIP
               "Request will be rescheduled to today." SKIP
               "Do You want to bypass original scheduling and"
               "run this request immediately ?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE " RUN REQUEST " SET ok.

ELSE MESSAGE "Run this request now ?" 
     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
     TITLE " RUN REQUEST "
     SET ok.

IF ok THEN DO:
   RUN Mm/runreqim.p(MsRequest.MsRequest).
           
   IF RETURN-VALUE > "" THEN
      MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR.
   ELSE MESSAGE "Request has been handled"
        VIEW-AS ALERT-BOX
        TITLE " DONE".
 
END.

