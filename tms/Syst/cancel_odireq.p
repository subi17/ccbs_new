/* cancel_odireq.p   10.05.07/aam 
*/

{commpaa.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).
END.

DEF VAR lcMemo AS CHAR NO-UNDO.
DEF VAR llOk   AS LOG  NO-UNDO.
DEF VAR liQty  AS INT  NO-UNDO.

FORM 
   SKIP(1)
   "All unprocessed (status 0) ODI requests will be cancelled." AT 5 SKIP(1)
   lcMemo AT 5
     FORMAT "X(60)" 
     HELP  "Reason for cancelling requests"
     LABEL "Reason"
   SKIP(1)
   WITH OVERLAY ROW 10 CENTERED TITLE " ODI CANCELLATION " SIDE-LABELS
      FRAME fCancel.
      

PAUSE 0.
VIEW FRAME fCancel.

REPEAT WITH FRAME fCancel ON ENDKEY UNDO, LEAVE:

   UPDATE lcMemo.
   
   IF lcMemo > "" THEN DO:
   
      llOk = FALSE.
      
      MESSAGE "Start cancelling ODI request?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.

      IF llOk THEN DO:
   
         FOR EACH MsRequest EXCLUSIVE-LOCK WHERE
                  MsRequest.Brand   = gcBrand AND
                  MsRequest.ReqType = 20      AND
                  MsRequest.ReqStat = 0:
               
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).
            
            ASSIGN 
               MsRequest.ReqStat = 4
               MsRequest.Memo    = lcMemo
               liQty             = liQty + 1.

            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
         
         END.
      
         MESSAGE liQty "requests were cancelled"
         VIEW-AS ALERT-BOX TITLE " DONE ".
         
      END.
   END.      
 
   LEAVE.            
END.   

HIDE FRAME fCancel NO-PAUSE.

