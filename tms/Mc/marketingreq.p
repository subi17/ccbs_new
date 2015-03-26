/* ----------------------------------------------------------------------------
  MODULE .......: marketingreq.p
  FUNCTION .....: customer marketing data handling
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.09.07 (separated from msrequest.i)
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{msreqfunc.i}
{fwebuser.i}
{eventval.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 12 THEN RETURN "ERROR".

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.


RUN pMarketing.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pMarketing:

   DEF VAR lcAllowed AS CHAR NO-UNDO INIT "1,TRUE".
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum
      EXCLUSIVE-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   IF NUM-ENTRIES(MsRequest.ReqCParam1,";") < 3 OR 
      NUM-ENTRIES(MsRequest.ReqCParam2,";") < 3
   THEN DO:
      fReqError("New marketing values have not been given properly").
      RETURN.
   END.
   
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).

   ASSIGN 
      Customer.DirMarkSMS   = (LOOKUP(ENTRY(1,MsRequest.ReqCParam1,";"),
                                      lcAllowed) > 0)
      Customer.DirMarkEMail = (LOOKUP(ENTRY(2,MsRequest.ReqCParam1,";"),
                                      lcAllowed) > 0)
      Customer.DirMarkPost  = (LOOKUP(ENTRY(3,MsRequest.ReqCParam1,";"),
                                      lcAllowed) > 0)
      Customer.OutMarkSMS   = (LOOKUP(ENTRY(1,MsRequest.ReqCParam2,";"),
                                      lcAllowed) > 0)
      Customer.OutMarkEMail = (LOOKUP(ENTRY(2,MsRequest.ReqCParam2,";"),
                                      lcAllowed) > 0)
      Customer.OutMarkPost  = (LOOKUP(ENTRY(3,MsRequest.ReqCParam2,";"),
                                      lcAllowed) > 0).

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
   
   RELEASE Customer.
             
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


