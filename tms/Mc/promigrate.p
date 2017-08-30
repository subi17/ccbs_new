/* Pro migration request handler */

{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{Func/custfunc.i}
{Func/profunc.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
END.

DEF INPUT PARAMETER iiRequest AS INTEGER NO-UNDO.

DEF VAR lcCategory AS CHAR.
DEF BUFFER bMobsub FOR Mobsub.
DEF BUFFER bCustomer FOR Customer.
DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest THEN RETURN "ERROR".
   
IF MsRequest.ReqType EQ {&REQTYPE_PRO_MIGRATION} THEN DO:

   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   IF NOT AVAIL bMobsub THEN RETURN "ERROR: NO Mobsub".
   FIND FIRST bCustomer WHERE
              bCustomer.custnum EQ bmobsub.agrcust NO-ERROR.
   IF NOT AVAIL bCustomer THEN RETURN "ERROR: NO Customer".
   fgetCustSegment(bCustomer.custidtype, fIsSelfEmpl(bcustomer.category),
                   TRUE, OUTPUT lcCategory).

   IF lcCategory NE bCustomer.category AND
      lcCategory NE "" THEN DO:
      lhCustomer = BUFFER bCustomer:HANDLE.
      RUN StarEventInitialize(lhCustomer).
      RUN StarEventSetOldBuffer ( lhCustomer ).
      bcustomer.category = lcCategory.
      RUN StarEventMakeModifyEvent(lhCustomer).
   END.

   RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                             bMobSub.CLIType,
                             0,
                             0,
                             0,
                             TRUE,                   /* create fees */
                             {&REQUEST_SOURCE_PRO_MIGRATION},
                             {&REQUEST_ACTIONLIST_ALL}).
             

END.
fCleanEventObjects(). 
fReqStatus(2,"").
