/* Pro migration request handler */

{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{Func/custfunc.i}
{Func/profunc.i}
{Syst/eventval.i}
{Func/ffeecont.i}
{Func/setfees.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
END.

DEF INPUT PARAMETER iiRequest AS INTEGER NO-UNDO.
DEF BUFFER bsubReq FOR MSRequest.

DEF VAR lcCategory AS CHAR.
DEF BUFFER bMobsub FOR Mobsub.
DEF BUFFER bCustomer FOR Customer.
DEF BUFFER bClitype FOR Clitype.
DEF VAR lhCustomer AS HANDLE NO-UNDO.
DEF VAR lcCharValue AS CHAR NO-UNDO.
DEF VAR lcContract AS CHAR NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_PRO_MIGRATION} THEN RETURN "ERROR".
   
/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

IF MsRequest.ReqType EQ {&REQTYPE_PRO_MIGRATION} THEN DO:

   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   IF NOT AVAIL bMobsub THEN lcError = "ERROR: NO Mobsub".
   FIND FIRST bCustomer WHERE
              bCustomer.custnum EQ bmobsub.agrcust NO-ERROR.
   IF NOT AVAIL bCustomer THEN lcError = "ERROR: NO Customer".
   FIND FIRST bClitype WHERE 
              bClitype.brand EQ Syst.Parameters:gcBrand AND
              bClitype.clitype EQ bMobsub.clitype NO-ERROR.
   IF NOT AVAIL bClitype THEN lcError = "ERROR: Unknown Clitype".
   IF NOT fIsSelfEmpl(bcustomer.category) THEN 
      lcError = "ERROR: Not selfemployed".
   IF lcError > "" THEN DO:
      fReqStatus(3,lcError).
      RETURN lcError.
   END.
   fgetCustSegment(bCustomer.custidtype, fIsSelfEmpl(bcustomer.category),
                   TRUE, OUTPUT lcCategory).
   DO TRANSACTION:
      IF lcCategory NE bCustomer.category AND
         lcCategory NE "" THEN DO:
         lhCustomer = BUFFER bCustomer:HANDLE.
         RUN StarEventInitialize(lhCustomer).
         RUN StarEventSetOldBuffer ( lhCustomer ).
         bcustomer.category = lcCategory.
         RUN StarEventMakeModifyEvent(lhCustomer).
      END.

      IF fIsPro(bcustomer.category) THEN DO:
         RUN Mc/creasfee.p (bMobSub.CustNum,
                       bMobSub.MsSeq,
                       Today,
                       "FeeModel",
                       daycampaign.feemodel,
                       9,
                       ?,
                       "Pro Migrate",    /* memo   */
                       FALSE,           /* no messages to screen */
                       katun,
                       "ContractChange",
                       0,
                       "",
                       "",
                       OUTPUT lcCharValue).
      END.
      /* Make subrequest by orderactions and add mandatory field true for
      getting this main process wait until handled */
      RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                                bMobSub.CLIType,
                                0,
                                0,
                                0,
                                TRUE, /* create fees */
                                {&REQUEST_SOURCE_PRO_MIGRATION},
                                {&REQUEST_ACTIONLIST_ALL}).
      FOR EACH bSubReq WHERE
               bsubreq.Brand     = gcBrand AND
               bsubreq.origRequest = iiRequest:
         bsubreq.mandatory = 1.
      END.
   END.
   /* Mark request as handled */
   IF NOT fReqStatus(7,"") THEN DO:
      fReqError("ERROR: St. update failed.").
      RETURN.
   END.             

END.

fCleanEventObjects(). 

