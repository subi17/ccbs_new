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
DEF BUFFER bMobsub FOR Mobsub.

DEF VAR lcCategory AS CHAR.
DEF VAR lhCustomer AS HANDLE NO-UNDO.
DEF VAR lcCharValue AS CHAR NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR liOrigStatus AS INT NO-UNDO.
DEF VAR liMsreq AS INT NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_PRO_MIGRATION} THEN RETURN "ERROR".
liOrigStatus = MsRequest.reqstatus.

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

IF liOrigStatus EQ {&REQUEST_STATUS_NEW} THEN DO:

   FIND FIRST Mobsub NO-LOCK WHERE
              Mobsub.MsSeq = MsRequest.MsSeq NO-ERROR.
   IF NOT AVAIL Mobsub THEN lcError = "ERROR: NO Mobsub".
   FIND FIRST Customer NO-LOCK WHERE
              Customer.custnum EQ Mobsub.agrcust NO-ERROR.
   IF NOT AVAIL Customer THEN lcError = "ERROR: NO Customer".
   FIND FIRST CLIType WHERE 
              CLIType.brand EQ Syst.Parameters:gcBrand AND
              CLIType.clitype EQ Mobsub.clitype NO-ERROR.
   IF NOT AVAIL CLIType THEN lcError = "ERROR: Unknown Clitype".

   IF (LOOKUP(Customer.CustIdType,"NIF,NIE") > 0 AND 
       NOT fIsSelfEmpl(Customer.category)) OR
       Customer.CustIdType EQ "passport" THEN 
      lcError = "ERROR: Not selfemployed".
   IF lcError > "" THEN DO:
      fReqStatus(3,lcError).
      RETURN lcError.
   END.
   fgetCustSegment(Customer.custidtype, fIsSelfEmpl(Customer.category),
                   TRUE, OUTPUT lcCategory).

   IF NOT fIsPro(lccategory) THEN DO:
      fReqStatus(3,SUBST("New category is not pro &1",lccategory)).
      RETURN lcError.
   END.

   DO TRANSACTION:

      IF lcCategory NE Customer.category  THEN DO:
         FIND CURRENT Customer EXCLUSIVE-LOCK.
         lhCustomer = BUFFER Customer:HANDLE.
         RUN StarEventInitialize(lhCustomer).
         RUN StarEventSetOldBuffer ( lhCustomer ).
         Customer.category = lcCategory.
         RUN StarEventMakeModifyEvent(lhCustomer).
      END.

      RUN Mc/creasfee.p (Mobsub.CustNum,
                    Mobsub.MsSeq,
                    Today,
                    "FeeModel",
                    fGetProFeemodel(Mobsub.clitype),
                    9,
                    ?,
                    "Pro Migrate",    /* memo   */
                    FALSE,           /* no messages to screen */
                    katun,
                    "ProMigrate",
                    0,
                    "",
                    "",
                    OUTPUT lcCharValue).

      IF lcCharValue BEGINS "ERROR:" OR lcCharValue BEGINS "0" THEN DO:
         fReqStatus(3,SUBST("Pro fee creation failed: &1",lcCharValue)).
         RETURN lcError.
      END.

      /* Make subrequest by orderactions and add mandatory field true for
      getting this main process wait until handled */
      RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                                Mobsub.CLIType,
                                0,
                                0,
                                0,
                                TRUE, /* create fees */
                                {&REQUEST_SOURCE_MIGRATION},
                                {&REQUEST_ACTIONLIST_ALL}).
      FOR EACH bSubReq EXCLUSIVE-LOCK WHERE
               bsubreq.Brand     = gcBrand AND
               bsubreq.origRequest = iiRequest:
         bsubreq.mandatory = 1.
      END.
   END.
   /* Mark request as handled */
   IF fChkSubRequest(iiRequest) = FALSE THEN DO:
      IF NOT fReqStatus(7,"") THEN DO:
         fReqError("ERROR: St. update failed.").
         RETURN.
      END.
   END.
   ELSE DO:
      IF NOT fReqStatus(8,"") THEN DO:
         fReqError("ERROR: St. update failed.").
         RETURN.
      END.
   END.
   IF MSRequest.origRequest EQ 0 THEN DO:
      /* Main request, check if other subscription to migrate */
      FOR EACH bMobsub WHERE
               bMobsub.brand EQ gcBrand AND
               bMobsub.agrCust EQ Mobsub.agrCust AND
               bMobsub.msseq NE Mobsub.msseq:
         FIND FIRST Clitype WHERE 
                    Clitype.brand EQ gcBrand AND 
                    Clitype.clitype EQ bMobsub.clitype NO-LOCK NO-ERROR.
         IF AVAIL Clitype AND 
                  Clitype.webstatuscode EQ {&CLITYPE_WEBSTATUSCODE_ACTIVE} 
         THEN DO:
            liMsReq = fProMigrationRequest(INPUT bMobsub.Msseq,
                                           INPUT MSRequest.salesman,
                                           INPUT {&REQUEST_SOURCE_MIGRATION},
                                           INPUT MSRequest.msrequest,
                                           OUTPUT lcResult).
         END.
         ELSE IF AVAIL Clitype AND
                 fgetActiveReplacement(bMobsub.clitype) GT "" THEN DO:
            /* Make iSTC according to mapping */
            liMsReq = fCTChangeRequest(bMobSub.msseq,
                           fgetActiveReplacement(bMobsub.clitype),
                           "", /* lcBundleID */
                           "", /*bank code validation is already done */
                           MSRequest.ActStamp,
                           0,  /* 0 = Credit check ok */
                           0, /* extend contract */
                           "" /* pcSalesman */,
                           FALSE, /* charge */
                           TRUE, /* send sms */
                           "",
                           0,
                           {&REQUEST_SOURCE_MIGRATION},
                           0,
                           MSRequest.msrequest,
                           "", /*contract id*/
                           OUTPUT lcError).

            IF liMsReq = 0 THEN DO:
               fReqStatus(3,"").
               fReqError("ERROR: Migration STC request creation failed.").
            END.
         
         END.
         ELSE DO:
            fReqStatus(3,"").
            fReqError("ERROR: Migration failed.").
         END.
           
      END.
   END.
END.
ELSE IF liOrigStatus EQ {&REQUEST_STATUS_SUB_REQUEST_DONE} THEN DO:
   IF fChkSubRequest(iiRequest) = FALSE THEN DO:
      fReqStatus(7,"").
      RETURN.
   END.
   fReqStatus(2,"").
END.

FINALLY:
   fCleanEventObjects(). 
END.

