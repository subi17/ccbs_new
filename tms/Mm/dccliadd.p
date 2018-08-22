/* ------------------------------------------------------
  MODULE .......: dccliadd.p
  FUNCTION .....: create a request for a new periodical contract 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 11.03.08
  MODIFIED .....: 
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/upsellbundle.i}
{Func/dss_request.i}
{Func/dss_matrix.i}
{Mm/fbundle.i}

DEF INPUT PARAMETER iiMsSeq AS INT NO-UNDO. 

DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR liCreated     AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR ldActStamp    AS DEC  NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR ldtContrDate  AS DATE NO-UNDO.
DEF VAR lcDCEvent     AS CHAR NO-UNDO.
DEF VAR llCreateFees  AS LOG  NO-UNDO.
DEF VAR lcResult      AS CHAR NO-UNDO.
DEF VAR liConCount    AS INT  NO-UNDO.
DEF VAR lcSource      AS CHAR NO-UNDO.
DEF VAR llContrSource  AS LOG  NO-UNDO.

/* DSS related variables */
DEF VAR ldeCurrMonthLimit   AS DEC  NO-UNDO.
DEF VAR ldeOtherMonthLimit  AS DEC  NO-UNDO.
DEF VAR ldeConsumedData     AS DEC  NO-UNDO.

FORM
   SKIP(1)
   "A periodical contract will be created." AT 5 
   SKIP(1)

   MobSub.CLI COLON 20
      LABEL "MSISDN" 
      SKIP
   MobSub.MsSeq COLON 20
      LABEL "Subscription ID"
      FORMAT ">>>>>>>>>>9"
      SKIP
   MobSub.CustNum COLON 20
      LABEL "Customer"
   lcCustName 
      NO-LABEL
      FORMAT "X(40)"
      SKIP(1)
   lcDCEvent COLON 20
      LABEL "Contract"
      HELP "Periodical contract ID (F9)"
      FORMAT "X(20)"
   DayCampaign.DCName
      FORMAT "X(30)"
      NO-LABEL
      SKIP
   llContrSource COLON 20
      LABEL "Contract Source"
      HELP "Contract source (New or Renewal)"
      FORMAT "New/Renewal"
      SKIP
   ldtContrDate COLON 20
      LABEL "Contract Date"
      HELP "Date when contract becomes effective"
      FORMAT "99-99-99" 
   llCreateFees COLON 20
      LABEL "Create Fees"
      HELP "Will monthtly/termination fee be created"
      FORMAT "Yes/No"
      SKIP(1)
WITH ROW 6 OVERLAY SIDE-LABELS CENTERED 
     TITLE " NEW PERIODICAL CONTRACT " FRAME fCriter.

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown subscription"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.

ASSIGN
   lcCustName   = Func.Common:mDispCustName(BUFFER Customer)
   ldtContrDate = TODAY
   llCreateFees = FALSE
   llContrSource = TRUE
   Syst.Var:toimi        = -1.

MakeReq:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO MakeReq, NEXT MakeReq:
   
   /*ILP*/
   IF LOOKUP (lcDCEvent , "DATA200_UPSELL,DSS200_UPSELL") > 0
   THEN DO:
      MESSAGE lcDCEvent "  adding not allowed in CUI" VIEW-AS ALERT-BOX.
      PAUSE 0.
      RETURN.     
   END.
   ELSE IF lcDCEvent > "" THEN 
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand = Syst.Var:gcBrand AND
                 DayCampaign.DCEvent = lcDCEvent NO-LOCK NO-ERROR.
   ELSE DISPLAY "" @ DayCampaign.DCName WITH FRAME fCriter.
                 
   PAUSE 0.
   DISPLAY MobSub.CLI
           MobSub.MsSeq
           MobSub.CustNum
           lcCustName
           lcDCEvent
           DayCampaign.DCName WHEN AVAILABLE DayCampaign
           llContrSource
           ldtContrDate
           llCreateFees
   WITH FRAME fCriter.

   IF Syst.Var:toimi < 0 THEN Syst.Var:toimi = 1.
   ELSE DO:
      ASSIGN
         Syst.Var:ufk    = 0  
         Syst.Var:ufk[1] = 7
         Syst.Var:ufk[5] = IF lcDCEvent > "" AND
                     ldtContrDate >= MobSub.ActivationDate AND
                     ldtContrDate <= TODAY
                  THEN 1027 
                  ELSE 0 
         Syst.Var:ufk[8] = 8 
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   IF Syst.Var:toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
      
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE 
            lcDCEvent
            llContrSource
            ldtContrDate
            llCreateFees
         WITH FRAME fCriter EDITING:
         
            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
               PAUSE 0.
        
               IF FRAME-FIELD = "lcDCEvent" THEN DO:

                  IF INPUT lcDCEvent = "" THEN LEAVE.
                   
                  FIND FIRST DayCampaign WHERE
                             DayCampaign.Brand = Syst.Var:gcBrand AND
                             DayCampaign.DCEvent = INPUT lcDCEvent 
                  NO-LOCK NO-ERROR.
                
                  IF NOT AVAIL DayCampaign THEN DO:
                     MESSAGE "Unknown periodical contract".
                     NEXT.
                  END.
                  
                  IF NOT (DayCampaign.ValidFrom <= TODAY AND
                      DayCampaign.ValidTo >= TODAY) THEN DO:
                     MESSAGE "Contract type is not active".
                     NEXT.
                  END.
                  
                  DISP DayCampaign.DCName WITH FRAME fCriter.
               END.

               ELSE IF FRAME-FIELD = "llContrSource" THEN DO:
                  IF NOT INPUT lcDCEvent BEGINS "TERM" AND
                     NOT INPUT llContrSource THEN DO:                        
                        MESSAGE "Only TERM contract can be Renewal.".
                        NEXT.
                  END.

                  IF INPUT llContrSource EQ ? OR
                     INPUT llContrSource 
                  THEN llContrSource = TRUE.
                  ELSE llContrSource = FALSE.

                  DISP llContrSource WITH FRAME fCriter.
               END.

               ELSE IF FRAME-FIELD = "ldtContrDate" THEN DO:
                  IF INPUT ldtContrDate > TODAY THEN DO:
                     MESSAGE "Activation cannot be set to future"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  ELSE IF INPUT ldtContrDate < MobSub.ActivationDate THEN DO:
                     MESSAGE "Contract cannot begin before subscription"
                             "has been activated."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "llCreateFees" THEN DO:
                  IF INPUT llCreateFees EQ ? THEN DO:
                     llCreateFees = FALSE.
                     DISP llCreateFees.
                  END.
               END.
            END.

            APPLY LASTKEY.
         END.
      
         LEAVE.
      END.
      
   END.

   ELSE IF Syst.Var:toimi = 5 THEN DO:

      ldActStamp = Func.Common:mMake2DT(ldtContrDate,
                            IF ldtContrDate = TODAY
                            THEN TIME
                            ELSE 0).
 
      IF DayCampaign.DCType = "1" THEN DO:

         FOR EACH MServiceLimit NO-LOCK WHERE
                  MServiceLimit.MsSeq  = MobSub.MsSeq AND
                  MServiceLimit.FromTS <= ldActStamp  AND
                  MServiceLimit.EndTS  >= ldActStamp, 
            FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
                  ServiceLimit.SLSeq     = MServiceLimit.SLSeq  AND
                  ServiceLimit.GroupCode = lcDCEvent:

            MESSAGE "Subscription already has a valid periodical contract"
                    "for the given contract ID and period."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.  
      ELSE IF DayCampaign.DCType = "5" AND
              lcDCEvent BEGINS "PAYTERM" THEN DO:
         
         liConCount = 0.
                  
         FOR EACH DCCLI NO-LOCK WHERE
                  DCCLI.Brand      = Syst.Var:gcBrand      AND
                  DCCLI.MsSeq      = MobSub.MsSeq AND
                  DCCLI.ValidTo   >= ldtContrDate AND 
                  DCCLI.DCEvent   BEGINS "PAYTERM":        
            liConCount = liConCount + 1.
         END.             
         
         IF liConCount >= 2 THEN DO:
            MESSAGE "Already two PayTerm contracts are actived on Subscription"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.   
      END.       
      ELSE IF CAN-FIND(FIRST DCCLI WHERE
                             DCCLI.Brand   = Syst.Var:gcBrand AND
                             DCCLI.DCEvent = lcDCEvent AND
                             DCCLI.MsSeq   = MobSub.MsSeq AND
                             DCCLI.ValidFrom <= ldtContrDate AND
                             DCCLI.ValidTo   >= ldtContrDate)
      THEN DO:
         MESSAGE "Subscription already has a valid periodical contract"
                 "for the given contract ID and period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      /* is there a change request already */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq   = MobSub.MsSeq AND
                        MsRequest.ReqType = 8            AND
                        MsRequest.ReqCParam3 = lcDCEvent AND
                        LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0)
      THEN DO:
         MESSAGE "There is a pending request for this subscription with"
                 "given contract ID and period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END. 

      llOk = FALSE.
      MESSAGE "A new periodical contract will be created." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
 
      IF lcDCEvent = "DSS2" THEN DO:
         MESSAGE "DSS2 Activation is not allowed"
            VIEW-AS ALERT-BOX ERROR.
         LEAVE.
      END.
      ELSE IF lcDCEvent = {&DSS} THEN DO:
         IF fOngoingDSSAct(INPUT MobSub.CustNum) THEN DO:
            MESSAGE "DSS activation request is ongoing."
               VIEW-AS ALERT-BOX ERROR.
            LEAVE.
         END. /* IF fOngoingDSSAct(INPUT MobSub.CustNum) THEN DO: */
         IF NOT fIsDSSAllowed(INPUT  MobSub.CustNum,
                              INPUT  MobSub.MsSeq,
                              INPUT  Func.Common:mMakeTS(),
                              INPUT  {&DSS},
                              INPUT  "",
                              OUTPUT ldeCurrMonthLimit,
                              OUTPUT ldeConsumedData,
                              OUTPUT ldeOtherMonthLimit,
                              OUTPUT lcResult) THEN DO:

            MESSAGE lcResult VIEW-AS ALERT-BOX ERROR.
            LEAVE.
         END. /* IF NOT fIsDSSAllowed(INPUT  MobSub.CustNum, */
         liCreated = fDSSRequest(MobSub.MsSeq,
                                 MobSub.CustNum,
                                 "CREATE",
                                 lcResult,
                                 lcDCEvent,
                                 Func.Common:mMakeTS(),
                                 {&REQUEST_SOURCE_MANUAL_TMS},
                                 "",
                                 llCreateFees,
                                 0,
                                 FALSE,
                                 OUTPUT lcError).
      END. /* IF lcDCEvent = {&DSS} THEN DO: */
      ELSE IF lcDCEvent MATCHES ("*_UPSELL") THEN
         fCreateUpsellBundle(MobSub.MsSeq,
                             lcDCEvent,
                             {&REQUEST_SOURCE_MANUAL_TMS},
                             Func.Common:mMakeTS(),
                             OUTPUT liCreated,
                             OUTPUT lcError).
      ELSE DO:
         /* user can select manually if a
            TERM contract source is "new" or "renewal" */
         IF lcDCEvent BEGINS "TERM" AND NOT llContrSource 
         THEN lcSource = {&REQUEST_SOURCE_RENEWAL}.
         ELSE lcSource = {&REQUEST_SOURCE_MANUAL_TMS}.

         liCreated = fPCActionRequest(MobSub.MsSeq,
                                   lcDCEvent,
                                   "act",
                                   ldActStamp,
                                   llCreateFees,
                                   lcSource,
                                   "",
                                   0,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcError).
      END. /* ELSE DO: */
      IF liCreated > 0 THEN 
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.
         
      ELSE 
         MESSAGE "Request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF Syst.Var:toimi = 8 THEN LEAVE.

END. /* MakeReq */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

