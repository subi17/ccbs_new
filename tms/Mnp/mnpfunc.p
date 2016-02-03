/* ----------------------------------------------------------------------
  MODULE .......: mnpfunc.p 
  TASK .........: MNP NC message creation CUI
  APPLICATIO ...: TMS
  AUTHOR .......: anttis 
  CREATED ......: 09/2009 
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i} 
{Mnp/mnpmessages.i}
{Syst/tmsconst.i}
{Func/create_eventlog.i}

DEF INPUT PARAMETER pimnpseq AS INT . 
DEF VAR lhTable AS HANDLE NO-UNDO.
lhTable = BUFFER MNPOperation:HANDLE.

/* mnp in */
&SCOPED-DEFINE MNP_CANCEL "Cancellation request"

/* mnp out */
&SCOPED-DEFINE MNP_CANCEL_PROPOSAL "Cancellation request proposal"
&SCOPED-DEFINE MNP_REJECT "Rejection request"
&SCOPED-DEFINE MNP_CONFIRM "Confirmation request"

/* mnp in & out */
&SCOPED-DEFINE MNP_PR_DETAIL "Portability request detail"
&SCOPED-DEFINE MNP_CANCEL_DETAIL "Cancellation detail request"

/* queries */
&SCOPED-DEFINE MNP_PR_REQUESTED "Requested query"
/* &SCOPED-DEFINE MNP_PR_PAGE_REQUESTED "Requested page query" */
&SCOPED-DEFINE MNP_PR_QUERY "Portability requests query"
&SCOPED-DEFINE MNP_NT_QUERY "Number termination query"
&SCOPED-DEFINE MNP_NR_QUERY "Number ranges query"
&SCOPED-DEFINE MNP_NR_DETAIL "Number range detail"

&SCOPED-DEFINE MNP_NM_DETAIL "Number migration detail"
&SCOPED-DEFINE MNP_NM_FINALIZE "Number migration finalize"

&SCOPED-DEFINE MNP_NMN_DETAIL "Number migration number detail"

/* numbering termination */
&SCOPED-DEFINE MNP_NT_CANCEL "Number termination cancel"
&SCOPED-DEFINE MNP_NT_DETAIL "Number termination detail"
  
DEFINE VARIABLE lcOptions AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSelected AS CHAR NO-UNDO. 

DEFINE VARIABLE lcCancellationReason AS CHARACTER NO-UNDO FORMAT "x(10)". 
DEFINE VARIABLE lcRejectionReason AS CHARACTER NO-UNDO FORMAT "x(10)". 
DEFINE VARIABLE llCancellationByDonor AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lcNumberRangeCode AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcPageCode AS CHARACTER NO-UNDO format "x(40)". 
DEFINE VARIABLE liPage AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPages AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llOngoingMNPMsg AS LOGICAL NO-UNDO. 

FIND MNPProcess WHERE
     MNPProcess.MNPSeq = piMNPSeq NO-LOCK NO-eRROR.

IF NOT AVAIL MNPProcess THEN DO:
   MESSAGE "MNP process was not found" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

CASE MNPProcess.MNPType:
   
   WHEN {&MNP_TYPE_IN} THEN lcOptions = {&MNP_PR_DETAIL}.
   
   WHEN {&MNP_TYPE_OUT} THEN lcOptions = {&MNP_PR_DETAIL}. 
   
   WHEN {&MNP_TYPE_TERMINATION} THEN DO:
      
      IF MNPProcess.StatusCode = {&MNP_ST_NEW} THEN
      /*   lcOptions = "".*/
         lcOptions = {&MNP_NT_DETAIL}.
      ELSE IF MNPProcess.StatusCode = {&MNP_ST_BNOT} THEN
         lcOptions = {&MNP_NT_CANCEL} + "|" + {&MNP_NT_DETAIL}.
      ELSE lcOptions = {&MNP_NT_DETAIL}.
   END.
/*   WHEN 4 THEN lcOptions = {&MNP_PR_QUERY} + "|" +
/*                           {&MNP_PR_PAGE_REQUESTED} + "|" +  */
                           {&MNP_NT_QUERY} + "|" +
                           {&MNP_NR_QUERY} + "|" +
                           {&MNP_NR_DETAIL} + "|" +
                           {&MNP_PR_REQUESTED}.
   WHEN 5 THEN lcOptions = {&MNP_NM_DETAIL} + "|" +
                           {&MNP_NM_FINALIZE}. 
   WHEN 6 THEN lcOptions = {&MNP_NMN_DETAIL}. */
END.

IF lcOptions = "" THEN DO:
   MESSAGE "Cannot create new messages" VIEW-AS ALERT-BOX.
   RETURN "".
END.
      
llOngoingMNPMsg = FALSE.

FOR EACH MNPOperation WHERE
   MNPOperation.MNPSeq = MNPProcess.MNPSeq AND
   MNPOperation.StatusCode < 10 NO-LOCK:
   llOngoingMNPMsg = TRUE.
END.

IF llOngoingMNPMsg THEN DO:
   MESSAGE "Ongoing messages" VIEW-AS ALERT-BOX ERROR.
   RETURN "".
END.

RUN selectbox.p(
  "Create MNP Message",
  lcOptions,
  OUTPUT lcSelected).

CASE lcSelected:

   WHEN {&MNP_CANCEL} THEN DO:
     
     ehto = 10.
     run ufkey.p.
     
     UPDATE 
      lcCancellationReason column-label "Reason code"
      WITH frame a 
      overlay centered row 5 title lcSelected.

     if lookup(keylabel(LASTKEY),"f1,return") > 0 then do:
         
         RUN mnp_operation.p(MNPProcess.MNPSeq,
            "cancel",
            lcCancellationReason).
         IF RETURN-VALUE NE "OK" THEN
            MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
      end.
   END.
   
   WHEN {&MNP_REJECT} THEN DO:
     ehto = 10.
     run ufkey.p.
     
     UPDATE 
      lcRejectionReason column-label "Reason code"
      WITH frame b 
      overlay centered row 5 title lcSelected.

     if lookup(keylabel(LASTKEY),"f1,return") > 0 then do:
         
         RUN mnp_operation.p(MNPProcess.MNPSeq,
            "reject",
            lcRejectionReason).
         IF RETURN-VALUE NE "OK" THEN
         MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
      end.
   END.
   
   WHEN {&MNP_CONFIRM} THEN fSendConfirmation(MNPProcess.PortRequest).
   WHEN {&MNP_CANCEL_DETAIL} THEN fSendCancellationDetailQuery(MNPProcess.PortRequest).
   WHEN {&MNP_PR_QUERY} THEN RUN pCreatePortabilityQuery.      
   WHEN {&MNP_NT_QUERY} THEN RUN pCreateNumberTermQuery.
   WHEN {&MNP_NR_QUERY} THEN RUN pCreateNumberRangesQuery.
   WHEN {&MNP_NM_DETAIL} THEN fSendMigrationDetailRequest(MNPProcess.PortRequest).
   WHEN {&MNP_NM_FINALIZE} THEN fSendMigrationFinalizeRequest(MNPProcess.PortRequest).
   WHEN {&MNP_NMN_DETAIL} THEN fSendMigrationNumberDetailRequest(MNPProcess.PortRequest).
   WHEN {&MNP_NR_DETAIL} THEN DO:
     
     ehto = 10.
     run ufkey.p.
     
     UPDATE 
      lcNumberRangeCode column-label "Code"
      WITH frame d 
      overlay centered row 5 title lcSelected.

     if lookup(keylabel(LASTKEY),"f1,return") > 0 then
     IF NOT fSendNumberRangesDetail(lcNumberRangeCode) then
      MESSAGE lcError VIEW-AS ALERT-BOX.
   
   END.
   WHEN {&MNP_PR_DETAIL} THEN fSendPortabilityDetailQuery(MNPProcess.PortRequest).
   WHEN {&MNP_PR_REQUESTED} THEN DO:
     
     ehto = 10.
     run ufkey.p.
     
     UPDATE 
      liPages column-label "Pages"
      WITH frame e 
      overlay centered row 5 title lcSelected.

     if lookup(keylabel(LASTKEY),"f1,return") > 0 then
     IF NOT fSendRequestedQuery(liPages) then
      MESSAGE lcError VIEW-AS ALERT-BOX.

   END.
   WHEN {&MNP_NT_CANCEL} THEN DO:
      fSendNumberTerminationCancel(MNPProcess.PortRequest).
      fMakeCreateEvent(lhTable,"",katun,"").
   END.
   WHEN {&MNP_NT_DETAIL} THEN fSendNumberTerminationDetail(MNPProcess.PortRequest).

END.
hide frame a NO-PAUSE.
hide frame b NO-PAUSE.
hide frame c NO-PAUSE.
hide frame d NO-PAUSE.
hide frame e NO-PAUSE.

PROCEDURE pCreatePortabilityQuery:
   
   DEF FRAME a.     
   CREATE ttPortabilityQuery.
   ehto = 10.
   run ufkey.p.

   UPDATE ttPortabilityQuery WITH 
      FRAME a overlay 2 col row 2 centered
      title "Create Portability Request Query".
   hide frame a NO-PAUSE.

   if lookup(keylabel(LASTKEY),"f1,return") > 0 then
   fSendPortabilityActivationRequestsQuery(INPUT TABLE ttPortabilityQuery BY-REFERENCE).
   
   EMPTY TEMP-TABLE ttPortabilityQuery.

END PROCEDURE. 

PROCEDURE pCreateNumberRangesQuery:
   
   DEF FRAME a.     
   CREATE ttNumberRangesQuery.
   ehto = 10.
   run ufkey.p.

   UPDATE ttNumberRangesQuery WITH 
      FRAME a overlay 1 col row 2 centered
      title "Create Numbering Ranges Query".
   hide frame a NO-PAUSE.

   if lookup(keylabel(LASTKEY),"f1,return") > 0 then
   fSendNumberRangesQuery(INPUT TABLE ttNumberRangesQuery BY-REFERENCE).
   
   EMPTY TEMP-TABLE ttNumberRangesQuery.

END PROCEDURE. 

PROCEDURE pCreateNumberTermQuery:
   
   DEF FRAME a.     
   CREATE ttNumberTermQuery.
   ehto = 10.
   run ufkey.p.

   UPDATE ttNumberTermQuery WITH 
      FRAME a overlay 2 col row 2 centered
      title "Create Numbering Termination Request Query".
   hide frame a NO-PAUSE.

   if lookup(keylabel(LASTKEY),"f1,return") > 0 then
   fSendNumberingTerminationRequestsQuery(INPUT TABLE ttNumberTermQuery BY-REFERENCE).
   
   EMPTY TEMP-TABLE ttNumberRangesQuery.

END PROCEDURE. 
