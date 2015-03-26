/* ------------------------------------------------------
  MODULE .......: msodirequest.p
  FUNCTION .....: create a request for on demand invoice 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.04.07
  MODIFIED .....: 
  Version ......: Yoigo
  ------------------------------------------------------ */

{commali.i}
{timestamp.i}
{cparam2.i}
{fmakemsreq.i}

DEF INPUT PARAMETER iiMsSeq AS INT NO-UNDO. 

DEF VAR llOk       AS LOG  NO-UNDO.
DEF VAR liCreated  AS INT  NO-UNDO.
DEF VAR lcError    AS CHAR NO-UNDO. 
DEF VAR ldODIStamp AS DEC  NO-UNDO.
DEF VAR ldODITime  AS DEC  NO-UNDO.
DEF VAR lcCustName AS CHAR NO-UNDO.
DEF VAR lcActTime  AS CHAR NO-UNDO.

FORM
   SKIP(1)
   "An on demand invoice will be created to subscription," AT 5 SPACE(5) SKIP
   "from all unbilled events up to today."  AT 5 SKIP(1)

   MobSub.CLI COLON 20
      LABEL "MSISDN" 
      SKIP
   MobSub.MsSeq COLON 20
      LABEL "Subscription ID"
      FORMAT ">>>>>>>>>>9"
      SKIP
   MobSub.InvCust COLON 20
      LABEL "Invoice Customer"
   lcCustName 
      NO-LABEL
      FORMAT "X(40)"
      SKIP(1)
   lcActTime COLON 20
      LABEL "Activation"
      FORMAT "X(20)" 
      SKIP(1)
WITH ROW 7 OVERLAY SIDE-LABELS CENTERED 
     TITLE " ON DEMAND INVOICE " FRAME fCriter.

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown subscription"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* is there a change request already */
IF fPendingRequest(iiMsSeq,20) THEN DO:
   MESSAGE "There is a pending request for OD invoice."
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END. 

FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
                              
/* activation time for odi requests */
ldODITime = fCParamDE("ODIRequestAct").
IF ldODITime = ? OR ldOdiTime = 0 THEN ldODITime = 12.
ldODIStamp = fMake2Dt(TODAY,
                      INTEGER(TRUNCATE(ldODITime,0) * 3600 + 
                              100 * (ldODITime - TRUNCATE(ldODITime,0)) * 60)).

lcActTime = fTS2HMS(ldODIStamp).

PAUSE 0.
DISPLAY MobSub.CLI
        MobSub.MsSeq
        MobSub.InvCust
        lcCustName
        lcActTime
WITH FRAME fCriter.

MakeReq:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO MakeReq, NEXT MakeReq:

   PAUSE 0.
   VIEW FRAME fCriter.

   ASSIGN
      ufk   = 0  
      ufk[5]= 1027  
      ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   IF toimi = 5 THEN DO:

      llOk = FALSE.
      MESSAGE "An on demand invoice will be created." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
      
      liCreated = fODInvoiceRequest(MobSub.InvCust,
                                    MobSub.MsSeq,
                                    ldODIStamp,
                                    FALSE,   /* create fees */
                                    FALSE,   /* send sms */
                                    "",
                                    OUTPUT lcError).
                                      
      IF liCreated > 0 THEN 
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.
         
      ELSE 
         MESSAGE "Request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. /* MakeReq */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

