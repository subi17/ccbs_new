/* ------------------------------------------------------
  MODULE .......: dcclifees.p
  FUNCTION .....: create a request for maintenance of a periodical contract 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 17.03.08
  MODIFIED .....: 
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fpcmaintreq.i}

DEF INPUT PARAMETER iiMsSeq   AS INT NO-UNDO. 
DEF INPUT PARAMETER icDCEvent AS CHAR NO-UNDO.

DEF VAR llOk         AS LOG  NO-UNDO.
DEF VAR liCreated    AS INT  NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO. 
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR llCreateFees AS LOG  NO-UNDO.
DEF VAR lcMemoText   AS CHAR NO-UNDO.

FORM
   SKIP(1)
   "Periodical contract will be changed." AT 5 
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
   icDCEvent COLON 20
      LABEL "Contract"
      HELP "Periodical contract ID"
      FORMAT "X(12)"
   DayCampaign.DCName
      FORMAT "X(30)"
      NO-LABEL
      SKIP
   llCreateFees COLON 20 
      LABEL "Create Fees"
      FORMAT "Yes/No"
      HELP "Create fees when contract is terminated"
   lcMemoText COLON 20
      LABEL "Memo"
      HELP "Reason for changing contract"
      VIEW-AS EDITOR SIZE 50 BY 4
WITH ROW 3 OVERLAY SIDE-LABELS CENTERED 
     TITLE " CHANGE PERIODICAL CONTRACT " FRAME fCriter.

FUNCTION fLocalPendingRequest RETURNS LOGICAL:

   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                            MsRequest.MsSeq      = iiMsSeq   AND
                            MsRequest.ReqType    = 8         AND
                            MsRequest.ReqCParam3 = icDCEvent AND
                            LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0).

   IF llExist THEN 
   MESSAGE "Subscription has a pending request for contract maintenance."
   VIEW-AS ALERT-BOX INFORMATION.

   RETURN llExist.
   
END FUNCTION.


FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown subscription"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.

FIND FIRST DCCLI WHERE
           DCCLI.Brand   = gcBrand   AND
           DCCLI.DCEvent = icDCEvent AND
           DCCLI.MSSeq   = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE DCCLI THEN DO:
   MESSAGE "Contract not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF DCCLI.ValidTo <= TODAY THEN DO:
   MESSAGE "Contract has already expired"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

IF fLocalPendingRequest() THEN RETURN.
 
FIND FIRST DayCampaign WHERE
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = icDCEvent NO-LOCK NO-ERROR.
IF NOT AVAILABLE DayCampaign THEN RETURN.
      
ASSIGN
   lcCustName   = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                   BUFFER Customer)
   llCreateFees = DCCLI.CreateFees
   toimi        = -1.

MakeReq:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO MakeReq, NEXT MakeReq:

   PAUSE 0.
   DISPLAY MobSub.CLI
           MobSub.MsSeq
           MobSub.CustNum
           lcCustName
           icDCEvent
           DayCampaign.DCName WHEN AVAILABLE DayCampaign
           llCreateFees
           lcMemoText
   WITH FRAME fCriter.

   IF toimi < 0 THEN toimi = 1.
   ELSE DO:
      ASSIGN
         ufk    = 0  
         ufk[1] = 7
         ufk[5] = IF llCreateFees NE DCCLI.CreateFees 
                  THEN 1027 
                  ELSE 0 
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   IF toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
      
         ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE 
            llCreateFees
            lcMemoText
         WITH FRAME fCriter.
         
         LEAVE.
      END.
      
   END.

   ELSE IF toimi = 5 THEN DO:

      /* is there a request already (check once more; who knows how much
         time has been spent on this screen) */
      IF fLocalPendingRequest() THEN NEXT.

      IF lcMemoText = "" THEN DO:
         MESSAGE "Mandatory value is missing"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      llOk = FALSE.
      MESSAGE "Periodical contract will be changed." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
 
      liCreated = fPCMaintenanceRequest(MobSub.MsSeq,
                                        icDCEvent,
                                        "CreateFees",
                                        STRING(llCreateFees),
                                        ?,
                                        FALSE,
                                        "4",
                                        "",
                                        0,
                                        FALSE,
                                        OUTPUT lcError).

      IF liCreated > 0 THEN DO:

         CREATE Memo.
         ASSIGN 
            Memo.Brand     = gcBrand
            Memo.HostTable = "MobSub"
            Memo.KeyValue  = STRING(MobSub.MsSeq)
            Memo.CustNum   = MobSub.CustNum
            Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
            Memo.CreUser   = katun 
            Memo.MemoTitle = "Periodical Contract Changed"
            Memo.MemoText  = "Contract: " + icDCEvent + CHR(10) + 
                             lcMemoText.
            Memo.CreStamp  = fMakeTS().
         
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.

      END.   

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

