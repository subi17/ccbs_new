/* ------------------------------------------------------
  MODULE .......: dccliterm.p
  FUNCTION .....: create a request for termination of a periodical contract 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.03.08
  MODIFIED .....: 
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fmakemsreq.i}

DEF INPUT PARAMETER iiMsSeq     AS INT  NO-UNDO. 
DEF INPUT PARAMETER icDCEvent   AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiPerContID AS INT  NO-UNDO. 

DEF VAR llOk         AS LOG  NO-UNDO.
DEF VAR liCreated    AS INT  NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO. 
DEF VAR ldActStamp   AS DEC  NO-UNDO.
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR ldtTermDate  AS DATE NO-UNDO.
DEF VAR ldtFirstDate AS DATE NO-UNDO.
DEF VAR lcMemoText   AS CHAR NO-UNDO.
DEF VAR ldCurrent    AS DEC  NO-UNDO.
DEF VAR ldtDate      AS DATE NO-UNDO.
DEF VAR liTime       AS INT  NO-UNDO.
DEF VAR llCreateFees AS LOG  NO-UNDO.

FORM
   SKIP(1)
   "Periodical contract will be terminated." AT 5 
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
      LABEL "Create Penalty Fee"
      FORMAT "Yes/No"
      SKIP
   ldtTermDate COLON 20
      LABEL "Termination Date"
      HELP "Date when contract will be terminated"
      FORMAT "99-99-99" 
      SKIP(1)
   lcMemoText COLON 20
      LABEL "Memo"
      HELP "Reason for terminating contract"
      VIEW-AS EDITOR SIZE 50 BY 4
WITH ROW 3 OVERLAY SIDE-LABELS CENTERED 
     TITLE " TERMINATE PERIODICAL CONTRACT " FRAME fCriter.

FUNCTION fLocalPendingRequest RETURNS LOGICAL:

   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                            MsRequest.MsSeq      = iiMsSeq   AND
                            MsRequest.ReqType    = 9         AND
                            MsRequest.ReqCParam3 = icDCEvent AND
                            LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0).

   IF llExist THEN 
   MESSAGE "Subscription has a pending request for contract termination"
           "on this contract type."
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

FIND FIRST DayCampaign WHERE
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = icDCEvent NO-LOCK NO-ERROR.
IF NOT AVAILABLE DayCampaign THEN RETURN.

ldCurrent = fMakeTS().

/* service package */
IF DayCampaign.DCType = "1" THEN DO:

   ldtFirstDate = ?.
    
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MsSeq  = MobSub.MsSeq AND
            MServiceLimit.FromTS <= ldCurrent  AND
            MServiceLimit.EndTS  >= ldCurrent, 
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            ServiceLimit.SLSeq     = MServiceLimit.SLSeq  AND
            ServiceLimit.GroupCode = icDCEvent:

      fSplitTS(MServiceLimit.FromTS,
               OUTPUT ldtDate,
               OUTPUT liTime).
               
      IF ldtFirstDate = ? THEN ldtFirstDate = ldtDate.
      ELSE IF ldtDate NE ? THEN ldtFirstDate = MIN(ldtFirstDate,ldtDate).
   END.

   IF ldtFirstDate = ? THEN DO:
      MESSAGE "Contract not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   llCreateFees = (DayCampaign.DCType = "3" AND
                   DayCampaign.TermFeeCalc > 0). 

END.

/* others */
ELSE DO:

   FIND FIRST DCCLI WHERE
              DCCLI.Brand         = gcBrand     AND
              DCCLI.DCEvent       = icDCEvent   AND
              DCCLI.MSSeq         = iiMsSeq     AND 
             (IF icDCEvent BEGINS "PAYTERM" THEN 
                 DCCLI.PerContractId = iiPerContID
              ELSE TRUE)                        NO-LOCK NO-ERROR.
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

   /* first possible termination date */
   ASSIGN 
     ldtFirstDate = DCCLI.ContractDate
     llCreateFees = DCCLI.CreateFees.

END.

IF fLocalPendingRequest() THEN RETURN.
 
IF DayCampaign.DCType = "1" OR
   DayCampaign.DCType = "2" THEN 
FOR EACH SubInvoice NO-LOCK WHERE
         SubInvoice.MsSeq = MobSub.MsSeq,
   FIRST Invoice OF SubInvoice NO-LOCK WHERE
         Invoice.CustNum = MobSub.InvCust AND
         Invoice.InvType = 1
BY Invoice.ToDate DESC:
   ldtFirstDate = Invoice.ToDate + 1.       
   LEAVE.
END.
          
ASSIGN
   lcCustName   = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                   BUFFER Customer)
   ldtTermDate = TODAY
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
           ldtTermDate
           lcMemoText
   WITH FRAME fCriter.

   IF toimi < 0 THEN toimi = 1.
   ELSE DO:
      ASSIGN
         ufk    = 0  
         ufk[1] = 7
         ufk[5] = IF lcMemoText > "" AND
                     ldtTermDate >= ldtFirstDate AND
                     ldtTermDate <= TODAY
                  THEN 1027 
                  ELSE 0 
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.
   END.
   
   IF toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
      
         ehto = 9.
         RUN Syst/ufkey.
         
         UPDATE 
            ldtTermDate
            lcMemoText
         WITH FRAME fCriter EDITING:
         
            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
               PAUSE 0.
        
               IF FRAME-FIELD = "ldtTermDate" THEN DO:
                  IF INPUT ldtTermDate > TODAY THEN DO:
                     MESSAGE "Termination cannot be set to future"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  ELSE IF INPUT ldtTermDate < ldtFirstDate THEN DO:
                     MESSAGE "First possible termination date is"
                            STRING(ldtFirstDate,"99-99-99")
                     VIEW-AS ALERT-BOX INFORMATION.
                     NEXT.
                  END.
               END.
            END.

            APPLY LASTKEY.
         END.
      
         LEAVE.
      END.
      
   END.

   ELSE IF toimi = 5 THEN DO:

      /* is there a request already (check once more; who knows how much
         time has been spent on this screen) */
      IF fLocalPendingRequest() THEN NEXT.

      llOk = FALSE.
      MESSAGE "Periodical contract will be terminated." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
 
      ldActStamp = fMake2Dt(ldtTermDate,
                            IF ldtTermDate = TODAY
                            THEN TIME
                            ELSE 86399).
                            
      liCreated = fPCActionRequest(MobSub.MsSeq,
                                   icDCEvent,
                                   "term",
                                   ldActStamp,
                                   llCreateFees,
                                   "4",
                                   "",
                                   0,
                                   FALSE,
                                   "",
                                   0,
                                   IF icDCEvent BEGINS "PAYTERM" 
                                      THEN iiPerContID
                                   ELSE 0,
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
            Memo.MemoTitle = "Periodical Contract Terminated"
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

