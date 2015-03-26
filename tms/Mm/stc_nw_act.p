/* ----------------------------------------------------------------------
  MODULE .......: stc_nw_act.p
  TASK .........: Send activation command without termination to network
                  in Postpaid->Prepaid or Prepaid->Postpaid cases. YDR-152.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 21.03.11
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commali.i}
{date.i}
{msreqfunc.i}
{provision.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiFromStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiToStatus   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icCparam     AS CHARACTER NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.

DEF BUFFER bOldType  FOR CLIType.

/* Get request to buffer */
FIND MsRequest WHERE
     MsRequest.MsRequest = iiMsRequest
NO-LOCK NO-ERROR.

FIND bOldType WHERE
     bOldType.Brand   = gcBrand AND
     bOldType.CLIType = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.

FIND CLIType WHERE
     CLIType.Brand   = gcBrand AND
     CLIType.CLIType = MsRequest.ReqCParam2 NO-LOCK NO-ERROR.

/* stc activation solog needed only when paytype changes */ 
IF bOldType.PayType NE CliType.PayType THEN DO TRANSACTION:

   IF CAN-FIND(FIRST Solog WHERE
               Solog.MsSeq = MsRequest.MsSeq AND
               Solog.MsRequest = MsRequest.MsRequest AND
               LOOKUP(STRING(Solog.Stat),"0,5") > 0) THEN DO:
       MESSAGE "Ongoing network commands" VIEW-AS ALERT-BOX ERROR.
       RETURN.
   END.

   /* Confirmation */
   llOk = FALSE.
   MESSAGE "Send activation command to network?" 
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET llOk.
      
   IF NOT llOk THEN DO:
      RETURN.
   END.
   
   CREATE Solog.
   ASSIGN
      Solog.Solog = NEXT-VALUE(Solog).
 
   ASSIGN
      Solog.CreatedTS    = fMakeTS()
      Solog.MsSeq        = MsRequest.MsSeq    /* Mobile Subscription No.    */
      Solog.CLI          = MsRequest.CLI      /* MSISDN                     */
      Solog.Stat         = 0                  /* just created               */
      Solog.Brand        = MSRequest.Brand
      Solog.Users        = MSREquest.UserCode
      Solog.TimeSlotTMS  = MsRequest.ActStamp
      Solog.ActivationTS = MsRequest.ActStamp
      Solog.MSrequest    = MSRequest.MSRequest.
      
   Solog.CommLine = fMakeCommLine2(Solog.Solog,MsRequest.MSrequest, True).

   ASSIGN
      SoLog.CommLine = TRIM(REPLACE(SoLog.CommLine,",,",","),",").

   /* mark payment plan id to request */
   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   MsRequest.Solog = Solog.Solog.
   fReqStatus(5,"").
   FIND CURRENT MsRequest NO-LOCK.

   MESSAGE "Network activation command sent" VIEW-AS ALERT-BOX.
END.
ELSE DO:
   MESSAGE "Function not allowed." SKIP
           "STC should be Postpaid->Prepaid or Prepaid->Postpaid"
  VIEW-AS ALERT-BOX.
END.
