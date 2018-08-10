/* fcreatereq.i       29.08.07/aam separated from fmakemsreq.i
                      14.12.07/jt  new status for pending passthrough 99 for
                                   agreement customer change (special case)
   common functions for request creation
   
   changed:   
   
*/
   
&IF "{&fcreatereq}" NE "YES"
&THEN

&GLOBAL-DEFINE fcreatereq YES

{Syst/tmsconst.i}

DEF BUFFER bReqSub   FOR MobSub.
DEF BUFFER bCreaReq  FOR MsRequest.

/*  customer based request types */
&SCOPED-DEFINE REQ_CUST_REQUESTS "5,6,7,11,12,22,23,83,84,86,99"
/* types that can be issued to a closed or non-existing subscription */
&SCOPED-DEFINE REQ_CLOSED_SUBS "8,9,13,14,20,87"
 
/* is there a pending request */
FUNCTION fPendingRequest RETURNS LOGICAL
   (iiTarget  AS INT,   /* Mobsub.MsSeq or Customer.CustNum */
    iiReqType AS INT).  /* if ? then check all requests */

   DEF VAR liPendReq AS INT NO-UNDO.
   
   DEF BUFFER bf_MsRequest FOR MsRequest.

   /* a specific type of request */
   IF iiReqType NE ? THEN DO:
   
      /* more than one are possible */
      IF LOOKUP(STRING(iiReqType),"9,13,81") > 0 THEN RETURN FALSE.
      
      IF LOOKUP(STRING(iiReqType),{&REQ_CUST_REQUESTS}) > 0 
      THEN RETURN CAN-FIND(FIRST MsRequest WHERE
                                 MsRequest.Brand     = Syst.Var:gcBrand   AND
                                 MsRequest.ReqType   = iiReqType AND
                                 MsRequest.CustNum   = iiTarget  AND
                                 MsRequest.ReqStatus = 0).
      ELSE 
      DO:
          IF iiReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN 
          DO:
              FOR EACH bf_MsRequest WHERE
                       bf_MsRequest.MsSeq   = iiTarget  AND
                       bf_MsRequest.ReqType = iiReqType AND
                       LOOKUP(STRING(bf_MsRequest.ReqStatus),"2,4,9,99") = 0 NO-LOCK:

                  IF bf_MsRequest.ReqStatus = {&REQUEST_STATUS_REJECTED} AND bf_MsRequest.Memo BEGINS "Fixed line fiber speed change request failed" THEN
                      NEXT.

                  RETURN TRUE.    
              END.         
          END.
          ELSE            
              RETURN CAN-FIND(FIRST MsRequest WHERE
                                    MsRequest.MsSeq     = iiTarget  AND
                                    MsRequest.ReqType   = iiReqType AND
                                    LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0).
      END.
   END.
                            
   /* any request */                             
   ELSE FOR EACH RequestType NO-LOCK WHERE
                 RequestType.Brand = Syst.Var:gcBrand AND
                 RequestType.InUse = TRUE:
      
      IF LOOKUP(STRING(RequestType.ReqType),{&REQ_CUST_REQUESTS}) > 0 THEN DO: 
         IF CAN-FIND(FIRST MsRequest WHERE
                           MsRequest.Brand     = Syst.Var:gcBrand   AND
                           MsRequest.ReqType   = RequestType.ReqType AND
                           MsRequest.CustNum   = iiTarget  AND
                           MsRequest.ReqStatus = 0)
         THEN RETURN TRUE.
      END.    
         
      ELSE IF 
         CAN-FIND(FIRST MsRequest WHERE
                        MsRequest.MsSeq     = iiTarget  AND
                        MsRequest.ReqType   = RequestType.ReqType AND
                        LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0)
      THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
    
END FUNCTION.

/* check basic validity of request */
FUNCTION fChkRequest RETURNS CHARACTER
   (iiTarget  AS INT,
    iiReqType AS INT,
    icParam   AS CHAR,
    icExtraParam AS CHAR).
   
   DEF VAR llExist AS LOG NO-UNDO.
   DEF VAR liLoop  AS INT NO-UNDO. 
   DEF VAR licount AS INT NO-UNDO.
   DEF BUFFER bCheckMsRequest FOR MsRequest.
   licount = NUM-ENTRIES({&REQ_ONGOING_STATUSES}).
   
   IF LOOKUP(STRING(iiReqType),{&REQ_CUST_REQUESTS}) = 0 THEN DO:
      
      FIND bReqSub WHERE bReqSub.MsSeq = iiTarget NO-LOCK NO-ERROR.
      IF LOOKUP(STRING(iiReqType),{&REQ_CLOSED_SUBS}) = 0 THEN DO:
         IF NOT AVAILABLE bReqSub THEN RETURN "Unknown subscription".
      END.
      
      /* if an owner change is pending then some requests are not allowed */
      IF LOOKUP(STRING(iiReqType),"0,3,4,8,10,13,19") > 0 AND
         fPendingRequest(iiTarget,10) 
      THEN RETURN "Pending ACC exists".
   END.
   
   llExist = FALSE.
   
   skip-MsRequest:
   DO:
      /* only one entry can be active */
      IF icParam = "" 
      THEN llExist = fPendingRequest(iiTarget,
                                     iiReqType).
      ELSE DO:
         CASE iiReqType:
         WHEN 22 OR WHEN 23 THEN DO:

            PENDING_CREDIT_NOTES:
            FOR EACH bCheckMsRequest NO-LOCK USE-INDEX Custnum where
                     bCheckMsRequest.Brand      = Syst.Var:gcBrand   AND
                     bCheckMsRequest.ReqType    = iiReqType AND
                     bCheckMsRequest.CustNum    = iiTarget  AND
                     bCheckMsRequest.ReqIParam1 = INTEGER(icParam) AND
                     LOOKUP(STRING(bCheckMsRequest.ReqStatus),"2,4,9,99") = 0:
               
               IF icExtraParam EQ "" OR
                  bCheckMsRequest.ReqCparam4 EQ "" THEN DO:
                  llExist = TRUE.
                  LEAVE PENDING_CREDIT_NOTES.
               END.
               /* Allow parallel subinvoice specific credit notes. YTS-8744 */
               ELSE DO:
                  DO liLoop = 1 TO NUM-ENTRIES(icExtraParam):
                     IF LOOKUP(ENTRY(liLoop, icExtraParam),
                               bCheckMsRequest.ReqCparam4) > 0 THEN DO:
                        llExist = TRUE.
                        LEAVE PENDING_CREDIT_NOTES.
                     END.
                  END.  
               END.
            END.

         END.
         WHEN 8 THEN DO:

            IF icExtraParam = "999" THEN LEAVE skip-MsRequest.

            /* special case by pass for Revert renewal Order source (ie 22) */
            ELSE IF icExtraParam <> "22" THEN DO:

               CHECK_LOOP:
               DO liLoop = 1 TO licount:
                  llExist = CAN-FIND(FIRST MsRequest WHERE
                                     MsRequest.MsSeq      = iiTarget  AND
                                     MsRequest.ReqType    = iiReqType AND
                                     MsRequest.ReqStatus  = INT(ENTRY(liLoop,({&REQ_ONGOING_STATUSES}))) AND
                                     MsRequest.ReqCParam3 = icParam).
                  IF llExist THEN LEAVE CHECK_LOOP.
               END.
            END.
         END.
         WHEN 13 THEN llExist = FALSE.

         /* Only one BTC request is allowed for one contract */
         WHEN 81 THEN
            llExist = CAN-FIND(FIRST MsRequest WHERE
                               MsRequest.MsSeq      = iiTarget  AND
                               MsRequest.ReqType    = iiReqType AND
                               MsRequest.ReqCParam1 = icParam   AND
                               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0).

         WHEN 83 THEN DO:
            IF icParam = "CREATE" OR icParam = "DELETE" THEN
               llExist = CAN-FIND(FIRST MsRequest USE-INDEX CustNum WHERE
                                  MsRequest.Brand      = Syst.Var:gcBrand   AND
                                  MsRequest.ReqType    = iiReqType AND
                                  MsRequest.CustNum    = iiTarget  AND
                                  MsRequest.ReqCParam1 = icParam   AND
                                  LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0).
         END. /* WHEN 83 THEN DO: */
         WHEN 84 THEN
            llExist = CAN-FIND(FIRST MsRequest USE-INDEX CustNum WHERE
                               MsRequest.Brand      = Syst.Var:gcBrand   AND
                               MsRequest.ReqType    = iiReqType AND
                               MsRequest.CustNum    = iiTarget  AND
                               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0).
      
         OTHERWISE DO:

            /* YTS-2473, YBU-1194 SHAPER/HSDPA request must be allowed 
             even if there's active SHAPER/HSDPA request */
            IF iiReqType EQ 1 AND LOOKUP(icParam,"SHAPER,HSDPA,BB,TEMPLATE") > 0
            THEN llExist = FALSE.
            ELSE DO:
               CHECK_LOOP2:
               DO liLoop = 1 TO licount:
                  llExist = CAN-FIND(FIRST MsRequest WHERE
                                          MsRequest.MsSeq      = iiTarget  AND
                                          MsRequest.ReqType    = iiReqType AND
                                          MsRequest.ReqCParam1 = icParam   AND
                                          MsRequest.ReqStatus  = INT(ENTRY(liLoop,({&REQ_ONGOING_STATUSES})))).
                  IF llExist THEN LEAVE CHECK_LOOP2.
               END.
            END.
         END.
         END CASE.                            
      END.
   END.   

   IF llExist THEN DO:
      
      /* web-user can cancel his own request */
      IF Syst.Var:katun BEGINS "WEB" THEN DO:
         IF icParam = "" THEN DO:
            IF LOOKUP(STRING(iiReqType),{&REQ_CUST_REQUESTS}) > 0
            THEN FIND FIRST bCreaReq NO-LOCK WHERE
                            bCreaReq.Brand     = Syst.Var:gcBrand   AND
                            bCreaReq.ReqType   = iiReqType AND
                            bCreaReq.CustNum   = iiTarget  AND
                            bCreaReq.ReqStatus = 0 NO-ERROR.
            ELSE FIND FIRST bCreaReq NO-LOCK WHERE
                            bCreaReq.MsSeq     = iiTarget   AND
                            bCreaReq.ReqType   = iiReqType AND
                            bCreaReq.ReqStatus = 0 NO-ERROR.
         END.                   
         ELSE FIND FIRST bCreaReq NO-LOCK WHERE
                         bCreaReq.MsSeq      = iiTarget   AND
                         bCreaReq.ReqType    = iiReqType AND
                         bCreaReq.ReqCParam1 = icParam   AND
                         bCreaReq.ReqStatus  = 0 NO-ERROR.
       
         IF AVAILABLE bCreaReq AND 
            bCreaReq.ReqStatus = 0 AND
            bCreaReq.UserCode  = "WEB / " + icExtraParam
         THEN DO:
            FIND CURRENT bCreaReq EXCLUSIVE-LOCK.
            ASSIGN bCreaReq.ReqStatus = 4
                   bCreaReq.Memo      = "WEB user cancelled".
            RETURN "".
         END.
      END.
      /*TODO MMM Migration: : if in migration should be support for
        multiple upsells here we must return "" even there are ongoing 
        requests. Now implementation is not done because migration
        project is on hold. */  
      RETURN (IF LOOKUP(STRING(iiReqType),{&REQ_CUST_REQUESTS}) > 0
              THEN "Customer" 
              ELSE "Subscription") + " already has an active request".
   END.
   
   ELSE RETURN "".
       
END FUNCTION.

/* create record with common values */
FUNCTION fCreateRequest RETURNS LOGICAL
   (INPUT  iiReqType    AS INT,
    INPUT  idChgStamp   AS DEC,
    INPUT  icCreator    AS CHAR,
    INPUT  ilCreateFees AS LOG,
    INPUT  ilSendSMS    AS LOG).

   DEF VAR llSubsRequest AS LOG NO-UNDO. 

   IF idChgStamp = 0 THEN idChgStamp = Func.Common:mMakeTS().
   llSubsRequest = (LOOKUP(STRING(iiReqType),{&REQ_CUST_REQUESTS}) = 0 AND 
                    AVAILABLE bReqSub).

   CREATE bCreaReq.
   ASSIGN bCreaReq.MsRequest  = NEXT-VALUE(MsRequest)
          bCreaReq.ReqType    = iiReqType
          bCreaReq.Brand      = Syst.Var:gcBrand
          bCreaReq.UserCode   = (Syst.Var:katun + (IF icCreator > ""
                                         THEN " / " + icCreator 
                                         ELSE ""))
          bCreaReq.ActStamp   = idChgStamp
          bCreaReq.ReqStatus  = 0
          bCreaReq.CreateFees = ilCreateFees
          bCreaReq.SendSMS    = INTEGER(ilSendSMS)
          bCreaReq.MsSeq      = bReqSub.MsSeq WHEN llSubsRequest
          bCreaReq.CLI        = bReqSub.CLI WHEN llSubsRequest
          bCreaReq.CustNum    = bReqSub.CustNum WHEN llSubsRequest
          bCreaReq.CreStamp   = Func.Common:mMakeTS().

END FUNCTION.

&ENDIF

