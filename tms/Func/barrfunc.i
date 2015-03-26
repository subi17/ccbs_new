
/* ----------------------------------------------------------------------
  MODULE .......: barrfunc.i
  TASK .........: Barring functions
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 28.01.08
  CHANGED ......: 15.05.08 Case for manual acceptance  
  Version ......: xfera
----------------------------------------------------------------------- */

&IF "{&barrfunc}" NE "YES"
&THEN

&GLOBAL-DEFINE barrfunc YES
{fmakemsreq.i}
{tmsconst.i}
{fuserright.i}

DEF BUFFER bBarrReq FOR MsRequest.

FUNCTION fCheckBarrStatus RETURNS CHARACTER
(INPUT iiMsSeq AS INTEGER,
 OUTPUT orBarring AS ROWID):

 /* Check if any other than "completed" master request exists for this mobsub */
   FIND FIRST bBarrReq WHERE
              bBarrReq.MsSeq = iiMsSeq      AND
              bBarrReq.ReqType = 35         AND
             (bBarrReq.ReqStat NE 2         AND 
              bBarrReq.ReqStat NE 4         AND 
              bBarrReq.ReqStat NE 9)
   NO-LOCK NO-ERROR.

   /* Ongoing master request */
   IF AVAIL bBarrReq THEN DO:
      orBarring = ROWID(bBarrReq).
      RETURN "91". /* Error or ongoing network commands */
   END.
   
   /* No ongoing master request, find last Barring request status */
   IF NOT AVAIL bBarrReq THEN DO:
      
      FOR EACH bBarrReq NO-LOCK WHERE
               bBarrReq.MsSeq = iiMsSeq AND
               bBarrReq.ReqType = 35    AND
              (bBarrReq.ReqStat = 2     OR
               bBarrReq.ReqStat = 4     OR
               bBarrReq.ReqStat = 9)
      BREAK BY bBarrReq.UpdateStamp:
      /* Get last barring IF last was removal then no barring on */
         IF LAST(bBarrReq.UpdateStamp) THEN DO:
            CASE bBarrReq.ReqStat:
               WHEN 2 THEN DO:
                  IF bBarrReq.ReqCParam1 BEGINS "UN" THEN RETURN "OK".  
                  ELSE DO:
                     orBarring = ROWID(bBarrReq).
                     RETURN bBarrReq.ReqCParam1.
                  END.
               END.
               OTHERWISE RETURN "OK". /* Cancelled or manually handled
                                         barring not valid */
            END.
         END.
      END.
   END.

   /* No previous barrings at all */
   RETURN "OK".

END FUNCTION.

FUNCTION fCheckStatus RETURNS CHARACTER
(INPUT iiMsSeq AS INTEGER):

   DEF VAR lrBarring AS ROWID NO-UNDO.
   
   RETURN fCheckBarrStatus(iiMsseq, OUTPUT lrBarring). 

END FUNCTION.

FUNCTION fCheckRestore RETURNS RECID
(INPUT iiMsSeq AS INTEGER,
 INPUT icType  AS CHAR):
 
   /* ReqCparam not in index */
   FOR FIRST bBarrReq NO-LOCK WHERE
             bBarrReq.MsSeq   = iiMsSeq AND
             bBarrReq.ReqType = 35      AND
             bBarrReq.ReqStat = 2       AND
             /* Look for CLB/OLB */
             bBarrReq.ReqCParam2 = icType:
      RETURN RECID(bBarrReq).
   END.

   /* No waiting CLB/OLB */
   RETURN ?.

END FUNCTION.

FUNCTION fCreateBP RETURNS LOGICAL
(INPUT icPackage AS CHARACTER,
 INPUT iiInvCust AS INTEGER,
 INPUT icCli     AS CHARACTER,
 INPUT iiMsSeq   AS INTEGER,
 INPUT icCLB     AS CHARACTER,
 INPUT iiMandReq AS INTEGER,
 INPUT icSource  AS CHARACTER,
 INPUT icCreator AS CHARACTER,
 INPUT idActStamp AS DECIMAL,
 INPUT icSMSText  AS CHAR,
 INPUT icPrevBarr AS CHAR,
 OUTPUT oiCrReqId AS INTEGER):
   
   fCreateRequest(35,idActStamp,icCreator,FALSE,FALSE).

    
   ASSIGN bCreaReq.ReqCParam1  = icPackage
          bCreaReq.CustNum     = iiInvCust
          bCreaReq.Cli         = icCli
          bCreaReq.MsSeq       = iiMsSeq
          bCreaReq.ReqCParam2  = icCLB
          bCreaReq.ReqCParam4  = icPrevBarr 
          /* another request must be completed before this request */
          bCreaReq.ReqIParam2  = iiMandReq
          bCreaReq.ReqSource   = icSource 
          oiCrReqId            = bCreaReq.MsRequest.

   /* Need to send SMS (fraud tool) */
   IF icSMSText > "" THEN ASSIGN bCreaReq.SMSText   = icSMSText
                                 bCreaReq.SendSMS   = 1.
   RELEASE bCreaReq.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fGetSpac RETURNS CHARACTER
(INPUT icCliType   AS CHARACTER,
       lcCurPack   AS CHARACTER,
       icPacks     AS CHARACTER): /* C_|Y_|D_ = all packages */

   DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INIT "|". /* DeLimiter */
   DEFINE VARIABLE lcList   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liEnt    AS INTEGER   NO-UNDO.
   /* All barr commands */

   DO liEnt = 1 TO NUM-ENTRIES(icPacks,"|"):
      FOR EACH CtServpac NO-LOCK WHERE
               CtServpac.Brand = "1" /* gcBrand */ AND
               CtServPac.ServPac BEGINS ENTRY(liEnt,icPacks,"|") AND
               CtServpac.CliType   = icCliType:
         FIND FIRST ServPac  WHERE
                    ServPac.Brand   = CTServPac.Brand  AND
                    ServPAc.ServPac = CTServPac.ServPac
         NO-LOCK NO-ERROR.
         IF ServPac.ServPac = lcCurPack THEN NEXT. /* remove existing pack */
         lcList = lcList + ServPac.ServPac + lcSep.
      END.
   END.
   /* TRIM | if last character */
   IF lcList NE "" AND SUBSTRING(lcList,LENGTH(lcList),1) = "|" THEN DO:
      lcList = SUBSTRING(lcList,1,LENGTH(lcList) - 1).
   END.
   
   RETURN lcList.

END FUNCTION.

FUNCTION fUserGroup RETURNS CHARACTER
(INPUT icUserId AS CHARACTER):
  
   IF fTokenRights(icUserId,"SYST") = "RW" THEN RETURN "C_|Y_|D_".
   IF fTokenRights(icUserId,"CCSUPER") = "RW" THEN RETURN "C_|Y_".
   IF fTokenRights(icUserId,"CC") = "RW" THEN RETURN "C_".
   
   RETURN "".

END FUNCTION.

FUNCTION fExistBarredSubForCustomer RETURNS LOGICAL 
         (INPUT piCustNum AS INT): 
   
   DEFINE VARIABLE lcListBarring AS CHARACTER NO-UNDO.  
   lcListBarring = fCParamC("BarringOrderPermission").

   DEF BUFFER MobSub FOR MobSub.

   FOR EACH MobSub NO-LOCK WHERE 
            MobSub.Brand = gcBrand AND 
            MobSub.CustNum = piCustNum AND 
            MobSub.MsStatus = 8 : 
      IF LOOKUP(fCheckStatus(MobSub.MsSeq),lcListBarring) > 0 THEN 
         RETURN TRUE.
   END.
   RETURN FALSE.
END FUNCTION.

&ENDIF
