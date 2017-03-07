/* ----------------------------------------------------------------------
  MODULE .......: mnpanalyse.p 
  TASK .........: Automatic analysing of requested mnp out processes
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 03.11.09
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "MNPAnalyse".
gcBrand = "1".

{Func/barrfunc.i}
{Syst/tmsconst.i}
{Func/log.i}
{Func/timestamp.i}
{Func/heartbeat.i}

DEF BUFFER bMNPProcess FOR MNPProcess.

DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause    AS INTEGER   NO-UNDO.
DEFINE VARIABLE llNagBeat  AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE liTimeOut  AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcURL      AS CHARACTER NO-UNDO. 

FORM
   SKIP(1)
   " Loops: " liLoop FORMAT ">>>>>9" lcTime FORMAT "X(20)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " MNP OUT PROCESS ANALYSIS " WIDTH 40 ROW 8
FRAME frmMain .

DISP
   liLoop 
   STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
WITH FRAME frmMain.
PAUSE 0.

DO WHILE TRUE:

   liLoop = liLoop + 1.

   DISP
      liLoop
      STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmMain.
   PAUSE 0.

   RUN pMNPAnalyse.   

   PUT SCREEN ROW 22 COL 1
      "F8 TO QUIT, OTHER KEYS START HANDLING IMMEDIATELLY".
    
   liPause = 60.
   
   IF llNagBeat THEN fKeepAlive("mnpanalyse:MNP Analyse"). 

   READKEY PAUSE liPause.
   
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
      fCloseLog().
      QUIT.
   END.
   
   PUT SCREEN ROW 22 COL 1
      "ANALYSING PROCESSES ............................".

END.

DEF BUFFER bTermMobsub FOR TermMobsub.

PROCEDURE pMNPAnalyse:

   DEFINE VARIABLE lcBarring AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcBarrings AS CHARACTER NO-UNDO INIT "Cust_LOST". 
   DEFINE VARIABLE lcRejectReason AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldeCreated AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
   DEFINE VARIABLE llIsPrepaid AS LOGICAL NO-UNDO. 

   /* just to make sure that MNP NC state is updated after reading in to tms */
   ldeCreated = fSecOffSet(fMakeTS(),-10).

   MNPPROCESS_LOOP:
   FOR EACH MNPProcess WHERE
      MNPProcess.Brand = gcBrand AND
      MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
      MNPProcess.StatusCode = {&MNP_ST_ASOL} AND
      MNPProcess.StateFlag = {&MNP_STATEFLAG_NOT_ANALYSED} AND
      MNPProcess.CreatedTS < ldeCreated EXCLUSIVE-LOCK:

      lcRejectReason = "".

      SUBS_LOOP:
      FOR EACH MNPSub WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq EXCLUSIVE-LOCK:
      
         liCustnum = -1.

         FIND MobSub WHERE
            Mobsub.MsSeq = MNPSub.MsSeq AND 
            Mobsub.cli = MNPSub.CLI NO-LOCK NO-ERROR.
         
         /* number termination process check is missing */
         IF AVAIL MobSub THEN DO:
            
            /* check icc for prepaids */
            IF Mobsub.PayType = True AND MobSub.ICC NE MNPSub.ICC THEN DO:
               lcRejectReason = "RECH_ICCID".
               LEAVE SUBS_LOOP.
            END.
            
            /* check current barring (or pending) */
            lcBarring = fCheckStatus(MobSub.MsSeq).

            /* no barring or pending barring */
            IF lcBarring = "OK" OR lcBarring = "91" THEN .
            ELSE IF LOOKUP(lcBarring,lcBarrings) > 0 THEN DO:
               lcRejectReason = "RECH_PERDI".
               LEAVE SUBS_LOOP.
            END.
            
            ASSIGN
               liCustnum = MobSub.Custnum
               llIsPrepaid = MobSub.PayType. 

         ENd.
         ELSE DO:
            
            FIND FIRST Msisdn WHERE
               Msisdn.Brand = gcBrand AND
               Msisdn.CLI = MNPSub.CLI NO-LOCK USE-INDEX CLI NO-ERROR.

            IF AVAIL Msisdn AND
                     Msisdn.StatusCode = {&MSISDN_ST_RETURNED} THEN DO:
               lcRejectReason = "RECH_BNUME".
               LEAVE SUBS_LOOP.
            END.
            
            IF NOT AVAIL Msisdn OR 
               (Msisdn.StatusCode NE {&MSISDN_ST_WAITING_RETURN} AND 
                Msisdn.StatusCode NE {&MSISDN_ST_RETURN_NOTICE_SENT}) THEN DO:
               lcRejectReason = "RECH_IDENT".
               LEAVE SUBS_LOOP.
            END.

            RELEASE TermMobsub.
            FOR FIRST bTermMobsub WHERE
                bTermMobsub.CLI = MnpSub.CLI NO-LOCK BY bTermMobsub.ActivationDate DESC:
               FIND TermMobsub WHERE
                  ROWID(TermMobsub) = ROWID(bTermMobsub) NO-LOCK.
            END.
           
            /* should not happen that TermMobsub is not found */
            IF NOT AVAIL TermMobsub THEN DO:
               fLogError("TermMobsub not found: " + MNPProcess.PortRequest).
               lcRejectReason = "RECH_IDENT".
               LEAVE SUBS_LOOP.
            END.

            IF MNPSub.MsSeq = ? OR MNPSub.MsSeq = 0 THEN MNPSub.MsSeq = TermMobsub.MsSeq.
         
            IF TermMobsub.PayType = True AND
               TermMobSub.ICC NE MNPSub.ICC THEN DO:
               lcRejectReason = "RECH_ICCID".
               LEAVE SUBS_LOOP.
            END.
            
            ASSIGN
               liCustnum = TermMobSub.Custnum
               llIsPrepaid = (TermMobSub.clitype begins "tarj"). 
         END.
           
         FIND Customer WHERE
              Customer.Custnum = liCustnum NO-LOCK NO-ERROR.
         
         IF NOT AVAIL Customer THEN DO:
            fLogError("Customer not found: " + MNPProcess.PortRequest).
            lcRejectReason = "RECH_IDENT".
            LEAVE SUBS_LOOP.
         END.

         FIND MNPDetails WHERE
            MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.
         IF NOT AVAIL MNPDetails THEN DO:
            fLogError("MNPDetails not found: " + MNPProcess.PortRequest).
            NEXT MNPPROCESS_LOOP.
         END.

         IF NOT llIsPrepaid AND
            (Customer.OrgId NE MNPDetails.CustId OR
            Customer.CustIdType NE MNPDetails.CustIdType) THEN DO:
            lcRejectReason = "RECH_IDENT".
            LEAVE SUBS_LOOP.
         END.

      END.
         
      /* Set possible number termination processes on hold */
      IF lcRejectReason = "" THEN DO:
      
         FOR EACH MNPSub WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:
         
            /* put possible number termination on hold */
            FIND FIRST Msisdn WHERE
               Msisdn.Brand = gcBrand AND
               Msisdn.CLI = MNPSub.CLI NO-LOCK USE-INDEX CLI NO-ERROR.
            
            IF AVAIL Msisdn AND
                     Msisdn.StatusCode = {&MSISDN_ST_RETURN_NOTICE_SENT} THEN DO:

               FIND FIRST bMNPProcess WHERE
                  bMNPProcess.MNPSeq = MNPSub.MNPSeq AND
                  bMNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
                  bMNPProcess.StatusCode = {&MNP_ST_BNOT}
               EXCLUSIVE-LOCK NO-ERROR.
         
               IF AVAIL bMNPProcess THEN DO:
                  ASSIGN
                     bMNPProcess.UpdateTS = fMakeTS()
                     bMNPProcess.StatusCode = {&MNP_ST_BDET}.
                  RELEASE bMNPProcess.
               END.

            END.
         END.
      END.
      
      ASSIGN
         MNPProcess.StateFlag = (
            IF lcRejectReason NE "" THEN {&MNP_STATEFLAG_REJECT_PROPOSAL}
            ELSE {&MNP_STATEFLAG_CONFIRM_PROPOSAL})
         MNPProcess.StatusReason = lcRejectReason.

   END.

END PROCEDURE. 
