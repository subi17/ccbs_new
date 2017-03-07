/* ----------------------------------------------------------------------
  Module .......: Mm/reacmobsub_cui.p
  Task .........: Reactivate the terminated Subscription
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.10.11
  Version ......: Yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fcreatereq.i}
{Mnp/mnp.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/freacmobsub.i}

DEFINE INPUT PARAMETER iiMsSeq           AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER icUserName        AS CHARACTER   NO-UNDO.

DEFINE VARIABLE        ldActStamp        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE        liMsReq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE        ocResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE        llOk              AS LOGICAL     NO-UNDO.

DEFINE BUFFER lbMobSub            FOR MobSub.
DEFINE BUFFER bTermMobSub         FOR TermMobSub.

ocResult = freacprecheck(INPUT iiMsSeq, INPUT icUserName, INPUT FALSE).
IF ocResult > "" THEN DO:
   MESSAGE ocResult VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF ocResult > "" THEN DO: */

FIND FIRST bTermMobSub WHERE
           bTermMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF AVAIL bTermMobSub AND bTermMobSub.MultiSIMId > 0 AND
   bTermMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = gcBrand AND
              lbMobSub.MultiSimID = bTermMobSub.MultiSimID AND
              lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
              lbMobSub.Custnum = bTermMobSub.Custnum NO-ERROR.
   IF NOT AVAIL lbMobSub THEN DO:
      llOk = FALSE.
      MESSAGE "Primary line of multi SIM is not active" SKIP
              "Do you want to continue with reactivation ?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN RETURN.
   END. /* IF NOT AVAIL lbMobSub THEN DO: */
END. /* IF AVAIL bTermMobSub THEN DO: */

/* Set Reactivation time */
IF ldActStamp = 0 OR ldActStamp = ? THEN
   ldActStamp = fMakeTS().

/* Create Reactivation Request */
liMsReq = fReactivationRequest(INPUT iiMsSeq,
                               INPUT 0,
                               INPUT ldActStamp,
                               INPUT icUserName,
                               INPUT {&REQUEST_SOURCE_MANUAL_TMS},
                               OUTPUT ocResult).
IF liMsReq = 0 THEN
   MESSAGE "Request creation failed: " CHR(10) ocResult
           VIEW-AS ALERT-BOX ERROR.
ELSE
   MESSAGE "Request ID for subscription reactivation is: " + STRING(liMsReq)
           VIEW-AS ALERT-BOX TITLE " REQUEST ADDED ".

