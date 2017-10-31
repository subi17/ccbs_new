/* This script update Mandate for MsSeq.
   New MsOwner record is created with new Mandate
   Old Msowner is terminated. Difference is 1 second.
   Eventlogging done for dumping.

   ENTER THESE 1-4:
   1. Enter customer number
   2. Enter MsSeq of oldest Mobsub of Customer 
      (Requires manual check both mobsub/termmobsub)

   3-4. Rename log files in the end of this script file:
   bowner is for New MsOwner and MsOwner is for old one

   Toggle Simulation TRUE -> FALSE in case of action 

   Later On there will be CUI TOOL for this.
*/

{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Syst/eventval.i}
{Func/fbankdata.i}
{Func/create_eventlog.i}

DEFINE VARIABLE idaOrderDate AS DATE      NO-UNDO.
DEFINE VARIABLE iiCustNum    AS INTEGER   NO-UNDO FORMAT ">>>>>>>9".
DEFINE VARIABLE liMsSeq      AS INTEGER   NO-UNDO FORMAT ">>>>>>>>9".
DEFINE VARIABLE lcMandate    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaDate      AS DATE      NO-UNDO.
DEFINE VARIABLE liManTime    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldenowTS     AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeoldTS     AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE llSimulate   AS LOGICAL   NO-UNDO INIT TRUE.

DEFINE BUFFER bOwner FOR MsOwner.

/* Asking for data */
UPDATE iiCustnum
       liMsSeq
       llSimulate.

/* Confirming when not in simulate mode */
IF llSimulate = FALSE THEN
DO:
   MESSAGE "You are NOT in simulate mode." SKIP(1)
           "CustNum:" iiCustnum SKIP
           "MsSeq:" liMsSeq SKIP(1) 
           "Do you really want to continue?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
   IF lContinue = FALSE THEN
       RETURN. 
END.
   
/* Checking there is a unique Order for the MsSeq provided.
   Note: it seems this is not mandatory from a businness logic point of view but I am adding this check because I still do not know the system. So I am not sure the next statement 
    
             FIND FIRST Order NO-LOCK WHERE Order.MsSeq = liMsSeq NO-ERROR.
         
         will be finding the oldest Order for MsSeq (as the comment suggest). The system could be not finding the oldest Order for different reasons. 
         But adding my validation I can only find an Order so it has to be the oldest  ;)    */
FIND Order NO-LOCK WHERE
           Order.MsSeq = liMsSeq NO-ERROR.
IF AMBIGUOUS Order THEN
DO:
   MESSAGE  "There is more than one Order of MsSeq: " liMsSeq "." SKIP 
            "This program is designed to work when there is only an Order for the introduced MsSeq." VIEW-AS ALERT-BOX.
   RETURN.
END.


/* Find oldest Order of MsSeq */
FIND FIRST Order NO-LOCK WHERE 
           Order.MsSeq = liMsSeq NO-ERROR. 

IF NOT AVAIL Order THEN
DO:
   MESSAGE  "Order of MsSeq: " liMsSeq " cannot be found" VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   ldaDate    = TODAY
   ldenowTS   = Func.Common:mMakeTS()
   ldeOldTS   = Func.Common:mSecOffSet(ldenowTS,-1).
      
fCalculateMandate(Order.MsSeq, ldaDate, iiCustNum, OUTPUT lcMandate).

/* END existing Latest MSOWNER */
IF llSimulate THEN 
    FIND FIRST MsOwner WHERE MsOwner.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.   
ELSE 
    FIND FIRST MsOwner WHERE MsOwner.MsSeq = Order.MsSeq EXCLUSIVE-LOCK NO-ERROR.
    
IF NOT AVAIL MsOwner THEN
DO:
   MESSAGE "MsOwner of MsSeq: " liMsSeq " cannot be found" VIEW-AS ALERT-BOX.
   LEAVE.
END.

IF AVAIL MsOwner THEN
DO:
   /* Update only those that already have mandate */
   IF MsOwner.MandateId = "" THEN 
   DO:
      MESSAGE "No changes made. Program can only update those that already have mandate."
        VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   
   MESSAGE "Generated: " SKIP 
           STRING(lcMandate,"X(60)") SKIP
           STRING(ldaDate) SKIP(1)
           "OLD:" SKIP
           STRING(MsOwner.MandateId,"X(60)") SKIP 
           STRING(MsOwner.MandateDate)  
      VIEW-AS ALERT-BOX.
   
   /* End old MsOwner */
   IF NOT llsimulate THEN
   DO:
      IF llDoEvent THEN
      DO:
         &GLOBAL-DEFINE STAR_EVENT_USER katun
         {Func/lib/eventlog.i}
         DEF VAR lhMSOwner AS HANDLE NO-UNDO.
         lhMSOwner = BUFFER Msowner:HANDLE.
         RUN StarEventInitialize(lhMSOwner).
         RUN StarEventSetOldBuffer(lhMSOwner).
      END.

      OUTPUT TO "yot_5407_old_msowner_case1.log".
        EXPORT msowner.
      OUTPUT CLOSE.
      
      MsOWner.TSEnd = ldeoldTS.

      IF llDoEvent THEN
      DO:
         RUN StarEventMakeModifyEvent(lhMSOwner).
      END.
   END.
END.

/* CREATE NEW MSOWNER */
IF NOT llSimulate THEN
DO:
   CREATE bOwner.
   BUFFER-COPY MsOwner EXCEPT MandateID MandateDate TsBegin TSEnd CLIEvent
      TO bOwner.
   ASSIGN
      bOwner.MandateID = lcMandate
      bOwner.MandateDate = ldaDate
      bOwner.TsBegin = ldenowTS
      bOwner.TSEnd   = 99999999.99999
      bOwner.CLIEvent = "M".   /* M = Mandate, C = Creation */

   IF llDoEvent THEN
   DO:
      lhMsOwner = BUFFER bOwner:HANDLE.
      fMakeCreateEvent (lhMsOwner, "", "", "").
      fCleanEventObjects().
   END.

   /* WRITE LOGS of old and new MsOwner records. */
   OUTPUT TO "yot_5407_new_msowner_case1.log".
      EXPORT bOwner.
   OUTPUT CLOSE.
   
   RELEASE MsOwner.
   RELEASE bOwner.
END.

