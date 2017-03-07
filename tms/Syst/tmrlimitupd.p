/* ----------------------------------------------------------------------
  MODULE .......: tmrlimit.p
  TASK .........: update changes in TMRLimit to customers
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 19.05.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhLimit AS HANDLE NO-UNDO.
   lhLimit = BUFFER Limit:HANDLE.
   RUN StarEventInitialize(lhLimit).
END.

DEF INPUT  PARAMETER iiTMRuleSeq AS INT  NO-UNDO.
           /* 0=create,1=update,2=terminate */
DEF INPUT  PARAMETER iiAction    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocResult    AS CHAR NO-UNDO.

DEF VAR llUpdated AS LOG  NO-UNDO.
DEF VAR llStart   AS LOG  NO-UNDO.
DEF VAR liUpdated AS INT  NO-UNDO.
DEF VAR liAll     AS INT  NO-UNDO.

IF TRANSACTION THEN DO:
   ocResult = "ERROR:Transaction already active".
   RETURN.
END.

FIND FIRST TMRule WHERE TMRule.TMRuleSeq = iiTMRuleSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE TMRule THEN DO:
   ocResult = "ERROR:Unknown rule".
   RETURN.
END.

llStart = FALSE.
MESSAGE "Start updating new default limits to all customers?"
VIEW-AS ALERT-BOX QUESTION
BUTTONS YES-NO
SET llStart.

IF NOT llStart THEN RETURN.


/* copy changed default values to old customers */
FOR EACH Customer NO-LOCK:
     
   ASSIGN
      llUpdated = FALSE
      liAll     = liAll + 1.
      
   /* termination */
   IF iiAction = 2 THEN DO:

      FOR EACH Limit EXCLUSIVE-LOCK USE-INDEX CustNum WHERE
               Limit.CustNum   = Customer.CustNum AND
               Limit.LimitType = 1                AND
               Limit.TMRuleSeq = TMRule.TMRuleSeq AND
               Limit.ToDate    > TMRule.ToDate:
      
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).

         ASSIGN
            Limit.ToDate = TMRule.ToDate
            llUpdated    = TRUE.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhLimit).
      END.
      
   END.
     
   /* creation or update */
   ELSE 
   FOR EACH TMRLimit OF TMRule NO-LOCK WHERE
            TMRLimit.ToDate > TODAY:

      FIND FIRST Limit USE-INDEX CustNum WHERE
                 Limit.CustNum   = Customer.CustNum AND
                 Limit.LimitType = 1                AND
                 Limit.TMRuleSeq = TMRule.TMRuleSeq AND
                 Limit.LimitID   = TMRLimit.LimitID AND
                 Limit.ToDate   >= TODAY NO-LOCK NO-ERROR.
                 
      IF AVAILABLE Limit THEN DO:
         
         /* limit has been altered manually -> cannot be updated */
         IF Limit.DefValue = FALSE THEN NEXT.
        
         IF Limit.LimitAmt  = TMRLimit.LimitAmt AND
            Limit.LimitPerc = TMRLimit.LimitPerc AND
            Limit.ToDate    = TMRLimit.ToDate
         THEN NEXT. 
                     
         /* new row will not be created, just update new values */
         FIND CURRENT Limit EXCLUSIVE-LOCK.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
         
         ASSIGN 
            Limit.LimitAmt  = TMRLimit.LimitAmt
            Limit.LimitPerc = TMRLimit.LimitPerc
            Limit.ToDate    = TMRLimit.ToDate
            llUpdated       = TRUE.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhLimit).
      END.   
         
      ELSE DO:   
         CREATE Limit.
         BUFFER-COPY TMRLimit TO Limit.
         ASSIGN 
            Limit.CustNum   = Customer.CustNum
            Limit.LimitType = 1
            Limit.DefValue  = TRUE
            llUpdated       = TRUE.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhLimit).
      END.
         
   END.   
   
   IF llUpdated THEN liUpdated = liUpdated + 1.
   
   IF liAll MOD 1000 = 0 THEN DO:
      PAUSE 0.
      DISP liAll     LABEL "Customers" COLON 11 SKIP
           liUpdated LABEL "Updated"   COLON 11 
      WITH SIDE-LABELS ROW 10 CENTERED OVERLAY TITLE " LIMITS " FRAME fQty.
   END.
END.

HIDE FRAME fQty NO-PAUSE.

ocResult = STRING(liUpdated) + " customers were updated".


 
