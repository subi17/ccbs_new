{Syst/testpaa.i}
katun = "anttis".

{Func/barrfunc.i}

FUNCTION fCheckStatus2 RETURNS CHARACTER
(INPUT iiMsSeq AS INTEGER,
 output ocUser AS CHARACTER):

 /* Check if any other than "completed" master request exists for this mobsub */
   FIND FIRST bBarrReq WHERE
              bBarrReq.MsSeq = iiMsSeq      AND
              bBarrReq.ReqType = 35         AND
             (bBarrReq.ReqStat NE 2         AND 
              bBarrReq.ReqStat NE 4         AND 
              bBarrReq.ReqStat NE 9)
   NO-LOCK NO-ERROR.

   /* Ongoing master request */
   IF AVAIL bBarrReq THEN RETURN "91". /* Error or ongoing network commands */
   
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
                     ocUser = bBarrReq.usercode.
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

def var i as int no-undo.
def var lcresult as char no-undo. 

def stream slog.
def stream sread.

input stream sread from /apps/snet/200906/as_ycm1602.input.
output stream slog to /apps/snet/200906/as_ycm1602.output append.

def var lcline as char no-undo.
def var lccli as char no-undo.

def var lcstat as char no-undo.

DEFINE VARIABLE lcUser AS CHARACTER NO-UNDO. 

DEFINE VARIABLE limsseq AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttDup
FIELD i AS INT
INDEX i IS PRIMARY UNIQUE i. 

DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO. 
ldeActStamp = fmakets().

repeat:
   import stream sread unformatted lcline.

   i = i + 1.

   if i <= 1000  then next.
   
   if i mod 1000 = 0 then ldeActStamp = fOffSet(ldeActStamp,1).

   limsseq = int(entry(3,lcline,";")).

   find first mobsub no-lock where
         mobsub.msseq = limsseq no-error.
   if not available mobsub then next.
  
   find ttDup where
      ttDup.i = mobsub.msseq NO-LOCK NO-ERROR.
    IF NOT AVAIL ttDup then do:
      create ttDup.
      assign ttDup.i = mobsub.msseq.
    ENd.
    else  do:
      disp mobsub.msseq.
      pause.
      next.
   end.
   
   pause 0.
   disp i with 1 down.

   lcstat = fCheckStatus2(MobSub.MsSeq, output lcUser).
   
   if lcStat ne "Y_HURP" then next.
   if lcUser ne "CreSub / CreSub" then next.
   
   
   RUN Mm/barrengine (Mobsub.MsSeq,
                    "UN" + lcstat,     /* package for unbarring */
                      "5",                /* source  */
                      katun,             /* creator */
                      ldeActStamp,  /* activate */
                      "",                 /* sms-text */
                      OUTPUT lcResult).

   put stream slog unformatted
      mobsub.cli "|"
      mobsub.msseq "|"
      lcstat "|"
      lcuser "|"
      lcResult "|"
      ldeActStamp skip.
end.

