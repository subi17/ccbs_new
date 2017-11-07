/* ----------------------------------------------------------------------
  MODULE .......: MOBCALLi.P
  TASK .........: Browse  mobile calls
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-09-99
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}

DEF  INPUT PARAMETER  icCLI    AS C.   /* NOTE: 467018.....  */
DEF  INPUT PARAMETER  iiInvseq AS I.


DEF VAR ocReasonCode                  AS C    NO-UNDO.
DEF VAR odtDate1                      AS DATE NO-UNDO.
DEF VAR odtDate2                      AS DATE NO-UNDO.
DEF VAR olAccept                      AS LOG NO-UNDO.

RUN Mm/mobguard2.p(INPUT  FALSE,
               OUTPUT ocReasonCode,
               OUTPUT odtDate1,
               OUTPUT odtdate2,
               OUTPUT olAccept).
               
IF olAccept = FALSE THEN LEAVE.

FIND FIRST InvSeq where 
           InvSeq.Invseq = iiInvSeq NO-LOCK NO-ERROR.
           
IF NOT AVAIL INVSEQ THEN DO:
   MESSAGE
   "Unknown Invoice sequence nbr:" + string(iiinvseq) 
   VIEW-AS ALERT-BOX.
   LEAVE.        
END.           

RUN Mm/mobcallbr.p(INPUT  "post",
              INPUT  InvSeq.FromDate,
              INPUT  InvSeq.ToDate,
              INPUT  0,
              INPUT  "",
              INPUT  icCli,
              INPUT  InvSeq.InvSeq,
              INPUT  0,
              INPUT  "",
              INPUT  "",
              INPUT  ocReasonCode,
              INPUT 0,
              INPUT 0).
