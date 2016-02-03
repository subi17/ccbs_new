/* ----------------------------------------------------------------------
  MODULE .......: fraudm.p
  TASK .........: Browse roaming fraud CDRs
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 5.3.2013 
  Version ......: yoigo 
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FraudCDR'} 
{Func/feventlog.i}

DEF INPUT PARAM icCLI AS C NO-UNDO.

DEF VAR ocReasonCode                  AS C    NO-UNDO  INIT "CCBrowse".
DEF VAR odtDate1                      AS DATE NO-UNDO.
DEF VAR odtDate2                      AS DATE NO-UNDO.
DEF VAR olAccept                      AS LOG  NO-UNDO.

RUN  mobguard2.p(INPUT  TRUE,
               OUTPUT ocReasonCode,
               OUTPUT odtDate1,
               OUTPUT odtdate2,
               OUTPUT olAccept).

IF olAccept = FALSE THEN LEAVE.

RUN mobcallbr.p(INPUT "fraud",
              INPUT  odtDate1,
              INPUT  odtDate2,
              INPUT  0,
              INPUT  "",
              INPUT  icCli,
              INPUT  0,
              INPUT  0,
              INPUT  "",
              INPUT  "",
              INPUT  ocReasonCode,
              INPUT  0,
              INPUT  0).
