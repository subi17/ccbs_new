/* ----------------------------------------------------------------------
  MODULE .......: edrm.p
  TASK .........: Browse EDRs
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 02/2013
  Version ......: yoigo 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/func.p} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PrepEDR'} 
{Func/feventlog.i}

DEF INPUT  PARAMETER  icCLI              AS C NO-UNDO.

DEF VAR ocReasonCode                  AS C    NO-UNDO  INIT "CCBrowse".
DEF VAR odtDate1                      AS DATE NO-UNDO.
DEF VAR odtDate2                      AS DATE NO-UNDO.

DEF VAR olAccept                      AS LOG  NO-UNDO.

RUN Mm/mobguard2(INPUT  TRUE,
               OUTPUT ocReasonCode,
               OUTPUT odtDate1,
               OUTPUT odtdate2,
               OUTPUT olAccept).

IF olAccept = FALSE THEN LEAVE.

RUN Mm/edrbrowse.p(INPUT "edr",
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
                                
