/* ----------------------------------------------------------------------
  MODULE .......: MOBCALLC.P
  TASK .........: Browse Billable mobile calls
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-09-99
  CHANGED ......:
  Version ......: SCRUNKO4 (10.06.99)
                  21.03.03 jp xbsub.i
                  25.03.03 jp xbsub.i -> func.i
                  21.05.03 jp fhidebsub needs 5 parameter
                  05.06.03 tk added custnum to every find
                  24.06.03 tk token
                  10.02.06/aam use MobCDR.CustNum when getting CustName
                  15.02.06/aam row to eventlog
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/func.p} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobCDR'} 
{Func/feventlog.i}

DEF INPUT  PARAMETER      CLI          AS C NO-UNDO.

DEF VAR ocReasonCode                  AS C    NO-UNDO  INIT "CCBrowse".
DEF VAR odtDate1                      AS DATE NO-UNDO.
DEF VAR odtDate2                      AS DATE NO-UNDO.

DEF VAR olAccept                      AS LOG  NO-UNDO.

RUN Mm/mobguard2.p(INPUT  TRUE,
               OUTPUT ocReasonCode,
               OUTPUT odtDate1,
               OUTPUT odtdate2,
               OUTPUT olAccept).

IF olAccept = FALSE THEN LEAVE.

RUN Mm/mobcallbr.p(INPUT "post,pre",
              INPUT  odtDate1,
              INPUT  odtDate2,
              INPUT  0,
              INPUT  "",
              INPUT  Cli,
              INPUT  0,
              INPUT  0,
              INPUT  "",
              INPUT  "",
              INPUT  ocReasonCode,
              INPUT  0,
              INPUT  0).
                                
