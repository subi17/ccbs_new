/* ----------------------------------------------------------------------------
  module .......: Mm/tarj7_termination.p
  task .........: Terminate TARJ7 manually if we don't receive unsucessfull EDR
  application ..: tms
  author .......: vikas
  created ......: 15.10.13
  version ......: yoigo
-----------------------------------------------------------------------------*/

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

DEF VAR ldaFromdate       AS DATE NO-UNDO.
DEF VAR liTime            AS INT  NO-UNDO.
DEF VAR ldeNow            AS DEC  NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcResult          AS CHAR NO-UNDO.
DEF VAR liRequest         AS INT  NO-UNDO.
DEF VAR liCount           AS INT  NO-UNDO.
DEF VAR lcGroupCodes      AS CHAR NO-UNDO.

DEF STREAM Sout.

ASSIGN lcLogDir     = fCParam("PrepaidBundle","PrepaidBundle_LogDir")
       ldeNow       = fMakeTS()
       lcGroupCodes = "TARJ7,TARJ9,TARJ10,TARJ11,TARJ12".

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "prepaid_bundle_termination_" +
            STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM Sout TO VALUE(lcLogFile).

DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes):
   FOR FIRST ServiceLimit WHERE
             ServiceLimit.GroupCode = ENTRY(liCount,lcGroupCodes,",")
             NO-LOCK,
        EACH MServiceLimit WHERE
             MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.EndTS    = 99999999.99999 NO-LOCK:

      fSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).
      
      IF ldaFromdate >= TODAY THEN NEXT.
      
      IF DAY(fLastDayOfMonth(TODAY)) >= DAY(ldaFromdate) THEN DO:
         IF DAY(TODAY) NE DAY(ldaFromdate) THEN NEXT.
      END.
      ELSE IF TODAY NE fLastDayOfMonth(TODAY) THEN NEXT.      

      FIND FIRST MsOwner WHERE
                 MsOwner.MsSeq = MServiceLimit.MsSeq AND
                 MsOwner.TSEnd = 99999999.99999 NO-LOCK NO-ERROR.
      IF NOT AVAIL MsOwner THEN
         FIND FIRST MsOwner WHERE
                    MsOwner.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.

      IF NOT AVAIL MsOwner THEN DO:
         PUT STREAM Sout UNFORMATTED 
            STRING(MServiceLimit.MsSeq) "|"
            ServiceLimit.GroupCode      "|"
           "MobSub not found" skip.
         NEXT.
      END.

      FIND FIRST PrepEDR NO-LOCK WHERE
                 PrepEDR.MsSeq  = MsOwner.MsSeq AND
                 PrepEDR.DateSt = TODAY AND
                 PrepEDR.SuccessCode EQ 1 NO-ERROR.
      IF AVAIL PrepEDR THEN NEXT.

      liRequest = fPCActionRequest(MsOwner.MsSeq,
                                   ServiceLimit.GroupCode,
                                   "term",
                                   ldeNow,
                                   TRUE,    /* fees */
                                   {&REQUEST_SOURCE_SCRIPT},
                                   "",   /* creator */
                                   0,    /* no father request */
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcResult). 
      IF liRequest = 0 THEN
         PUT STREAM Sout UNFORMATTED 
            STRING(MServiceLimit.MsSeq) "|"
            ServiceLimit.GroupCode      "|"
           "Contract termination request failed: " + lcResult skip.
      ELSE
         PUT STREAM Sout UNFORMATTED 
            STRING(MServiceLimit.MsSeq) "|"
            ServiceLimit.GroupCode      "|"
           "Contract termination request successfully created" skip.
   END. /* FOR FIRST ServiceLimit WHERE */
END. /* DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes): */

OUTPUT STREAM Sout CLOSE.
