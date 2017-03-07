/* ----------------------------------------------------------------------
  MODULE .......: mnp_acterr.p 
  TASK .........: Reports errors: All failed network activations.
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04.02.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
DEFINE VARIABLE liSecs  AS INTEGER NO-UNDO.
DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.


DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).

FOR EACH MNPProcess NO-LOCK WHERE
         MNPProcess.Brand = gcBrand AND
         MNPProcess.MNPType = {&MNP_TYPE_IN} AND
         MNPProcess.StatusCode EQ {&MNP_ST_ACON} AND
         MNPProcess.PortingTime < fMakeTS(),
   FIRST Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.OrderId = MNPProcess.OrderId:

   IF Order.OrderType = 3 THEN
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq     = Order.MsSeq AND
                 MsRequest.ReqType   = 82 AND
                 MsRequest.ReqStatus = 3 NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq     = Order.MsSeq AND
                 MsRequest.ReqType   = 13 AND
                 MsRequest.ReqStatus = 3 NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      /* msrequest actstamp should be same as changeWindowDate */
      fSplitTS(INPUT  MsRequest.ActStamp,
               OUTPUT ldaDate,
               OUTPUT liSecs).

      PUT STREAM sout unformatted
          STRING(ldaDate,"99-99-9999") lcDelimiter 
          MsRequest.CLI        lcDelimiter
          Order.CurrOper       lcDelimiter
          "Yoigo"              lcDelimiter
          MNPProcess.FormRequest SKIP.

      oiEvents = oiEvents + 1.

   END. /* IF AVAIL MsRequest THEN DO: */
END. /* FOR EACH MNPProcess NO-LOCK WHERE */

OUTPUT STREAM sout close.

