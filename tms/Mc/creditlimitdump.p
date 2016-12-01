{commali.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

DEF STREAM sOut.

OUTPUT STREAM sOut TO VALUE(icFile).

IF icDumpMode = "Full" THEN
   FOR EACH Limit NO-LOCK WHERE
            Limit.Brand      = gcBrand                 AND
            Limit.LimitType  = {&LIMIT_TYPE_RISKLIMIT} AND
            Limit.ToDate    >= TODAY:
      PUT STREAM sOut UNFORMATTED 
         Limit.CustNum  lcDelimiter
         Limit.LimitAmt
         SKIP.
      oiEvents = oiEvents + 1.
   END.
ELSE
   FOR EACH Limit NO-LOCK WHERE
            Limit.Brand      = gcBrand                 AND
            Limit.LimitType  = {&LIMIT_TYPE_RISKLIMIT} AND
            Limit.FromDate  >= TODAY - 1               AND
            Limit.ToDate    >= TODAY:
      PUT STREAM sOut UNFORMATTED
         Limit.CustNum  lcDelimiter
         Limit.LimitAmt
         SKIP.
      oiEvents = oiEvents + 1.
   END. 

OUTPUT STREAM sOut CLOSE.

