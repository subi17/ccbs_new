/* -----------------------------------------------
  MODULE .......: PPCOMPREP.P
  FUNCTION .....: Report from CC PrePaidRequests
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 29.03.07 kl
  MODIFIED .....: PrePaidRequest.UserCode

  Version ......: Yoigo
------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/multitenantfunc.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE STREAM sDump.

OUTPUT STREAM sDump TO VALUE(icFile).

FOR EACH PrePaidRequest NO-LOCK WHERE
         PrePaidRequest.Brand  = gcBrand  AND
         PrePaidRequest.Source = "CC",
   FIRST Payment NO-LOCK WHERE
         Payment.Brand  = gcBrand AND
         Payment.RefNum = STRING(PrePaidRequest.PPRequest):
   
   PUT STREAM sDump UNFORMATTED
      PrePaidRequest.CLI      CHR(9)
      PrePaidRequest.UserCode CHR(9)
      PrePaidRequest.TopUpAmt CHR(9)
      ""                      CHR(9)
      fTS2HMS(PrePaidRequest.TSRequest) CHR(10).
   oiEvents = oiEvents + 1.
END.

OUTPUT STREAM sDump CLOSE.
