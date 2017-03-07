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

DEFINE VARIABLE lcTimeStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutgDir   AS CHARACTER NO-UNDO.

DEFINE STREAM sDump.

ASSIGN
   lcTimeStamp = fTS2HMS(fMakeTS())
   lcTimeStamp = REPLACE(lcTimeStamp,".","")
   lcTimeStamp = REPLACE(lcTimeStamp,":","")
   lcTimeStamp = REPLACE(lcTimeStamp," ","")
   lcSpoolDir  = fCParam("DUMPSPOOL","ppcomprep.p")
   lcOutgDir   = fCParam("DUMPOUTGOING","ppcomprep.p")
   lcFileName  = lcSpoolDir + "cc_prepaid_comp_" + lcTimeStamp + ".txt".

OUTPUT STREAM sDump TO VALUE(lcFileName).

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

END.

OUTPUT STREAM sDump CLOSE.

UNIX SILENT VALUE("mv " + lcFileName + " " + lcOutgDir).
