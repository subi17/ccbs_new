/* ----------------------------------------------------------------------
  MODULE .......: prepcdr_pupu_dump.p
  TASK .........: Create a dump file for cdrs for High performance data
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 10.06.13
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT PARAMETER idaReadDate    AS DATE NO-UNDO.
DEF INPUT PARAMETER ideReadInTS    AS DEC  NO-UNDO.
DEF INPUT PARAMETER ideCurrStamp   AS DEC  NO-UNDO.
DEF INPUT PARAMETER icLogFile      AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilAppend       AS LOG  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER odeCDRStamp AS DEC  NO-UNDO.

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.
DEF VAR lcDCEvent     AS CHAR NO-UNDO.
DEF STREAM sFile.

lcDel2 = CHR(255).

IF ilAppend THEN 
   OUTPUT STREAM sFile TO VALUE(icLogFile) APPEND.
ELSE OUTPUT STREAM sFile TO VALUE(icLogFile).

FOR EACH PrepCDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepCDR.ReadDate >= idaReadDate AND
         PrepCDR.ReadinTS  > ideReadInTS AND
         PrepCDR.ErrorCode = 0 :

   ASSIGN
      liEvents   = liEvents + 1
      lcKeyValue = PrepCDR.CLI + lcDel2 + STRING(PrepCDR.DtlSeq) + lcDel2 +
                   STRING(PrepCDR.DateSt).

   IF PrepCDR.DCEvent > "" THEN
      lcDCEvent = PrepCDR.DCEvent.
   ELSE IF PrepCDR.BillCode = "PREMDUB" THEN
      lcDCEvent = "PMDUB".
   ELSE IF PrepCDR.CLIType = "TARJ7" AND PrepCDR.Charge = 0 THEN
      lcDCEvent = "TARJ7".
   ELSE IF PrepCDR.CLIType = "TARJ9" AND PrepCDR.EventType = "CALL" AND 
      PrepCDR.accumulator > 0 THEN
      lcDCEvent = "TARJ9".
   ELSE IF PrepCDR.CLIType = "TARJ9" AND PrepCDR.Charge = 0 AND
      LOOKUP(PrepCDR.GsmBnr,{&YOIGO_FREE_NUMBERS}) = 0 THEN
      lcDCEvent = "TARJ9".
   ELSE lcDCEvent = "".
  
   PUT STREAM sFile UNFORMATTED
      "PrepCDR"                            lcDel
      "CREATE"                             lcDel
      STRING(RECID(PrepCDR))               lcDel
      lcKeyValue                           lcDel
      STRING(ideCurrStamp)                 lcDel
      PrepCDR.MsSeq                        lcDel
      PrepCDR.CLI                          lcDel
      PrepCDR.CLIType                      lcDel
      PrepCDR.InvCust                      lcDel
      STRING(PrepCDR.DateSt,"99.99.9999")  lcDel
      STRING(PrepCDR.TimeSt,"HH:MM:SS")    lcDel
      PrepCDR.ReadInTS                     lcDel
      PrepCDR.EventType                    lcDel
      PrepCDR.GsmBnr                       lcDel
      PrepCDR.BillCode                     lcDel
      PrepCDR.CCN                          lcDel
      PrepCDR.BillDur                      lcDel
      PrepCDR.DataIn + PrepCDR.DataOut     lcDel
      TRIM(STRING(PrepCDR.Charge,"->>>>>>>>>>>9.9<<<<<")) lcDel
      lcDCEvent                            lcDel
      PrepCDR.BDest                        lcDel
      PrepCDR.Accumulator                  SKIP.
   
   odeCDRStamp = MAX(odeCDRStamp,PrepCDR.ReadinTS).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "PrepCDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.
