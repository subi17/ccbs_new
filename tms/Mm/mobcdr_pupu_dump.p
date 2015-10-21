/* ----------------------------------------------------------------------
  MODULE .......: mobcdr_pupu_dump.p
  TASK .........: Create a dump file for cdrs for High performance data
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 10.06.13
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}

DEF INPUT PARAMETER idaReadDate    AS DATE NO-UNDO.
DEF INPUT PARAMETER ideReadInTS    AS DEC  NO-UNDO.
DEF INPUT PARAMETER idtCurrStamp   AS DATETIME NO-UNDO.
DEF INPUT PARAMETER icLogFile      AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilAppend       AS LOG  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER odeCDRStamp AS DEC  NO-UNDO.

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.

DEF STREAM sFile.

lcDel2 = CHR(255).

IF ilAppend THEN 
   OUTPUT STREAM sFile TO VALUE(icLogFile) APPEND.
ELSE OUTPUT STREAM sFile TO VALUE(icLogFile).

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ReadDate >= idaReadDate AND
         MobCDR.ReadinTS  > ideReadInTS AND
         MobCDR.ErrorCode = 0 :

   ASSIGN
      liEvents   = liEvents + 1
      lcKeyValue = MobCDR.CLI + lcDel2 + STRING(MobCDR.DtlSeq) + lcDel2 +
                   STRING(MobCDR.DateSt).
  
   PUT STREAM sFile UNFORMATTED
      "MobCDR"                            lcDel
      "CREATE"                            lcDel
      STRING(RECID(MobCDR))               lcDel
      lcKeyValue                          lcDel
      STRING(idtCurrStamp)                lcDel
      MobCDR.MsSeq                        lcDel
      MobCDR.CLI                          lcDel
      MobCDR.CLIType                      lcDel
      MobCDR.InvCust                      lcDel
      STRING(MobCdr.DateSt,"99.99.9999")  lcDel
      STRING(MobCdr.TimeSt,"HH:MM:SS")    lcDel
      MobCDR.ReadInTS                     lcDel
      MobCDR.EventType                    lcDel
      MobCDR.GsmBnr                       lcDel
      MobCDR.BillCode                     lcDel
      MobCDR.CCN                          lcDel
      MobCDR.BillDur                      lcDel
      MobCDR.DataIn + MobCDR.DataOut      lcDel
      TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<")) lcDel
      MobCDR.DCEvent                      lcDel
      MobCDR.BDest                        lcDel
      MobCDR.Accumulator                  SKIP.
  
   odeCDRStamp = MAX(odeCDRStamp,MobCDR.ReadinTS).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MobCDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.
