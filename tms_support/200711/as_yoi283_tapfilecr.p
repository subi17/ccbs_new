/* ----------------------------------------------------------------------
  MODULE .......: TAPFILECR.P
  TASK .........: Creates TAP files
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 13.03.07
  CHANGED ......: 14.03.07 kl use dates from parameters
                  19.03.07 Production version
                  03.04.07/aam CurrRate index changed to descending
                  09.05.07 kl GPRS from RoamGPRS table
                  11.05.07 kl GPRS rates from file
                  15.05.07 kl ENTRY(16... error fixed
                  15.05.07 kl liKB into ldeKB
                  24.07.07 as Handles new roaming pricing

  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{roamtariff.i}
{csvfuntion.i}
{commali.i}
{cparam.i2}

{timestamp.i}

gcBrand = "1".

DEFINE INPUT PARAMETER pcPLMN     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pdaDate1   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pdaDate2   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pcCallType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER plTest     AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER piFileSeq  AS INTEGER   NO-UNDO.

RUN roamdouble(pdaDate1,pdaDate2).

DEFINE TEMP-TABLE ttFields NO-UNDO
   FIELD tiField AS INTEGER 
   FIELD tcField AS CHARACTER
   
   INDEX tiField AS PRIMARY tiField.

DEFINE TEMP-TABLE ttTAPFile NO-UNDO LIKE RoamCDR
   FIELD RowType    AS INTEGER
   FIELD RowNum     AS RECID
   FIELD TotUnits   AS INT64 
   FIELD Duration   AS INTEGER
   FIELD DataIn     AS INTEGER
   FIELD DataOut    AS INTEGER
   FIELD CloseCause AS CHARACTER.

DEFINE TEMP-TABLE ttAnal NO-UNDO
   FIELD RowNum AS RECID
   
   INDEX RowNum AS PRIMARY RowNum.
      
DEFINE VARIABLE lcHdrFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCDRFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO.
DEFINE VARIABLE ldaCurrDate AS DATE      NO-UNDO.
DEFINE VARIABLE lcRoamOper  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcHeader    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCutDate   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFields    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liField     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcField     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRoamCDR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeExchRate AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liFileSeq   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTapGen    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcSpoolDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutGDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCSV       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFileType  AS INTEGER   NO-UNDO.

DEFINE VARIABLE lcIMSI     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPLMN     AS CHARACTER NO-UNDO INIT "N/A".
DEFINE VARIABLE lcGroup    AS CHARACTER NO-UNDO INIT "N/A".

INPUT FROM /apps/tms/Rate/roamfields.txt.
REPEAT:
   
   IMPORT UNFORMATTED lcTapGen.

   IF lcTapGen NE "" THEN DO:
      CREATE ttFields.
      ASSIGN
         liLoop           = liLoop + 1
         ttFields.tcField = lcTapGen
         ttFields.tiField = liLoop.
   END.

END.

DEFINE STREAM sHeader.
DEFINE STREAM sCdr.

DEFINE BUFFER bufOper FOR RoamOper.

IF plTest THEN ASSIGN
/*
   lcSpoolDir = "/home/anttis/taps/spool/" 
   lcOutGDir  = "/home/anttis/taps/out/".
*/   
   lcSpoolDir = fCParam("DUMPSPOOL","roamtestui.p")
   lcOutGDir  = fCParam("DUMPOUTGOING","roamtestui.p").

ELSE ASSIGN
/*
  lcSpoolDir = "/home/anttis/taps/spool/" 
  lcOutGDir  = "/home/anttis/taps/out/".
*/   
   lcSpoolDir = fCParam("DUMPSPOOL","tapfilecr.p")
   lcOutGDir  = fCParam("DUMPOUTGOING","tapfilecr.p").

FOR EACH RoamOper NO-LOCK:
   
   IF pcPLMN NE RoamOper.PLMN THEN NEXT.
   
   CASE plTest:
      WHEN TRUE  THEN
         /*IF pcPLMN NE RoamOper.PLMN THEN NEXT.*/
         IF RoamOper.Production = 0 THEN NEXT.
      WHEN FALSE THEN
         IF RoamOper.Production = 0 THEN NEXT.
   END.
   
   DO TRANS:
    
      FIND FIRST bufOper WHERE
           RECID(bufOper) = RECID(RoamOper)
      EXCLUSIVE-LOCK NO-ERROR.

      IF plTest THEN ASSIGN
         /*bufOper.TestFileSeq = bufOper.TestFileSeq + 1*/
         /*liFileSeq           = bufOper.TestFileSeq.*/
         liFileSeq       = piFileSeq. /*bufOper.FileSeq*/
      ELSE ASSIGN
        /* bufOper.FileSeq = bufOper.FileSeq + 1*/
         liFileSeq       = piFileSeq. /*bufOper.FileSeq*/

   END.

   ASSIGN
      ldaDate     = TODAY
      lcRoamOper  = RoamOper.PLMN
      lcCDRFile   = lcSpoolDir + lcRoamOper + STRING(liFileSeq) + "_tap3.cdr"
      lcHdrFile   = lcSpoolDir + lcRoamOper + STRING(liFileSeq) + "_tap3.hdr"
      lcCutDate   = STRING(YEAR(ldaDate),"9999")  +
                    STRING(MONTH(ldaDate),"99")   +
                    STRING(DAY(ldaDate),"99")
      lcHeader   = "ESPXF"     + "|" +
                    lcRoamOper + "|" +
                    STRING(liFileSeq,"99999") + "|" +
                    lcCutDate  + "|" +
                   "000000"    + "|" +
                   "+0100"     + "|" +
                   "EUR"       + "|" +
                   "6".

   OUTPUT STREAM sHeader TO VALUE(lcHdrFile).

   PUT STREAM sHeader UNFORMATTED lcHeader CHR(10).

   OUTPUT STREAM sHeader CLOSE.

   IF plTest THEN liFileType = RoamOper.Production. /*LOOKUP(pcCallType,"-,V,G,A") - 1.*/
   ELSE           liFileType = RoamOper.Production.

   RUN pTAPFile(liFileType,plTest).

   OS-COMMAND SILENT VALUE
     ("mv " + lcSpoolDir + "*ESPXF* " + lcOutGDir). 
   
END.

OUTPUT CLOSE.
PROCEDURE pTAPFile:

   DEFINE INPUT PARAMETER piFileType AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER plTest     AS LOGICAL NO-UNDO.

   DEFINE VARIABLE lcMessage AS CHARACTER NO-UNDO.
   
   CASE piFileType:
      
      /* voice and sms */
      WHEN 1 THEN RUN pFillTempTable1.
      
      /* gprs */
      WHEN 2 THEN RUN pFillTempTable2.
   
      /* all */
      WHEN 3 THEN DO:
         RUN pFillTempTable1.
         RUN pFillTempTable2.
      END.

   END.

   IF CAN-FIND(FIRST ttTAPFile) THEN DO:

      OUTPUT STREAM sCdr TO VALUE(lcCDRFile).

      FOR EACH ttTAPFile NO-LOCK:
         
         RUN pTap3File. 

      END.

      OUTPUT STREAM sCdr CLOSE.
      
      IF NOT plTest THEN
         lcTapGen = "cd " + lcSpoolDir + " ; /opt/local/bin/tap3gen " + lcHdrFile + " " + lcCDRFile.
      ELSE
         lcTapGen = "cd " + lcSpoolDir + " ; test3gen --test " + lcHdrFile + " " + lcCDRFile.
         
      UNIX SILENT VALUE(lcTapGen).
         
   END.
   ELSE DO:

      IF NOT plTest THEN
         lcTapGen = "cd " + lcSpoolDir + " ; /opt/local/bin/tap3gen --notification " + lcHdrFile.
      ELSE
         lcTapGen = "cd " + lcSpoolDir + " ; test3gen  --test --notification " + lcHdrFile.
         
      UNIX SILENT VALUE(lcTapGen).

   END.

   EMPTY TEMP-TABLE ttTAPFile.

   IF plTest THEN DO:
   
      IF llFound THEN lcMessage = "TAP file with CDRs done.".
      ELSE            lcMessage = "TAP file with NO CDRs done.".

      MESSAGE lcMessage VIEW-AS ALERT-BOX.
  
   END.

END PROCEDURE.

PROCEDURE pTap3File:

   ASSIGN
      ldaCurrDate = ttTAPFile.DateSt
      liLoop      = MONTH(ldaCurrDate).

   DO WHILE liLoop = MONTH(ldaCurrDate):
      ldaCurrDate = ldaCurrDate - 1.
   END.

   DO WHILE DAY(ldaCurrDate) NE 23:
      ldaCurrDate = ldaCurrDate - 1.
   END.
         
   FOR EACH Currency NO-LOCK WHERE
            Currency.Currency = "SDR",
      FIRST CurRate OF Currency NO-LOCK WHERE
            CurRate.RateDate <= ldaCurrDate:

      ldeExchRate = CurRate.ExchRate.

   END.

   llFound = TRUE.

   ASSIGN
      lcRoamCDR = ""
      lcCSV     = "".
      
   CASE ttTAPFile.RowType:
   
      WHEN 1 THEN DO:
         FIND FIRST RoamCDR WHERE
              RECID(RoamCDR) = ttTAPFile.RowNum
         NO-LOCK NO-ERROR.
         lcCSV = RoamCDR.CSV.
      END.

      WHEN 2 THEN DO:
         FIND FIRST RoamGPRS WHERE
              RECID(RoamGPRS) = ttTAPFile.RowNum
         NO-LOCK NO-ERROR.
         lcCSV = RoamGPRS.CSV.
      END.

   END.
   
   FOR EACH ttFields NO-LOCK:
         
      lcField = REPLACE(ttFields.tcField,"_"," ").

      CASE ttFields.tcField:
         WHEN "duration" THEN lcValue = STRING(ttTAPFile.Duration).
         WHEN "charged_item" THEN CASE ttTAPFile.EventType:
            WHEN "CALL" THEN lcValue = "D".
            WHEN "SMS"  THEN lcValue = "E".
            WHEN "MMS"  THEN lcValue = "E".
            WHEN "GPRS" THEN lcValue = "X".
         END.
         WHEN "chargeable_units" THEN DO:
            lcValue = STRING(ttTAPFile.TotUnits,"9999999999").
         END.
         WHEN "charge" THEN ASSIGN
            lcValue = REPLACE(STRING(ttTAPFile.Amount,"9999.999999"),",","")
            lcValue = REPLACE(STRING(ttTAPFile.Amount,"9999.999999"),".","").
         WHEN "exchange_rate" THEN ASSIGN
            lcValue = REPLACE(STRING(ldeExchRate,"9.99999"),",","")
            lcValue = REPLACE(STRING(ldeExchRate,"9.99999"),".","").
         /* document page 53 for call type levels 1-3 */
         WHEN "call_type_level_1" THEN CASE ttTAPFile.EventType:
            WHEN "GPRS" THEN lcValue = "10".
            OTHERWISE DO:
               IF ttTAPFile.GsmBNr BEGINS "34" THEN lcValue = "1".
               ELSE                               lcValue = "2".
            END.
         END.
         WHEN "call_type_level_2" THEN DO:
            IF ttTAPFile.RSubType = 1 THEN lcValue = "0".
            ELSE IF ttTAPFile.CLI BEGINS "34" THEN CASE ttTAPFile.ChargedParty:
               WHEN 1 THEN CASE SUBSTR(ttTAPFile.CLI,3,1):
                  WHEN "1" THEN lcValue = "4".
                  WHEN "6" THEN lcValue = "1".
                  WHEN "7" THEN lcValue = "3".
                  WHEN "8" THEN lcValue = "2".
                  WHEN "9" THEN lcValue = "2".
                  OTHERWISE lcValue = "0".
               END.
            END.
            ELSE lcValue = "0".
         END.
         WHEN "call_type_level_3" THEN DO:
            lcValue = "0".
         END.
         WHEN "originating_address" THEN DO:
            lcValue = ENTRY(ttFields.tiField + 7,lcCSV,"|").
            IF SUBSTR(lcValue,1,1) = "0" THEN lcValue = SUBSTR(lcValue,2).
         END.
         /* GRPS fields */
         WHEN "partial_indicator"        THEN lcValue = "0".
         WHEN "partial_record_number"    THEN lcValue = "0".
         WHEN "data_amount_incoming"     THEN lcValue = STRING(ttTAPFile.DataIn).
         WHEN "data_amount_outgoing"     THEN lcValue = STRING(ttTapFile.DataOut).
         WHEN "cause_for_record_closing" THEN lcValue = STRING(ttTapFile.CloseCause).
         /* by default orignal value */
         OTHERWISE
            lcValue = ENTRY(ttFields.tiField + 7,lcCSV,"|").
      END.

      lcRoamCDR = lcRoamCDR + "|" + lcValue.

   END.

   PUT STREAM sCdr UNFORMATTED SUBSTR(lcRoamCDR,2) CHR(10).

END PROCEDURE.

PROCEDURE pFillTempTable1:

   FOR EACH RoamCDR NO-LOCK WHERE
            RoamCDR.PLMN       = lcRoamOper AND
            RoamCDR.DateStart >= pdaDate1   AND
            RoamCDR.DateStart <= pdaDate2:
         
      CREATE ttTAPFile.
      BUFFER-COPY RoamCDR EXCEPT RoamCDR.CSV TO ttTapFile.

      ASSIGN
         ttTAPFile.RowType  = 1
         ttTAPFile.RowNum   = RECID(RoamCDR)
         ttTAPFile.Duration = INT(ENTRY(16,RoamCDR.CSV,"|"))
         ttTAPFile.TotUnit  = ttTAPFile.Units.

   END.

END PROCEDURE.

PROCEDURE pFillTempTable2:
   
   DEFINE BUFFER bufPartial FOR RoamGPRS.

   DEFINE VARIABLE ldeKB AS DECIMAL NO-UNDO.
   
   FOR EACH RoamGPRS NO-LOCK WHERE
            RoamGPRS.PLMN       = lcRoamOper AND
            RoamGPRS.DateStart >= pdaDate1   AND
            RoamGPRS.DateStart <= pdaDate2:

      IF CAN-FIND(FIRST ttAnal WHERE
                        ttAnal.RowNum = RECID(RoamGPRS)) THEN NEXT.
   
      CREATE ttTAPFile.
   
      BUFFER-COPY RoamGPRS EXCEPT
         RoamGPRS.CSV
         RoamGPRS.CallIdNum
         RoamGPRS.GGSNAddr
         RoamGPRS.PartInd
         RoamGPRS.PartRecNum
      TO ttTAPFile.
   
      ASSIGN
         ttTAPFile.RowType  = 2
         ttTAPFile.RowNum   = RECID(RoamGPRS)
         ttTAPFile.TotUnits = INT(ENTRY(62,RoamGPRS.CSV,"|")) +
                              INT(ENTRY(63,RoamGPRS.CSV,"|"))
         ttTAPFile.DataIn   = INT(ENTRY(62,RoamGPRS.CSV,"|"))
         ttTAPFile.DataOut  = INT(ENTRY(63,RoamGPRS.CSV,"|"))
         ttTAPFile.Duration = INT(ENTRY(16,RoamGPRS.CSV,"|")).
                                                                  
      IF RoamGPRS.PartRecNum NE 0 THEN DO:

         FOR EACH bufPartial NO-LOCK WHERE
                  bufPartial.DateRead  = RoamGPRS.DateRead  AND
                  bufPartial.DateStart = RoamGPRS.DateStart AND
                  bufPartial.CallIdNum = RoamGPRS.CallIdNum AND
                  bufPartial.GGSNAddr  = RoamGPRS.GGSNAddr  AND
            RECID(bufPartial) NE RECID(RoamGPRS):
         
            ASSIGN
               ttTAPFile.TotUnits   = ttTAPFile.TotUnits + INT(ENTRY(62,bufPartial.CSV,"|")) +
                                                           INT(ENTRY(63,bufPartial.CSV,"|"))
               ttTAPFile.DataIn     = ttTAPFile.DataIn   + INT(ENTRY(62,bufPartial.CSV,"|"))
               ttTAPFile.DataOut    = ttTAPFile.DataOut  + INT(ENTRY(63,bufPartial.CSV,"|"))
               ttTAPFile.Duration   = ttTAPFile.Duration + INT(ENTRY(16,bufPartial.CSV,"|"))
               ttTAPFile.CloseCause = "0".

            CREATE ttAnal.
            ttAnal.RowNum = RECID(bufPartial).
         
         END.
      
      END.
      
      /* some data exists */
      IF ttTAPFile.TotUnits > 0 THEN DO:
         
         ASSIGN   
            lcIMSI   = ENTRY(21,RoamGPRS.CSV,"|") /* originating */
            lcPLMN   = "N/A" 
            lcGroup  = "N/A".
         
         DO liLoop = 6 TO 5 BY -1:
            
            FIND FIRST RoamOper WHERE
                       RoamOper.IMSI = SUBSTR(lcIMSI,1,liLoop)
            NO-LOCK NO-ERROR.

            IF AVAIL RoamOper THEN ASSIGN
               lcPLMN  = RoamOper.PLMN
               lcGroup = RoamOper.RoamGroup
               liLoop  = 0.
               
         END.
         
         ttTAPFile.Amount = fGPRS(RoamGPRS.DateStart, ttTAPFile.TotUnits, 1,
            lcPLMN, lcGroup).
         
      END.
      ELSE DELETE ttTapFile.
   
   END.

   EMPTY TEMP-TABLE ttAnal.

END PROCEDURE.
