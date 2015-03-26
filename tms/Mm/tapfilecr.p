/* ----------------------------------------------------------------------
  MODULE .......: TAPFILECR.P
  TASK .........: Creates Roaming IN TAP 3.11 files
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
                  27.11.08 as Split large GPRS traffic 
                  29.02.09 as New Ascii output format

  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{roamtariff.i}
{commali.i}
{cparam2.i}
{timestamp.i}

gcBrand = "1".

DEFINE INPUT PARAMETER pcPLMN     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pdaDate1   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pdaDate2   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pcCallType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER plTest     AS LOGICAL   NO-UNDO.

IF NOT plTest THEN 
   RUN roamdouble(pdaDate1,pdaDate2).

DEFINE TEMP-TABLE ttFields NO-UNDO
   FIELD tiField AS INTEGER 
   FIELD tcField AS CHARACTER
   FIELD tiCsvPos AS INTEGER EXTENT 3
   
   INDEX tiField AS PRIMARY tiField.

DEFINE TEMP-TABLE ttTAPFile NO-UNDO LIKE RoamCDR
   FIELD RowType    AS INTEGER
   FIELD RowNum     AS RECID
   FIELD TotUnits   AS INT64 
   FIELD Duration   AS INT64
   FIELD DataIn     AS INT64
   FIELD DataOut    AS INT64
   FIELD SMSTermIsCharged AS LOG
   FIELD CloseCause AS CHARACTER.

DEFINE TEMP-TABLE ttAnal NO-UNDO
   FIELD RowNum AS RECID
   
   INDEX RowNum AS PRIMARY RowNum.
   
DEFINE TEMP-TABLE ttSMSTCharged
   FIELD PLMN LIKE RoamCDR.PLMN
   FIELD DateStart LIKE RoamCDR.DateStart
   FIELD SMSTermIsCharged AS LOGICAL
INDEX PLMN IS PRIMARY PLMN DateStart.

      
DEFINE VARIABLE lcCDRFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaCurrDate AS DATE      NO-UNDO.
DEFINE VARIABLE lcRoamOper  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
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
DEFINE VARIABLE liLength    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liPosCloseCause AS INTEGER NO-UNDO EXTENT 3. 
DEFINE VARIABLE liVersion   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeDumpStarted AS DEC NO-UNDO.

INPUT FROM /apps/tms/Rate/roamfields.txt.
REPEAT:
   
   IMPORT UNFORMATTED lcTapGen.

   IF lcTapGen NE "" THEN DO:
      CREATE ttFields.
      ASSIGN
         liLoop            = liLoop + 1
         ttFields.tcField  = lcTapGen
         ttFields.tiField  = liLoop.
         ttFields.tiCsvPos[1] = fGetPosition("0102MM",
                                             REPLACE(lcTapGen,"_"," ")).
         ttFields.tiCsvPos[2] = fGetPosition("0103MM",
                                             REPLACE(lcTapGen,"_"," ")).
         ttFields.tiCsvPos[3] = fGetPosition("0104MM",
                                             REPLACE(lcTapGen,"_"," ")).
      
       
      IF ttFields.tcField = "cause_for_record_closing" THEN ASSIGN 
         liPosCloseCause[1] = ttFields.tiCsvPos[1]
         liPosCloseCause[2] = ttFields.tiCsvPos[2]
         liPosCloseCause[3] = ttFields.tiCsvPos[3].
   END.

END.

DEFINE STREAM sCdr.

DEFINE BUFFER bufOper FOR RoamOper.

IF plTest THEN ASSIGN
   lcSpoolDir = fCParam("DUMPSPOOL","roamtestui.p")
   lcOutGDir  = fCParam("DUMPOUTGOING","roamtestui.p").
ELSE ASSIGN
   lcSpoolDir = fCParam("DUMPSPOOL","tapfilecr.p")
   lcOutGDir  = fCParam("DUMPOUTGOING","tapfilecr.p").

ldeDumpStarted = fMakeTS().

FOR EACH RoamOper NO-LOCK:
   
   CASE plTest:
      WHEN TRUE  THEN
         IF pcPLMN NE RoamOper.PLMN THEN NEXT.
      WHEN FALSE THEN
         IF RoamOper.Production = 0 THEN NEXT.
   END.
   
   DO TRANS:
    
      FIND FIRST bufOper WHERE
           RECID(bufOper) = RECID(RoamOper)
      EXCLUSIVE-LOCK.

      IF plTest THEN ASSIGN
         bufOper.TestFileSeq = bufOper.TestFileSeq + 1
         liFileSeq           = bufOper.TestFileSeq.
      ELSE ASSIGN
         bufOper.FileSeq = bufOper.FileSeq + 1
         liFileSeq       = bufOper.FileSeq.

   END.

   ASSIGN
      lcRoamOper  = RoamOper.PLMN.

   IF plTest THEN liFileType = LOOKUP(pcCallType,"-,V,G,A") - 1.
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
   DEFINE VARIABLE liLines AS INTEGER NO-UNDO. 
   
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
   
   ASSIGN
      lcCDRFile   = lcSpoolDir + (IF plTest THEN "TD" ELSE "CD") +
                    "ESPXF" + lcRoamOper + STRING(liFileSeq,"99999") + ".asc".
      
   OUTPUT STREAM sCdr TO VALUE(lcCDRFile).

   liLines = 0.
   FOR EACH ttTAPFile NO-LOCK:
      
      IF ttTAPFile.EventType = "SMS" AND
         ttTAPFile.ChargedParty = 2 AND               
         ttTAPFile.SMSTermIsCharged = FALSE THEN NEXT.
      
      liLines = liLines + 1.
      
      IF liLines > 4000 THEN DO:
         liFileSeq = liFileSeq + 1.
         liLines = 1.
         OUTPUT STREAM sCdr CLOSE.
         lcCDRFile = lcSpoolDir + (IF plTest THEN "TD" ELSE "CD") +
                    "ESPXF" + lcRoamOper + STRING(liFileSeq,"99999") + ".asc".
         OUTPUT STREAM sCdr TO VALUE(lcCDRFile).
      END.
      
      RUN pTap3File. 
   END.

   IF liLines = 0 THEN RUN pTap3FileEmpty.

   OUTPUT STREAM sCdr CLOSE.
   
   DO TRANS:
    
      IF plTest THEN ASSIGN
         bufOper.TestFileSeq = liFileSeq.
      ELSE ASSIGN
         bufOper.FileSeq = liFileSeq.

   END.
   
   RELEASE bufOper.
   EMPTY TEMP-TABLE ttTAPFile.

   IF plTest THEN DO:
   
      IF llFound THEN lcMessage = "TAP file with CDRs done.".
      ELSE            lcMessage = "TAP file with NO CDRs done.".

      MESSAGE lcMessage VIEW-AS ALERT-BOX.
  
   END.

END PROCEDURE.

PROCEDURE pTap3FileEmpty:
      
   ASSIGN
      lcRoamCDR = "".
   
   FOR EACH ttFields NO-LOCK:
      
      CASE ttFields.tcField:
         WHEN "event_type" THEN lcValue = "NOCDR".
         WHEN "sender" THEN lcValue = "ESPXF".
         WHEN "recipient" THEN lcValue = RoamOper.PLMN.
         WHEN "file_sequence_number" THEN lcValue = STRING(liFileSeq,"99999").
         OTHERWISE lcValue = "".
      END.
   
   lcRoamCDR = lcRoamCDR + "," + lcValue.

   END.
   
   PUT STREAM sCdr UNFORMATTED SUBSTR(lcRoamCDR,2) CHR(10).

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
      
   CASE ttTAPFile.Version:
   WHEN "0102" THEN liVersion = 1.
   WHEN "0103" THEN liVersion = 2.
   WHEN "0104" OR WHEN "0105" THEN liVersion = 3.
   OTHERWISE liVersion = 0.
   END CASE.
   
   IF liVersion = 0 THEN DO TRANS:
   
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "TAPFILE"
             ErrorLog.TableName = "TAP"
             ErrorLog.KeyValue  = STRING(TODAY,"999999")
             ErrorLog.ErrorChar = ""
             ErrorLog.ErrorMsg  = "Unknown version " + ttTAPFile.Version
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
      NEXT.
   END.

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
      
      CASE ttFields.tcField:
         WHEN "sender" THEN lcValue = "ESPXF".
         WHEN "recipient" THEN lcValue = RoamOper.PLMN.
         WHEN "file_sequence_number" THEN lcValue = STRING(liFileSeq,"99999").
         WHEN "duration" THEN lcValue = STRING(ttTAPFile.Duration).
         WHEN "charged_item" THEN CASE ttTAPFile.EventType:
            WHEN "CALL" THEN lcValue = "D".
            WHEN "SMS"  THEN lcValue = "E".
            WHEN "MMS"  THEN lcValue = "E".
            WHEN "GPRS" THEN lcValue = "X".
         END.
         WHEN "chargeable_units" THEN DO:
            lcValue = STRING(ttTAPFile.TotUnits).
         END.
         WHEN "charge" THEN DO:
            ASSIGN
              lcValue = REPLACE(STRING((ttTAPFile.Amount / ldeExchRate),
                            ">>>>>9.99999"),",","")
              lcValue = TRIM(REPLACE(lcValue,".","")). 
            
            DO WHILE SUBSTR(lcValue,1,1) = "0":
               lcValue = SUBSTR(lcValue, 2).
            END.
            IF lcValue = "" THEN lcValue = "0".
         END.
         WHEN "exchange_rate" THEN ASSIGN
            lcValue = REPLACE(STRING(ldeExchRate,"9.99999"),",","")
            lcValue = REPLACE(lcValue,".","").
         /* document page 53 for call type levels 1-3 */
         WHEN "call_type_level_1" THEN DO:
            IF ttTAPFile.ChargedParty = 2 THEN lcValue = "0". /* YDR-590 */
            ELSE DO:
               CASE ttTAPFile.EventType:
               WHEN "GPRS" THEN lcValue = "10".
               OTHERWISE DO:
                  IF ttTAPFile.GsmBNr BEGINS "34" THEN lcValue = "1".
                  ELSE                               lcValue = "2".
               END.
               END CASE.
            END.
         END.
         WHEN "call_type_level_2" THEN DO:
            IF ttTAPFile.ChargedParty = 2 THEN lcValue = "0".
            ELSE IF ttTAPFile.RSubType = 1 THEN lcValue = "0".
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
            lcValue = ENTRY(ttFields.tiCSVPos[liVersion], lcCSV, "|").
            IF SUBSTR(lcValue,1,1) = "0" THEN lcValue = SUBSTR(lcValue,2).
         END.
         /* remove possible MCC&MNC from MCC&MNC&LAC&SAC */
         WHEN "first_calling_location" OR WHEN "first_called_location" THEN DO:
            lcValue = ENTRY(ttFields.tiCSVPos[liVersion], lcCSV, "|").
            liLength = LENGTH(lcValue).
            IF liLength > 8 THEN lcValue = SUBSTR(lcValue, liLength - 7).
         END.
         /* GRPS fields */
         WHEN "partial_indicator"      THEN lcValue = "0".
         WHEN "data_amount_incoming"   THEN lcValue = STRING(ttTAPFile.DataIn).
         WHEN "data_amount_outgoing"   THEN lcValue = STRING(ttTapFile.DataOut).
         WHEN "cause_for_record_closing" THEN DO:
            IF LOOKUP(ttTapFile.CloseCause,"3,4,5,20,21") > 0
               THEN lcValue = ttTapFile.CloseCause. 
               ELSE lcValue = "".
         END.
         WHEN "tap_decimal_places" THEN lcValue = "5".
         WHEN "exchange_rate_decimal_places" THEN lcValue = "5".
         WHEN "local_currency" THEN lcValue = "EUR".
         
         /* by default use original value from cdr */
         OTHERWISE IF ttFields.tiCSVPos[liVersion] > 0 THEN
            lcValue = ENTRY(ttFields.tiCSVPos[liVersion], lcCSV, "|").
            ELSE lcValue = "".

      END.

      lcRoamCDR = lcRoamCDR + "," + lcValue.

   END.

   PUT STREAM sCdr UNFORMATTED SUBSTR(lcRoamCDR,2) CHR(10).

END PROCEDURE.

PROCEDURE pFillTempTable1:

   DEFINE VARIABLE lrRecId AS RECID NO-UNDO.
   DEF BUFFER bRoamCDR FOR RoamCDR.

   FOR EACH RoamCDR NO-LOCK WHERE
            RoamCDR.PLMN       = lcRoamOper AND
            RoamCDR.DateStart >= pdaDate1   AND
            RoamCDR.DateStart <= pdaDate2:
         
      CREATE ttTAPFile.
      BUFFER-COPY RoamCDR EXCEPT RoamCDR.CSV TO ttTapFile.

      IF NOT plTest THEN
         FIND FIRST bRoamCDR WHERE
              ROWID(bRoamCDR) EQ ROWID(RoamCDR) EXCLUSIVE-LOCK.

      /* check if SMS-T (terminating) price exists */
      IF RoamCDR.EventType = "SMS" AND
         RoamCDR.ChargedParty = 2 THEN DO:

         FIND FIRST ttSMSTCharged WHERE
            ttSMSTCharged.PLMN = lcRoamOper AND
            ttSMSTCharged.DateStart = RoamCDR.DateStart NO-LOCK NO-ERROR.

         IF NOT AVAIL ttSMSTCharged THEN DO:
            
            lrRecId = fFindTariff(RoamCDR.DateStart, 4, 2, "", 
                                  lcRoamOper, RoamOper.RoamGroup).
            CREATE ttSMSTCharged.
            ASSIGN
               ttSMSTCharged.PLMN = lcRoamOper 
               ttSMSTCharged.DateStart = RoamCDR.DateStart
               ttSMSTCharged.SMSTermIsCharged = (lrRecId NE ?).
         END.

         ttTapFile.SMSTermIsCharged = ttSMSTCharged.SMSTermIsCharged.
         IF NOT plTest AND ttTapFile.SMSTermIsCharged EQ True THEN
            bRoamCDR.TSDump = ldeDumpStarted.
            
      END.
      ELSE IF NOT plTest THEN
         bRoamCDR.TSDump = ldeDumpStarted.

      IF NOT plTest THEN RELEASE bRoamCDR.
      
      fRoamCSV(RoamCDR.Version).
      
      CASE RoamCDR.Version:
      WHEN "0102" THEN liVersion = 1.
      WHEN "0103" THEN liVersion = 2.
      OTHERWISE liVersion = 3.
      END CASE. 
      
      ASSIGN
         ttTAPFile.RowType  = 1
         ttTAPFile.RowNum   = RECID(RoamCDR)
         ttTAPFile.Duration = INT(ENTRY(ttRoamCSV.Duration,RoamCDR.CSV,"|"))
         ttTAPFile.TotUnit  = ttTAPFile.Units
         ttTAPFile.CloseCause = (IF ttTAPFile.EventType = "CALL"
               THEN ENTRY(liPosCloseCause[liVersion],RoamCDR.CSV,"|") ELSE "").

   END.

END PROCEDURE.
      
PROCEDURE pGPRSAggrStart:

   DEF INPUT PARAM pidGPRS AS ROWID.
   DEF BUFFER bufGPRS FOR RoamGPRS.
   
   FIND bufGPRS WHERE
      ROWID(bufGPRS) = pidGPRS NO-LOCK.
   
   CREATE ttTAPFile.

   BUFFER-COPY bufGPRS EXCEPT
      bufGPRS.CSV
      bufGPRS.CallIdNum
      bufGPRS.GGSNAddr
      bufGPRS.PartInd
      bufGPRS.PartRecNum
   TO ttTAPFile.

   fRoamCSV(bufGPRS.Version). 

   CASE bufGPRS.Version:
   WHEN "0102" THEN liVersion = 1.
   WHEN "0103" THEN liVersion = 2.
   OTHERWISE liVersion = 3.
   END CASE. 
    
   ASSIGN
      ttTAPFile.RowType  = 2
      ttTAPFile.RowNum   = RECID(bufGPRS)
      ttTAPFile.TotUnits = INT(ENTRY(ttRoamCSV.DataIn,bufGPRS.CSV,"|")) +
                           INT(ENTRY(ttRoamCSV.DataOut,bufGPRS.CSV,"|"))
      ttTAPFile.DataIn   = INT(ENTRY(ttRoamCSV.DataIn,bufGPRS.CSV,"|"))
      ttTAPFile.DataOut  = INT(ENTRY(ttRoamCSV.DataOut,bufGPRS.CSV,"|"))
      ttTAPFile.Duration = INT(ENTRY(ttRoamCSV.Duration,bufGPRS.CSV,"|"))
      ttTAPFile.CloseCause = ENTRY(liPosCloseCause[liVersion],bufGPRS.CSV,"|").

END PROCEDURE. 
      
PROCEDURE pGPRSAggrEnd:
      
   DEF INPUT PARAM pidGPRS AS ROWID.
   DEF BUFFER bufGPRS FOR RoamGPRS.
   
   FIND bufGPRS WHERE
      ROWID(bufGPRS) = pidGPRS NO-LOCK.
      
      /* some data exists */
      IF ttTAPFile.TotUnits > 0 THEN DO:
         ttTAPFile.Amount = fGPRS(bufGPRS.DateStart, ttTAPFile.TotUnits, 1,
            RoamOper.PLMN, RoamOper.RoamGroup).
         
      END.
      ELSE DELETE ttTapFile.

END.

PROCEDURE pFillTempTable2:
   
   DEFINE BUFFER bufPartial FOR RoamGPRS.
   DEFINE BUFFER bRoamGPRS FOR RoamGPRS.

   DEFINE VARIABLE liDataCounter AS INT64 NO-UNDO.  
   DEFINE VARIABLE liPartVersion AS INT   NO-UNDO.

   FOR EACH RoamGPRS NO-LOCK WHERE
            RoamGPRS.PLMN       = lcRoamOper AND
            RoamGPRS.DateStart >= pdaDate1   AND
            RoamGPRS.DateStart <= pdaDate2:

      IF CAN-FIND(FIRST ttAnal WHERE
                        ttAnal.RowNum = RECID(RoamGPRS)) THEN NEXT.
   
      RUN pGPRSAggrStart(ROWID(RoamGPRS)). 
 
      CASE RoamGPRS.Version:
      WHEN "0102" THEN liVersion = 1.
      WHEN "0103" THEN liVersion = 2.
      OTHERWISE liVersion = 3.
      END CASE. 
                                                                   
      IF RoamGPRS.PartRecNum NE 0 THEN DO:
         
         liDataCounter = ttTAPFile.TotUnits.
         
         GPRS_PARTIAL_LOOP:
         FOR EACH bufPartial NO-LOCK WHERE
                  bufPartial.PLMN = RoamGPRS.PLMN AND
                  bufPartial.DateRead  = RoamGPRS.DateRead  AND
                  bufPartial.DateStart = RoamGPRS.DateStart AND
                  bufPartial.CallIdNum = RoamGPRS.CallIdNum AND
                  bufPartial.GGSNAddr  = RoamGPRS.GGSNAddr  AND
            RECID(bufPartial) NE RECID(RoamGPRS):
         
            CASE bufPartial.Version:
            WHEN "0102" THEN liPartVersion = 1.
            WHEN "0103" THEN liPartVersion = 2.
            OTHERWISE liPartVersion = 3.
            END CASE. 
            
            fRoamCSV(bufPartial.Version).
                    
            liDataCounter = liDataCounter + 
               INT(ENTRY(ttRoamCSV.DataIn,bufPartial.CSV,"|")) +
               INT(ENTRY(ttRoamCSV.DataOut, bufPartial.CSV,"|")).

            /* Max. amount is 2^31-1 due to 32bit integer */
            IF liDataCounter > 2147483647 THEN DO:
         
               ttTAPFile.CloseCause = ENTRY(liPosCloseCause[liPartVersion],
                                            bufPartial.CSV,"|").
               
               RUN pGPRSAggrEnd(ROWID(bufPartial)).
               RUN pGPRSAggrStart(ROWID(bufPartial)).
               
               liDataCounter = 
                  INT(ENTRY(ttRoamCSV.DataIn,bufPartial.CSV,"|")) +
                  INT(ENTRY(ttRoamCSV.DataOut, bufPartial.CSV,"|")).
               
               CREATE ttAnal.
               ttAnal.RowNum = RECID(bufPartial).
               
               NEXT GPRS_PARTIAL_LOOP.
            END.

            ASSIGN
               ttTAPFile.TotUnits   = liDataCounter 
               ttTAPFile.DataIn     = ttTAPFile.DataIn   + 
                            INT(ENTRY(ttRoamCSV.DataIn,bufPartial.CSV,"|"))
               ttTAPFile.DataOut    = ttTAPFile.DataOut  + 
                            INT(ENTRY(ttRoamCSV.DataOut, bufPartial.CSV,"|"))
               ttTAPFile.Duration  = ttTAPFile.Duration + 
                            INT(ENTRY(ttRoamCSV.Duration, bufPartial.CSV,"|")).
               ttTAPFile.CloseCause = ENTRY(liPosCloseCause[liPartVersion],
                                            bufPartial.CSV,"|").

            CREATE ttAnal.
            ttAnal.RowNum = RECID(bufPartial).
         
            IF NOT plTest THEN DO:
               FIND FIRST bRoamGPRS WHERE
                    ROWID(bRoamGPRS) EQ ROWID(bufPartial) EXCLUSIVE-LOCK.
               bRoamGPRS.TSDump = ldeDumpStarted.
               RELEASE bRoamGPRS.
            END.
         
         END.
      
      END.
  
      RUN pGPRSAggrEnd(ROWID(RoamGPRS)).
         
      IF NOT plTest THEN DO:
         FIND FIRST bRoamGPRS WHERE
              ROWID(bRoamGPRS) EQ ROWID(RoamGPRS) EXCLUSIVE-LOCK.
         bRoamGPRS.TSDump = ldeDumpStarted.
         RELEASE bRoamGPRS.
      END.
   
   END.

   EMPTY TEMP-TABLE ttAnal.

END PROCEDURE.
