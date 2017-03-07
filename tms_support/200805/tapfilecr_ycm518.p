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

{Syst/commpaa.i}
gcBrand = "1".
katun = "anttis".
DEFINE TEMP-TABLE ttRoamCDR LIKE RoamCDR.
DEFINE TEMP-TABLE ttRoamGPRS LIKE RoamGPRS.
{/apps/snet/200805/roamtariff2.i}
{csvfuntion.i}
{Func/cparam.i2}


gcBrand = "1".
/*
DEFINE INPUT PARAMETER pcPLMN     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pdaDate1   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pdaDate2   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER pcCallType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER plTest     AS LOGICAL   NO-UNDO.
*/


{Func/timestamp.i}

DEFINE STREAM sCDR.

DEFINE NEW SHARED VARIABLE callrec AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTemp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPort         AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPause        AS INTEGER   NO-UNDO INIT 60.
DEFINE VARIABLE llOL           AS LOGICAL   NO-UNDO INIT TRUE.
DEFINE VARIABLE llLeave        AS LOGICAL   NO-UNDO FORMAT "Yes/No".
DEFINE VARIABLE lcCDR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaFrmDate     AS DATE      NO-UNDO.
DEFINE VARIABLE lcFrmTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName     AS CHARACTER NO-UNDO init "/apps/snet/200805/cdr20080506.asc".
DEFINE VARIABLE liFileAmt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcEventType    AS CHARACTER NO-UNDO.


/*lcFileName = "/apps/snet/200805/testi.cdr".*/

INPUT STREAM sCDR FROM VALUE(lcFileName).

PROCEDURE pFoo:

   ROAMCDR:
   DO WHILE TRUE:

      RELEASE ttRoamGPRS.
      RELEASE ttRoamCDR.
      

      IMPORT STREAM sCDR UNFORMATTED callrec.
      liFileAmt = liFileAmt + 1.
      

      IF callrec BEGINS "*" OR callrec BEGINS "#" THEN DO:
         
         NEXT ROAMCDR.

      END.
      
      DO liLoop = 1 TO NUM-ENTRIES(callrec,"|"):

         ASSIGN
            lcTemp = ENTRY(liLoop,callrec,"|")
            lcTemp = TRIM(lcTemp).
            
         IF liLoop = 1 THEN lcCDR = lcTemp.
         ELSE               lcCDR = lcCDR + "|" + lcTemp.

      END.
      
      DO TRANSACTION:
         
         lcEventType = ENTRY(8,lcCDR,"|").
         CASE lcEventType:
            WHEN "GPRS" THEN DO:
               CREATE ttroamgprs.
               ttroamgprs.CSV = lcCDR.
            END.
            WHEN "CALL" OR WHEN "SMS" THEN DO:
               CREATE ttroamcdr.
               ttroamcdr.CSV = lcCDR.
            END.
         END.   

         IF AVAIL ttroamgprs OR AVAIL ttroamcdr THEN DO:     

            fRoamTariff(TRUE, BUFFER ttroamcdr, BUFFER ttroamgprs). 
            
         END.

      END.
   
   END.
END PROCEDURE. 
run pfoo.
MESSAGE "foo" VIEW-AS ALERT-BOX.
IF NOT llOL THEN DO:

   MESSAGE 
      "File reading is finished ! " + STRING(liFileAmt) + " CDRs"
   VIEW-AS ALERT-BOX TITLE " CDRs from file ".

END.


/*-----------------------------------------------------------------------*/



DEFINE VAR pcPLMN     AS CHARACTER NO-UNDO.
DEFINE VAR pdaDate1   AS DATE      NO-UNDO.
DEFINE VAR pdaDate2   AS DATE      NO-UNDO.
DEFINE VAR pcCallType AS CHARACTER NO-UNDO.
DEFINE VAR plTest     AS LOGICAL   NO-UNDO init false.


/*RUN roamdouble(pdaDate1,pdaDate2).*/

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

liLoop = 0.

INPUT FROM /apps/snet/200805/roamfields_mm0102.txt.
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
   lcSpoolDir = fCParam("DUMPSPOOL","roamtestui.p")
   lcOutGDir  = fCParam("DUMPOUTGOING","roamtestui.p").
ELSE ASSIGN
   lcSpoolDir = "/apps/snet/200805/ycm518spool/".
   lcOutGDir  = "/apps/snet/200805/ycm518out/".

FOR EACH RoamOper NO-LOCK:
   
   CASE plTest:
      WHEN TRUE  THEN
         IF pcPLMN NE RoamOper.PLMN THEN NEXT.
      WHEN FALSE THEN
         IF RoamOper.Production = 0 THEN NEXT.
   END.
 /*  
   DO TRANS:
    
      FIND FIRST bufOper WHERE
           RECID(bufOper) = RECID(RoamOper)
      EXCLUSIVE-LOCK NO-ERROR.

      IF plTest THEN ASSIGN
         bufOper.TestFileSeq = bufOper.TestFileSeq + 1
         liFileSeq           = bufOper.TestFileSeq.
      ELSE ASSIGN
         bufOper.FileSeq = bufOper.FileSeq + 1
         liFileSeq       = bufOper.FileSeq.

   END.
*/
      FIND FIRST bufOper WHERE
           RECID(bufOper) = RECID(RoamOper)
      NO-LOCK NO-ERROR.
   liFileSeq = bufOper.FileSeq.
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

   IF plTest THEN liFileType = LOOKUP(pcCallType,"-,V,G,A") - 1.
   ELSE           RoamOper.Production.

   RUN pTAPFile(3,plTest).

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
         lcTapGen = "cd " + lcSpoolDir + " ; /home/kpasanen/tap3gen/src/tap3gen " + lcHdrFile + " " + lcCDRFile.
         
      ELSE
         lcTapGen = "cd " + lcSpoolDir + " ; test3gen --test " + lcHdrFile + " " + lcCDRFile.
         
      UNIX SILENT VALUE(lcTapGen).
         
   END.
   ELSE DO:
      
      IF NOT plTest THEN
         lcTapGen = "cd " + lcSpoolDir + " ; /home/kpasanen/tap3gen/src/tap3gen --notification " + lcHdrFile.
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
         FIND FIRST ttRoamCDR WHERE
              RECID(ttRoamCDR) = ttTAPFile.RowNum
         NO-LOCK NO-ERROR.
         lcCSV = ttRoamCDR.CSV.
      END.

      WHEN 2 THEN DO:
         FIND FIRST ttRoamGPRS WHERE
              RECID(ttRoamGPRS) = ttTAPFile.RowNum
         NO-LOCK NO-ERROR.
         lcCSV = ttRoamGPRS.CSV.
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
            lcValue = REPLACE(lcValue,".","").
         WHEN "exchange_rate" THEN ASSIGN
            lcValue = REPLACE(STRING(ldeExchRate,"9.99999"),",","")
            lcValue = REPLACE(lcValue,".","").
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

   FOR EACH ttRoamCDR NO-LOCK WHERE
            ttRoamCDR.PLMN       = lcRoamOper /*AND
            ttRoamCDR.DateStart >= pdaDate1   AND
            ttRoamCDR.DateStart <= pdaDate2*/ :
         
      CREATE ttTAPFile.
      BUFFER-COPY ttRoamCDR EXCEPT ttRoamCDR.CSV TO ttTapFile.

      ASSIGN
         ttTAPFile.RowType  = 1
         ttTAPFile.RowNum   = RECID(ttRoamCDR)
         ttTAPFile.Duration = INT(ENTRY(16,ttRoamCDR.CSV,"|"))
         ttTAPFile.TotUnit  = ttTAPFile.Units.

   END.

END PROCEDURE.

PROCEDURE pFillTempTable2:
   
   DEFINE BUFFER bufPartial FOR ttRoamGPRS.

   DEFINE VARIABLE ldeKB AS DECIMAL NO-UNDO.
   
   FOR EACH ttRoamGPRS NO-LOCK WHERE
            ttRoamGPRS.PLMN       = lcRoamOper: /*AND
            
            ttRoamGPRS.DateStart >= pdaDate1   AND
            ttRoamGPRS.DateStart <= pdaDate2:*/

      IF CAN-FIND(FIRST ttAnal WHERE
                        ttAnal.RowNum = RECID(ttRoamGPRS)) THEN NEXT.
   
      CREATE ttTAPFile.
   
      BUFFER-COPY ttRoamGPRS EXCEPT
         ttRoamGPRS.CSV
         ttRoamGPRS.CallIdNum
         ttRoamGPRS.GGSNAddr
         ttRoamGPRS.PartInd
         ttRoamGPRS.PartRecNum
      TO ttTAPFile.
   
      ASSIGN
         ttTAPFile.RowType  = 2
         ttTAPFile.RowNum   = RECID(ttRoamGPRS)
         ttTAPFile.TotUnits = INT(ENTRY(62,ttRoamGPRS.CSV,"|")) +
                              INT(ENTRY(63,ttRoamGPRS.CSV,"|"))
         ttTAPFile.DataIn   = INT(ENTRY(62,ttRoamGPRS.CSV,"|"))
         ttTAPFile.DataOut  = INT(ENTRY(63,ttRoamGPRS.CSV,"|"))
         ttTAPFile.Duration = INT(ENTRY(16,ttRoamGPRS.CSV,"|")).
                                                                  
      IF ttRoamGPRS.PartRecNum NE 0 THEN DO:

         FOR EACH bufPartial NO-LOCK WHERE
                  bufPartial.DateRead  = ttRoamGPRS.DateRead  AND
                  bufPartial.DateStart = ttRoamGPRS.DateStart AND
                  bufPartial.CallIdNum = ttRoamGPRS.CallIdNum AND
                  bufPartial.GGSNAddr  = ttRoamGPRS.GGSNAddr  AND
            RECID(bufPartial) NE RECID(ttRoamGPRS):
         
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
            lcIMSI   = ENTRY(21,ttRoamGPRS.CSV,"|") /* originating */
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
         
         ttTAPFile.Amount = fGPRS(ttRoamGPRS.DateStart, ttTAPFile.TotUnits, 1,
            lcPLMN, lcGroup).
         
      END.
      ELSE DELETE ttTapFile.
   
   END.

   EMPTY TEMP-TABLE ttAnal.

END PROCEDURE.

