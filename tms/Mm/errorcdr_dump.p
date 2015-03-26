{commali.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaFromDate   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate     AS DATE NO-UNDO.
DEF INPUT  PARAMETER ilAppend      AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE lcTemp       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTitle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcCallIdNr   AS CHARACTER  NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE lcMessType   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTransAddr  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liRound      AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcVersion    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcNumeric    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liNumOfFields AS INT NO-UNDO. 

DEF VAR lhMCDR    AS HANDLE NO-UNDO.
DEF VAR lhMDetail AS HANDLE NO-UNDO.
DEF VAR lhECDR    AS HANDLE NO-UNDO.
DEF VAR lhEDetail AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttHeader NO-UNDO
   FIELD tcVersion AS CHARACTER
   FIELD tiCall    AS INTEGER
   FIELD tiMess    AS INTEGER
   FIELD tiTrans   AS INTEGER
   FIELD tiOrig    AS INTEGER.

DEF STREAM sFile.

IF ilAppend THEN
   OUTPUT STREAM sFile TO VALUE(icFile) APPEND.
ELSE
   OUTPUT STREAM sFile TO VALUE(icFile).


FOR EACH CSVHeader NO-LOCK:
   
   CREATE ttHeader.

   ttHeader.tcVersion = CSVHeader.Version.

   DO liRound = 1 TO NUM-ENTRIES(CSVHeader.CSV,"|"):
               
      ASSIGN
         lcTemp  = ENTRY(liRound,CSVHeader.CSV,"|")
         lcTitle = ENTRY(2,(ENTRY(1,lcTemp,">")),"=")
      NO-ERROR.
      
      CASE lcTitle:

          WHEN "Call identification number" THEN 
             ttHeader.tiCall  = liRound.
          WHEN "Message type indicator" THEN 
             ttHeader.tiMess = liRound.
          WHEN "Translated address" THEN 
             ttHeader.tiTrans = liRound.
          WHEN "Original cdr type" THEN
             ttHeader.tiOrig  = liRound.
   
      END CASE.

   END.

END.

ASSIGN
   lhMCDR      = BUFFER MobCDR:HANDLE
   lhMDetail   = BUFFER McdrDtl2:HANDLE
   lhECDR      = BUFFER ErrorCDR:HANDLE
   lhEDetail   = BUFFER ErrorDtl:HANDLE.

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ErrorCode > 0         AND
         MobCDR.ReadDate  >= idaFromDate AND
         MobCDR.ReadDate  <= idaToDate
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END. /* IF RETRY THEN DO: */

   FIND FIRST MCDRDtl2 WHERE 
              MCDRDtl2.DateSt = MobCDR.DateSt AND
              MCDRDtl2.DtlSeq = MobCDR.DtlSeq
   NO-LOCK NO-ERROR.
   IF AVAILABLE MCDRDtl2 THEN
      RUN pWrite2File(lhMCDR,
                      lhMDetail).
END. /* FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE */

IF NOT olInterrupted THEN
FOR EACH ErrorCDR NO-LOCK USE-INDEX ReadDate WHERE
         ErrorCDR.ReadDate >= idaFromDate AND
         ErrorCDR.ReadDate <= idaToDate
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END. /* IF RETRY THEN DO: */

   FIND FIRST ErrorDtl WHERE 
              ErrorDtl.DateSt = ErrorCDR.DateSt AND
              ErrorDtl.DtlSeq = ErrorCDR.DtlSeq
   NO-LOCK NO-ERROR.
   IF AVAILABLE ErrorDtl THEN
      RUN pWrite2File(lhECDR,
                      lhEDetail).
END. /* FOR EACH ErrorCDR NO-LOCK USE-INDEX ReadDate WHERE */

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.


PROCEDURE pWrite2File:

   DEF INPUT PARAMETER ihCDR    AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER ihDetail AS HANDLE NO-UNDO.
   
   IF NOT ihDetail:AVAILABLE THEN DO:
       
       ASSIGN
          lcCallIdNr  = "null"
          lcTransAddr = "null".

       IF ihCDR::EventType = "SMS" THEN lcMessType = "null".
       ELSE                             lcMessType = "".

   END.
   ELSE DO:           

      ASSIGN
         lcVersion = ihDetail::Version
         liNumOfFields = NUM-ENTRIES(ihDetail::Detail,"|").
      
      IF INDEX(lcVersion,"YC") = 0 and liNumOfFields >= 4 THEN 
         lcVersion = lcVersion + ENTRY(4,ihDetail::Detail,"|").
   
      FIND FIRST ttHeader WHERE
                  ttHeader.tcversion = lcVersion
       NO-LOCK NO-ERROR. 
       
       IF NOT AVAIL ttHeader THEN DO:

           ASSIGN
              lcCallIdNr  = "null" 
              lcTransAddr = "null".
           
           IF ihCDR::EventType = "SMS" THEN lcMessType = "null".
           ELSE                             lcMessType = "".
       END.

       ELSE DO:
           
           IF ttHeader.tiCall = 0 THEN lcCallIdNr = "".
           ELSE IF liNumOfFields >= ttHeader.tiCall THEN
              lcCallIdNr = ENTRY(ttHeader.tiCall,ihDetail::Detail,"|").
           
           IF ttHeader.tiMess = 0 THEN lcMessType = "".
           ELSE IF liNumOfFields >= ttHeader.tiMess THEN
              lcMessType = ENTRY(ttHeader.tiMess,ihDetail::Detail,"|"). 
           
           IF ttHeader.tiTrans = 0 THEN lcTransAddr = "".
           ELSE IF liNumOfFields >= ttHeader.tiTrans THEN
              lcTransAddr = ENTRY(ttHeader.tiTrans,ihDetail::Detail,"|").  
       
           IF (ttHeader.tiOrig = 0 OR 
               (liNumOfFields >= ttHeader.tiOrig AND
                ENTRY(ttHeader.tiOrig,ihDetail::Detail,"|") NE "MOSMS"))
           THEN lcMessType = "".
       END.
   END.    

   EXPORT STREAM sFile DELIMITER "|"
      ihCDR::ErrorCode
      ihCDR::CLI
      ihCDR::CurrUnit
      ihCDR::GsmBnr
      ihCDR::MpmRid
      ihCDR::ServRid
      ihCDR::CustNum
      ihCDR::InvCust
      ihCDR::BillTarget
      ihCDR::TimeStart
      ihCDR::BillDur
      ihCDR::DateSt
      ihCDR::CCN
      ihCDR::BDest
      ihCDR::TotDisc
      ihCDR::Amount
      ihCDR::DiscType
      ihCDR::StartCharge
      ihCDR::BillCode
      ihCDR::ErrorCode
      ihCDR::CLIType
      ihCDR::Disc%
      ihCDR::DiscFP
      ihCDR::DiscValue
      ihCDR::Fdisc
      ihCDR::RefPrice
      ihCDR::TariffNum
      ihCDR::MsSeq
      ihCDR::SPOcmt
      ihCDR::Ccharge
      ihCDR::Charge
      ihCDR::BNET
      ihCDR::InvSeq
      ihCDR::VatIncl
      ihCDR::DialType
      ihCDR::DataIn
      ihCDR::DataOut
      ihCDR::tariffClass
      ihCDR::Pulses
      ihCDR::ServiceName
      ihCDR::ServiceAddress
      ihCDR::AType
      ihCDR::BType
      ihCDR::BPref
      ihCDR::RateCCN
      ihCDR::RoutingNumber
      ? /* ihCDR::OrigRecordType */ /* field is not used with Yoigo */
      ihCDR::DtlSeq
      ihCDR::MPMAmt
      ihCDR::IMEI
      ihCDR::IMEI2
      ihCDR::MSCID
      ihCDR::AddBPref
      ihCDR::RoamingInd
      ihCDR::ForwardingType
      ihCDR::EventSubType
      ihCDR::IMSI2
      ihCDR::IMSI
      ihCDR::Currency
      ihCDR::PPFlag
      ihCDR::xSub
      ihCDR::ReadinTS
      ihCDR::CaseType
      ihCDR::GrossAmt
      ihCDR::EventType
      ihCDR::SubsType
      
      lcCallIdNr
      lcTransAddr
      lcMessType.
           

   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "ERROR CDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO: */

END PROCEDURE.

