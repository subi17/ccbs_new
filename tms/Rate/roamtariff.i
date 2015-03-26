/* ----------------------------------------------------------------------
  MODULE .......: ROAMTARIFF.I
  TASK .........: Calculate tariff for roaming cdrs
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 01.06.07
  CHANGED ......: 03.10.07 kl INT64 for CallIdNum

  Version ......: xfera 
  ---------------------------------------------------------------------- */

&IF "{&roamtariff}" NE "YES"
&THEN

&GLOBAL-DEFINE roamtariff YES

{timestamp.i}
{detailvalue.i}

DEFINE TEMP-TABLE ttRoamCSV NO-UNDO
   FIELD Version AS CHAR
   FIELD CDRSeq AS INT
   FIELD EventType AS INT
   FIELD EventSub AS INT
   FIELD StartDate AS INT
   FIELD StartTime AS INT
   FIELD Duration AS INT
   FIELD Charged AS INT
   FIELD OrigAddr AS INT
   FIELD OrigIMSI AS INT
   FIELD DestIMSI AS INT
   FIELD DestAddr AS INT
   FIELD CallId AS INT
   FIELD PartInd AS INT
   FIELD PartRecNum AS INT
   FIELD SGSN AS INT
   FIELD DataIn AS INT
   FIELD DataOut AS INT
   FIELD CamelServiceKey AS INT
   FIELD CamelDestNumber AS INT
INDEX version IS PRIMARY UNIQUE version. 

DEF TEMP-TABLE ttRoamCDR NO-UNDO LIKE roamcdr.RoamCDR.

DEF TEMP-TABLE ttRoamGPRS NO-UNDO LIKE roamcdr.RoamGPRS.


FUNCTION fRoamCSV RETURNS LOGICAL
   (pcVersion AS CHAR):
   
   FIND FIRST ttRoamCSV WHERE ttRoamCSV.Version EQ pcVersion NO-LOCK NO-ERROR.
   
   IF NOT AVAIL ttRoamCSV THEN DO:
     
     CREATE ttRoamCSV.
     
     ttRoamCSV.Version  = pcVersion.

     pcVersion = pcVersion + "MM".
     
     ASSIGN   
        ttRoamCSV.CDRSeq       = fGetPosition(pcVersion,"Cdr sequence number")
        ttRoamCSV.EventType    = fGetPosition(pcVersion,"Event type")
        ttRoamCSV.EventSub     = fGetPosition(pcVersion,"Event subtype")
        ttRoamCSV.StartDate    = fGetPosition(pcVersion,"Start date")
        ttRoamCSV.StartTime    = fGetPosition(pcVersion,"Start time")
        ttRoamCSV.Duration     = fGetPosition(pcVersion,"Duration")
        ttRoamCSV.Charged   = fGetPosition(pcVersion,"Charged party")
        ttRoamCSV.OrigAddr  = fGetPosition(pcVersion,"Originating address")
        ttRoamCSV.OrigIMSI  = fGetPosition(pcVersion,"Originating IMSI")
        ttRoamCSV.DestIMSI  = fGetPosition(pcVersion,"Destination IMSI")
        ttRoamCSV.DestAddr  = fGetPosition(pcVersion,"Destination address")
        ttRoamCSV.CallId       = fGetPosition(pcVersion,"Call identification number")
        ttRoamCSV.PartInd      = fGetPosition(pcVersion,"Partial indicator")
        ttRoamCSV.PartRecNum   = fGetPosition(pcVersion,"Partial record number")
        ttRoamCSV.SGSN    = fGetPosition(pcVersion,"SGSN address")
        ttRoamCSV.DataIn  = fGetPosition(pcVersion,"Data amount incoming")
        ttRoamCSV.DataOut = fGetPosition(pcVersion,"Data amount outgoing")
        ttRoamCSV.CamelServiceKey = fGetPosition(pcVersion,"CAMEL Service Key")
        ttRoamCSV.CamelDestNumber = fGetPosition(pcVersion,"Translated address").
     
   END.

END FUNCTION. 

FUNCTION fFindTariff RETURNS RECID
  (INPUT pdaDate    AS DATE,
   INPUT piType     AS INTEGER,
   INPUT piRoamType AS INTEGER,
   INPUT pcService  AS CHARACTER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE lrReturn AS RECID NO-UNDO.

   /* PLMN based tariff */
   FOR FIRST RoamTariff NO-LOCK WHERE
             RoamTariff.PriceList   = pcPLMN     AND
             RoamTariff.ValidFrom  <= pdaDate    AND
             RoamTariff.ValidTo    >= pdaDate    AND
             RoamTariff.RateType    = piType     AND
             RoamTariff.RoamingType = piRoamType AND
             RoamTariff.Service     = pcService USE-INDEX PriceList:
   
      lrReturn = RECID(RoamTariff).

   END.

   /* Group based tariff */
   IF lrReturn = ? THEN DO:
   
      FOR FIRST RoamTariff NO-LOCK WHERE
                RoamTariff.PriceList   = pcGroup    AND
                RoamTariff.ValidFrom  <= pdaDate    AND
                RoamTariff.ValidTo    >= pdaDate    AND
                RoamTariff.RateType    = piType     AND
                RoamTariff.RoamingType = piRoamType AND
                RoamTariff.Service     = pcService USE-INDEX PriceList:
   
         lrReturn = RECID(RoamTariff).

      END.
   
   END.

   /* DEFAULT tariff */
   IF lrReturn = ? THEN DO:
      FOR FIRST RoamTariff NO-LOCK WHERE
                RoamTariff.PriceList   = "DEFAULT"  AND
                RoamTariff.ValidFrom  <= pdaDate    AND
                RoamTariff.ValidTo    >= pdaDate    AND
                RoamTariff.RateType    = piType     AND
                RoamTariff.RoamingType = piRoamType AND
                RoamTariff.Service     = pcService USE-INDEX PriceList:
   
         lrReturn = RECID(RoamTariff).

      END.
   
   END.
   
   RETURN lrReturn.

END FUNCTION.

FUNCTION fVoiceSteps RETURNS DECIMAL
  (INPUT piTariffNum AS INTEGER,
   INPUT piSec       AS INTEGER):

   DEFINE VARIABLE ldeTariff  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liUnits    AS INTEGER NO-UNDO.
   DEFINE VARIABLE liStepSize AS INTEGER NO-UNDO.
   DEFINE VARIABLE liCycles   AS INTEGER NO-UNDO.
   
   FOR EACH RTItem WHERE
            RTItem.TariffNum = piTariffNum
   BY RTItem.StepNum:

      CASE RTItem.StepNum:
         
         /* 1st step */
         WHEN 1 THEN DO:
         
            ldeTariff = ldeTariff + RTItem.Tariff.
            CASE RTItem.Unit:
               WHEN "SEC" THEN liUnits = piSec - RTItem.StepSize.
               WHEN "MIN" THEN liUnits = piSec - (RTItem.StepSize * 60).
            END.
      
         END.

         /* rest of steps */
         WHEN 2 THEN DO:
            IF liUnits > 0 THEN DO:
               
               CASE RTItem.Unit:
                  /* seconds: no convert */
                  WHEN "SEC" THEN liStepSize = RTItem.StepSize.
                  /* minutes: how many minutes is one step */
                  WHEN "MIN" THEN liStepSize = RTItem.StepSize * 60.
               END.
               
               IF liUnits MODULO liStepSize = 0 THEN
                  liCycles  = TRUNC(liUnits / liStepSize,0).
               ELSE 
                  liCycles  = TRUNC(liUnits / liStepSize,0) + 1.
               ldeTariff = ldeTariff + (liCycles * RTItem.Tariff).
            END.

         END.
         
      END.

   END.

   RETURN ldeTariff.

END FUNCTION.

FUNCTION fVoice RETURNS DECIMAL
  (INPUT pdaDate    AS DATE,
   INPUT piSec      AS INTEGER,
   INPUT piRoamType AS INTEGER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE lrRecId     AS RECID   NO-UNDO.
   DEFINE VARIABLE ldeConnRate AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldeTariff   AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liUnits     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liCycles    AS INTEGER NO-UNDO.
   
   /* get connection tariff */
   lrRecId = fFindTariff(pdaDate, 2, 1, "", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeConnRate = RoamTariff.Tariff.
   END.
   
   /* get voice tariff */
   IF piRoamType EQ 2 THEN /* MTC Pricing */
      lrRecId = fFindTariff(pdaDate, 10, piRoamType, "", pcPLMN, pcGroup).
   ELSE
      lrRecId = fFindTariff(pdaDate, 3, piRoamType, "", pcPLMN, pcGroup).

   IF lrRecId > 0 THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
   
      IF ldeConnRate = 0 THEN ldeConnRate = RoamTariff.Tariff.

      ldeTariff = fVoiceSteps(RoamTariff.TariffNum, piSec).
      
   END.

   ldeTariff = ldeTariff + ldeConnRate.

   /* ad-hoc discount */
   lrRecId = fFindTariff(pdaDate, 1, 1, "VOICE", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = ldeTariff * (1 - RoamTariff.Tariff / 100).
   
   END.
   
   RETURN ldeTariff.

END FUNCTION.

FUNCTION fSMS RETURNS DECIMAL
  (INPUT pdaDate    AS DATE,
   INPUT piRoamType AS INTEGER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE ldeTariff AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lrRecId   AS RECID   NO-UNDO.
   
   lrRecId = fFindTariff(pdaDate, 4, piRoamType, "", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = RoamTariff.Tariff.

   END.

   /* ad-hoc discount */
   lrRecId = fFindTariff(pdaDate, 1, 1, "SMS", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = ldeTariff * (1 - RoamTariff.Tariff / 100).
   
   END.
 
   RETURN ldeTariff.

END FUNCTION.

FUNCTION fCountryZone RETURNS DECIMAL
  (INPUT pdaDate    AS DATE,
   INPUT piSec      AS INTEGER,
   INPUT piRoamType AS INTEGER,
   INPUT pcBDest    AS CHARACTER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE lrRecId     AS RECID     NO-UNDO.
   DEFINE VARIABLE liIdx       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcZone      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPrefix    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeTariff   AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeConnRate AS DECIMAL   NO-UNDO.
   
   /* get connection tariff */
   lrRecId = fFindTariff(pdaDate, 2, 1, "", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeConnRate = RoamTariff.Tariff.
   END.
   
   PREFIX_LOOP:
   DO liIdx = 5 TO 1 BY -1:
       
      IF CAN-FIND(FIRST RoamCountry WHERE
                        RoamCountry.Prefix = SUBSTR(pcBDest,1,liIdx)) THEN DO:

         FIND FIRST RoamCountry WHERE
                    RoamCountry.Prefix = SUBSTR(pcBDest,1,liIdx)
         NO-LOCK NO-ERROR.
         lcPrefix = RoamCountry.Prefix.
         lcZone = STRING(RoamCountry.RateZone).
         LEAVE PREFIX_LOOP.

      END.

   END.

   lrRecId = fFindTariff(pdaDate, 9, piRoamType, lcPrefix, pcPLMN, pcGroup).
   
   IF lrRecId EQ ? THEN 
      lrRecId = fFindTariff(pdaDate, 8, piRoamType, lcZone, pcPLMN, pcGroup).

   IF lrRecId NE ? THEN DO:

      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      IF ldeConnRate = 0 THEN ldeConnRate = RoamTariff.Tariff.

      ldeTariff = fVoiceSteps(RoamTariff.TariffNum, piSec).
      
   END.
   ldeTariff = ldeTariff + ldeConnRate.

   /* ad-hoc discount */
   lrRecId = fFindTariff(pdaDate, 1, 1, "VOICE", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = ldeTariff * (1 - RoamTariff.Tariff / 100).
   
   END.

   RETURN ldeTariff.

END.

FUNCTION fVAN RETURNS DECIMAL
  (INPUT pdaDate    AS DATE,
   INPUT piRoamType AS INTEGER,
   INPUT piStart    AS INTEGER,
   INPUT piSec      AS INTEGER,
   INPUT pcBDest    AS CHARACTER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE ldeTariff AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lrRecId   AS RECID   NO-UNDO.
   DEFINE VARIABLE liFrom    AS INTEGER NO-UNDO.
   DEFINE VARIABLE liTo      AS INTEGER NO-UNDO.
   DEFINE VARIABLE liSec     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liDur     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liStart   AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcFrom    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liCycles  AS INTEGER NO-UNDO.
  
   lrRecId = fFindTariff(pdaDate, 7, piRoamType, "", pcPLMN, pcGroup).
   
   IF lrRecId NE ? THEN DO:

      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      
      lcFrom = STRING(piStart,"HH:MM").
      FIND FIRST RoamBDest WHERE
                 RoamBDest.TariffNum = RoamTariff.TariffNum AND
                 RoamBDest.TZFrom   <= lcFrom               AND
                 RoamBDest.TZTo     >= lcFrom
      NO-LOCK NO-ERROR.

      IF piSec MODULO 60 = 0 THEN
         liCycles  = TRUNC(piSec / 60,0).
      ELSE 
         liCycles  = TRUNC(piSec / 60,0) + 1.
               
      ldeTariff = ldeTariff + (liCycles * RoamBDest.Tariff). 
   END.
   
   /* ad-hoc discount */
   lrRecId = fFindTariff(pdaDate, 1, piRoamType, "VOICE", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = ldeTariff * (1 - RoamTariff.Tariff / 100).
   
   END.

   RETURN ldeTariff.

END FUNCTION.

FUNCTION fGPRS RETURNS DECIMAL
  (INPUT pdaDate    AS DATE,
   INPUT piData     AS INT64,
   INPUT piRoamType AS INTEGER,
   INPUT pcPLMN     AS CHARACTER,
   INPUT pcGroup    AS CHARACTER):

   DEFINE VARIABLE ldeTariff   AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lrRecId     AS RECID   NO-UNDO.
   DEFINE VARIABLE liSize      AS INTEGER NO-UNDO.
   DEFINE VARIABLE liData      AS INT64 NO-UNDO.
   DEFINE VARIABLE liLoop      AS INTEGER NO-UNDO.
   DEFINE VARIABLE liLimit     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liLoopLimit AS INTEGER NO-UNDO.

   lrRecId = fFindTariff(pdaDate, 5, piRoamType, "", pcPLMN, pcGroup).

   liData = piData.

   IF lrRecId NE ? THEN DO:

      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.

      FOR EACH RTItem WHERE
               RTItem.TariffNum = RoamTariff.TariffNum
      BREAK BY RTItem.StepNum:
      
         IF liData > 0 THEN DO:
            CASE RTItem.Unit:
          
               WHEN "B" THEN DO: 
                  liSize    = RTItem.StepSize.
                  IF RTItem.SizeLimit = 0 OR RTItem.SizeLimit > piData
                     THEN liLimit = piData. 
                  ELSE liLimit = RTItem.SizeLimit.
               END.
               
               WHEN "KB" THEN DO:
                  liSize     = RTItem.StepSize * 1024.
                  IF RTItem.SizeLimit = 0 OR RTItem.SizeLimit * 1024 > piData
                     THEN liLimit = piData.
                  ELSE liLimit = RTItem.SizeLimit * 1024.
               END. 
               
               WHEN "MB" THEN DO:
                  liSize    = RTItem.StepSize * 1048576.
                  IF RTItem.SizeLimit = 0 OR RTItem.SizeLimit * 1048576 > piData
                     THEN liLimit = piData.
                  ELSE liLimit = RTItem.SizeLimit * 1048576.
               END.   

            END.

            liLoopLimit = liLimit - (piData - liData).
            liLoop = liSize.
            
            DO WHILE liLoop <= liLoopLimit:
               ASSIGN
                  liLoop = liSize + liLoop
                  ldeTariff = ldeTariff + RTItem.Tariff.
            END.
            
            IF liLoopLimit MOD liSize NE 0 THEN DO:
               ldeTariff = ldeTariff + RTItem.Tariff.
            END.
            
            liData = piData - liLimit.
            IF liData <= 0 THEN LEAVE.
            
         END.

      END.

   END.
   
   /* ad-hoc discount */
   lrRecId = fFindTariff(pdaDate, 1, piRoamType, "GPRS", pcPLMN, pcGroup).
   IF lrRecId NE ? THEN DO:
   
      FIND FIRST RoamTariff WHERE
           RECID(RoamTariff) = lrRecId
      NO-LOCK NO-ERROR.
      ldeTariff = ldeTariff * (1 - RoamTariff.Tariff / 100).
   
   END.
   
   RETURN ldeTariff.

END FUNCTION.

FUNCTION fRoamTariff RETURNS DECIMAL
  (INPUT  plNew   AS  LOGICAL,
   BUFFER bufCDR  FOR ttRoamCDR,
   BUFFER bufGPRS FOR ttRoamGPRS):

   DEFINE VARIABLE ldeTariff  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liUnits    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lhField    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcCDR      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhTable    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTemp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTime     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE lcCLI      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBDest    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llEmerg    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcIMSI     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPLMN     AS CHARACTER NO-UNDO INIT "N/A".
   DEFINE VARIABLE lcGroup    AS CHARACTER NO-UNDO INIT "N/A".
   DEFINE VARIABLE liRoamType AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcVersion  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liIdx AS INTEGER   NO-UNDO.
   DEF VAR llCountryZone AS LOG NO-UNDO. 


   IF AVAIL bufCDR  THEN ASSIGN
      lcCDR   = bufCDR.CSV
      lhTable = BUFFER bufCDR:HANDLE.
   ELSE IF AVAIL bufGPRS THEN ASSIGN
      lcCDR   = bufGPRS.CSV
      lhTable = BUFFER bufGPRS:HANDLE.
   
   lhField = lhTable:BUFFER-FIELD("Version").
   IF plNew THEN DO:
      lcVersion = ENTRY(5,lcCDR,"|").
      lhField:BUFFER-VALUE = lcVersion.
   END.
   ELSE
      lcVersion = lhField:BUFFER-VALUE.

   fRoamCSV(lcVersion).

   ASSIGN
      llEmerg    = ENTRY(ttRoamCSV.EventSub,lcCDR,"|") = "EMERG"
      liRoamType = INT(ENTRY(ttRoamCSV.Charged,lcCDR,"|")).
   ASSIGN   
      lcIMSI     = ENTRY(ttRoamCSV.OrigIMSI,lcCDR,"|") WHEN liRoamType = 1  /*org */
      lcIMSI     = ENTRY(ttRoamCSV.DestIMSI,lcCDR,"|") WHEN liRoamType = 2. /*term*/
   
   DO liLoop = 6 TO 5 BY -1:
      
      FIND FIRST RoamOper WHERE
                 RoamOper.IMSI = SUBSTR(lcIMSI,1,liLoop)
      NO-LOCK NO-ERROR.

      IF AVAIL RoamOper THEN ASSIGN
         lcPLMN  = RoamOper.PLMN
         lcGroup = RoamOper.RoamGroup
         liLoop  = 0.
         
   END.

   /* new CDR, split information */
   IF plNew THEN DO:

      ASSIGN
         lcTemp  = ENTRY(ttRoamCSV.StartDate,lcCDR,"|")
         lcTemp  = SUBSTR(lcTemp,7,2) + "-" + 
                   SUBSTR(lcTemp,5,2) + "-" +
                   SUBSTR(lcTemp,1,4)
         ldeDate = DATE(lcTemp)
         lcTemp  = ENTRY(ttRoamCSV.StartTime,lcCDR,"|")
         liTime  = INT(SUBSTR(lcTemp,1,2)) * 3600 +
                   INT(SUBSTR(lcTemp,3,2)) * 60   +
                   INT(SUBSTR(lcTemp,5,2))
         lcCLI   = ENTRY(ttRoamCSV.OrigAddr,lcCDR,"|")
         lcBDest = ENTRY(ttRoamCSV.DestAddr,lcCDR,"|")
         liUnits = INT(ENTRY(ttRoamCSV.Duration,lcCDR,"|")).
   
      DO liLoop = 1 TO lhTable:NUM-FIELDS:
         
         lhField = lhTable:BUFFER-FIELD(liLoop).
         
         CASE lhField:NAME:
      
            /* common fields */
            WHEN "TSRead"       THEN lhField:BUFFER-VALUE = fMakeTS().
            WHEN "DateRead"     THEN lhField:BUFFER-VALUE = TODAY.
            WHEN "DateStart"    THEN lhField:BUFFER-VALUE = ldeDate.
            WHEN "TimeStart"    THEN lhField:BUFFER-VALUE = liTime.
            WHEN "Units"        THEN lhField:BUFFER-VALUE = liUnits. 
            WHEN "EventType"    THEN lhField:BUFFER-VALUE = 
                                   ENTRY(ttRoamCSV.EventType,lcCDR,"|").
            WHEN "ChargedParty" THEN lhField:BUFFER-VALUE =
                                   INT(ENTRY(ttRoamCSV.Charged,lcCDR,"|")).
            WHEN "CLI"          THEN lhField:BUFFER-VALUE = lcCLI.
            WHEN "GsmBnr"       THEN lhField:BUFFER-VALUE = lcBDest.
            WHEN "PLMN"         THEN lhField:BUFFER-VALUE = lcPLMN.

            /* GPRS fields */
            WHEN "CallIdNum"    THEN lhField:BUFFER-VALUE = 
                                    INT64(ENTRY(ttRoamCSV.CallId,lcCDR,"|")).
            WHEN "PartInd"      THEN lhField:BUFFER-VALUE = 
                                    INT(ENTRY(ttRoamCSV.PartInd,lcCDR,"|")).
            WHEN "PartRecNum"   THEN lhField:BUFFER-VALUE = 
                                    INT(ENTRY(ttRoamCSV.PartRecNum,lcCDR,"|")).        
            WHEN "GGSNAddr"     THEN lhField:BUFFER-VALUE = 
                                    ENTRY(ttRoamCSV.SGSN,lcCDR,"|").
         
         END CASE.

      END.

      IF lhTable:BUFFER-FIELD("EventType"):BUFFER-VALUE EQ "GPRS" THEN
         ASSIGN
            liUnits = INT(ENTRY(ttRoamCSV.DataIn,lcCDR,"|")) + 
                      INT(ENTRY(ttRoamCSV.DataOut,lcCDR,"|"))
            lhField = lhTable:BUFFER-FIELD("Units")
            lhField:BUFFER-VALUE = liUnits.

   END.
   ELSE ASSIGN
      lhField = lhTable:BUFFER-FIELD("DateStart")
      ldeDate = lhField:BUFFER-VALUE
      lhField = lhTable:BUFFER-FIELD("TimeStart")
      liTime  = lhField:BUFFER-VALUE
      lhField = lhTable:BUFFER-FIELD("CLI")
      lcCLI   = lhField:BUFFER-VALUE
      lhField = lhTable:BUFFER-FIELD("GsmBnr")
      lcBDest = lhField:BUFFER-VALUE
      lhField = lhTable:BUFFER-FIELD("Units")
      liUnits = lhField:BUFFER-VALUE.
  
   lhField = lhTable:BUFFER-FIELD("EventType").
   IF NOT llEmerg THEN CASE STRING(lhField:BUFFER-VALUE):
      WHEN "CALL" THEN DO:

         IF ENTRY(ttRoamCSV.CamelServiceKey,lcCDR,"|") NE "" THEN DO:
            lcBDest = ENTRY(ttRoamCSV.CamelDestNumber,lcCDR,"|").
         END.

         IF liRoamType = 2 THEN DO:
            ldeTariff = fVoice(ldeDate,liUnits,liRoamType,lcPLMN,lcGroup).
         END.
         ELSE DO:
            IF lcBDest BEGINS "34" THEN DO: 
               llCountryZone = FALSE.
               DO liIdx = 5 TO 3 BY -1:
                  IF CAN-FIND(FIRST RoamCountry WHERE
                     RoamCountry.Prefix = SUBSTR(lcBDest,1,liIdx)) THEN DO:
                     llCountryZone = TRUE.
                     LEAVE.
                  END.
               END.
            END.
            ELSE llCountryZone = TRUE.

            IF llCountryZone THEN 
               ldeTariff = fCountryZone(ldeDate,liUnits,liRoamType,lcBDest,
                                        lcPLMN,lcGroup).
            ELSE
               ldeTariff = fVoice(ldeDate,liUnits,liRoamType,lcPLMN,lcGroup).
         END.
      END.
      WHEN "SMS"  THEN ldeTariff = fSMS(ldeDate,liRoamType,lcPLMN,lcGroup).
      WHEN "GPRS" THEN ldeTariff = fGPRS(ldeDate,liUnits,liRoamType,lcPLMN,lcGroup).
   END CASE.
   ELSE ldeTariff = 0.0.

   ASSIGN
      lhField = lhTable:BUFFER-FIELD("Amount")
      lhField:BUFFER-VALUE = ldeTariff.

   RETURN ldeTariff.

END FUNCTION.

&ENDIF
