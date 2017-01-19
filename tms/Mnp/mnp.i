/* ----------------------------------------------------------------------
  MODULE .......: mnp.i
  TASK .........: MNP related functions
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.11.06
  CHANGED ......: 03.09.07 kl pcPortCode for fSendACK

  Version ......: xfera
  ---------------------------------------------------------------------- */

&IF "{&MNP_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MNP_I YES

{commali.i}
{timestamp.i}
{fgettxt.i}
{tmsconst.i}
{fixedlinefunc.i}
{orderfunc.i}
{orderchk.i}

FUNCTION fMNPCallAlarm RETURNS LOGICAL
  (INPUT pcAction  AS CHARACTER,
   INPUT pdeActTS  AS DECIMAL,
   INPUT pcFormReq AS CHARACTER,
   INPUT pcCLI     AS CHARACTER,
   INPUT piCustNum AS INTEGER,
   INPUT piLang    AS INTEGER,
   INPUT pcSender  AS CHARACTER,
   INPUT piOrderId AS INTEGER):

   DEFINE VARIABLE ldeActStamp  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcAlarmMess  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDate       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTime       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaDate      AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime       AS INTEGER   NO-UNDO.

   IF pcCLI = "" THEN RETURN TRUE.

   /* others don't exist yet except 1 and 5 ! */
   IF piLang NE 5 THEN piLang = 1.
   
   ASSIGN
      lcAlarmMess = fGetSMSTxt(pcAction, TODAY, piLang, OUTPUT ldeActStamp)
      lcAlarmMess = REPLACE(lcAlarmMess,"#CLI",pcCli).

   IF NOT lcAlarmMess > "" THEN RETURN FALSE.
   
   fSplitTS(pdeActTS,ldaDate,liTime).
            
   CASE pcAction:
      WHEN "MNPConfTime" THEN DO:
         ASSIGN
            lcDate      = STRING(DAY(ldaDate),"99") + "/" +
                          STRING(MONTH(ldaDate),"99")
            lcAlarmMess = REPLACE(lcAlarmMess,"#PORTDATE",lcDate)
            lcAlarmMess = REPLACE(lcAlarmMess,"#MNPID",pcFormReq).
      END.
      WHEN "MNPConf" THEN DO:
         ASSIGN
            lcDate      = STRING(DAY(ldaDate),"99") + "/" +
                          STRING(MONTH(ldaDate),"99")
            lcAlarmMess = REPLACE(lcAlarmMess,"#PORTDATE",lcDate).
      END.
      WHEN "MNPReject" OR WHEN
           "MNPIdentDirect" OR WHEN
           "MNPIdentPos" THEN ASSIGN
              lcAlarmMess = REPLACE(lcAlarmMess,"#MNPID",pcFormReq).

      WHEN "MNPFInRemPos" OR WHEN
           "MNPFinRemDirect" THEN DO:
         ASSIGN
            ldaDate     = ldaDate - 1
            ldeActStamp = fHMS2TS(ldaDate,"09:00:00").
      END.
      WHEN "MNPCancelRetention" OR WHEN
           "MNPCancelRetentionOnHold" THEN DO:
         ASSIGN
            lcAlarmMess = REPLACE(lcAlarmMess,"#ORDER_NUMBER",STRING(piOrderId)).
      END.
   END.
   
   CREATE CallAlarm.
   ASSIGN
      CallAlarm.ActStamp   = ldeActStamp
      CallAlarm.CLSeq      = 0
      CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
      CallAlarm.CustNo     = piCustNum
      CallAlarm.CLI        = pcCLI
      CallAlarm.DeliStat   = 1
      CallAlarm.Delitype   = 1
      CallAlarm.DeliPara   = "1"
      CallAlarm.DeliMsg    = lcAlarmMess
      CallAlarm.Limit      = 0
      CallAlarm.CreditType = 12
      CallAlarm.Orig       = pcSender
      CallAlarm.Brand      = gcBrand.
      
   RELEASE CallAlarm.

   RETURN TRUE.

END FUNCTION.

FUNCTION fMNPMessage RETURNS LOGICAL
  (INPUT piMNPSeq   AS INTEGER,
   INPUT pcXML      AS CHARACTER,
   INPUT lcType     AS CHARACTER):

   CREATE MNPMessage.

   ASSIGN
      MNPMessage.CreatedTS   = fMakeTS()
      MNPMessage.MNPSeq      = piMNPSeq
      MNPMessage.Sender      = 1
      MNPMessage.XMLMessage  = pcXML
      MNPMessage.StatusCode  = 1
      MNPMessage.MessageType = lcType.
         
   RETURN TRUE.      

END.

FUNCTION fSendACK RETURNS LOGICAL
   (INPUT piMNPSeq   AS INTEGER,
    INPUT pcFormCode AS CHARACTER,
    INPUT pcPortCode AS CHARACTER,
    INPUT pcError    AS CHARACTER,
    INPUT pcAckType  AS CHARACTER):

   DEFINE VARIABLE lhSAX AS HANDLE NO-UNDO.
   DEFINE VARIABLE lmXML AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lcXML AS CHAR   NO-UNDO. 
  
   CREATE SAX-WRITER lhSAX.

   lhSAX:FORMATTED  = FALSE. 
   lhSAX:ENCODING   = "UTF-8".
   lhSAX:STANDALONE = TRUE.
   lhSAX:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   lhSAX:START-DOCUMENT().
   lhSAX:START-ELEMENT("ns2:confirmACK"). 
   lhSAX:INSERT-ATTRIBUTE("xmlns:ns2",
                          "http://www.example.org/confirmACK").

   lhSAX:WRITE-DATA-ELEMENT("portabilityRequestCode",pcPortCode).
   lhSAX:WRITE-DATA-ELEMENT("formRequestCode",pcFormCode).
   
   IF pcError = "" OR pcError = "OK" THEN 
      lhSAX:WRITE-DATA-ELEMENT("errorCode","0").
   ELSE DO:
      lhSAX:WRITE-DATA-ELEMENT("errorCode","1").
      lhSAX:WRITE-DATA-ELEMENT("errorDescription",pcError).
   END.
   lhSAX:END-ELEMENT("ns2:confirmACK").
   lhSAX:END-DOCUMENT().

   lcXML = GET-STRING(lmXML,1).

   DELETE OBJECT lhSAX.
   SET-SIZE(lmXML) = 0.

   fMNPMessage(piMNPSeq, lcXML, "confirmACK," + pcAckType).

   RETURN TRUE.
END.

FUNCTION fSendCancel RETURNS LOGICAL
  (INPUT piMNPSeq  AS INTEGER, 
   INPUT pcFormReq AS CHARACTER,
   INPUT pcPortReq AS CHARACTER,
   INPUT pcReason  AS CHARACTER,
   INPUT pcComment AS CHARACTER):
   

   DEFINE VARIABLE lhSAX AS HANDLE NO-UNDO.
   DEFINE VARIABLE lmXML AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lcXML AS CHAR   NO-UNDO.

   CREATE SAX-WRITER lhSAX.

   lhSAX:FORMATTED  = FALSE. 
   lhSAX:ENCODING   = "UTF-8".
   lhSAX:STANDALONE = TRUE.
   lhSAX:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   lhSAX:START-DOCUMENT().
   lhSAX:START-ELEMENT("ns2:portabilityCancel").
   lhSAX:INSERT-ATTRIBUTE("xmlns:ns2",
                          "http://www.example.org/cancelacionPortabilidad").
   lhSAX:WRITE-DATA-ELEMENT("formRequestCode",pcFormReq).
   lhSAX:WRITE-DATA-ELEMENT("portabilityRequestCode",pcPortReq).
   lhSAX:WRITE-DATA-ELEMENT("cancelReason",pcReason).
   IF pcComment NE "" THEN 
      lhSAX:WRITE-DATA-ELEMENT("Commentaries",pcComment).
   lhSAX:END-ELEMENT("ns2:portabilityCancel").
   lhSAX:END-DOCUMENT().

   lcXML = GET-STRING(lmXML,1).
   
   DELETE OBJECT lhSAX.
   SET-SIZE(lmXML) = 0.

   fMNPMessage(piMNPSeq, lcXML, "portabilityCancel").

   RETURN TRUE.
END.

FUNCTION fMNPHoliday RETURNS DATE 
(idtDate AS DATE,
 ilForward AS LOGICAL):

   DEFINE VARIABLE liDir AS INTEGER NO-UNDO. 
   IF ilForward THEN liDir = 1. ELSE liDir = -1.

   DO WHILE TRUE:
      /* weekend */
      IF WEEKDAY(idtDate) = 7 OR
         WEEKDAY(idtDate) = 1
      THEN idtDate = idtDate + liDir.

      /* national holiday */
      ELSE IF CAN-FIND(FIRST NatHoliday WHERE
                             NatHoliday.Holiday = idtDate)
      THEN idtDate = idtDate + liDir.

      ELSE LEAVE.
   END.
   RETURN idtDate.

END FUNCTION. 
      
FUNCTION fMNPCreatedTS RETURNS DECIMAL
(iiMNPSeq AS INT):

   DEF VAR ldNewFrom  AS DEC  NO-UNDO.
   DEF VAR ldtNewDate AS DATE NO-UNDO.
   DEF VAR liNewTime  AS INT  NO-UNDO.
      
   ldNewFrom = fMakeTS().

   /* make sure that there is atleast 1 second gap between rows */
   REPEAT:
      IF MNPProcess.MNPType NE 0 THEN DO:
         IF NOT CAN-FIND(MNPOperation WHERE
                         MNPOperation.MNPSeq    = MNPProcess.MNPSeq   AND
                         MNPOperation.CreatedTS = ldNewFrom)
         THEN LEAVE.
      END.
      ELSE DO: 
         IF NOT CAN-FIND(MNPMessage WHERE
                         MNPMessage.MNPSeq    = MNPProcess.MNPSeq   AND
                         MNPMessage.CreatedTS = ldNewFrom)
         THEN LEAVE.
      END.
      fSplitTS(ldNewFrom,
               OUTPUT ldtNewDate,
               OUTPUT liNewTime).
      IF liNewTime >= 86400 THEN ASSIGN
         ldtNewDate = ldtNewDate + 1
         liNewTime  = 1.
      ELSE liNewTime = liNewTime + 1.
      ldNewFrom = fMake2Dt(ldtNewDate,liNewTime).
   END.

   RETURN ldNewFrom.

END.   

/* Count periods periods between times */
FUNCTION fMNPPeriods RETURNS INT
   (INPUT ideTimeFrom   AS DEC,
    INPUT ideTimeTo     AS DEC,
    INPUT iiMaxPeriods  AS INT,
    OUTPUT odaDueDate    AS DATE).
   
   DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
   DEFINE VARIABLE ldeTime AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldeNowTime AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE ldaNowdate AS DATE NO-UNDO.

   liPeriods = 0.

   fSplitTS(
   input ideTimeTo,
   output ldaNowDate,
   output ldeNowTime).

   fSplitTS(
      input ideTimeFrom,
      output ldaDate,
      output ldeTime).
   
   IF ldaDate > ldaNowDate THEN RETURN 0.
   
   odaDueDate = fMNPHoliday(ldaDate, TRUE).

   /* Check if Modify day and today are the same */
   /* 28800 = 08:00, 50400 = 14:00, 72000 = 20:00 */
   IF ldaDate EQ fMNPHoliday(ldaDate, TRUE) AND ldaDate = ldaNowDate THEN DO:
      IF ldeTime < 28800 AND ldeNowTime >= 50400
         THEN liPeriods = liPeriods + 1.
      IF ldeTime < 50400 AND ldeNowTime >= 72000 
         THEN liPeriods = liPeriods + 1.
      RETURN liPeriods.   
   END.
   /* Check 1st day */
   IF ldaDate = fMNPHoliday(ldaDate, TRUE) THEN DO:
      IF ldeTime < 28800 THEN liPeriods = liPeriods + 2.
      ELSE IF ldeTime < 50400 THEN liPeriods = liPeriods + 1.
   END.
   
   IF liPeriods <= iiMaxPeriods THEN odaDueDate = ldaDate.
   ldaDate = fMNPHoliday(ldaDate + 1, TRUE).
   
   /* Count full days */
   DO WHILE ldaDate < ldaNowdate:
      liPeriods = liPeriods + 2.
      IF liPeriods <= iiMaxPeriods + 1 THEN odaDueDate = ldaDate. 
      ldaDate = fMNPHoliday(ldaDate + 1, TRUE).
   END.
   
   /* Check last day */  
   IF ldaDate = ldaNowDate THEN DO:
      IF ldeNowTime >= 50400
         THEN DO: 
            liPeriods = liPeriods + 1.
            IF liPeriods <= iiMaxPeriods THEN odaDueDate = ldaDate. 
         END.   
      IF ldeNowTime >= 72000 THEN DO:
         liPeriods = liPeriods + 1.
         IF liPeriods <= iiMaxPeriods THEN odaDueDate = ldaDate. 
      END.
   END.

  RETURN liPeriods.

END FUNCTION.

FUNCTION fCalculateDueDate RETURNS DECIMAL
   (INPUT idePortingTime AS DEC,
    INPUT iiperiods AS int).
   
   DEFINE VARIABLE ldaDueDate AS DATE NO-UNDO.
   DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldaPortingDate AS DATE NO-UNDO. 
   DEFINE VARIABLE liPortingTime AS DECIMAL NO-UNDO.

   DEFINE VARIABLE liEndTime AS INTEGER NO-UNDO.
   DEFINE VARIABLE liDay AS INTEGER NO-UNDO. 

   IF iiperiods <= 0 THEN RETURN 0.

   fSplitTS(
      input idePortingTime,
      output ldaPortingDate,
      output liPortingTime).

   ldaDueDate = fMNPHoliday(ldaPortingDate, false).
   IF ldaDueDate NE ldaPortingDate THEN liDay = 1.

   DAY_LOOP:
   DO WHILE TRUE:

     liDay = liday + 1.

      /* 28800 = 08:00, 50400 = 14:00, 72000 = 20:00 */
      IF liDay > 1 OR liPortingTime > 72000 THEN liPeriods = liPeriods + 1.
      liEndTime = 50400.
      IF liPeriods = iiperiods then LEAVE DAY_LOOP.
      
      IF liDay > 1 OR liPortingTime > 50400 THEN liPeriods = liPeriods + 1.
      liEndTime = 28800.
      IF liPeriods = iiperiods then LEAVE DAY_LOOP.
      
      ldaDueDate = fMNPHoliday(ldaDueDate - 1, false).

   END.
   
   RETURN fMake2Dt(ldaDueDate,liEndTime).

END FUNCTION.

FUNCTION fMNPDueMessage RETURNS INT
   (INPUT ideTimeFrom   AS DEC,
    INPUT ideTimeTo     AS DEC):
   
   DEFINE VARIABLE liMaxPeriods AS INTEGER NO-UNDO INIT 0. 
   DEFINE VARIABLE ldaDueDate   AS DATE NO-UNDO INIT TODAY.

   RETURN fMNPPeriods(
     INPUT ideTimeFrom,
     INPUT ideTimeTo,
     INPUT liMaxPeriods,
     OUTPUT ldaDueDate).

END FUNCTION. 

def buffer bufMNPCal FOR MNPCal.
FUNCTION fFindMNPCal RETURNS INT
   (icMessageType AS CHAR,
    icRegion AS CHAR,
    icOrderChannel AS CHAR,
    icMNPProduct AS CHAR,
    icMNPTariff AS CHAR):

   icOrderChannel = REPLACE(icOrderChannel,"fusion_","").

   /* All */
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* Product empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Product & Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* General Region */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* General Region, Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* General Region, Product empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* General Region, Product & Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = icOrderChannel AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* Channel empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Channel empty & Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* Channel empty & Product empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Channel empty, Tariff & Product empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = icRegion AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* Channel empty, General Region */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Channel empty, General Region, Tariff empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = icMNPProduct AND
      bufMNPCal.MNPTariff = "" NO-LOCK NO-ERROR.

   /* Channel empty, General Region Product empty */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType AND
      bufMNPCal.MNPProduct = "" AND
      bufMNPCal.MNPTariff = icMNPTariff NO-LOCK NO-ERROR.

   /* Use default value if calendar is not found */
   IF NOT AVAIL bufMNPCal THEN
   FIND FIRST bufMNPCal WHERE
      bufMNPCal.OrderChannel = "" AND
      bufMNPCal.Region = "99" AND
      bufMNPCal.MessageType = icMessageType NO-LOCK NO-ERROR.

END FUNCTION.

FUNCTION fGetMinMNPWindow RETURNS INTEGER:
  
  DEF VAR liPeriods AS INT NO-UNDO.

   FIND FIRST TMSParam WHERE
      TMSParam.Brand = "1" AND
      TMSParam.ParamCode = "MNPMinWindow" AND
      TMSParam.ParamGroup = "MNP" NO-LOCK NO-ERROR.
   
   IF AVAIL TMSParam THEN RETURN TMSParam.IntVal.

   RETURN 10.

END FUNCTION. 

FUNCTION fMNPChangeWindowDate RETURNS DATE (
   ideOrderStamp  AS DECIMAL,
   icOrderChannel AS CHARACTER,
   icRegion       AS CHARACTER,
   icProduct      AS CHARACTER,
   icTariff       AS CHARACTER):

   DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO INIT 0. 
   DEFINE VARIABLE ldDueDate AS DATE NO-UNDO.
   DEFINE VARIABLE liMinPeriodSum AS INT NO-UNDO.
   DEFINE VARIABLE liOrderTime AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO. 

   fFindMNPCal("ASOL", icRegion, icOrderChannel,icProduct,icTariff).    
   liPeriods = liPeriods + bufMNPCal.Periods.
   fFindMNPCal("ACON", icRegion, icOrderChannel,icProduct,icTariff).    
   liPeriods = liPeriods + bufMNPCal.Periods.
   fFindMNPCal("APOR", icRegion, icOrderChannel,icProduct,icTariff).    
   liPeriods = liPeriods + bufMNPCal.Periods.

   liMinPeriodSum  = fGetMinMNPWindow().

   fSplitTS(ideOrderStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

   IF liPeriods < liMinPeriodSum THEN
      liPeriods = liMinPeriodSum. 
  
   /* YBU-1857 */ 
   IF fMNPHoliday(ldaOrderDate, TRUE) NE ldaOrderDate THEN
      liPeriods = liPeriods + 1.
   ELSE IF LOOKUP(icOrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
      (liOrderTime > 70800 OR liOrderTime < 28800) THEN /* 19:40 - 08:00 */
      liPeriods = liPeriods + 1.
   
   fMNPPeriods(ideOrderStamp, 20301231.00000, liPeriods, OUTPUT ldDueDate).

   RETURN ldDueDate.

END.

   DEFINE TEMP-TABLE ttRegion
   FIELD region AS CHAR 
   INDEX region IS PRIMARY UNIQUE region. 

def buffer bMNPCal FOR MNPCal.

FUNCTION fMNPTotalPeriods RETURNS INT (
   icMessageType  AS CHARACTER,
   iiPeriods      AS INTEGER,
   icOrderChannel AS CHARACTER,
   icRegion       AS CHARACTER,
   icProduct      AS CHARACTER,
   icTariff       AS CHARACTER):

   DEFINE VARIABLE ldDueDate AS DATE NO-UNDO.
   DEFINE VARIABLE lcMessages AS CHARACTER NO-UNDO INIT "ASOL,ACON,APOR".
   DEFINE VARIABLE lcMessage AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liMaxPeriods AS INTEGER NO-UNDO INIT 100.
   DEFINE VARIABLE liCounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 

   IF icRegion = "99" OR 
      icOrderChannel = "" THEN DO:
      
      FOR EACH bMNPCal NO-LOCK USE-INDEX Region:

         liCounter = iiPeriods.
         DO i = 1 TO NUM-ENTRIES(lcMessages):
            lcMessage = ENTRY(i,lcMessages).
            
            IF lcMessage EQ icMessageType THEN NEXT.
            
            IF icRegion = "99" AND icOrderChannel NE "" THEN DO:
               fFindMNPCal(lcMessage, bMNPCal.Region, icOrderChannel,
               icProduct,icTariff).    
            END.
            ELSE IF icRegion NE "99" AND icOrderChannel EQ "" THEN DO: 
               fFindMNPCal(lcMessage, icRegion, bMNPCal.OrderChannel,
               icProduct,icTariff).
            END.   
            ELSE DO:
               fFindMNPCal(lcMessage, bMNPCal.Region, bMNPCal.OrderChannel,
               icProduct,icTariff).    
            END.
            liCounter= liCounter + bufMNPCal.Periods.
         END.
         IF liCounter < liMaxPeriods THEN
            liMaxPeriods = liCounter.
      
      END.
      
   END.
   
   ELSE DO:
      
      DO i = 1 TO NUM-ENTRIES(lcMessages):
         lcMessage = ENTRY(i,lcMessages).
         IF lcMessage EQ icMessageType THEN NEXT.
         fFindMNPCal(lcMessage, icRegion, icOrderChannel,
               icProduct,icTariff).    
         iiPeriods = iiPeriods + bufMNPCal.Periods.
      END.
      liMaxPeriods = iiPeriods.
   END.

   RETURN liMaxPeriods.

END.

FUNCTION fIsNCTime RETURNS LOGICAL:
   
   DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
   ASSIGN
      ldeNow = fMakeTS()
      ldeNow = ldeNow - INT(ldeNow - 0.4).

   /* outside 8:10 - 19:50, inside 14:00 - 14:15, weekend or holiday */ 
   IF ldeNow < 0.29400 OR ldeNow > 0.71400 OR 
      fMNPHoliday(TODAY, TRUE) NE TODAY THEN RETURN FALSE.
   
   RETURN TRUE.

END FUNCTION. 

FUNCTION fIsNCSendTime RETURNS LOGICAL:
   
   DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
   ASSIGN
      ldeNow = fMakeTS()
      ldeNow = ldeNow - INT(ldeNow - 0.4).

   /* inside 14:00 - 14:15 */ 
   IF ldeNow >= 0.50400 AND ldeNow <= 0.51300 THEN RETURN FALSE.

   RETURN fIsNCTime().

END FUNCTION. 

FUNCTION fIsMNPTermOngoing RETURNS LOGICAL (INPUT icCLI AS CHARACTER):

   FOR EACH MNPSub WHERE
            MNPSub.CLI = icCLI NO-LOCK,
       EACH MNPProcess WHERE
            MNPProcess.MNPSeq = MNPSub.MNPSeq AND
            MNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
            LOOKUP(STRING(MNPProcess.StatusCode),"0,20,23") > 0 NO-LOCK:
      RETURN True.
   END. /* FOR EACH MNPSub WHERE */
   RETURN False.

END FUNCTION. /* FUNCTION fIsMNPTermOngoing RETURNS LOGICAL */

FUNCTION fRetention RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
   INPUT iiLang AS INT,
   INPUT iiCustNum AS INT,
   INPUT icFormRequest AS CHAR,
   INPUT icCLI AS CHAR):
   DEF BUFFER bOrder FOR Order.
   DEF VAR lcMNPSMSText       AS CHAR  NO-UNDO.

   FIND FIRST bOrder WHERE
              bOrder.MsSeq EQ iiMsSeq AND
              bOrder.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION}
        NO-LOCK NO-ERROR.
   IF AVAIL bOrder THEN DO:
      IF fIsConvergenceTariff(bOrder.CliType) EQ TRUE AND
         bOrder.Ordertype NE {&ORDER_TYPE_RENEWAL} THEN
         RUN orderinctrl.p(bOrder.OrderId, 0, TRUE).
      ELSE DO:
         FIND FIRST OrderCustomer WHERE
                    OrderCustomer.Brand   = gcBrand AND
                    OrderCustomer.OrderId = bOrder.OrderId AND
                    OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
         IF AVAIL OrderCustomer THEN DO:
            IF bOrder.OrderChannel = "retention_stc" THEN DO:
               lcMNPSMSText = "MNPCancelRetention".
               IF LOOKUP(OrderCustomer.CustIdType,"CIF,CInternal,CFraud") > 0 THEN
                  fSetOrderStatus(bOrder.OrderId,
                                  {&ORDER_STATUS_RENEWAL_STC_COMPANY}).
               ELSE
                  fSetOrderStatus(bOrder.OrderId,
                                  {&ORDER_STATUS_RENEWAL_STC}).
            END. /* IF bOrder.OrderChannel = "retention_stc" THEN DO: */
            ELSE DO:
               IF fCheckRenewalData() THEN DO:
                  lcMNPSMSText = "MNPCancelRetention".
                  fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_RENEWAL}).
               END. /* IF fCheckRenewalData() THEN DO: */
               ELSE DO:
                  lcMNPSMSText = "MNPCancelRetentionOnHold".
                  fSetOrderStatus(bOrder.OrderId,
                                  {&ORDER_STATUS_RENEWAL_HOLD}).
               END. /* ELSE DO: */
            END. /* ELSE DO: */
         END. /* IF AVAIL OrderCustomer AND */
      END. /*Convergent*/
      IF lcMNPSMSText > "" THEN DO:
            fMNPCallAlarm(lcMNPSMSText,
                          fMakeTS(),
                          icFormRequest,
                          icCLI,
                          iiCustnum,
                          iiLang,
                          "622",
                          bOrder.OrderId). 
      END. /* IF lcMNPSMSText > "" THEN DO: */
   END. /* IF AVAIL bOrder THEN DO: */
   RETURN TRUE.


END FUNCTION. /*fRetention*/   
    

&ENDIF
