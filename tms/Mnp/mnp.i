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

{Syst/commali.i}
{Func/fgettxt.i}
{Syst/tmsconst.i}
{Func/fixedlinefunc.i}
{Func/orderfunc.i}
{Func/orderchk.i}

FUNCTION fcheckSMSsendingtime RETURNS DECIMAL
(INPUT ideActTS        AS DECIMAL).

DEF VAR ldaButtonDate   AS DATE NO-UNDO.
DEF VAR liButtonSeconds AS INTEGER  NO-UNDO.
DEF VAR lIniSeconds     AS INTEGER   NO-UNDO.
DEF VAR lEndSeconds     AS INTEGER   NO-UNDO.
DEF VAR ldsendingstamp  AS DECIMAL   NO-UNDO.

   Func.Common:mSplitTS(ideActTS,ldaButtonDate,liButtonSeconds).   
   ldsendingstamp = ideActTS.
   
   /* ie. "41400-63000" Send between 11:30-17:30 YDR-2594 */
   ASSIGN /* 11:30-17:30 */
      lIniSeconds = 41400
      lEndSeconds = 63000.      

   /* If is too late, schedule to start next morning */
   IF (liButtonSeconds > lEndSeconds) THEN
   DO:
      ldaButtonDate = ADD-INTERVAL (ldaButtonDate, 1, "days").
      ldsendingstamp = Func.Common:mHMS2TS(ldaButtonDate, STRING(lIniSeconds,"hh:mm:ss")) .
   END.

   /* If is too early, schedule to start when window opens */
   IF (liButtonSeconds < lIniSeconds) THEN
   DO:
      ldsendingstamp = Func.Common:mHMS2TS(ldaButtonDate, STRING(lIniSeconds,"hh:mm:ss")) .
   END.
   RETURN ldsendingstamp.
   
END.

FUNCTION fGetSpecialDelTypes RETURNS CHARACTER:

   DEFINE BUFFER TMSCodes FOR TMSCodes.

   DEFINE VARIABLE lcTypes AS CHARACTER NO-UNDO.

   FOR EACH TMSCodes NO-LOCK WHERE
      TMSCodes.TableName = "MNPCal"       AND
      TMSCodes.FieldName = "DeliveryType":

      IF TMSCodes.CodeValue = "0"
      THEN NEXT.

      lcTypes = lcTypes + "," + TMSCodes.CodeValue.

   END.

   IF lcTypes > ""
   THEN RETURN SUBSTRING(lcTypes,2).

   RETURN "".

END FUNCTION.

DEFINE VARIABLE gcSpecialDelTypes AS CHARACTER NO-UNDO.
gcSpecialDelTypes = fGetSpecialDelTypes().

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
   
   Func.Common:mSplitTS(pdeActTS,ldaDate,liTime).
            
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
           "MNPIdentPos" THEN ASSIGN
              lcAlarmMess = REPLACE(lcAlarmMess,"#MNPID",pcFormReq).

      WHEN "MNPIdentDirect" THEN ASSIGN
              lcAlarmMess = REPLACE(lcAlarmMess,"#MNPID",pcFormReq)
              ldeActStamp = fcheckSMSsendingtime(ldeActStamp).

      WHEN "MNPIccidPos" THEN ASSIGN
              ldeActStamp = fcheckSMSsendingtime(ldeActStamp).

      WHEN "MNPFInRemPos" OR WHEN
           "MNPFinRemDirect" THEN DO:
         ASSIGN
            ldaDate     = ldaDate - 1
            ldeActStamp = Func.Common:mHMS2TS(ldaDate,"09:00:00").
      END.
      WHEN "MNPCancelRetention" OR WHEN
           "MNPCancelRetentionOnHold" THEN DO:
         ASSIGN
            lcAlarmMess = REPLACE(lcAlarmMess,"#ORDER_NUMBER",STRING(piOrderId)).
      END.
   END.
   
   /* YDR-2660 Note for Codes:
        pAction: "MNPIdentDirect" for "RECH_IDENT"
        pAction: "MNPIccIdPOS" for "RECH_ICCID" */
   IF Mm.MManMessage:mGetMessage("SMS", pcAction, pilang) = FALSE THEN
   DO:
      /* Existing behaviour. Message not enable for using Message Manager */
      CREATE CallAlarm.
      ASSIGN
         CallAlarm.ActStamp   = ldeActStamp
         CallAlarm.CLSeq      = 0
         CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
         CallAlarm.CustNo     = piCustNum
         CallAlarm.CLI        = pcCLI
         CallAlarm.DeliStat   = 1
         CallAlarm.Delitype   = 1
         CallAlarm.DeliPara   = IF LOOKUP(pcAction,"MNPIdentDirect,MNPReject,MNPEnumePOS,MNPIccidPOS") > 0 THEN pcFormReq ELSE "1" /* Previously "1" ; unused field */
         CallAlarm.DeliMsg    = lcAlarmMess
         CallAlarm.Limit      = 0
         CallAlarm.CreditType = {&SMSTYPE_MNP}
         CallAlarm.Orig       = pcSender
         CallAlarm.Brand      = Syst.Var:gcBrand.
      
      RELEASE CallAlarm.

      RETURN TRUE.
   END.
   ELSE 
   DO:
      /* New behaviour for YDR-2660: delay rejection SMS manage by Message Manager */
      IF INDEX(Mm.MManMessage:ParamKeyValue,"#CLI") > 0 THEN
         Mm.MManMessage:ParamKeyValue = REPLACE(Mm.MManMEssage:ParamKeyValue,"#CLI",pcCli).
      RETURN Mm.MManMessage:mCreateMMLogSMS(pcCLI).
   END.

END FUNCTION.

FUNCTION fMNPMessage RETURNS LOGICAL
  (INPUT piMNPSeq   AS INTEGER,
   INPUT pcXML      AS CHARACTER,
   INPUT lcType     AS CHARACTER):

   CREATE MNPMessage.

   ASSIGN
      MNPMessage.CreatedTS   = Func.Common:mMakeTS()
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
      
   ldNewFrom = Func.Common:mMakeTS().

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
      Func.Common:mSplitTS(ldNewFrom,
               OUTPUT ldtNewDate,
               OUTPUT liNewTime).
      IF liNewTime >= 86400 THEN ASSIGN
         ldtNewDate = ldtNewDate + 1
         liNewTime  = 1.
      ELSE liNewTime = liNewTime + 1.
      ldNewFrom = Func.Common:mMake2DT(ldtNewDate,liNewTime).
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

   Func.Common:mSplitTS(
   input ideTimeTo,
   output ldaNowDate,
   output ldeNowTime).

   Func.Common:mSplitTS(
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

   Func.Common:mSplitTS(
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
   
   RETURN Func.Common:mMake2DT(ldaDueDate,liEndTime).

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

FUNCTION fMNPCalQuery RETURNS INTEGER
   (icMessageType AS CHAR,
    icRegion AS CHAR,
    icOrderChannel AS CHAR,
    icMNPProduct AS CHAR,
    icMNPTariff AS CHAR,
    iiDelType AS INT):

   DEFINE BUFFER MNPCal FOR MNPCal.

   FOR FIRST MNPCal NO-LOCK WHERE
      MNPCal.OrderChannel = icOrderChannel AND
      MNPCal.Region       = icRegion       AND
      MNPCal.MessageType  = icMessageType  AND
      MNPCal.MNPProduct   = icMNPProduct   AND
      MNPCal.MNPTariff    = icMNPTariff    AND
      MNPCal.DeliveryType = iiDelType:

      RETURN MNPCal.Periods.

   END.

   RETURN ?.

END FUNCTION.

FUNCTION fFindMNPCal RETURNS INT
   (icMessageType AS CHAR,
    icRegion AS CHAR,
    icOrderChannel AS CHAR,
    icMNPProduct AS CHAR,
    icMNPTariff AS CHAR,
    iiDelType AS INT):

   DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO.

   icOrderChannel = REPLACE(icOrderChannel,"fusion_","").

   /* All */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, icOrderChannel, icMNPProduct, icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, icOrderChannel, icMNPProduct, "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Product empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, icOrderChannel, "", icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Product & Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, icOrderChannel, "", "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region */
   liPeriods = fMNPCalQuery(icMessageType, "99", icOrderChannel, icMNPProduct, icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", icOrderChannel, icMNPProduct, "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Product empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", icOrderChannel, "", icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Product & Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", icOrderChannel, "", "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Channel empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, "", icMNPProduct, icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Channel empty & Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, "", icMNPProduct, "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Channel empty & Product empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, "", "", icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* Channel empty, Product & Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, icRegion, "", "", "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Channel empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", "", icMNPProduct, icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Channel empty & Tariff empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", "", icMNPProduct, "", iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Channel empty & Product empty */
   liPeriods = fMNPCalQuery(icMessageType, "99", "", "", icMNPTariff, iiDelType).
   IF liPeriods NE ? THEN RETURN liPeriods.

   /* General Region, Channel empty, Product & Tariff empty
      (use a default value if calendar is not found) */
   liPeriods = fMNPCalQuery(icMessageType, "99", "", "", "", iiDelType).

   RETURN liPeriods.

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
   icTariff       AS CHARACTER,
   iiDelType      AS INTEGER):

   DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO INIT 0. 
   DEFINE VARIABLE ldDueDate AS DATE NO-UNDO.
   DEFINE VARIABLE liMinPeriodSum AS INT NO-UNDO.
   DEFINE VARIABLE liOrderTime AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO.
   DEFINE VARIABLE liCalPeriods AS INTEGER NO-UNDO.
   DEFINE VARIABLE lii AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcTypes AS CHARACTER INITIAL "ASOL,ACON,APOR" NO-UNDO.

   DO lii = 1 TO NUM-ENTRIES(lcTypes):
      liCalPeriods = ?.
      IF LOOKUP(STRING(iiDelType),gcSpecialDelTypes) > 0
      THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcTypes), icRegion, icOrderChannel,icProduct,icTariff,iiDelType).
      IF liCalPeriods EQ ?
      THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcTypes), icRegion, icOrderChannel,icProduct,icTariff,0).

      IF liCalPeriods NE ? THEN liPeriods = liPeriods + liCalPeriods.
   END.

   liMinPeriodSum  = fGetMinMNPWindow().

   Func.Common:mSplitTS(ideOrderStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

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

FUNCTION fMNPTotalPeriods RETURNS INT (
   icMessageType  AS CHARACTER,
   iiPeriods      AS INTEGER,
   icOrderChannel AS CHARACTER,
   icRegion       AS CHARACTER,
   icProduct      AS CHARACTER,
   icTariff       AS CHARACTER,
   iiDelType      AS INTEGER):

   DEFINE VARIABLE lcAllMessages AS CHARACTER NO-UNDO INIT "ASOL,ACON,APOR".
   DEFINE VARIABLE lcMessages    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liMaxPeriods  AS INTEGER   NO-UNDO INIT 100.
   DEFINE VARIABLE liCounter     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lii           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liCalPeriods  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE llSpecialType AS LOGICAL   NO-UNDO.

   llSpecialType = LOOKUP(STRING(iiDelType),gcSpecialDelTypes) > 0.

   DO lii = 1 TO NUM-ENTRIES(lcAllMessages):
      IF ENTRY(lii,lcAllMessages) NE icMessageType
      THEN lcMessages = lcMessages + "," + ENTRY(lii,lcAllMessages).
   END.
   lcMessages = SUBSTRING(lcMessages,2).

   IF icRegion NE "99" AND icOrderChannel NE ""
   THEN DO:
      DO lii = 1 TO NUM-ENTRIES(lcMessages):
         liCalPeriods = ?.
         IF llSpecialType
         THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcMessages),
                                         icRegion,
                                         icOrderChannel,
                                         icProduct,
                                         icTariff,
                                         iiDelType).
         IF liCalPeriods EQ ?
         THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcMessages),
                                         icRegion,
                                         icOrderChannel,
                                         icProduct,
                                         icTariff,
                                         0).

         IF liCalPeriods NE ?
         THEN iiPeriods = iiPeriods + liCalPeriods.
      END.
      RETURN iiPeriods.
   END.

   DEFINE BUFFER MNPCal FOR MNPCal.

   DEFINE VARIABLE lhQuery AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llOK    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcQuery AS CHARACTER INITIAL "FOR EACH MNPCal NO-LOCK &1" NO-UNDO.

   IF icRegion = "99" AND icOrderChannel = ""
   THEN lcQuery = SUBSTITUTE(lcQuery, "USE-INDEX Region").
   ELSE IF icRegion EQ "99"
   THEN lcQuery = SUBSTITUTE(lcQuery, "WHERE OrderChannel = " + QUOTER(icOrderChannel)).
   ELSE lcQuery = SUBSTITUTE(lcQuery, "WHERE Region = " + QUOTER(icRegion)).
   
   CREATE QUERY lhQuery.

   lhQuery:SET-BUFFERS(BUFFER MNPCal:HANDLE).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().

   DO WHILE TRUE:

      llOK = lhQuery:GET-NEXT(NO-LOCK).

      /* Query handle is invalid, no more records, or query is not open */
      IF llOK = ? OR NOT llOK
      THEN LEAVE.

      liCounter = iiPeriods.
      DO lii = 1 TO NUM-ENTRIES(lcMessages):

         liCalPeriods = ?.
         IF llSpecialType
         THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcMessages),
                                         MNPCal.Region,
                                         MNPCal.OrderChannel,
                                         icProduct,
                                         icTariff,
                                         iiDelType).
         IF liCalPeriods EQ ?
         THEN liCalPeriods = fFindMNPCal(ENTRY(lii,lcMessages),
                                         MNPCal.Region,
                                         MNPCal.OrderChannel,
                                         icProduct,
                                         icTariff,
                                         0).

         IF liCalPeriods NE ?
         THEN liCounter = liCounter + liCalPeriods.
      END.

      liMaxPeriods = MINIMUM(liMaxPeriods, liCounter).
   END.

   lhQuery:QUERY-CLOSE().

   RETURN liMaxPeriods.

   FINALLY:
      IF VALID-HANDLE(lhQuery)
      THEN DO:
         lhQuery:QUERY-CLOSE().
         DELETE OBJECT lhQuery.
      END.
   END FINALLY.

END.

FUNCTION fIsNCTime RETURNS LOGICAL:
   
   DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
   ASSIGN
      ldeNow = Func.Common:mMakeTS()
      ldeNow = ldeNow - INT(ldeNow - 0.4).

   /* outside 8:10 - 19:50, inside 14:00 - 14:15, weekend or holiday */ 
   IF ldeNow < 0.29400 OR ldeNow > 0.71400 OR 
      fMNPHoliday(TODAY, TRUE) NE TODAY THEN RETURN FALSE.
   
   RETURN TRUE.

END FUNCTION. 

FUNCTION fIsNCSendTime RETURNS LOGICAL:
   
   DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
   ASSIGN
      ldeNow = Func.Common:mMakeTS()
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

   FOR EACH bOrder NO-LOCK WHERE
            bOrder.MsSeq EQ iiMsSeq AND
            bOrder.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION}:

      lcMNPSMSText = "".

      IF fIsConvergenceTariff(bOrder.CliType) EQ TRUE AND
         bOrder.Ordertype NE {&ORDER_TYPE_RENEWAL} THEN
         RUN Mc/orderinctrl.p(bOrder.OrderId, 0, TRUE).
      ELSE DO:
         FIND FIRST OrderCustomer WHERE
                    OrderCustomer.Brand   = Syst.Var:gcBrand AND
                    OrderCustomer.OrderId = bOrder.OrderId AND
                    OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
         IF AVAIL OrderCustomer THEN DO:
            IF bOrder.OrderChannel = "retention_stc" THEN DO:
               lcMNPSMSText = "MNPCancelRetention".
               IF OrderCustomer.CustIdType EQ "CIF" THEN
                  fSetOrderStatus(bOrder.OrderId,
                                  {&ORDER_STATUS_RENEWAL_STC_COMPANY}).
               ELSE
                  fSetOrderStatus(bOrder.OrderId,
                                  {&ORDER_STATUS_RENEWAL_STC}).
            END. /* IF bOrder.OrderChannel = "retention_stc" THEN DO: */
            ELSE DO:
               IF fCheckRenewalData(bOrder.Orderid) THEN DO:
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
                          Func.Common:mMakeTS(),
                          icFormRequest,
                          icCLI,
                          iiCustnum,
                          iiLang,
                          "622",
                          bOrder.OrderId). 
      END. /* IF lcMNPSMSText > "" THEN DO: */
   END. /* FOR EACH bOrder NO-LOCK WHERE: */
   RETURN TRUE.


END FUNCTION. /*fRetention*/   


FUNCTION fGetMNPOperatorName RETURNS CHAR
   (INPUT icOperCode AS CHAR):

   DEF BUFFER MNPOperator FOR MNPOperator.

   icOperCode = TRIM(icOperCode).
   IF icOperCode EQ "" OR icOperCode EQ ? THEN RETURN "".

   FIND MNPOperator NO-LOCK WHERE
        MNPOperator.Brand    = Syst.Var:gcBrand         AND
        MNPOperator.OperCode = icOperCode AND
        MNPOperator.Active   = TRUE NO-ERROR.
   
   IF NOT AVAIL MNPOperator THEN 
      FIND MNPOperator NO-LOCK WHERE
           MNPOperator.Brand    = Syst.Var:gcBrand         AND
           MNPOperator.OperCode = icOperCode NO-ERROR.
   IF AVAIL MNPOperator THEN RETURN MNPOperator.OperName.

   IF NOT AVAIL MNPOperator THEN
      FIND FIRST MNPOperator NO-LOCK WHERE
                 MNPOperator.Brand    = Syst.Var:gcBrand AND
                 MNPOperator.OperCode = icOperCode AND
                 MNPOperator.Active = TRUE  NO-ERROR.

   IF NOT AVAIL MNPOperator THEN 
      FIND FIRST MNPOperator NO-LOCK WHERE
                 MNPOperator.Brand    = Syst.Var:gcBrand AND
                 MNPOperator.OperCode = icOperCode NO-ERROR.
   
   IF AVAIL MNPOperator THEN DO:
      IF MNPOperator.OperBrand > "" THEN RETURN MNPOperator.OperBrand.
      ELSE RETURN MNPOperator.OperName.
   END.

   RETURN "".
END.

&ENDIF
