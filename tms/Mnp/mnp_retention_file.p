/* ----------------------------------------------------------------------
  MODULE .......: mnp_retention_file.p
  TASK .........: Creates MNP retention files(s)
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.04.12
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Func/date.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/tmsconst.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/fgettxt.i}
{Func/fmakesms.i}
{Mnp/mnp.i}

DEF STREAM sExclude.

FUNCTION fGetPenalty RETURN DECIMAL
   (OUTPUT odaEndDate AS DATE):

  DEF VAR ldeCurrPen AS DEC NO-UNDO.
  DEF VAR lcPriceList AS CHAR NO-UNDO.
  DEF VAR ldePrice AS DEC NO-UNDO.

  CONTRACT_LOOP:
   FOR EACH DCCLI WHERE
            DCCLI.Brand = gcBrand AND
            DCCLI.DCEvent BEGINS "TERM" AND
            DCCLI.MsSeq = Mobsub.Msseq AND
            DCCLI.ValidTo >= TODAY NO-LOCK,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK:

      /* Count possible penalty fee for contract termination */
      lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                       MobSub.BillTarget,
                                       DayCampaign.TermFeeModel,
                                       TODAY).

      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand     = gcBrand       AND
                 FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                 FMItem.PriceList = lcPriceList AND
                 FMItem.FromDate <= TODAY     AND
                 FMItem.ToDate   >= TODAY NO-ERROR.

      IF AVAIL FMItem THEN ldePrice = FMItem.amount.

      IF DCCLI.Amount NE ? THEN ldePrice = DCCLI.Amount.

      /* calculate a factor for the fee (full / proportional) */
      ldeCurrPen = fCalculateFactor(DCCLI.ValidFrom,
                                    DCCLI.RenewalDate,
                                    DCCLI.ValidTo,
                                    DCCLI.ValidToOrig,
                                    TODAY,
                                    DayCampaign.TermFeeCalc).

      odaEndDate = DCCLI.validto.
      RETURN TRUNCATE(ldeCurrPen * ldePrice,0).
   END.

   RETURN 0.

END.

FUNCTION fCheckRetentionRule RETURN LOGICAL
   (BUFFER Mobsub FOR MobSub,
    BUFFER Segmentation FOR Segmentation,
    OUTPUT ocSMSText AS CHAR):

   DEF VAR ldePenalty AS DEC NO-UNDO. 
   DEF VAR ldaEndDate AS DATE NO-UNDO. 
      
   RULE_LOOP:
   FOR EACH MNPRetentionRule NO-LOCK WHERE
            MNPRetentionRule.Brand = gcBrand AND
            MNPRetentionRule.ToDate >= TODAY AND
            MNPRetentionRule.FromDate <= TODAY:

      IF MNPRetentionRule.SegmentCode > "" AND 
         Segmentation.SegmentOffer NE MNPRetentionRule.SegmentCode
         THEN NEXT RULE_LOOP.

      IF MNPRetentionRule.CLIType > "" AND
         NOT MobSub.CLIType BEGINS MNPRetentionRule.CLIType THEN
         NEXT RULE_LOOP.
      
      IF MobSub.PayType EQ {&MOBSUB_PAYTYPE_POSTPAID} AND
         (MNPRetentionRule.PenaltyLeft > 0 OR
          MNPRetentionRule.PenaltyMonthsLeft > 0) THEN DO:

         ldePenalty = fGetPenalty(output ldaEndDate).

         IF ldePenalty NE 0 THEN DO:

            IF MNPRetentionRule.PenaltyLeft > 0 AND
               ldePenalty > MNPRetentionRule.PenaltyLeft THEN DO:
               PUT STREAM sExclude UNFORMATTED
                  MobSub.CLI ";R5"
                  SKIP.
               NEXT RULE_LOOP.
            END.
            
            IF MNPRetentionRule.PenaltyMonthsLeft > 0 THEN DO:
               IF ((ldaEndDate - TODAY) / 30) > 
                  MNPRetentionRule.PenaltyMonthsLeft THEN  NEXT RULE_LOOP.
            END.
         END.
      END.
      
      IF MNPRetentionRule.ConsumptionAverage > 0 AND
         MNPRetentionRule.ConsumptionAverage > Segmentation.SegmentCons THEN DO:
         IF MNPRetentionRule.CLIType BEGINS "CONT" THEN
            PUT STREAM sExclude UNFORMATTED
               MobSub.CLI ";F1"
               SKIP.
         ELSE
            PUT STREAM sExclude UNFORMATTED
               MobSub.CLI ";F2"
               SKIP.
         NEXT RULE_LOOP.
      END.

      ocSMSText = MNPRetentionRule.SMSText.
      RETURN TRUE.
   END.

   RETURN FALSE.

END.

DEF STREAM sout.

DEFINE TEMP-TABLE ttData
   FIELD custnum LIKE MobSub.Custnum 
   FIELD msseq LIKE MobSub.MsSeq 
   FIELD mnpseq LIKE mnpprocess.mnpseq 
   FIELD smstext AS CHAR
   FIELD RetentionPlatform AS CHAR
INDEX RetentionPlatform RetentionPlatform 
INDEX custnum IS PRIMARY UNIQUE custnum mnpseq msseq.

DEF VAR i AS INT NO-UNDO. 
DEF VAR liPlatform AS INT NO-UNDO INIT 1. 
DEF VAR liPlatForms AS INT NO-UNDO. 
DEF VAR lcSMSSender AS CHAR EXTENT 100 NO-UNDO. 
DEF VAR liCasesPerPlatform AS INT EXTENT 100 NO-UNDO. 
DEF VAR lcRetentionPlatform AS CHAR EXTENT 100 NO-UNDO. 
DEF VAR lcRetentionPlatformName AS CHAR EXTENT 100 NO-UNDO. 
DEF VAR lcRetentionFile AS CHAR EXTENT 100 NO-UNDO. 
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcStatusCodes AS CHAR NO-UNDO INIT "2,5". 
DEF VAR lcRetentionSMSText AS CHAR NO-UNDO. 
DEF VAR liLoop AS INTEGER NO-UNDO. 
DEF VAR lcRootDir AS CHARACTER NO-UNDO. 
DEF VAR liOldOngoing AS INT NO-UNDO.
DEF VAR liExcludeOffset AS INT NO-UNDO.
DEF VAR lcRetExcludeFile AS CHAR NO-UNDO.

lcRootDir = fCParam("MNP","MNPRetention").
IF lcRootDir = ? OR lcRootDir EQ "" THEN DO:
   MESSAGE "Missing dump dir configuration".
   RETURN.
END.

IF NOT CAN-FIND(FIRST MNPRetPlatForm NO-LOCK WHERE
                      MNPRetPlatForm.Brand = gcBrand AND
                      MNPRetPlatForm.Todate >= TODAY AND
                      MNPRetPlatForm.FromDate <= TODAY AND
                      MNPRetPlatForm.Percentage > 0) THEN DO:
   MESSAGE "Missing retention platform configuration".
   RETURN.
END.

DEF BUFFER bMNPDetails FOR MNPDetails.
DEF BUFFER bMNPProcess FOR MNPProcess.
DEF BUFFER bMNPSub FOR MNPSub.

lcRetExcludeFile = lcRootDir + "/spool/" + "mnp_retention_exclude_" +
                   STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
                   "_" + STRING(TIME) + ".txt".

OUTPUT STREAM sExclude TO VALUE(lcRetExcludeFile).

DO liLoop = 1 TO NUM-ENTRIES(lcStatusCodes):

   MNP_LOOP:
   FOR EACH MNPProcess NO-LOCK WHERE
            MNPProcess.Brand = gcBrand AND
            MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
            MNPProcess.StatusCode = INT(ENTRY(liLoop,lcStatusCodes)),
      FIRST MNPDetails NO-LOCK WHERE
            MNPDetails.mnpseq = MNPProcess.mnpseq:

      IF mnpdetails.statuslimitts < fMakeTS() THEN NEXT MNP_LOOP.
      /* IF mnpdetails.custidtype EQ "CIF" THEN NEXT MNP_LOOP. 
      Commented out YOT-4095 */
   
      IF MNPProcess.StatusCode EQ {&MNP_ST_ASOL} AND NOT 
         (MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM_PROPOSAL} OR 
          MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM}) THEN NEXT.

      MNP_SUB_LOOP:
      FOR EACH mnpsub NO-LOCK WHERE
               mnpsub.mnpseq = mnpprocess.mnpseq,
         FIRST MobSub NO-LOCK WHERE
               MobSub.MsSeq = MNPSub.MsSeq:
         FIND FIRST Segmentation NO-LOCK WHERE
                    Segmentation.MsSeq = MNPSub.MsSeq NO-ERROR.
         IF AVAILABLE Segmentation THEN DO:

            IF MobSub.PayType = TRUE THEN
               liExcludeOffset = -720. /* Prepaid 30 days (YOT-4929) */
            ELSE
               liExcludeOffset = 0. /* -1440. Commented out YOT-4095 */ /* Postpaid 60 days */

            /* Exclude Prepaid/postpaid clients from generated retention file */
            FOR EACH bMNPSub NO-LOCK WHERE
                     bMNPSub.MsSeq = MobSub.MsSeq,
               FIRST bMNPProcess NO-LOCK WHERE
                     bMNPProcess.MNPSeq = bMNPSub.MNPSeq AND
                     bMNPProcess.MNPType = {&MNP_TYPE_OUT} AND
                     bMNPProcess.MNPSeq NE MNPProcess.MNPSeq.
               IF bMNPProcess.CreatedTS > fOffSetTS(liExcludeOffset) THEN DO:
                  PUT STREAM sExclude UNFORMATTED
                     MobSub.CLI ";R1"
                     SKIP.
                  NEXT MNP_SUB_LOOP.
               END.
            END.

            /* already sent */
            IF mnpsub.RetentionPlatform > "" THEN NEXT.

            /* YOT-2301 - Exclude all data subs. and segmentation code with SN */
            IF LOOKUP(MobSub.CLIType,"CONTRD,CONTD,TARJRD1") > 0 OR
               Segmentation.SegmentCode = "SN" AND
               MobSub.ActivationTS > fOffSetTS(liExcludeOffSet) THEN DO:
               PUT STREAM sExclude UNFORMATTED
                  MobSub.CLI ";R3"
                  SKIP.
               NEXT.
            END.
            /* YOT-4929, If customer created less than 1 month ago */

            IF NOT fCheckRetentionRule(BUFFER MobSub, BUFFER Segmentation, OUTPUT lcRetentionSMSText) THEN NEXT.

            /* YOT-4956 R6: If suscriber has not any invoices paid, subscription is excluded from Retention file */
            IF NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                                  Invoice.Brand   = gcBrand            AND
                                  Invoice.CustNum = MobSub.CustNum     AND
                                  Invoice.InvType = {&INV_TYPE_NORMAL} AND
                                  Invoice.PaymState = 2) THEN DO:
               PUT STREAM sExclude UNFORMATTED
                  MobSub.CLI ";R6"
                  SKIP.
               NEXT.
            END.

            FIND FIRST ttData NO-LOCK WHERE
                       ttData.custnum = MobSub.custnum AND 
                       ttData.msseq = MobSub.msseq AND 
                       ttData.mnpseq = mnpprocess.mnpseq NO-ERROR.
            IF AVAIL ttData THEN NEXT MNP_LOOP.

            CREATE ttData.
            ASSIGN
               ttData.custnum = MobSub.custnum
               ttData.MsSeq = MobSub.msseq
               ttData.mnpseq = mnpprocess.mnpseq
               ttData.smsText = lcRetentionSMSText
               i = i + 1.

            MNP_OTHER_LOOP:
            FOR EACH bMNPDetails NO-LOCK WHERE
                     bMNPDetails.CustId = MNPDetails.CustId AND
                     bMNPDetails.DonorCode = "005" AND
                     bMNPDetails.MNPSeq NE MNPDetails.MNPSeq USE-INDEX CustId,
               FIRST bMNPProcess NO-LOCK WHERE
                     bMNPProcess.MNPSeq = bMNPDetails.MNPSeq AND
                     bMNPProcess.MNPType = {&MNP_TYPE_OUT} AND
                     LOOKUP(STRING(bMNPProcess.StatusCode),"2,5") > 0 USE-INDEX MNPSeq,
                EACH bMNPSub NO-LOCK WHERE
                     bMNPSub.MNPSeq = bMNPProcess.MNPSeq AND
                     bMNPSub.RetentionPlatform > "":
               IF NOT CAN-FIND(FIRST MNPRetPlatForm NO-LOCK WHERE
                                     MNPRetPlatForm.Brand = gcBrand AND
                                     MNPRetPlatForm.RetentionPlatform = bMNPSub.RetentionPlatform AND
                                     MNPRetPlatForm.Percentage > 0 AND
                                     MNPRetPlatForm.Todate >= TODAY AND
                                     MNPRetPlatForm.FromDate <= TODAY) THEN NEXT.
               ttData.RetentionPlatform = bMNPSub.RetentionPlatform.
               liOldOngoing = liOldOngoing + 1.
               LEAVE MNP_OTHER_LOOP.
            END.

            IF NOT SESSION:BATCH THEN DO:
               IF i mod 10 = 0 then do:
                  disp i.
                  pause 0.
               end.  
            END.
         END.
         ELSE
            PUT STREAM sExclude UNFORMATTED
               MobSub.CLI ";R4"
               SKIP.
      END.
   END.
END.

FOR EACH MNPRetPlatForm NO-LOCK WHERE
         MNPRetPlatForm.Brand = gcBrand AND
         MNPRetPlatForm.Todate >= TODAY AND
         MNPRetPlatForm.FromDate <= TODAY AND
         MNPRetPlatForm.Percentage > 0:
   ASSIGN
      liPlatForms = liPlatForms + 1
      lcSMSSender[liPlatForms] = MNPRetPlatForm.SMSSender
      lcRetentionPlatform[liPlatForms] = MNPRetPlatForm.RetentionPlatform
      lcRetentionPlatformName[liPlatForms] = 
         REPLACE(MNPRetPlatForm.Name, " ", "_")
      liCasesPerPlatform[liPlatForms] = INT((i - liOldOngoing) * 
                                        (MNPRetPlatForm.Percentage / 100)).
END.

IF liPlatForms = 0 THEN RETURN.

RUN pFileDump(0).

DO liPlatform = 1 TO liPlatforms:
   RUN pFileDump(liPlatform).
   IF lcRetentionFile[liPlatform] > "" THEN
      fMove2TransDir(lcRetentionFile[liPlatform], "", lcRootDir + "/outgoing/").
END.

OUTPUT STREAM sExclude CLOSE.

IF lcRetExcludeFile > "" THEN
      fMove2TransDir(lcRetExcludeFile, "", lcRootDir + "/outgoing/").

PROCEDURE pFileDump:

   DEF INPUT PARAM iiPlatform AS INT NO-UNDO.

   DEF VAR liPlatForm AS INT NO-UNDO. 
   DEF VAR lcDate AS CHARACTER NO-UNDO. 
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR lcOperName AS CHAR NO-UNDO. 
   DEF VAR lcRetention AS CHAR NO-UNDO. 
   DEF VAR ldaDueDate AS DATE NO-UNDO. 
   DEF VAR liMaxPeriods AS INT NO-UNDO init 100. 
   DEF VAR liPeriods AS INT NO-UNDO. 

   lcDate = string(year(today),"9999") + 
            string(month(today),"99") +
            string(day(today),"99").
   
   i = 0.
      
   IF iiPlatForm EQ 0 THEN DO:
      liPlatform = 1.
      lcRetentionFile[liPlatForm] = lcRootDir + "/spool/" + 
         "mnp_retention_" + lcRetentionPlatformName[liPlatform] + "_" + 
         lcDate + "_" + STRING(TIME) + ".txt". 
      lcRetention = "".
      OUTPUT STREAM sout TO VALUE (lcRetentionFile[liPlatform]).
   END.
   ELSE DO:
      IF lcRetentionFile[iiPlatForm] = "" THEN ASSIGN
         lcRetentionFile[iiPlatForm] = lcRootDir + "/spool/" + 
            "mnp_retention_" + lcRetentionPlatformName[iiPlatform] + "_" + 
            lcDate + "_" + STRING(TIME) + ".txt". 
      lcRetention = lcRetentionPlatform[iiPlatform].
      OUTPUT STREAM sout TO VALUE (lcRetentionFile[iiPlatform]) APPEND.
      liPlatform = iiPlatform.
   END.

   FOR EACH ttData WHERE
      ttData.RetentionPlatform = lcRetention NO-LOCK,
      FIRST Customer NO-LOCK WHERE
            Customer.Custnum = ttData.Custnum,
      FIRST MobSub NO-LOCK WHERE
            MobSub.MsSeq = ttData.MsSeq,
      FIRST Segmentation NO-LOCK WHERE
            Segmentation.MsSeq = ttData.MsSeq,
      FIRST MNPProcess NO-LOCK WHERE
            MNPProcess.MNPSeq = ttData.MNPSeq
      BREAK BY ttData.Custnum:

      i = i + 1.

      FIND MNPOperator NO-LOCK WHERE 
           MNPOperator.Brand = gcBrand AND
           MNPOperator.OperCode = MNPProcess.OperCode
      NO-ERROR.
      
      IF AVAIL MNPOperator THEN lcOperName = MNPOperator.OperName.
      ELSE DO:
         FIND FIRST MNPOperator WHERE
                    MNPOperator.Brand = gcBrand AND
                    MNPOperator.OperCode = MNPProcess.OperCode
         NO-ERROR.
         IF AVAIL MNPOperator AND
                  MNPOperator.OperBrand > ""
         THEN lcOperName = MNPOperator.OperBrand.
         ELSE lcOperName = STRING(MNPProcess.OperCode).
      END.

      liPeriods = fMNPPeriods  
         (INPUT MNPProcess.CreatedTS,
          INPUT MNPProcess.PortingTime,
          INPUT liMaxPeriods,
          OUTPUT ldaDueDate).
      
      put stream sout unformatted 
         mobsub.cli "|"
         fts2hms(mnpprocess.CreatedTS) "|"
         fts2hms(mnpprocess.portingtime) "|"
         lcopername "|"
         mobsub.clitype "|"
         customer.firstname "|"
         customer.custname "|"
         customer.surname2 "|"
         customer.smsnumber "|"
         customer.phone "|"
         Segmentation.SegmentCode "|"
         (IF liPeriods <= 3 THEN "Y" ELSE "")
         skip.

      lcRetentionSMSText = fGetSMSTxt(ttData.SMSText,
                             TODAY,
                             Customer.Language,
                             OUTPUT ldeSMSStamp).

      IF lcRetentionSMSText > "" THEN DO:
         lcRetentionSMSText = REPLACE(lcRetentionSMSText,"#SENDER", lcSMSSender[liPlatform]).

         fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        {&SMSTYPE_MNP_RETENTION},
                        lcRetentionSMSText,
                        ldeSMSStamp,
                        lcSMSSender[liPlatform],
                        ""). 
      END.
      
      FIND FIRST mnpsub EXCLUSIVE-LOCK WHERE
                 mnpsub.mnpseq = mnpprocess.Mnpseq AND
                 Mnpsub.msseq = mobsub.msseq NO-ERROR.

      IF AVAIL mnpsub THEN DO:
         MNPSub.RetentionPlatform = lcRetentionPlatform[liPlatform].  
         RELEASE MNPSub.
      END.

      IF iiPlatform = 0 THEN DO:
         IF i >= liCasesPerPlatForm[liPlatform] AND 
            liPlatform < liPlatForms AND
            LAST-OF (ttData.Custnum) AND
            NOT LAST (ttData.Custnum) THEN DO:
            OUTPUT STREAM sout CLOSE.
            liPlatform = liPlatform + 1.
            lcRetentionFile[liPlatform] = lcRootDir + "/spool/" + 
               "mnp_retention_" + lcRetentionPlatformName[liPlatform] + "_" + 
               lcDate + "_" + STRING(TIME) + ".txt". 
            OUTPUT STREAM sout TO VALUE(lcRetentionFile[liPlatform]).
            i = 0.
         END.
      END.
   END.
   OUTPUT STREAM sout close.

END PROCEDURE. 



