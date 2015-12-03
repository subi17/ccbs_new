&GLOBAL-DEFINE SIMULATERUN 0
&GLOBAL-DEFINE MODIFYDB 1

DEF TEMP-TABLE ttDayCampaign NO-UNDO LIKE DayCampaign.
DEF TEMP-TABLE ttFeeModel NO-UNDO LIKE FeeModel.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttFMItem NO-UNDO LIKE FMItem.
DEF TEMP-TABLE ttRepText NO-UNDO LIKE RepText.
DEF TEMP-TABLE ttDPTarget NO-UNDO LIKE DPTarget.
DEF TEMP-TABLE ttProgLimit NO-UNDO LIKE ProgLimit.
DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF TEMP-TABLE ttBDest NO-UNDO LIKE BDest.
DEF TEMP-TABLE ttDCServicePackage NO-UNDO LIKE DCServicePackage.
DEF TEMP-TABLE ttDCServiceComponent NO-UNDO LIKE DCServiceComponent.
DEF TEMP-TABLE ttServiceLimitGroup NO-UNDO LIKE ServiceLimitGroup.
DEF TEMP-TABLE ttServiceLimit NO-UNDO LIKE ServiceLimit.
DEF TEMP-TABLE ttRequestActionRule NO-UNDO LIKE RequestActionRule.
DEF TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEF TEMP-TABLE ttDiscountPlan NO-UNDO LIKE DiscountPlan.

FUNCTION fcreateRepText RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                           INPUT icDCEvent AS CHAR,
                                           INPUT icTransText AS CHAR,
                                           INPUT idaValidFrom AS DATE,
                                           INPUT iiLanguage AS INT,
                                           INPUT iiUpdateMode AS INT):
   FIND FIRST RepText WHERE
              RepText.LinkCode EQ icBaseDCEvent AND
              RepText.Language EQ iiLanguage AND
              RepText.ToDate > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL RepText THEN DO:
      MESSAGE "RepText not found / " + STRING(iiLanguage) VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttRepText.
   BUFFER-COPY RepText TO ttRepText.
   /*Set correct values to new entry*/
   ttRepText.LinkCode = icDCEvent.
   ttRepText.RepText = icTransText.
   ttRepText.FromDate = idaValidFrom.

   DISPLAY ttRepText with frame a.
   pause 0. 
   IF iiUpdateMode NE 0 THEN DO:
      CREATE RepText.
      BUFFER-COPY ttRepText TO RepText.
      DELETE ttRepText. /*for safety reasons*/
   END.
   IF AVAIL RepText THEN RELEASE RepText.
   RETURN TRUE.
END FUNCTION.


/*********************************************************/
/* Create BillItem item and monthlyfee (DATA7 and DATA7MF) */
FUNCTION fcreateBillItem RETURNS LOGICAL ( INPUT icBasebillcode AS CHAR,
                                           INPUT icbillcode AS CHAR,
                                           INPUT icBIName AS CHAR,
                                           INPUT iiUpdateMode AS INT):

   FIND FIRST BillItem WHERE
              BillItem.billcode EQ icBasebillcode NO-LOCK NO-ERROR.
   IF NOT AVAIL BillItem THEN DO:
      MESSAGE "BillItem not found:  " + icBasebillcode VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttBillItem.
   BUFFER-COPY BillItem TO ttBillItem.
   /*Set correct values to new entry*/
   ttBillItem.BillCode = icbillcode.
   ttBillItem.BIName = icBIName.

   DISPLAY ttBillItem with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE BillItem.
      BUFFER-COPY ttBillItem TO BillItem.
      DELETE ttBillItem. /*for safety reasons*/
   END.
   IF AVAIL BillItem THEN RELEASE BillItem.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create FeeModel */
FUNCTION fcreateFeeModel RETURNS LOGICAL ( INPUT icBaseMFFeeModel AS CHAR,
                                           INPUT icMFFeeModel AS CHAR,
                                           INPUT icFeeName  AS CHAR,
                                           INPUT iiUpdateMode AS INT):
   FIND FIRST FeeModel WHERE
              Feemodel.feeModel EQ icBaseMFFeeModel NO-LOCK NO-ERROR.
   IF NOT AVAIL Feemodel THEN DO:
      MESSAGE "Feemodel not found:  " + icBaseMFFeemodel VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttFeeModel.
   BUFFER-COPY Feemodel TO ttFeeModel.
   /*Set correct values to new entry*/
   ttFeeModel.feemodel = icMFFeemodel.
   ttFeeModel.feename = icFeename.

   DISPLAY ttFeeModel with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE FeeModel.
      BUFFER-COPY ttFeeModel TO FeeModel.
      DELETE ttFeeModel. /*for safety reasons*/
   END.
   IF AVAIL FeeModel THEN RELEASE FeeModel.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create DayCampaign */
FUNCTION fcreateDayCampaign RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                              INPUT icDCEvent AS CHAR,
                                              INPUT icMFFeeModel AS CHAR,
                                              INPUT icDCName  AS CHAR,
                                              INPUT idaValidFrom AS DATE,
                                              INPUT iiUpdateMode AS INT):

   FIND FIRST DayCampaign WHERE
              DayCampaign.dcevent EQ icBaseDCEvent AND
              DayCampaign.validto > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DayCampaign THEN DO:
      MESSAGE "DayCampaign not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttDayCampaign.
   BUFFER-COPY DayCampaign TO ttDaycampaign.
   /*Set correct values to new entry*/
   ttDayCampaign.dcEvent = icdcEvent.
   ttDayCampaign.BillCode = icdcEvent.
   ttDaycampaign.feemodel = icMFFeeModel.
   ttDaycampaign.dcname = icDCName.
   ttDaycampaign.validFrom = idaValidFrom.

   DISPLAY ttDayCampaign with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE DayCampaign.
      BUFFER-COPY ttDayCampaign TO DayCampaign.
      DELETE ttdayCampaign. /*for safety reasons*/
   END.
   IF AVAIL DayCampaign THEN RELEASE DayCampaign.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create FMItem                                         */
FUNCTION fcreateFMItem RETURNS LOGICAL ( INPUT icBaseMFFeeModel AS CHAR,
                                         INPUT icMFFeeModel AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT idFMAmount  AS DEC,
                                         INPUT iiUpdateMode AS INT):
   FIND FIRST FMItem WHERE
              FMItem.FeeModel EQ icBaseMFFeeModel AND
              FMItem.todate > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL FMItem THEN DO:
      MESSAGE "FMItem not found:  " + icBaseMFFeeModel VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttFMItem.
   BUFFER-COPY FMItem TO ttFMItem.
   /*Set correct values to new entry*/
   ttFMItem.FeeModel = icMFFeeModel.
   ttFMItem.BillCode = icMFFeeModel.
   ttFMItem.Fromdate = idaVAlidFrom.
   ttFMItem.amount = idFMAmount.

   DISPLAY ttFMItem with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE FMItem.
      BUFFER-COPY ttFMItem TO FMItem.
      DELETE ttFMItem. /*for safety reasons*/
   END.
   IF AVAIL FMItem THEN RELEASE FMItem.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create Discount plan                                         */
FUNCTION fcreateDiscountPlan RETURNS LOGICAL ( INPUT icBaseMFFeemodel AS CHAR,
                                         INPUT icMFFeemodel AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT icBaseDp AS CHAR,
                                         INPUT icDp AS CHAR,
                                         INPUT icDpName AS CHAR,
                                         INPUT idAmt AS DEC,
                                         INPUT iiUpdateMode AS INT):
   DEF VAR liDpId AS INT NO-UNDO.
   FIND LAST DiscountPlan USE-INDEX DpId NO-LOCK NO-ERROR.
   liDpId = DiscountPlan.DpId + 1.
   FIND FIRST DPTarget WHERE
              DPTarget.targetKey EQ icBaseMFFeeModel AND
              DPTarget.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DPTarget THEN DO:
      MESSAGE "DPTarget not found:  " + icBaseMFFeeModel VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.billcode = icBaseDp AND
              DiscountPlan.ValidTo > TODAY NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN DO:
      MESSAGE "DiscountPlan not found:  " + icBaseDp VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   CREATE ttDPTarget.
   BUFFER-COPY DPTarget TO ttDPTarget.
   CREATE ttDiscountPlan.
   BUFFER-COPY DiscountPlan TO ttDiscountPlan.
   /*Set correct values to new entry*/
   ttDPTarget.TargetKey = icMFFeeModel.
   ttDPTarget.ValidFrom = idaVAlidFrom. 
   ttDPTarget.dpId = liDpId.

   ttDiscountPlan.billcode = icDp.
   ttDiscountPlan.dpRuleId = icDp.
   ttDiscountPlan.DPName = icDpName.
   ttDiscountPlan.ValidFrom = idaVAlidFrom.
   ttDiscountPlan.dpid = liDpId.

   DISPLAY ttDPTarget with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      
      FIND FIRST DPTarget WHERE
                 DPTarget.targetKey EQ icMFFeeModel AND
                 DPTarget.validTo > TODAY NO-LOCK NO-ERROR.

      IF NOT AVAIL DPTarget THEN DO:                                  
         CREATE DPTarget.
         BUFFER-COPY ttDPTarget TO DPTarget.
         DELETE ttDPTarget. /*for safety reasons*/
      END.
      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.billcode = icDp AND
                 DiscountPlan.ValidTo > TODAY NO-ERROR.
      IF NOT AVAIL DiscountPlan THEN DO:
         CREATE DiscountPlan.
         BUFFER-COPY ttDiscountPlan TO Discountplan.
         DELETE ttDiscountPlan. /*for safety reasons*/
      END.
      FIND FIRST DPRate WHERE
                 DPRate.dpid = liDpId NO-LOCK NO-ERROR.
      IF NOT AVAIL DPRate THEN DO:
         CREATE DPRate.
         DPRate.dpid = liDpId.
         DPRate.discValue = idAmt.
         DPRate.ValidTo = 12/31/49.
         DPRate.ValidFrom = idaValidFrom.
      END.
   END.
   IF AVAIL DPTarget THEN RELEASE DPTarget.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Add Matrix Item                                       */

FUNCTION faddMatrixValue RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                          INPUT icDCEvent AS CHAR,
                          INPUT iiUpdateMode AS INT):
   
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH MXItem WHERE LOOKUP(icBaseDCEvent, MXItem.MXValue) > 0:
         MXItem.MXValue = MXItem.MXValue + "," + icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Add Request action rules                              */

FUNCTION faddRequestActionRules RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                                                 INPUT icDCEvent AS CHAR,
                                                 INPUT iiUpdateMode AS INT):
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH RequestActionRule WHERE LOOKUP(icBaseDCEvent, 
                                       RequestActionRule.paramvalue) > 0:
         RequestActionRule.paramvalue = RequestActionRule.paramvalue + "," + 
                                        icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

FUNCTION fcreateServiceLimit RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                         INPUT icDCEvent AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT idLimitValue AS DEC,
                                         INPUT icServLimitName AS CHAR,
                                         INPUT iiUpdateMode AS INT):
   FIND FIRST ServiceLimitGroup WHERE
              ServiceLimitGroup.groupcode EQ icBaseDCEvent AND
              ServiceLimitGroup.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL ServiceLimitGroup THEN DO:
      MESSAGE "ServiceLimitgroup not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   CREATE ttServiceLimitGroup.
   BUFFER-COPY ServiceLimitGroup TO ttServiceLimitGroup.
 
   FIND FIRST ServiceLimit WHERE
              ServiceLimit.groupcode EQ icBaseDCEvent AND
              ServiceLimit.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL ServiceLimit THEN DO:
      MESSAGE "ServiceLimit not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttServiceLimit.
   BUFFER-COPY ServiceLimit TO ttServiceLimit.
   /*Set correct values to new entry*/
   ttServiceLimitGroup.groupcode = icDCEvent.
   ttServiceLimitGroup.ValidFrom = idaVAlidFrom.
   ttServiceLimitGroup.groupname = icServLimitName.
   ttServiceLimit.groupcode = icDCEvent.
   ttServiceLimit.ValidFrom = idaVAlidFrom.
   ttServiceLimit.inclamt = idLimitValue.
   ttServiceLimit.slname = icServLimitName.
   ttServiceLimit.slcode =  icDCEvent.
   find last servicelimit  use-index slseq no-lock no-error.
   ttServiceLimit.slseq = servicelimit.slseq + 1.
   DISPLAY ttServiceLimit with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE ServiceLimitGroup.
      BUFFER-COPY ttServiceLimitGroup TO ServiceLimitGroup.
      DELETE ttServiceLimitGroup. /*for safety reasons*/
      CREATE ServiceLimit.
      BUFFER-COPY ttServiceLimit TO ServiceLimit.
      DELETE ttServiceLimit. /*for safety reasons*/
   END.
   RETURN TRUE.
END FUNCTION.

FUNCTION fcreateProgLimit RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                         INPUT icDCEvent AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT idLimitValue AS DEC,
                                         INPUT iiUpdateMode AS INT):
   FIND FIRST ProgLimit WHERE
              ProgLimit.groupcode EQ icBaseDCEvent AND
              ProgLimit.limitfrom = 0 AND
              ProgLimit.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL ProgLimit THEN DO:
      MESSAGE "ProgLimit not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttProgLimit.
   BUFFER-COPY ProgLimit TO ttProgLimit.
   /*Set correct values to new entry*/
   ttProgLimit.groupcode = icDCEvent.
   ttProgLimit.ValidFrom = idaVAlidFrom.
   ttProgLimit.BDest     = REPLACE(ProgLimit.BDest, icBaseDCEvent, icDCEvent).
   ttProgLimit.limitto = idLimitValue.
   FIND FIRST ServiceLimit WHERE
              ServiceLimit.groupcode EQ icDCEvent AND
              ServiceLimit.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL ServiceLimit THEN DO:
      MESSAGE "ServiceLimit not found:  " + icDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   ttProgLimit.slseq = ServiceLimit.slseq.
   DISPLAY ttProgLimit with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE ProgLimit.
      BUFFER-COPY ttProgLimit TO ProgLimit.
      DELETE ttProgLimit. /*for safety reasons*/
   END.

   FIND FIRST ProgLimit WHERE
              ProgLimit.groupcode EQ icBaseDCEvent AND
              ProgLimit.limitfrom > 0 AND
              ProgLimit.validTo > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL ProgLimit THEN DO:
      MESSAGE "ProgLimit not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttProgLimit.
   BUFFER-COPY ProgLimit TO ttProgLimit.
   /*Set correct values to new entry*/
   ttProgLimit.groupcode = icDCEvent.
   ttProgLimit.ValidFrom = idaVAlidFrom.
   ttProgLimit.BDest     = REPLACE(ProgLimit.BDest, icBaseDCEvent, icDCEvent).
   ttProgLimit.limitfrom = idLimitValue + 0.000001.
   ttProgLimit.slseq = ServiceLimit.slseq.

   DISPLAY ttProgLimit with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE ProgLimit.
      BUFFER-COPY ttProgLimit TO ProgLimit.
      DELETE ttProgLimit. /*for safety reasons*/
   END.
   IF AVAIL ProgLimit THEN RELEASE ProgLimit.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create SLGAnalyse                                     */

FUNCTION fcreateSLGAnalyse RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icDCEvent AS CHAR,
                                             INPUT idaVAlidFrom AS DATE,
                                             INPUT icclitypeList AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   DEF VAR iCount AS INT NO-UNDO.
   DEF VAR lcCliType AS CHAR NO-UNDO.
   DEF VAR lcClitypeList AS CHAR NO-UNDO.
   IF icClitypeList = "" THEN DO:
      FOR EACH SlgAnalyse WHERE 
               SLGAnalyse.servicelimitgroup EQ icBaseDCEvent AND
               SLGAnalyse.validTo > TODAY NO-LOCK:
          lcClitypeList = lcClitypeList + SLGAnalyse.clitype + ",".
      END.
      lcClitypeList = RIGHT-TRIM(lcClitypeList,",").
   END.
   ELSE lcClitypeList = icClitypeList.
   DO iCount = 1 TO NUM-ENTRIES(lcCliTypeList):
      lcCliType = ENTRY(iCount,lcCliTypeList).
      FIND FIRST SLGAnalyse WHERE
                 SLGAnalyse.servicelimitgroup EQ icBaseDCEvent AND
                 SLGAnalyse.clitype = lcCliType AND
                 SLGAnalyse.validTo > TODAY NO-LOCK NO-ERROR.
      IF NOT AVAIL SLGAnalyse THEN DO:
         FIND FIRST SLGAnalyse WHERE
                 SLGAnalyse.servicelimitgroup EQ icBaseDCEvent AND
                 SLGAnalyse.clitype = "CONT" AND
                 SLGAnalyse.validTo > TODAY NO-LOCK NO-ERROR.
         
         IF NOT AVAIL SLGAnalyse THEN DO:
            MESSAGE "SLGAnalyse not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
            RETURN FALSE.
         END.
      END.
                 
      CREATE ttSLGAnalyse.
      BUFFER-COPY SLGAnalyse TO ttSLGAnalyse.
      FIND FIRST SLGAnalyse WHERE
                 SLGAnalyse.servicelimitgroup EQ icDCEvent AND
                 SLGAnalyse.clitype = lcCliType AND
                 SLGAnalyse.validTo > TODAY NO-LOCK NO-ERROR.
      IF NOT AVAIL SLGAnalyse THEN DO:
         /*Set correct values to new entry*/
         
         ttSLGAnalyse.servicelimitgroup = icDCEvent.
         ttSLGAnalyse.ValidFrom = idaVAlidFrom.
         ttSLGAnalyse.clitype = lcCliType.

         DISPLAY ttSLGAnalyse with frame a.
         pause 0.
         IF iiUpdateMode NE 0 THEN DO:
            CREATE SLGAnalyse.
            BUFFER-COPY ttSLGAnalyse TO SLGAnalyse.
            DELETE ttSLGAnalyse. /*for safety reasons*/
         END.
      END.
      IF AVAIL ttSLGAnalyse THEN RELEASE ttSLGAnalyse.
   END.
   IF AVAIL SLGAnalyse THEN RELEASE SLGAnalyse.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Add TMSParam                              */

FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                                                 INPUT icDCEvent AS CHAR,
                                                 INPUT iiUpdateMode AS INT):
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH TMSParam WHERE LOOKUP(icBaseDCEvent,
                                       tmsParam.charval) > 0:
         tmsParam.charval = tmsParam.charval + "," +
                                        icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create BDest                                          */

FUNCTION fcreateBDest RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icDCEvent AS CHAR,
                                             INPUT idaVAlidFrom AS DATE,
                                             INPUT iiUpdateMode AS INT):
   DEF VAR iCount AS INT NO-UNDO.
   DEF VAR liID AS INT NO-UNDO.
   DEF BUFFER bBDest FOR bDest.
   
   FIND FIRST BDest WHERE
              INDEX(BDest.BDest, icBaseDCEvent) > 0 AND
              INDEX(BDest.BDest,"UPSELL") = 0 AND
              BDest.ToDate > TODAY NO-LOCK NO-ERROR.   

   IF NOT AVAIL BDest THEN DO:
      MESSAGE "BDest not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   
   CREATE ttBDest.
   BUFFER-COPY BDest TO ttBDest.
   /*Set correct values to new entry*/
   ttBDest.BDest = REPLACE(BDest.Bdest, icBaseDCEvent, icDCEvent).
   ttBDest.FromDate = idaVAlidFrom.
   FIND LAST bBDest USE-INDEX BDestID NO-LOCK NO-ERROR.
   liId =  bBDest.BDestId + 1.
   ttBDest.BdestId = liId.
   liId = liId + 1.
   DISPLAY ttBDest with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      FIND FIRST bBdest WHERE bBdest.brand = ttBdest.brand AND
                              bBdest.bdest = ttBdest.bdest AND
                              bBdest.destType = ttBdest.destType AND
                              bBdest.todate > TODAY NO-LOCK NO-ERROR.
      IF NOT AVAIL bBdest THEN DO:
         CREATE BDest.
         BUFFER-COPY ttBdest TO Bdest.
         DELETE ttBDest. /*for safety reasons*/
         RELEASE BDest.
      END.
      RELEASE bBdest.
   END. 
   RELEASE ttBDest.
   REPEAT:
      FIND NEXT BDest WHERE
              INDEX(BDest.BDest, icBaseDCEvent) > 0 AND
              INDEX(BDest.BDest,"UPSELL") = 0 AND
              BDest.ToDate > TODAY.
      IF NOT AVAIL BDest THEN LEAVE.
      CREATE ttBDest.
      BUFFER-COPY BDest TO ttBDest.
      /*Set correct values to new entry*/
      ttBDest.BDest = REPLACE(BDest.Bdest, icBaseDCEvent, icDCEvent).
      ttBDest.FromDate = idaVAlidFrom.
      ttBDest.BdestId = liId.
      liId = liId + 1.
      DISPLAY ttBDest with frame a.
      pause 0.
      IF iiUpdateMode NE 0 THEN DO:
         FIND FIRST bBdest WHERE bBdest.brand = ttBdest.brand AND
                                 bBdest.bdest = ttBdest.bdest AND
                                 bBdest.destType = ttBdest.destType AND
                                 bBdest.todate > TODAY NO-LOCK NO-ERROR.
         IF NOT AVAIL bBdest THEN DO:
            CREATE BDest.
            BUFFER-COPY ttBdest TO Bdest.
            DELETE ttBDest. /*for safety reasons*/
            RELEASE BDest.
         END.
         RELEASE bBdest.
      END.
   END.
   IF AVAIL BDest THEN RELEASE BDest.
   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create Discount plan                                         */
FUNCTION fcreateDCService RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                         INPUT icDCEvent AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT icParam AS CHAR,
                                         INPUT iiUpdateMode AS INT):
   DEF VAR lcservpackId AS INT NO-UNDO.
   DEF VAR lcservcomId AS INT NO-UNDO.
   FIND FIRST DCServicePackage WHERE
              DCServicePackage.dcEvent EQ icBaseDCEvent AND
              DCServicePackage.ToDate > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DCServicePackage THEN DO:
      MESSAGE "DCServicePackage not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   FIND FIRST DCServiceComponent WHERE
              DCServiceComponent.dcservicepackageid EQ 
              DCServicePackage.dcservicepackageid AND
              DCServiceComponent.ToDate > TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DCServiceComponent THEN DO:
      MESSAGE "DCServiceComponent not found:" + icBaseDCEvent VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttDCServicePackage.
   CREATE ttDCServiceComponent.
   BUFFER-COPY DCServicePackage TO ttDCServicePackage.
   BUFFER-COPY DCServiceComponent TO ttDCServiceComponent.
   /*Set correct values to new entry*/
   FIND LAST DCServicePackage USE-INDEX DCServicePackageID NO-LOCK NO-ERROR.
   lcservpackId = DCServicePackage.DCServicePackageID + 1.
   ttDCServicePackage.dcEvent = icDCEvent.
   ttDCServicePackage.FromDate = idaVAlidFrom.
   ttDCServicePackage.DCServicePackageID = lcservpackId.

   DISPLAY ttDCServicePackage with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE DCServicePackage.
      BUFFER-COPY ttDCServicePackage TO DCServicePackage.
      DELETE ttDCServicePackage. /*for safety reasons*/
   END.
   IF AVAIL DCServicePackage THEN RELEASE DCServicePackage.

   /*Set correct values to new entry*/
   FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID NO-LOCK NO-ERROR.
   lcservcomId = DCServiceComponent.DCServiceComponentID + 1.
   ttDCServiceComponent.dcservicecomponentid = lcservcomId.
   ttDCServiceComponent.FromDate = idaVAlidFrom.
   ttDCServiceComponent.DCServicePackageID = lcservpackId.
   ttDCServiceComponent.defParam = icparam.

   DISPLAY ttDCServiceComponent with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE DCServiceComponent.
      BUFFER-COPY ttDCServiceComponent TO DCServiceComponent.
      DELETE ttDCServiceComponent. /*for safety reasons*/
   END.
   IF AVAIL DCServiceComponent THEN RELEASE DCServiceComponent.   

   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create RequestActionRule                              */
FUNCTION fcreateRequestActionRule RETURNS LOGICAL ( INPUT icBasebillcode AS CHAR,
                                           INPUT icbillcode AS CHAR,
                                           INPUT iiUpdateMode AS INT):

   FOR EACH RequestActionRule WHERE
              LOOKUP(icBasebillcode,requestactionrule.paramvalue) > 0 AND
              LOOKUP(icBillcode,requestactionrule.paramvalue) = 0. 
      requestactionrule.paramvalue = requestactionrule.paramvalue + "," +
                                     icbillcode.
   END.

   RETURN TRUE.
END FUNCTION.

/*********************************************************/
/* Create Tariff                                         */
FUNCTION fcreateTariff RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                         INPUT icDCEvent AS CHAR,
                                         INPUT idaVAlidFrom AS DATE,
                                         INPUT iiUpdateMode AS INT):
   DEF VAR lcNum AS INT NO-UNDO.
   FIND LAST Tariff use-index TariffNum no-lock no-error.
   lcNum = Tariff.tariffnum + 1.
   FOR EACH BDest NO-LOCK WHERE
              INDEX(BDest.BDest, icBaseDCEvent) > 0 AND
              INDEX(BDest.BDest,"UPSELL") = 0 AND
              BDest.ToDate > TODAY:
      FIND FIRST Tariff WHERE
                 Tariff.BDest EQ BDest.bdest AND
                 Tariff.validTo > TODAY NO-LOCK NO-ERROR.
      IF NOT AVAIL Tariff THEN DO:
         MESSAGE "Tariff not found:  " + icBaseDCEvent VIEW-AS ALERT-BOX.
         RETURN FALSE.
      END.

      CREATE ttTariff.
      BUFFER-COPY Tariff TO ttTariff.
      FIND LAST Tariff use-index TariffNum no-lock no-error. 
      /*Set correct values to new entry*/
      ttTariff.BDest = REPLACE(BDest.bdest, icBaseDCEvent, icDCEvent).
      ttTariff.ValidFrom = idaVAlidFrom.
      ttTariff.tariffnum = lcNum.
      ttTariff.billCode = icDCEvent.
      lcNum = lcNum + 1.
      DISPLAY ttTariff with frame a.
      pause 0.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE Tariff.
         BUFFER-COPY ttTariff TO Tariff.
         DELETE ttTariff. /*for safety reasons*/
      END.
      IF AVAIL Tariff THEN RELEASE Tariff.
      IF AVAIL ttTariff THEN RELEASE ttTariff.
   END.
   RETURN TRUE.
END FUNCTION.
