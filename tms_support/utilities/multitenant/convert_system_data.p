/*
   RUN THIS SCRIPT USING YOIGO-TENANT!!!
   This script do conversion to multi-tenancy for tables that
   are not tenant specific. It also convert data to Yoigo side
   after updating the data from d-files (or fixture files).
   It is possible to run the script multiple times
   without breaking anything */

/* Generic replace procedure */
PROCEDURE pReplace:

   DEFINE INPUT  PARAMETER icReplaceFrom AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icReplaceTo   AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER iocText AS CHARACTER NO-UNDO.

   IF INDEX(iocText, icReplaceTo) > 0
   THEN RETURN.

   IF INDEX(iocText, icReplaceFrom) = 0
   THEN RETURN.

   iocText = REPLACE(iocText,icReplaceFrom,icReplaceTo).

END PROCEDURE.

/* Function to convert DumpFile.FileName */
FUNCTION fDumpFileConvert RETURNS LOGICAL
   (iiDumpID AS INTEGER,
    icAddToBegin AS CHARACTER,
    icReplaceFrom AS CHARACTER,
    icReplaceTo   AS CHARACTER):

   FIND Dumpfile EXCLUSIVE-LOCK WHERE
        Dumpfile.DumpID = iiDumpID
   NO-ERROR.

   IF NOT AVAILABLE DumpFile THEN RETURN TRUE.

   IF icAddToBegin > ""
   THEN DO:
      IF INDEX(DumpFile.FileName,icAddToBegin) = 0
      THEN DumpFile.FileName = icAddToBegin + DumpFile.FileName.
   END.
   ELSE RUN pReplace(icReplaceFrom, icReplaceTo, INPUT-OUTPUT DumpFile.FileName).

   RETURN FALSE.

END FUNCTION.


/* DATA CONVERSION BEGINS */

/* Common data */

/* Add tenant to FuncRunConfig RunCommand */
FOR EACH FuncRunConfig EXCLUSIVE-LOCK:

   IF INDEX(FuncRunConfig.RunCommand,"#TENANT") > 0
   THEN NEXT.

   FuncRunConfig.RunCommand = REPLACE(FuncRunConfig.RunCommand,"tenant=yoigo", "tenant=#TENANT").
END.

/* DumpFiles */

/* DumpName: CustDumpTXT */
fDumpFileConvert(5,"","#CAT","#CAT_#TENANT").
/* DumpName: RequestDumpTXT */
fDumpFileConvert(9,"","#CAT","#CAT_#TENANT").
/* DumpName: SubscriptionDumpTXT */
fDumpFileConvert(10,"","#CAT","#CAT_#TENANT").
/* DumpName: OrderDumpTXT */
fDumpFileConvert(12,"#TENANT_","","").
/* DumpName: OrderCustDumpTXT */
fDumpFileConvert(13,"","#CAT","#CAT_#TENANT").
/* DumpName: OrderPaymentTXT */
fDumpFileConvert(14,"","#CAT","#CAT_#TENANT").
/* DumpName: Logistics */
fDumpFileConvert(16,"","#CAT","#CAT_#TENANT").
/* DumpName: BillingPermTXT */
fDumpFileConvert(18,"","#CAT","#CAT_#TENANT").
/* DumpName: CreditNotesTxt */
fDumpFileConvert(19,"","#CAT","#CAT_#TENANT").
/* DumpName: FreeAirTimeTxt */
fDumpFileConvert(20,"","#CAT","#CAT_#TENANT").
/* DumpName: MobCDR */
fDumpFileConvert(21,"","#CAT","#CAT_#TENANT").
/* DumpName: ACCRequest */
fDumpFileConvert(29,"","#CAT","#CAT_#TENANT").
/* MB-142 DumpName: ??? */
fDumpFileConvert(30,"#TENANT_","","").
/* MB-142 DumpName: ??? */
fDumpFileConvert(31,"#TENANT_","","").
/* DumpName: IFSInvoice */
fDumpFileConvert(32,"#TENANT_","","").
/* DumpName: ChargeCompDump */
fDumpFileConvert(33,"","#CAT","#CAT_#TENANT").
/* DumpName: MSOwner */
fDumpFileConvert(40,"#TENANT_","","").
/* DumpName: OrderAccessory_ORIG */
fDumpFileConvert(41,"#TENANT_","","").
/* DumpName: PerContrDump */
fDumpFileConvert(42,"#TENANT_","","").
/* DumpName: OrderPerContrDump */
fDumpFileConvert(43,"#TENANT_","","").
/* DumpName: ActiveServices */
fDumpFileConvert(45,"#TENANT_","","").
/* DumpName: MSISDN */
fDumpFileConvert(46,"","#CAT","#CAT_#TENANT").
/* DumpName: MServiceLPoolDump */
fDumpFileConvert(47,"#TENANT_","","").
/* DumpName: BillingItems */
fDumpFileConvert(49,"","#CAT","#CAT_#TENANT").
/* DumpName: MNPKPI */
fDumpFileConvert(50,"#TENANT_","","").
/* DumpName: ErrorCDRDump */
fDumpFileConvert(51,"","#CAT","#CAT_#TENANT").
/* DumpName: InvRowDump */
fDumpFileConvert(52,"#TENANT_","","").
/* MB-142 DumpName: ??? */
fDumpFileConvert(54,"#TENANT_","","").
/* MB-142 DumpName: ??? */
fDumpFileConvert(55,"#TENANT_","","").
/* DumpName: CustomerReport */
fDumpFileConvert(56,"","#CAT","#CAT_#TENANT").
/* DumpName: InvoiceDump */
fDumpFileConvert(57,"#TENANT_","","").
/* MB-142 DumpName: ??? */
fDumpFileConvert(62,"#TENANT_","","").
/* DumpName: ExternalAPI */
fDumpFileConvert(63,"#TENANT_","","").
/* DumpName: OrderAction */
fDumpFileConvert(64,"#TENANT_","","").
/* DumpName: IPRange */
fDumpFileConvert(65,"","#CAT","#CAT_#TENANT").
/* DumpName: DiscountMember */
fDumpFileConvert(66,"","#CAT","#CAT_#TENANT").
/* DumpName: MNPCancellationLog */
fDumpFileConvert(68,"","#CAT","#CAT_#TENANT").
/* DumpName: SingleFeeEvent */
fDumpFileConvert(69,"#TENANT_","","").
/* DumpName: BillingPermEvents */
fDumpFileConvert(70,"","#CAT","#CAT_#TENANT").
/* DumpName: DPLLog */
fDumpFileConvert(71,"","#CAT","#CAT_#TENANT").
/* MB-142 DumpName: ??? */
fDumpFileConvert(73,"#TENANT_","","").
/* DumpName: TMSuser */
fDumpFileConvert(74,"#TENANT_","","").
/* DumpName: CustIdOrderLimit */
fDumpFileConvert(75,"#TENANT_","","").
/* DumpName: MNP_IN */
fDumpFileConvert(76,"#TENANT_","","").
/* DumpName: MNP_OUT */
fDumpFileConvert(77,"#TENANT_","","").
/* DumpName: MNP_Termination */
fDumpFileConvert(78,"#TENANT_","","").
/* DumpName: MNP_Error */
fDumpFileConvert(79,"#TENANT_","","").
/* DumpName: MNPActivationFails */
fDumpFileConvert(80,"#TENANT_","","").
/* DumpName: MNPDelayResp */
fDumpFileConvert(81,"#TENANT_","","").
/* DumpName: SalesmanDump */
fDumpFileConvert(82,"#TENANT_","","").
/* DumpName: ICC_Dump */
fDumpFileConvert(83,"#TENANT_","","").
/* DumpName: IFSInvoiceFusion */
fDumpFileConvert(91,"#TENANT_","","").
/* DumpName: BicMissingCheck */
fDumpFileConvert(92,"#TENANT_","","").
/* DumpName: ??? */
fDumpFileConvert(94,"#TENANT_","","").
/* DumpName: EventLogDump */
fDumpFileConvert(95,"#TENANT_","","").
/* DumpName: CustEventLog */
fDumpFileConvert(96,"","#CAT","#CAT_#TENANT").
/* DumpName: ??? */
fDumpFileConvert(101,"#TENANT_","","").
/* DumpName: HPD_Barring */
fDumpFileConvert(200,"#TENANT_","","").
/* DumpName: HPD_BillItem */
fDumpFileConvert(202,"#TENANT_","","").
/* DumpName: HPD_BItemGroup */
fDumpFileConvert(203,"#TENANT_","","").
/* DumpName: HPD_Customer */
fDumpFileConvert(204,"#TENANT_","","").
/* DumpName: HPD_MServiceLimit */
fDumpFileConvert(205,"#TENANT_","","").
/* DumpName: HPD_MServiceLPool */
fDumpFileConvert(206,"#TENANT_","","").
/* DumpName: HPD_DayCampaign */
fDumpFileConvert(207,"#TENANT_","","").
/* DumpName: HPD_DCCLI */
fDumpFileConvert(208,"#TENANT_","","").
/* DumpName: HPD_DiscountPlan */
fDumpFileConvert(209,"#TENANT_","","").
/* DumpName: HPD_DMS */
fDumpFileConvert(210,"#TENANT_","","").
/* DumpName: HPD_DMSDoc */
fDumpFileConvert(211,"#TENANT_","","").
/* DumpName: HPD_DPMember */
fDumpFileConvert(212,"#TENANT_","","").
/* DumpName: HPD_DPTarget */
fDumpFileConvert(213,"#TENANT_","","").
/* DumpName: HPD_FATGroup */
fDumpFileConvert(214,"#TENANT_","","").
/* DumpName: HPD_FATime */
fDumpFileConvert(215,"#TENANT_","","").
/* DumpName: HPD_FixedFee */
fDumpFileConvert(216,"#TENANT_","","").
/* DumpName: HPD_FMItem */
fDumpFileConvert(217,"#TENANT_","","").
/* DumpName: HPD_IMSI */
fDumpFileConvert(218,"#TENANT_","","").
/* DumpName: HPD_MobCDR */
fDumpFileConvert(219,"#TENANT_","","").
/* DumpName: HPD_MobSub */
fDumpFileConvert(220,"#TENANT_","","").
/* DumpName: HPD_MsRequest */
fDumpFileConvert(221,"#TENANT_","","").
/* DumpName: HPD_Order */
fDumpFileConvert(223,"#TENANT_","","").
/* DumpName: HPD_OrderAction */
fDumpFileConvert(224,"#TENANT_","","").
/* DumpName: HPD_OrderCustomer */
fDumpFileConvert(225,"#TENANT_","","").
/* DumpName: HPD_OrderDelivery */
fDumpFileConvert(226,"#TENANT_","","").
/* DumpName: HPD_OrderFusion */
fDumpFileConvert(227,"#TENANT_","","").
/* DumpName: HPD_PrepaidRequest */
fDumpFileConvert(228,"#TENANT_","","").
/* DumpName: HPD_PrepCDR */
fDumpFileConvert(229,"#TENANT_","","").
/* DumpName: HPD_PrepEDR */
fDumpFileConvert(230,"#TENANT_","","").
/* DumpName: HPD_RepText */
fDumpFileConvert(231,"#TENANT_","","").
/* DumpName: HPD_ServiceLimit */
fDumpFileConvert(233,"#TENANT_","","").
/* DumpName: HPD_ServPac */
fDumpFileConvert(234,"#TENANT_","","").
/* DumpName: HPD_SingleFee */
fDumpFileConvert(235,"#TENANT_","","").
/* DumpName: HPD_SubSer */
fDumpFileConvert(236,"#TENANT_","","").
/* DumpName: HPD_SubsTerminal */
fDumpFileConvert(237,"#TENANT_","","").
/* DumpName: HPD_TermReturn */
fDumpFileConvert(238,"#TENANT_","","").
/* DumpName: HPD_TopupSchemeRow */
fDumpFileConvert(239,"#TENANT_","","").
/* DumpName: HPD_Invoice */
fDumpFileConvert(240,"#TENANT_","","").
/* DumpName: HPD_SubInvoice */
fDumpFileConvert(241,"#TENANT_","","").
/* DumpName: HPD_InvRow */
fDumpFileConvert(242,"#TENANT_","","").
/* DumpName: HPD_EDRHistory */
fDumpFileConvert(243,"#TENANT_","","").
/* DumpName: HPD_Payment */
fDumpFileConvert(244,"#TENANT_","","").
/* DumpName: HPD_ServiceLCounter */
fDumpFileConvert(245,"#TENANT_","","").
/* DumpName: HPD_CustContact */
fDumpFileConvert(246,"#TENANT_","","").
/* DumpName: HPD_Limit */
fDumpFileConvert(247,"#TENANT_","","").
/* DumpName: HPD_CLIType */
fDumpFileConvert(248,"#TENANT_","","").

/* Yoigo data */

FUNCTION fUpdateTMSParam RETURNS LOGICAL
   (icParamGroup  AS CHARACTER,
    icParamCode   AS CHARACTER,
    icReplaceFrom AS CHARACTER,
    icReplaceTo   AS CHARACTER ):

   FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ icParamGroup AND
              TMSParam.ParamCode EQ icParamCode
   NO-ERROR.

   IF NOT AVAILABLE TMSParam
   THEN RETURN TRUE.

   IF INDEX(TMSParam.charval,icReplaceTo) > 0
   THEN RETURN TRUE.

   TMSParam.charval = REPLACE(TMSParam.charval,icReplaceFrom,icReplaceTo).

END FUNCTION.

fUpdateTMSParam("Report", "SIMStatistics", "SIMStatistics", "Yoigo_SIMStatistics_").
fUpdateTMSParam("Report", "ErrorFile", "icc_msisdn", "Yoigo_icc_msisdn").
fUpdateTMSParam("Report", "MSISDNStatistics", "MSISDNStatistics", "Yoigo_MSISDNStatistics").
fUpdateTMSParam("Terminate", "SubsTermLog", "spool/terminate", "spool/#TENANT_terminate").
fUpdateTMSParam("IFS", "IFSPaymStatusFile", "/IFS_PAY", "/#TENANT_IFS_PAY").
fUpdateTMSParam("IFS", "IFSCollActionFile", "/IFS_BAR", "/#TENANT_IFS_BAR").
fUpdateTMSParam("BillReport", "CCNReportFile", "/billing", "/#TENANT_billing").
fUpdateTMSParam("BillReport", "BillQualityFileName", "/billing", "/#TENANT_billing").
fUpdateTMSParam("FuncRun", "FRDaemonLockFile", "daemon.lock", "daemon.yoigo.lock").
fUpdateTMSParam("Reports", "ErrorLogRepFile", "log/errorlog","log/YOIGO_errorlog").
fUpdateTMSParam("CDR", "DoubleCallLog", "mobcdr", "YOIGO_mobcdr").
fUpdateTMSParam("BillRun", "BillRunStatFile", "billrun_statistic", "YOIGO_billrun_statistic").
fUpdateTMSParam("IFS", "IFSPaymStatusLog", "spool/IFS", "spool/YOIGO_IFS").
fUpdateTMSParam("Pentaho", "PentahoBITotals", "/billing", "/#TENANT_billing").
fUpdateTMSParam("Pentaho", "InvGrainFile", "invoice_row_dump", "#TENANT_invoice_row_dump").

FOR EACH BankAccount EXCLUSIVE-LOCK:
   bankaccount.presenterID = bankAccount.creditorid.
END.

FOR FIRST Company EXCLUSIVE-LOCK.
   company.CreditorName = "YOIGO".
END.

FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "IMSI_Begin" NO-ERROR.
IF AVAIL TMSParam THEN tmsparam.charval = "2140401".

/* Antti: I'M PLANNING TO CHANGE THESE TABLES TO TENANT-SPECIFIC */
/*
FUNCTION fCreateInvGroup RETURNS LOGICAL
   ( BUFFER ibInvGroup FOR InvGroup,
     icNewInvGroup AS CHARACTER,
     icNewInvForm  AS CHARACTER,
     icReplaceFrom AS CHARACTER,
     icReplaceTo   AS CHARACTER ):

   DEFINE BUFFER bInvGroup FOR InvGroup.

   FIND FIRST bInvGroup EXCLUSIVE-LOCK WHERE
      bInvGroup.Brand = "1" AND
      bInvGroup.InvGroup = icNewInvGroup
   NO-ERROR.

   IF NOT AVAILABLE bInvGroup
   THEN CREATE bInvGroup.

   ASSIGN
      bInvGroup.InvGroup = icNewInvGroup
      bInvGroup.InvForm  = icNewInvForm
      bInvGroup.IGName = REPLACE(ibInvGroup.IGName, icReplaceFrom, icReplaceTo).

   BUFFER-COPY ibInvGroup EXCEPT taxzone invgroup invform IgName TO bInvGroup.
      bInvGroup.taxzone = STRING(INT(ibInvGroup.taxzone) + 5).

   RETURN FALSE.

END FUNCTION.

FOR EACH InvGroup NO-LOCK WHERE
         INT(InvGroup.taxzone) GE 1 AND
         INT(InvGroup.taxzone) LE 5:

   CASE InvGroup.invGroup:
      WHEN "VAT1"
      THEN fCreateInvGroup(BUFFER InvGroup, "VAT2", "VAT2", " 1", " 2").
      WHEN "IGIC1"
      THEN fCreateInvGroup(BUFFER InvGroup, "IGIC2", "IGIC2", " 1", " 2").
      WHEN "IPSIC1"
      THEN fCreateInvGroup(BUFFER InvGroup, "IPSIC2", "IPSIC2", " 1", " 2").
      WHEN "IPSIM1"
      THEN fCreateInvGroup(BUFFER InvGroup, "IPSIM2", "IPSIM2", " 1", " 2").
      WHEN "YOIGO"
      THEN fCreateInvGroup(BUFFER InvGroup, "MASMOVIL", "MASMOVIL", "Yoigo", "MasMovil").
      OTHERWISE NEXT.
   END CASE.

END.

DEFINE BUFFER bTaxZone FOR TaxZone.

FIND FIRST TaxZone WHERE
           Taxzone.taxzone EQ "10" NO-ERROR.
IF NOT AVAIL TAXZone THEN DO:
   FOR EACH TaxZone WHERE
            INT(Taxzone.taxzone) < 6:
      CREATE bTaxzone.
      bTaxzone.taxzone = STRING(INT(Taxzone.taxzone) + 5).
      IF Taxzone.taxzone EQ "5" THEN
         bTaxzone.tzname = "MasMovil (own usage)".
      ELSE bTaxzone.tzname = Taxzone.tzname.
   END.
END.

DEF BUFFER bvatcode FOR vatcode.

DEFINE VARIABLE liCode AS INT NO-UNDO.

FIND FIRST vatcode NO-LOCK WHERE
           vatcode.vatcode >= 50 NO-ERROR.
IF NOT AVAIL vatcode THEN
FOR EACH vatcode NO-LOCK WHERE
         vatcode.todate > TODAY:

   CASE vatcode.vatcode:
      WHEN 5  THEN liCode = 50.
      WHEN 6  THEN liCode = 51.
      WHEN 12  THEN liCode = 60.
      WHEN 13  THEN liCode = 61.
      WHEN 21  THEN liCode = 70.
      WHEN 22  THEN liCode = 71.
      WHEN 30  THEN liCode = 80.
      WHEN 31  THEN liCode = 81.
      WHEN 40  THEN liCode = 90.
      OTHERWISE NEXT.
   END.

   FIND bvatcode EXCLUSIVE-LOCK WHERE
      bvatcode.vatcode = liCode
   NO-ERROR.

   IF NOT AVAILABLE bvatcode
   THEN CREATE bvatcode.
   ASSIGN
      bvatcode.vatcode = liCode
      bvatcode.taxzone = STRING(INTEGER(vatcode.taxzone) + 5)
      bvatcode.accnum = vatcode.accnum. /* change to correct */
   BUFFER-COPY vatcode EXCEPT vatcode taxzone accnum TO bvatcode.
END.
*/
IF CURRENT-VALUE(OfferItemSeq) <= 1 THEN DO:
   FOR EACH OfferItem NO-LOCK BY OfferItem.OfferItemId DESC:
      LEAVE.
   END.
   IF AVAIL OfferItem THEN
      CURRENT-VALUE(OfferItemSeq) = OfferItem.OfferItemID + 1.
END.

IF CURRENT-VALUE(OfferCriteriaSeq) <= 1 THEN DO:
   FOR EACH OfferCriteria NO-LOCK
      BY OfferCriteria.OfferCriteriaId DESC:
      LEAVE.
   END.
   IF AVAIL OfferCriteria THEN
      CURRENT-VALUE(OfferCriteriaSeq) = OfferCriteria.OfferCriteriaID + 1.
END.

FOR EACH CliType EXCLUSIVE-LOCK WHERE CliType.ServicePack = "12":

   CliType.ServicePack = "42".

END.

FOR EACH CliType EXCLUSIVE-LOCK WHERE CliType.ServicePack = "11":

   CliType.ServicePack = "41".

END.

