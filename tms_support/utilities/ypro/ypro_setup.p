DEF BUFFER bCustCat FOR CustCat.
DEF BUFFER bTMSCodes FOR TMSCodes.
DEF BUFFER bservicelimitTarget FOR ServiceLimitTarget.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER tempSLGAnalyse FOR SLGAnalyse.
DEF BUFFER breqstatus FOR requeststatus.

DEF VAR ldaFrom AS DATE INIT TODAY.
DEF VAR limode AS INT INIT 1.
DEF VAR laskuri AS INT.

DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE giSlSeq AS INTEGER NO-UNDO.

/* BillItem */
FUNCTION fCreateBillItem RETURNS LOGICAL
   ( icBillCode AS CHARACTER ):

   FIND FIRST BillItem EXCLUSIVE-LOCK WHERE
      BillItem.Brand     = "1"       AND
      BillItem.BillCode = icBillCode
   NO-ERROR.

   IF NOT AVAILABLE BillItem
   THEN DO:
      CREATE BillItem.
      CREATE BItemGroup.
   END.

   ASSIGN
      BItemGroup.BIGName = "PRO"
      BITEMGroup.BIGroup = "48"
      BItemGroup.Brand     = "1"
      BItemGroup.grouptype = 1
      BItemGroup.invoiceorder = 33
      BillItem.Brand     = "1"
      BillItem.BillCode = icBillCode
      BillItem.BIName = "PRO monthly fee"
      BillItem.AccNum = 70518100
      BillItem.BIGroup = "48"
      BillItem.EUAccNum = 70518100
      BillItem.FSAccNum = 70518100
      BillItem.Brand = "1"
      BillItem.EUConAccNum = 70518100
      BillItem.CostCentre = "SL"
      BillItem.AltAccNum = 70518100
      BillItem.TaxClass = "1"
      BillItem.SAPRid = "070"
      BillItem.VIPAccNum = 70518100.

END FUNCTION.

FUNCTION fCreatePriceList RETURNS LOGICAL
   ( icPricelist AS CHARACTER ):

   FIND FIRST Pricelist EXCLUSIVE-LOCK WHERE
      pricelist.Brand     = "1"       AND
      pricelist.pricelist = icpricelist
   NO-ERROR.

   IF NOT AVAILABLE pricelist
   THEN DO:
      CREATE pricelist.

   ASSIGN
      Pricelist.AutoCreate = "" 
      Pricelist.Brand      = "1"
      Pricelist.Currency   = "EUR"
      Pricelist.CurrUnit   = TRUE
      Pricelist.DedicList  = FALSE
      Pricelist.InclVAT    = FALSE
      Pricelist.Memo       = "PRO pricelist"
      Pricelist.PLName     = "PRO fee for " + icPricelist
      Pricelist.Prefix     = ""
      Pricelist.PriceList  = icPriceList
      Pricelist.Rounding   = 4.
   END.
END FUNCTION.

FUNCTION fCreateFMItem RETURNS LOGICAL
   ( icItemName AS CHARACTER,
     icFeemodel AS CHARACTER,
     icPricelist AS CHARACTER,
     idamt AS DEC ):

   FIND FIRST FMItem EXCLUSIVE-LOCK WHERE
      fmitem.Brand     = "1"       AND
      fmitem.feemodel = icfeemodel AND
      fmitem.billcode = icItemName AND
      fmitem.pricelist = icPricelist
   NO-ERROR.

   IF NOT AVAILABLE fmitem
   THEN DO:
      CREATE fmitem.

   ASSIGN
      fmitem.Amount            = idamt 
      fmitem.BillCode          = icitemname
      fmitem.BillCycle         = 2
      fmitem.BillMethod        = FALSE
      fmitem.BillType          = "MF" 
      fmitem.Brand             = "1"
      fmitem.BrokenRental      = 1
      fmitem.FeeModel          = icFeemodel
      fmitem.FromDate          = TODAY
      fmitem.Interval          = 1
      fmitem.PriceList         = icPricelist
      fmitem.ServiceLimitGroup = ""
      fmitem.ToDate            = 12/31/49.
   END.
END FUNCTION.

/*********************************************************/
/* Add TMSParam                              */

FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icParam AS CHAR,
                                       INPUT icgroup AS CHAR,
                                       INPUT icValue AS CHAR):
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramcode EQ icParam NO-ERROR.
   IF NOT AVAIL TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.brand = "1"
         TMSParam.paramcode = icParam
         TMSParam.paramgroup = icgroup
         TMSParam.paramname = "PRO contracts"
         TMSParam.paramtype = "C"
         TMSParam.charval = icValue.
   END. 
   RETURN TRUE.
END FUNCTION.

FUNCTION fcreateCustcat RETURNS CHAR (
   INPUT icbasecat AS CHAR,
   INPUT icnr AS CHAR,
   INPUT icname AS CHAR,
   INPUT iiSubLimit AS INT,
   INPUT iiActLimit AS INT,
   INPUT ilpro AS LOG,
   INPUT icSegment AS CHAR).
   
   IF CAN-FIND (FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icnr) THEN RETURN "Already exist".
   FIND FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icbasecat NO-ERROR.
   IF NOT AVAIL CustCat THEN RETURN "Base not exist".
   CREATE bCustCat.
   BUFFER-COPY CustCat EXCEPT custcat.category TO bCustCat.   
   ASSIGN bCustCat.category = icnr
          bCustCat.catname = icname
          bCustCat.mobsublimit = iiSubLimit
          bCustCat.activationlimit = iiActLimit
          bCustCat.pro = ilpro
          bCustCat.segment = icsegment.
   DISP bcustcat.
   RELEASE bCustCat.
END FUNCTION.



fCreateBillItem("CONTPROMF").
/*fCreateBillItem("CONTFHPROMF").*/
faddTMSParam("PRO_CONTRACTS","Bundles","VOICE200,VOICE5000,SMS5000," +
             "INT_VOICE100,INT_FIX_VOICE1000,FLEX_UPSELL_5GB," +
             "FLEX_UPSELL_500MB,FIX_VOICE1000").
faddTMSParam("PRO_CHANNELS","YPRO",
             "Telesales_PRO,Fusion_Telesales_PRO").
faddTMSParam("NON_PRO_CHANNELS","YPRO",
             "self,POS,Fusion_POS,ccorder").

faddTMSParam("SVA_BO_EMAIL_ADDRESS","YPRO",
             "kari.aikas@qvantel.com").

fCreatePriceList("PRO_CONTDSL39").
fCreatePriceList("PRO_CONTFH39_50").
fCreatePriceList("PRO_CONTFH49_300").
fCreatePriceList("PRO_CONTDSL48").
fCreatePriceList("PRO_CONTFH48_50").
fCreatePriceList("PRO_CONTFH58_300").
fCreatePriceList("PRO_CONTDSL52").
fCreatePriceList("PRO_CONTFH52_50").
fCreatePriceList("PRO_CONTFH62_300").
fCreatePriceList("PRO_CONTDSL59").
fCreatePriceList("PRO_CONTFH59_50").
fCreatePriceList("PRO_CONTFH69_300").
fCreatePriceList("PRO_CONT10").
fCreatePriceList("PRO_CONT15").
fCreatePriceList("PRO_CONT26").
fCreatePriceList("PRO_CONT25").

fCreateFMItem("CONTPROMF","CONTDSLMF","PRO_CONTDSL39",2.77).
fCreateFMItem("CONTPROMF","CONTDSLMF","PRO_CONTDSL48",1.98).
fCreateFMItem("CONTPROMF","CONTDSLMF","PRO_CONTDSL52",7.02).
fCreateFMItem("CONTPROMF","CONTDSLMF","PRO_CONTDSL59",8.24).

fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH39_50",2.77).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH48_50",1.98).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH52_50",7.02).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH59_50",8.24).

fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH49_300",3.50).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH58_300",2.72).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH62_300",7.76).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH69_300",8.98).

fCreateFMItem("CONTPROMF","CONT10MF","PRO_CONT10",0.08).
fCreateFMItem("CONTPROMF","CONT15MF","PRO_CONT15",4.30).
fCreateFMItem("CONTPROMF","CONT25MF","PRO_CONT25",2.55).
fCreateFMItem("CONTPROMF","CONT26MF","PRO_CONT26",2.34).


/*YPRO-83: SVA configs */
/* DO NOT USE; these 
fCreateFMItem("FAXTOEMAILMF","FAXTOEMAILMF","COMMON",9.00). 
fCreateFMItem("OFFICE365MF","OFFICE365MF","COMMON",4.20). 
fCreateFMItem("SAGEONEMF","SAGEONEMF","COMMON",8.00). 
fCreateFMItem("IPFIJAMF","IPFIJAMF","COMMON",12.00).
fCreateFMItem("CENTRALITAMF","CENTRALITAMF","COMMON",0.00). 
*/

fcreateCustCat("20", "22", "PRO SOHO Company CIF", 25, 35, TRUE, "PRO-SOHO-COMPANY").
fcreateCustCat("20", "21", "Big Companies CIF", 25, 35, FALSE, "").
fcreateCustCat("20", "23", "SOHO Company CIF", 25, 35, FALSE, "SOHO-COMPANY").
fcreateCustCat("40", "42", "PRO Self-employee NIF", 5, 7, TRUE, "PRO-SOHO-AUTONOMO").
fcreateCustCat("41", "43", "PRO Self-employee NIE", 5, 7, TRUE, "PRO-SOHO-AUTONOMO").
fcreateCustCat("40", "44", "Self-employee NIF", 5, 7, FALSE, "SOHO-AUTONOMO").
fcreateCustCat("41", "45", "Self-employee NIE", 5, 7, FALSE, "SOHO-AUTONOMO").

FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "10".
   ASSIGN Custcat.segment = "CONSUMER"
          Custcat.catname = "Residential NIF".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "11".
   ASSIGN Custcat.segment = "CONSUMER"
          Custcat.catname = "Residential NIE".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "12".
   ASSIGN Custcat.segment = "CONSUMER".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "13".
   ASSIGN Custcat.segment = "CONSUMER".
FIND FIRST CustCat WHERE 
           custcat.brand EQ "1" AND
           custcat.category EQ "20".
   ASSIGN Custcat.catname = "Company CIF"
          Custcat.segment = "COMPANY".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "31".
   ASSIGN Custcat.catname = "VIP - MM group external customer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "40".
   ASSIGN Custcat.catname = "Self Employee NIF"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7
          Custcat.segment = "AUTONOMO".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "41".
   ASSIGN Custcat.catname = "Self Employee NIE"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7
          Custcat.segment = "AUTONOMO".

FOR EACH CustCat:
   CustCat.segment = CAPS(CustCat.segment).
END.

IF NOT CAN-FIND (FIRST Tmscodes WHERE
           tmscodes.codegroup EQ "order" AND
           tmscodes.fieldname EQ "orderchannel" AND
           tmscodes.codevalue EQ "fusion_telesales_pro") THEN DO:
   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "fusion_telesales" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.

   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "telesales" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.   

   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "CC" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.

END.

DEF TEMP-TABLE ttDaycampaign NO-UNDO LIKE Daycampaign.

FUNCTION fcreateDaycampaign RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icEvent AS CHAR,
                                             INPUT icname AS CHAR,
                                             INPUT icdctype AS CHAR,
                                             INPUT iiUpdateMode AS INT,
                                             INPUT iiTarget AS INT):
   FIND FIRST Daycampaign WHERE
              Daycampaign.brand EQ "1" AND
              Daycampaign.dcevent EQ icBaseDCEvent NO-ERROR.
      IF NOT AVAIL Daycampaign THEN DO:
         MESSAGE icbasedcevent + " Not found" VIEW-AS ALERT-BOX.
         RETURN FALSE.
      END.

      CREATE ttDaycampaign.
      BUFFER-COPY daycampaign TO ttDaycampaign.
      ASSIGN
      ttDaycampaign.dctype = icDctype
      ttDaycampaign.dcevent = icevent
      /*ttDaycampaign.billcode = icevent + "MF"
      ttDaycampaign.feemodel = icevent + "MF"*/
      ttDaycampaign.dcname = icName
      ttDaycampaign.bundleupsell = ""
      ttDaycampaign.bundletarget = iiTarget.

      IF iiUpdateMode NE 0 THEN DO:
         CREATE Daycampaign.
         BUFFER-COPY ttDaycampaign TO Daycampaign.
         DELETE ttDaycampaign. /*ror safety reasons*/
      END.
      ELSE DISP ttDayCampaign.

END.

/*  old ones, can be removec */
fcreateDaycampaign("FLEX_UPSELL","FLEX_UPSELL_500MB","Flex upsell national GPRS","6",limode,0).
fcreateDaycampaign("FLEX_UPSELL","FLEX_UPSELL_5GB","Flex upsell national GPRS","6",limode,0).
fcreateDaycampaign("VOICE100","VOICE5000","Cont national voice","1",limode,0).
fcreateDaycampaign("VOICE100","VOICE200","Cont national voice","1",limode,0).
fcreateDaycampaign("VOICE100","INT_VOICE100","Cont international voice","1",limode,0).
fcreateDaycampaign("VOICE100","FIX_VOICE1000","Fixed national voice","1",limode,1).
fcreateDaycampaign("VOICE100","INT_FIX_VOICE1000","Fixed international voice","1",limode,1).
fcreateDaycampaign("VOICE100","SMS5000","National SMS","1",limode,0).

FOR EACH TMSParam WHERE INDEX(TMSParam.charval,"FLEX_UPSELL") > 0:
   IF LOOKUP("FLEX_UPSELL_500MB", TMSParam.charval) eq 0 then
   TMSParam.charval = TMSParam.charval + ",FLEX_UPSELL_500MB,FLEX_UPSELL_5GB".
END.


FUNCTION create_group returns log (INPUT lcCode as CHAR, INPUT lcName AS CHAR):
   IF CAN-FIND(FIRST ServiceLimitgroup WHERE
                     Servicelimitgroup.brand = "1" AND
                     servicelimitgroup.groupcode EQ lcCode) THEN
      return false.
   CREATE servicelimitgroup.
   ASSIGN servicelimitgroup.brand = "1"
          servicelimitgroup.groupcode = lcCode
          servicelimitgroup.groupname = lcName
          servicelimitgroup.validfrom = ldafrom
          servicelimitgroup.validto = 12/31/49.
   return true.
END.

FUNCTION create_limit returns log (INPUT lcCode as CHAR,
                                   INPUT lcName AS CHAR,
                                   INPUT lclimit AS CHAR,
                                   INPUT ldAmt AS DEC,
                                   INPUT liDialType AS INT,
                                   INPUT liUnit AS INT,
                                   INPUT lcbasegroup AS CHAR):
   DEF VAR lcTempBI AS CHAR.

   IF CAN-FIND(FIRST ServiceLimit WHERE servicelimit.groupcode EQ lccode AND
                                  servicelimit.slname EQ lcName) THEN
      RETURN false.
   find last servicelimit  use-index slseq no-lock no-error.

           if not avail servicelimit then laskuri = 1.
           else laskuri = servicelimit.slseq + 1.

   CREATE servicelimit.
   ASSIGN servicelimit.dialtype = lidialtype
          servicelimit.inclunit = liUnit
          servicelimit.groupcode = lcCode
          servicelimit.slcode = lcCode + lcLimit
          servicelimit.slname = lcName
          servicelimit.validfrom = ldafrom
          servicelimit.validto = 12/31/49
          servicelimit.slseq = laskuri
          servicelimit.inclamt = ldAmt
          servicelimit.firstmonthcalc = 0
          servicelimit.lastmonthcalc = 0
          servicelimit.webdisp = 1.
   IF lcbaseGroup NE "SMS" THEN DO:
      FIND FIRST Servicelimit WHERE
                 Servicelimit.groupcode EQ lcbaseGroup NO-ERROR.
      IF AVAIL servicelimit THEN DO:
         FOR EACH servicelimittarget WHERE
                  servicelimittarget.slseq eq Servicelimit.slseq:
            CREATE bservicelimitTarget.
            BUFFER-COPY servicelimittarget EXCEPT slseq TO bservicelimittarget.
            ASSIGN bservicelimittarget.slseq = laskuri
                   bservicelimittarget.insiderate = lcCode.
         END.         
      END.
   END.
   ELSE DO:
      CREATE servicelimittarget.
      ASSIGN
         Servicelimittarget.InsideRate = lcCode
         Servicelimittarget.OutsideRate = ""
         Servicelimittarget.ServiceLimitMT = 0
         Servicelimittarget.ServiceLMember = "12100001"
         Servicelimittarget.SLSeq = laskuri.
      CREATE servicelimittarget.
      ASSIGN
         Servicelimittarget.InsideRate = lcCode
         Servicelimittarget.OutsideRate = ""
         Servicelimittarget.ServiceLimitMT = 0
         Servicelimittarget.ServiceLMember = "12100002"
         Servicelimittarget.SLSeq = laskuri.
      CREATE servicelimittarget.
      ASSIGN
         Servicelimittarget.InsideRate = lcCode
         Servicelimittarget.OutsideRate = ""
         Servicelimittarget.ServiceLimitMT = 0
         Servicelimittarget.ServiceLMember = "12100003"
         Servicelimittarget.SLSeq = laskuri.
      CREATE servicelimittarget.
      ASSIGN
         Servicelimittarget.InsideRate = lcCode
         Servicelimittarget.OutsideRate = ""
         Servicelimittarget.ServiceLimitMT = 0
         Servicelimittarget.ServiceLMember = "12100004"
         Servicelimittarget.SLSeq = laskuri.         
   END.
   return true.
END.

create_group("VOICE5000", "Voice 5000").
/*create_group("VOICE200", "Voice 200").*/
create_group("INT_VOICE100", "International Voice 100").
create_group("FIX_VOICE1000", "National fixed voice 1000").
create_group("INT_FIX_VOICE1000", "International fixed Voice 1000").
create_group("SMS5000", "SMS 5000").
create_group("FLEX_UPSELL_500MB", "Mobile Data Usage Flex Upsell 500MB").
create_group("FLEX_UPSELL_5GB", "Mobile Data Usage Flex Upsell 5GB").

create_limit("VOICE5000", "National calls", "_MIN",5000.0, 4, 1,"VOICE100").
/*create_limit("VOICE200", "National calls", "_MIN",200.0, 4, 1, "VOICE100").*/
create_limit("INT_VOICE100", "International calls", "_MIN",100.0, 4, 1,"VOICE100").
create_limit("FIX_VOICE1000", "National fixed calls", "_MIN",1000.0, 1, 1,"VOICE100").
create_limit("INT_FIX_VOICE1000", "International fixed calls", "_MIN",1000.0, 1, 1,"VOICE100").
create_limit("SMS5000", "National sms", "_QTY",5000.0, 5, 5,"SMS").
create_limit("FLEX_UPSELL_500MB", "Flex Upsell 500MB", "",500.0, 7, 4,"FLEX_UPSELL").
create_limit("FLEX_UPSELL_5GB", "Flex Upsell 5GB", "",5000.0, 7, 4,"FLEX_UPSELL").


FUNCTION fCreateSLGAnalyse RETURNS LOGICAL
   ( icClitype AS CHARACTER,
     icBillCode AS CHARACTER,
     iiCCN AS INTEGER,
     icBDest AS CHARACTER,
     icServiceLimitGroup AS CHARACTER,
     iiSLGAType AS INTEGER ):

   FIND FIRST SLGAnalyse EXCLUSIVE-LOCK WHERE
      SLGAnalyse.Brand    = "1"        AND
      SLGAnalyse.BelongTo = TRUE       AND
      SLGAnalyse.Clitype  = icClitype  AND
      SLGAnalyse.BillCode = icBillCode AND
      SLGAnalyse.CCN      = iiCCN      AND
      SLGAnalyse.BDest    = icBDest    AND
      SLGAnalyse.Prior    = 0          AND
      SLGAnalyse.ValidTo  = DATE(12,31,2049) AND
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup AND
      SLGAnalyse.SLGAType = iiSLGAType
   NO-ERROR.

   IF NOT AVAILABLE SLGAnalyse
   THEN CREATE SLGAnalyse.

   ASSIGN
      SLGAnalyse.Brand    = "1"
      SLGAnalyse.BelongTo = TRUE
      SLGAnalyse.Clitype  = icClitype
      SLGAnalyse.BillCode = icBillCode
      SLGAnalyse.CCN      = iiCCN
      SLGAnalyse.BDest    = icBDest
      SLGAnalyse.Prior    = 20
      SLGAnalyse.ValidFrom = ldaFrom
      SLGAnalyse.ValidTo  = DATE(12,31,2049)
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup
      SLGAnalyse.SLGAType = iiSLGAType
      .

END FUNCTION.


FUNCTION fcreateFixSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icbasegroup AS CHAR,
                                             INPUT icgroup AS CHAR):
   FIND FIRST SLGAnalyse where INDEX(SLGAnalyse.servicelimitgroup,
                                     icGroup) > 0 AND
                                     SLGANalyse.clitype EQ icclitype NO-ERROR.
   IF NOT AVAIL SLGanalyse THEN DO:     
      FOR EACH tempSLGAnalyse no-lock where INDEX(tempSLGAnalyse.servicelimitgroup, 
                                        icBaseGroup) > 0 AND 
                                        tempSLGANalyse.clitype EQ icclitype:
         CREATE bSLGAnalyse.
         BUFFER-COPY tempSLGAnalyse TO bSLGAnalyse.
         ASSIGN
            bSLGAnalyse.servicelimitgroup = icGroup.
               
      END.                            
   END.

END.

FUNCTION fcreateVoiceSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icgroup AS CHAR):
   fCreateSLGAnalyse(icClitype, "10100001", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100003", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100005", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFOTHER", 30, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFYOIGO", 30, "*", icGroup, 1).
END.

FUNCTION fcreateSMSSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icgroup AS CHAR):
   fCreateSLGAnalyse(icClitype, "12100001", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100002", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100003", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100004", 51, "*", icgroup, 1).
END.


DEF VAR lcListofclitypes AS CHAR.
DEF VAR liLoop AS INT.
DEF VAR lcCli AS CHAR.
lcListofclitypes = "CONT10,CONT15,CONT25,CONT26".

DO liLoop = 1 TO NUM-ENTRIES(lcListofClitypes):
   lcCli = ENTRY(liLoop,lcListOfClitypes).

   fcreateVoiceSLGAnalyses(lcCli,"VOICE5000").
   /*fcreateVoiceSLGAnalyses(lcCli,"VOICE200").*/
   fcreateVoiceSLGAnalyses(lcCli,"INT_VOICE100").
   fcreateSMSSLGAnalyses(lcCli,"SMS5000").

END.

lcListofclitypes = "CONTDSL39,CONTDSL48,CONTDSL52,CONTDSL59,CONTFH39_50,CONTFH48_50,CONTFH49_300,CONTFH52_50,CONTFH58_300,CONTFH59_50,CONTFH62_300,CONTFH69_300".

DO liLoop = 1 TO NUM-ENTRIES(lcListofClitypes):
   lcCli = ENTRY(liLoop,lcListOfClitypes).

   fcreateFixSLGAnalyses(lcCli,"CONTDSL","FIX_VOICE1000").
   fcreateFixSLGAnalyses(lcCli,"CONTDSL","INT_FIX_VOICE1000").
END.

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

END FUNCTION.

FUNCTION fGetNextBDestID RETURNS INTEGER ():

   DEFINE BUFFER BDest FOR BDest.

   FIND LAST BDest USE-INDEX BDestID NO-LOCK NO-ERROR.

   IF AVAILABLE BDest
   THEN RETURN BDest.BDestID + 1.

   RETURN 1.

END FUNCTION.

FUNCTION fGetNextTariffNum RETURNS INTEGER ():

   DEFINE BUFFER Tariff FOR Tariff.

   FIND LAST Tariff USE-INDEX tariffnum NO-LOCK NO-ERROR.

   IF NOT AVAILABLE Tariff
   THEN DO:
      CURRENT-VALUE(tariff) = 1.
      RETURN 1.
   END.

   CURRENT-VALUE(tariff) = Tariff.Tariffnum + 1.
   RETURN CURRENT-VALUE(tariff).

END FUNCTION.

FUNCTION fCreateMatrix RETURNS LOGICAL
   ( icMXName  AS CHARACTER,
     icMXKey   AS CHARACTER,
     iiMXRes   AS INTEGER,
     iiPrior   AS INTEGER ):

   FIND FIRST Matrix EXCLUSIVE-LOCK WHERE
      Matrix.Brand  = "1"      AND
      Matrix.MXKey  = icMXKey  AND
      Matrix.Prior  = iiPrior  AND
      Matrix.MXName = icMXName
   NO-ERROR.

   IF NOT AVAILABLE Matrix
   THEN CREATE Matrix.
   ELSE DO:
      giMXSeq       = Matrix.MXSeq.
      RETURN TRUE.
   END.
   IF Matrix.MXSeq = 0
   THEN Matrix.MXSeq = fGetNextMXSeq().

   ASSIGN
      giMXSeq       = Matrix.MXSeq
      Matrix.Brand  = "1"
      Matrix.MXKey  = icMXKey
      Matrix.Prior  = iiPrior
      Matrix.MXName = icMXName
      Matrix.MXRes  = iiMXRes.

END FUNCTION.

FUNCTION fCreateMXItem RETURNS LOGICAL
   ( iiMXSeq   AS INTEGER,
     icMXName  AS CHARACTER,
     icMXValue AS CHARACTER):

   FIND FIRST MXItem EXCLUSIVE-LOCK WHERE
      MXItem.MXSeq  = iiMXSeq AND
      MXItem.MXName = icMXName AND
      MXItem.MXValue = icMXValue
   NO-ERROR.

   IF NOT AVAILABLE MXItem
   THEN CREATE MXItem.

   ASSIGN
      MXItem.MXSeq   = iiMXSeq
      MXItem.MXName  = icMXName
      MXItem.MXValue = icMXValue.

END FUNCTION.

FUNCTION fCreateTariff RETURNS LOGICAL
   ( iiCCN AS INTEGER,
     icBillCode AS CHARACTER,
     icBDest AS CHARACTER,
     icPriceList AS CHARACTER ):

   FIND FIRST Tariff EXCLUSIVE-LOCK WHERE
      Tariff.Brand     = "1"            AND
      Tariff.CCN       = iiCCN          AND
      Tariff.PriceList = icPriceList    AND
      Tariff.BDest     = icBDest        AND
      Tariff.ValidFrom = DATE(6,1,2017) AND
      Tariff.ValidTo   = DATE(12,31,2052) AND
      Tariff.BillCode  = icBillCode
   NO-ERROR.

   IF NOT AVAILABLE Tariff
   THEN DO:
      CREATE Tariff.
      Tariff.TariffNum = fGetNextTariffNum().
   END.

   ASSIGN
      Tariff.Brand       = "1"
      Tariff.CCN         = iiCCN
      Tariff.PriceList   = icPriceList
      Tariff.BDest       = icBDest
      Tariff.ValidFrom   = DATE(6,1,2017)
      Tariff.ValidTo     = DATE(12,31,2052)
      Tariff.BillCode    = icBillCode
      Tariff.TZTo[1]     = "2400"
      Tariff.Discount[4] = YES
      Tariff.DayType[1]  = 1
      Tariff.TZName[1]   = "Off Peak"
      Tariff.TZName[2]   = "Peak"
      Tariff.TZName[3]   = "Off Peak"
      Tariff.TariffType  = 0
      Tariff.DataType    = 1.

END FUNCTION.

/* TMRItemValue */
FUNCTION fCreateTMRItemValue RETURNS LOGICAL
   ( iiTMRuleSeq AS INTEGER,
     icCounterItemValues AS CHARACTER ):

   FIND FIRST TMRItemValue EXCLUSIVE-LOCK WHERE
      TMRItemValue.TMRuleSeq = iiTMRuleSeq AND
      TMRItemValue.ToDate  = DATE(12,31,2049) AND
      TMRItemValue.CounterItemValues = icCounterItemValues
   NO-ERROR.

   IF NOT AVAILABLE TMRItemValue
   THEN DO:
      CREATE TMRItemValue.
   END.

   ASSIGN
      TMRItemValue.TMRuleSeq = iiTMRuleSeq
      TMRItemValue.FromDate  = DATE(6,1,2017)
      TMRItemValue.ToDate  = DATE(12,31,2049)
      TMRItemValue.CounterItemValues = icCounterItemValues.

END FUNCTION.

/* BDest */
FUNCTION fCreateBDest RETURNS LOGICAL
   ( icBDest AS CHARACTER,
     icBDName AS CHARACTER,
     iiCCN AS INTEGER ):

   FIND FIRST BDest EXCLUSIVE-LOCK WHERE
      BDest.Brand  = "1" AND
      BDest.CCN    = iiCCN AND
      BDest.BDName = icBDName AND
      BDest.BDest  = icBDest
   NO-ERROR.

   IF NOT AVAILABLE BDest
   THEN DO:
      CREATE BDest.
      BDest.BDestID = fGetNextBDestID().
   END.

   ASSIGN
      BDest.Brand    = "1"
      BDest.CCN      = iiCCN
      BDest.BDName   = icBDName
      BDest.BDest    = icBDest
      BDest.FromDate = DATE(6,1,2017)
      BDest.ToDate   = DATE(12,31,2049)
      .

END FUNCTION.
/* Verde */
fCreateMatrix("Convergent 5GB  mobile", "PERCONTR", 1, 40).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* Azul */
fCreateMatrix("CONTDSL59", "PERCONTR", 1, 44).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH59_50", "PERCONTR", 1, 45).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH69_300", "PERCONTR", 1, 46).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* Naranda */
fCreateMatrix("CONTDSL39", "PERCONTR", 1, 47).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH39_50", "PERCONTR", 1, 49).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH49_300", "PERCONTR", 1, 50).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* Morada */
fCreateMatrix("CONTDSL52", "PERCONTR", 1, 48).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH52_50", "PERCONTR", 1, 51).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

fCreateMatrix("CONTFH62_300", "PERCONTR", 1, 52).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* La infinita 5GB */
fCreateMatrix("CONT26", "PERCONTR", 1, 42).
/*fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").*/
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* La del cerp 1,5GB */
fCreateMatrix("CONT10", "PERCONTR", 1, 43).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
/*fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").*/
/*fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").*/
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* La Sinfin */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 41).
/*fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").*/
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_5GB").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").

/* La del cero 5GB */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 29).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
fCreateMXItem(giMXSeq, "PerContract", "OFFICE365").
fCreateMXItem(giMXSeq, "PerContract", "FAXTOEMAIL").
fCreateMXItem(giMXSeq, "PerContract", "CENTRALITA").
fCreateMXItem(giMXSeq, "PerContract", "SAGEONE").
fCreateMXItem(giMXSeq, "PerContract", "IPFIJA").


fCreateTariff(81,"VOICE5000","VOICE5000","CONTRATO8").
fCreateTariff(30,"VOICE5000CF","VOICE5000","CONTRATO8").
fCreateTariff(51,"SMS5000","SMS5000","CONTRATO8").
fCreateTariff(2,"INT_VOICE100","INT_VOICE100","CONTRATO8").

fCreateTariff(81,"VOICE5000","VOICE5000","CONTRATOS").
fCreateTariff(30,"VOICE5000CF","VOICE5000","CONTRATOS").
fCreateTariff(51,"SMS5000","SMS5000","CONTRATOS").
fCreateTariff(2,"INT_VOICE100","INT_VOICE100","CONTRATOS").

fCreateTariff(1081,"FIX_VOICE1000","FIX_VOICE1000","CONTRATOFIXED").
fCreateTariff(1002,"INT_FIX_VOICE1000","INT_FIX_VOICE1000","CONTRATOFIXED").


fCreateBDest("VOICE5000","Voice 5000 Minutes",81).
fCreateBDest("SMS5000","SMS 5000 QTY",51).
fCreateBDest("INT_VOICE100","International voice 100 Minutes",2).

fCreateBDest("FIX_VOICE1000","Fixed Voice 1000 Minutes",1081).
fCreateBDest("INT_FIX_VOICE1000","International Fixed Voice 1000 Minutes",1002).

fCreateTMRItemValue(43,"SMS5000").
fCreateTMRItemValue(44,"INT_VOICE100").
fCreateTMRItemValue(45,"FIX_VOICE1000").
fCreateTMRItemValue(46,"INT_FIX_VOICE1000").

/* new request status for SVA in 8 and 9 requests not needed, because no
   actions in these states
FIND FIRST requeststatus WHERE
           requeststatus.brand EQ "1" AND
           requeststatus.reqtype EQ 8 AND
           requeststatus.reqstatus EQ 6 NO-ERROR.
IF NOT AVAIL requeststatus THEN DO:
   FIND FIRST breqstatus WHERE
              breqstatus.brand EQ "1" AND
              breqstatus.reqtype EQ 8 AND
              breqstatus.reqstatus EQ 0 NO-ERROR.
   CREATE RequestStatus.
   BUFFER-COPY breqstatus EXCEPT breqstatus.reqstatus TO requeststatus.
   ASSIGN requeststatus.reqstatus = 6.
   CREATE RequestStatus.
   BUFFER-COPY breqstatus EXCEPT breqstatus.reqstatus TO requeststatus.
   ASSIGN requeststatus.reqstatus = 19.
END.

FIND FIRST requeststatus WHERE
           requeststatus.brand EQ "1" AND
           requeststatus.reqtype EQ 9 AND
           requeststatus.reqstatus EQ 6 NO-ERROR.
IF NOT AVAIL requeststatus THEN DO:
   FIND FIRST breqstatus WHERE
              breqstatus.brand EQ "1" AND
              breqstatus.reqtype EQ 9 AND
              breqstatus.reqstatus EQ 0 NO-ERROR.
   CREATE RequestStatus.
   BUFFER-COPY breqstatus EXCEPT breqstatus.reqstatus TO requeststatus.
   ASSIGN requeststatus.reqstatus = 6.
   CREATE RequestStatus.
   BUFFER-COPY breqstatus EXCEPT breqstatus.reqstatus TO requeststatus.
   ASSIGN requeststatus.reqstatus = 19.
END.
*/
