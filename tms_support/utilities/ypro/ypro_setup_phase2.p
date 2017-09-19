/* YPRO phase 2 setups */
{Syst/tmsconst.i}
DEF BUFFER btmscodes for tmscodes.
DEF VAR liactionid AS INT No-UNDO.
FUNCTION fUpdateTMSParam RETURNS LOGICAL (INPUT icParam AS CHAR,
                                       INPUT icgroup AS CHAR,
                                       INPUT icValue AS CHAR):
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramcode EQ icParam NO-ERROR.
   IF AVAIL TMSParam THEN DO:
         IF LOOKUP(icValue, TMSParam.charval) EQ 0 THEN
            TMSParam.charval = TMSParam.charval + "," + icValue.
   END.
   RETURN TRUE.
END FUNCTION.
/*
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "cc").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "pos").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Fusion_POS").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "renewal").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "renewal_telesales").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Renewal_CTC").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Renewal_POS").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Renewal_POS_STC").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "self").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "telesales").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Retention").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "Retention_STC").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "emission").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "fusion_telesales").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "fusion_emission").
fUpdateTMSParam("NON_PRO_CHANNELS", "YPRO", "fusion_cc").
*/

fUpdateTMSParam("PRO_CHANNELS", "YPRO", "telesalesorder_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "cc_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "emission_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "fusion_ccorder_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "fusion_emission_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "fusion_telesales_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "telesales_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "fusion_POS_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "pos_PRO").

IF NOT CAN-FIND (FIRST Tmscodes WHERE
           tmscodes.codegroup EQ "order" AND
           tmscodes.fieldname EQ "orderchannel" AND
           tmscodes.codevalue EQ "pos_pro") THEN DO:
   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "pos" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.
END.

IF NOT CAN-FIND (FIRST Tmscodes WHERE
           tmscodes.codegroup EQ "order" AND
           tmscodes.fieldname EQ "orderchannel" AND
           tmscodes.codevalue EQ "fusion_pos_pro") THEN DO:
   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "fusion_pos" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.
END.


DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER tempSLGAnalyse FOR SLGAnalyse.

DEF VAR ldaFrom AS DATE INIT TODAY.
DEF VAR limode AS INT INIT 1.
DEF VAR laskuri AS INT.

DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE giSlSeq AS INTEGER NO-UNDO.


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
   END.

   ASSIGN
      BillItem.Brand     = "1"
      BillItem.BillCode = icBillCode
      BillItem.BIName = "La De Casa PRO monthly fee"
      BillItem.AccNum = 70518100
      BillItem.BIGroup = "46"
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


fCreateBillItem("CONTDSL35PRO").
fCreateBillItem("CONTFH35_50PRO").
fCreateBillItem("CONTFH45_300PRO").

fCreatePriceList("PRO_CONTDSL35").
fCreatePriceList("PRO_CONTFH35_50").
fCreatePriceList("PRO_CONTFH45_300").

fCreateFMItem("CONTPROMF","CONTDSLMF","PRO_CONTDSL35",3.07).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH35_50",3.07).
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH45_300",2.81).

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


DEF VAR lcListofclitypes AS CHAR.
DEF VAR liLoop AS INT.
DEF VAR lcCli AS CHAR.

lcListofclitypes = "CONTDSL35,CONTFH35_50,CONTFH45_300".

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

/* Casa */
fCreateMatrix("CONTDSL35", "PERCONTR", 1, 53).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
/*fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").
*/
fCreateMatrix("CONTFH35_50", "PERCONTR", 1, 55).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
/*fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").
*/
fCreateMatrix("CONTFH45_300", "PERCONTR", 1, 54).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
/*fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").
*/
FIND FIRST REquestType WHERE 
           requesttype.reqtype EQ {&REQTYPE_PRO_MIGRATION} NO-ERROR.
IF NOT AVAIL requesttype THEN DO:
   CREATE Requesttype.
   ASSIGN
      RequestType.Brand    = "1"
      RequestType.Queue    = 2
      RequestType.ReqType  = {&REQTYPE_PRO_MIGRATION}
      RequestType.InUse    = TRUE
      RequestType.ReqName  = "Pro migration"
      RequestType.Program  = ""
      RequestType.UserCode = "ProMig".
END.

FIND FIRST REquestStatus WHERE
           requeststatus.brand EQ "1" AND
           requestStatus.reqtype EQ {&REQTYPE_PRO_MIGRATION} AND
           requestStatus.reqstatus EQ 0 NO-ERROR.
IF NOT AVAIL requeststatus THEN DO:
   CREATE Requeststatus.
   ASSIGN
      RequestStatus.Brand    = "1"
      RequestStatus.ReqType  = {&REQTYPE_PRO_MIGRATION}
      RequestStatus.reqstatus    = 0
      RequestStatus.Program  = "Mc/promigrate.p".
END.
FIND FIRST REquestStatus WHERE
           requeststatus.brand EQ "1" AND
           requestStatus.reqtype EQ {&REQTYPE_PRO_MIGRATION} AND
           requestStatus.reqstatus EQ 8 NO-ERROR.
IF NOT AVAIL requeststatus THEN DO:
   CREATE Requeststatus.
   ASSIGN
      RequestStatus.Brand    = "1"
      RequestStatus.ReqType  = {&REQTYPE_PRO_MIGRATION}
      RequestStatus.reqstatus    = 8
      RequestStatus.Program  = "Mc/promigrate.p".
END.

/* RequestAction */
FUNCTION fcreateRequestAction RETURNS LOGICAL (INPUT iireqtype AS INT,
                                               INPUT icclitype AS CHAR,
                                               INPUT iiAction  AS INT,
                                               INPUT icActType AS CHAR,
                                               INPUT icKey     AS CHAR):
   FIND FIRST RequestAction WHERE
              RequestAction.brand      EQ "1" AND
              RequestAction.clitype    EQ icclitype AND
              RequestAction.reqtype    EQ iireqtype AND
              RequestAction.validto    GE TODAY AND
              RequestAction.action     EQ iiAction AND
              RequestAction.actionKey  EQ icKey NO-ERROR.
   IF NOT AVAIL Requestaction THEN DO:
      FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.      
      
      CREATE Requestaction.
      ASSIGN
         RequestAction.brand = "1" 
         RequestAction.RequestActionID = liActionID         
         RequestAction.reqtype         = iireqtype
         RequestAction.validfrom       = TODAY
         RequestAction.validto         = 12/31/49
         RequestAction.action          = iiAction
         RequestAction.actiontype      = icActType
         RequestAction.clitype         = icclitype
         RequestAction.actionkey       = icKey.

   END.
END FUNCTION.

/* RequestActionRule */
FUNCTION fCreateRequestActionRule RETURNS LOGICAL
   (iiRequestActionID AS INT,
    icParamField      AS CHAR,
    icParamValue      AS CHAR):

   FIND FIRST RequestActionRule EXCLUSIVE-LOCK WHERE
              RequestActionRule.RequestActionID = iiRequestActionID AND
              RequestActionRule.ParamField      = icParamField AND
              RequestActionRule.ToDate          = DATE(12,31,2049) NO-ERROR.

   IF NOT AVAILABLE RequestActionRule THEN 
      CREATE RequestActionRule.

   ASSIGN
      RequestActionRule.RequestActionID = iiRequestActionID
      RequestActionRule.ParamField      = icParamField
      RequestActionRule.FromDate        = TODAY
      RequestActionRule.ToDate          = DATE(12,31,2049)
      RequestActionRule.ParamValue      = icParamValue.

END FUNCTION.

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL35",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL52",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL59",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH35_50",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH48_50",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH52_50",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH59_50",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH45_300",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH58_300",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH62_300",1,"DayCampaign","FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH69_300",1,"DayCampaign","FIX_VOICE1000").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL35",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL52",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL59",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH35_50",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH48_50",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH52_50",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH59_50",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH45_300",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH58_300",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH62_300",1,"DayCampaign","INT_FIX_VOICE1000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH69_300",1,"DayCampaign","INT_FIX_VOICE1000").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL35",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL52",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL59",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH35_50",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH48_50",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH52_50",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH59_50",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH45_300",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH58_300",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH62_300",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH69_300",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT10",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT15",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT25",1,"DayCampaign","SMS5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT26",1,"DayCampaign","SMS5000").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL35",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL52",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL59",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH35_50",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH48_50",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH52_50",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH59_50",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH45_300",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH58_300",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH62_300",1,"DayCampaign","INT_VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH69_300",1,"DayCampaign","INT_VOICE100").
/*
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL35",1,"DayCampaign","FLEX_500MB_UPSELL").
*/
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","FLEX_500MB_UPSELL").
/*
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH35_50",1,"DayCampaign","FLEX_500MB_UPSELL").
*/
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","FLEX_500MB_UPSELL").
/*
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH45_300",1,"DayCampaign","FLEX_500MB_UPSELL").
*/
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","FLEX_500MB_UPSELL").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL52",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL59",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH52_50",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH59_50",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH62_300",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH69_300",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT25",1,"DayCampaign","FLEX_5GB_UPSELL").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT26",1,"DayCampaign","FLEX_5GB_UPSELL").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH39_50",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH48_50",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH49_300",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH58_300",1,"DayCampaign","VOICE5000").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT15",1,"DayCampaign","VOICE5000").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT10",1,"DayCampaign","VOICE200").

/* STC => 2P */
/*fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL35",1,"DayCampaign","FLEX_500MB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH35_50",1,"DayCampaign","FLEX_500MB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH45_300",1,"DayCampaign","FLEX_500MB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
*/

/* STC 2P to 3P */

/* Naranja 20 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Verde 20 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL48",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL48",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL48",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Morada 20 */
/*
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL52",1,"DayCampaign","VOICE5000").
*/
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL52",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL52",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL52",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Azul 20 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL59",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL59",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL59",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL59",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Naranja 50 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Verde 50 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH48_50",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH48_50",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH48_50",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Morada 50 */
/*
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH52_50",1,"DayCampaign","VOICE5000").
*/
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH52_50",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH52_50",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH52_50",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Azul 50 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH59_50",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH59_50",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH59_50",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH59_50",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Naranja 300 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Verde 300 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH58_300",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH58_300",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH58_300",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Morada 300 */
/*
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH62_300",1,"DayCampaign","VOICE5000").
*/
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH62_300",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH62_300",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH62_300",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

/* Azul 300 */
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH69_300",1,"DayCampaign","VOICE5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH69_300",1,"DayCampaign","INT_VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH69_300",1,"DayCampaign","SMS5000").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH69_300",1,"DayCampaign","FLEX_5GB_UPSELL").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

FUNCTION fAddDFField RETURNS LOGICAL (INPUT icdumpname AS CHAR,
                                      INPUT icField AS CHAR,
                                      INPUT icTable AS CHAR,
                                      INPUT icLabel AS CHAR,
                                      INPUT iicolumn AS INT):
   FIND FIRST Dumpfile WHERE
              Dumpfile.brand EQ "1" AND
              Dumpfile.dumpname EQ icDumpname NO-ERROR.
   IF NOT AVAIL Dumpfile THEN RETURN FALSE.
   FIND FIRST DFField WHERE
              DFField.dumpid EQ dumpfile.dumpid AND
              DFField.ordernbr EQ iicolumn NO-ERROR.
   IF NOT AVAIL DFField THEN DO:
      CREATE DFField.
      ASSIGN
         DFField.brand = "1"
         DFField.dumpid = dumpfile.dumpid
         DFField.dffield = icField
         DFField.dftable = icTable
         DFField.dflabel = icLabel
         DFField.ordernbr = iicolumn
         Dffield.fromdate = TODAY
         DFField.todate = 12/31/49.

   END.
END FUNCTION.

fadddffield("DiscountMember","#Segment","DPMember","Segment",9).
fadddffield("PerContrDump","#Segment","DCCLI","Segment",21).
fadddffield("RequestDumpTXT","#Segment","MSRequest","Segment",39).
