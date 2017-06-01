/* It should be safe to run this multiple times as it will just
   update the data if it exists already */

DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE giSlSeq AS INTEGER NO-UNDO.

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

FUNCTION fGetNextSlSeq RETURNS INTEGER ():

   DEFINE BUFFER ServiceLimit FOR ServiceLimit.

   FIND LAST ServiceLimit USE-INDEX slseq NO-LOCK NO-ERROR.

   IF AVAILABLE ServiceLimit
   THEN RETURN ServiceLimit.SlSeq + 1.

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
      SLGAnalyse.Prior    = 0
      SLGAnalyse.ValidFrom = DATE(5,1,2017)
      SLGAnalyse.ValidTo  = DATE(12,31,2049)
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup
      SLGAnalyse.SLGAType = iiSLGAType
      .
      
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
      BillItem.BIName = "Cont national voice"
      BillItem.AccNum = 70510100
      BillItem.BIGroup = "1"
      BillItem.EUAccNum = 70510100
      BillItem.FSAccNum = 70510100
      BillItem.Brand = "1"
      BillItem.EUConAccNum = 70510100
      BillItem.CostCentre = "SL"
      BillItem.AltAccNum = 70510100
      BillItem.TaxClass = "1"
      BillItem.SAPRid = "022"
      BillItem.VIPAccNum = 70510100.   

END FUNCTION.


/* DayCampaign */
FUNCTION fCreateDayCampaign RETURNS LOGICAL
   ( icDCEvent AS CHARACTER ):
   
   FIND FIRST DayCampaign EXCLUSIVE-LOCK WHERE
      DayCampaign.Brand     = "1"       AND
      DayCampaign.DCEvent = icDCEvent
   NO-ERROR.
   
   IF NOT AVAILABLE DayCampaign
   THEN DO:
      CREATE DayCampaign.
   END.
   
   ASSIGN
      DayCampaign.Brand     = "1"
      DayCampaign.DCEvent = icDCEvent
      DayCampaign.ValidFrom = DATE(6,1,2017)
      DayCampaign.ValidTo   = DATE(12,31,2049)
      DayCampaign.DCName = "Promo 200 min/mes gratis"
      DayCampaign.DCType = "1"
      DayCampaign.CalcMethod = 1
      DayCampaign.DurType = 1
      DayCampaign.Effective = 1
      DayCampaign.PayType = 1.

END FUNCTION.

/* RepText */
FUNCTION fCreateRepText RETURNS LOGICAL
   ( iiTextType AS INTEGER,
     icLinkCode AS CHARACTER,
     iiLanguage AS INTEGER,
     icRepText  AS CHARACTER ):
   
   FIND FIRST RepText EXCLUSIVE-LOCK WHERE
      RepText.Brand     = "1"       AND
      RepText.TextType = iiTextType AND
      RepText.LinkCode = icLinkCode AND
      RepText.Language = iiLanguage AND
      RepText.ToDate   = DATE(12,31,2049)
   NO-ERROR.
   
   IF NOT AVAILABLE RepText
   THEN DO:
      CREATE RepText.
   END.
   
   ASSIGN
      RepText.Brand     = "1"
      RepText.TextType = iiTextType
      RepText.LinkCode = icLinkCode
      RepText.Language = iiLanguage
      RepText.FromDate = DATE(6,1,2017)
      RepText.ToDate   = DATE(12,31,2049)
      RepText.RepText  = icRepText.

END FUNCTION.

/* ServiceLimitGroup */
FUNCTION fCreateServiceLimitGroup RETURNS LOGICAL
   ( icGroupCode AS CHARACTER ):
   
   FIND FIRST ServiceLimitGroup EXCLUSIVE-LOCK WHERE
      ServiceLimitGroup.Brand      = "1" AND
      ServiceLimitGroup.GroupCode  = icGroupCode
   NO-ERROR.
   
   IF NOT AVAILABLE ServiceLimitGroup
   THEN CREATE ServiceLimitGroup.
   
   ASSIGN
      ServiceLimitGroup.Brand    = "1"
      ServiceLimitGroup.GroupCode = icGroupCode
      ServiceLimitGroup.GroupName = "Voice 200 Minutes Package"
      ServiceLimitGroup.ValidFrom = DATE(6,1,2017)
      ServiceLimitGroup.ValidTo = DATE(12,31,2049).

END FUNCTION.

/* ServiceLimit */
FUNCTION fCreateServiceLimit RETURNS LOGICAL
   ( icGroupCode AS CHARACTER ):
   
   FIND FIRST ServiceLimit EXCLUSIVE-LOCK WHERE
      ServiceLimit.GroupCode  = icGroupCode
   NO-ERROR.
   
   IF NOT AVAILABLE ServiceLimit
   THEN DO:
      CREATE ServiceLimit.
      ServiceLimit.SlSeq = fGetNextSlSeq().
   END.
   
   ASSIGN
      giSlSeq       = ServiceLimit.SlSeq
      ServiceLimit.GroupCode = icGroupCode
      ServiceLimit.WebDisp = 0
      ServiceLimit.DialType = 4
      ServiceLimit.SLCode = "VOICE200_MIN"
      ServiceLimit.SLName = "Voice 200 Minutes Package"
      ServiceLimit.InclUnit = 1
      ServiceLimit.InclAmt = 200
      ServiceLimit.ValidFrom = DATE(6,1,2017)
      ServiceLimit.ValidTo = DATE(12,31,2049)
      ServiceLimit.BDestLimit = 100.

END FUNCTION.

/* ServiceLimitTarget */
FUNCTION fCreateServiceLimitTarget RETURNS LOGICAL
   ( iiSlSeq AS INTEGER,
     icServiceLMember AS CHARACTER,
     icInsideRate AS CHARACTER ):
   
   FIND FIRST ServiceLimitTarget EXCLUSIVE-LOCK WHERE
      ServiceLimitTarget.SlSeq  = iiSlSeq AND
      ServiceLimitTarget.ServiceLMember  = icServiceLMember AND
      ServiceLimitTarget.InsideRate  = icInsideRate
   NO-ERROR.
   
   IF NOT AVAILABLE ServiceLimitTarget
   THEN DO:
      CREATE ServiceLimitTarget.
   END.
   
   ASSIGN
      ServiceLimitTarget.SlSeq  = iiSlSeq
      ServiceLimitTarget.ServiceLMember  = icServiceLMember
      ServiceLimitTarget.InsideRate  = icInsideRate.

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


/* In 5.6.2017 Deployment we need following configuration changes */

fCreateTariff(81,"VOICE200","VOICE200","CONTRATO8").
fCreateTariff(30,"VOICE200CF","VOICE200","CONTRATO8").

fCreateBDest("VOICE200","Voice 200 Minutes",81).
fCreateBillItem("VOICE200").
fCreateBillItem("VOICE200CF").
fCreateDayCampaign("VOICE200").
fCreateServiceLimitGroup("VOICE200").
fCreateServiceLimit("VOICE200").
fCreateServiceLimitTarget(giSlSeq, "10100001","VOICE200").
fCreateServiceLimitTarget(giSlSeq, "10100003","VOICE200").
fCreateServiceLimitTarget(giSlSeq, "10100005","VOICE200").
fCreateServiceLimitTarget(giSlSeq, "CFOTHER","VOICE200").
fCreateServiceLimitTarget(giSlSeq, "CFYOIGO","VOICE200").
fCreateTMRItemValue(40,"VOICE200").
fCreateTMRItemValue(41,"VOICE200").

FIND TMRule EXCLUSIVE-LOCK WHERE TMRule.TMRuleSeq = 40 NO-ERROR.
IF AVAILABLE TMRule
THEN TMRule.Name = "VOICE100/200 Minutes".

FIND TMRule EXCLUSIVE-LOCK WHERE TMRule.TMRuleSeq = 41 NO-ERROR.
IF AVAILABLE TMRule
THEN TMRule.Name = "VOICE100/200 BDestination".

fCreateRepText(1,"VOICE200",1,"En 200 min").
fCreateRepText(1,"VOICE200",2,"En 200 min").
fCreateRepText(1,"VOICE200",3,"200 minutuan").
fCreateRepText(1,"VOICE200",5,"In 200 min").
fCreateRepText(1,"VOICE200CF",1,"En 200 min").
fCreateRepText(1,"VOICE200CF",2,"En 200 min").
fCreateRepText(1,"VOICE200CF",3,"200 minutuan").
fCreateRepText(1,"VOICE200CF",5,"In 200 min").

FIND InvText EXCLUSIVE-LOCK WHERE InvText.ItNum = 479 NO-ERROR.
IF AVAILABLE InvText
THEN InvText.TxtTitle = "VOICE100 or VOICE200 minutes limit is exceeded".

/* CONTDSL48, CONTFH48_50, CONTFH58_300: giMXSeq should be 115 after the call */
fCreateMatrix("Convergent 5GB  mobile", "PERCONTR", 1, 40).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateSLGAnalyse("CONTDSL48", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL48", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL48", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL48", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL48", "CFYOIGO", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH48_50", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH48_50", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH48_50", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH48_50", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH48_50", "CFYOIGO", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH58_300", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH58_300", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH58_300", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH58_300", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH58_300", "CFYOIGO", 30, "*", "VOICE200", 1).

/* CONTDSL39: giMXSeq should be 124 after the call */
fCreateMatrix("CONTDSL39", "PERCONTR", 1, 47).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateSLGAnalyse("CONTDSL39", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL39", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL39", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL39", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTDSL39", "CFYOIGO", 30, "*", "VOICE200", 1).

/* CONTFH39_50: giMXSeq should be 126 after the call */
fCreateMatrix("CONTFH39_50", "PERCONTR", 1, 49).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateSLGAnalyse("CONTFH39_50", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH39_50", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH39_50", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH39_50", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH39_50", "CFYOIGO", 30, "*", "VOICE200", 1).

/* CONTFH49_300: giMXSeq should be 127 after the call */
fCreateMatrix("CONTFH49_300", "PERCONTR", 1, 50).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateSLGAnalyse("CONTFH49_300", "10100001", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH49_300", "10100003", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH49_300", "10100005", 81, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH49_300", "CFOTHER", 30, "*", "VOICE200", 1).
fCreateSLGAnalyse("CONTFH49_300", "CFYOIGO", 30, "*", "VOICE200", 1).




/* After the 5.6.2017 deployment following can be used (maybe?) */

/*
/* Create new tariffs */
fCreateTariff(81, "VOICE100").
fCreateTariff(30, "VOICE100CF").

/* CONTF8: giMXSeq should get a new value after the call */
fCreateMatrix("CONTF8", "PERCONTR", 1, 54).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateMXItem(giMXSeq, "SubsTypeTo", "CONTF8").
fCreateSLGAnalyse("CONTF", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONTF", "CFYOIGO", 30, "*", "VOICE100", 1).

/* CONT9: giMXSeq should be 93 after the call */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 28).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateSLGAnalyse("CONT9", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT9", "CFYOIGO", 30, "*", "VOICE100", 1).

/* CONT10: giMXSeq should be 120 after the call */
fCreateMatrix("CONT10", "PERCONTR", 1, 43).
fCreateMXItem(giMXSeq, "PerContract", "VOICE100").
fCreateSLGAnalyse("CONT10", "10100001", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "10100003", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "10100005", 81, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "CFOTHER", 30, "*", "VOICE100", 1).
fCreateSLGAnalyse("CONT10", "CFYOIGO", 30, "*", "VOICE100", 1).

*/
