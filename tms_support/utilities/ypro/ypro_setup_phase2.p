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


fUpdateTMSParam("PRO_CHANNELS", "YPRO", "POS_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "Fusion_POS_PRO").

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
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").

fCreateMatrix("CONTFH35_50", "PERCONTR", 1, 55).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").

fCreateMatrix("CONTFH45_300", "PERCONTR", 1, 54).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "FLEX_UPSELL_500MB").
fCreateMXItem(giMXSeq, "PerContract", "DSS_FLEX_UPSELL_500MB").

FIND FIRST REquestType WHERE 
           requesttype.reqtype EQ 95 NO-ERROR.
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

FUNCTION fcreateRequestAction RETURNS LOG (INPUT iireqtype AS INT,
                                           INPUT icclitype AS CHAR,
                                           INPUT iiAction  AS INT,
                                           INPUT icActType AS CHAR,
                                           INPUT icKey     AS CHAR):
   FIND FIRST RequestAction WHERE
              RequestAction.brand EQ "1" AND
              RequestAction.reqtype EQ iireqtype AND
              RequestAction.validto GE TODAY NO-ERROR.
   IF NOT AVAIL Requestaction THEN DO:
      FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.      
      
      CREATE Requestaction.
      ASSIGN
         REQUESTAction.brand = "1" 
         RequestAction.RequestActionID = liActionID         
         RequestAction.reqtype = iireqtype
         RequestAction.validto = 12/31/49
         RequestAction.action  = iiAction
         RequestAction.actiontype = icActType
         RequestAction.clitype = icclitype
         RequestAction.actionkey     = icKey.

   END.
END.
