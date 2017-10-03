/*Additional configs to Azul 1GB*/

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

/* RequestAction */
FUNCTION fcreateRequestAction RETURNS LOGICAL (INPUT iireqtype AS INT,
                                               INPUT icclitype AS CHAR,
                                               INPUT iiAction  AS INT,
                                               INPUT icActType AS CHAR,
                                               INPUT icKey     AS CHAR):
   DEF VAR liActionID AS INT NO-UNDO.
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

FUNCTION fCreateRepText RETURNS LOGICAL
   (INPUT iiTextType AS INT,
    INPUT icLinkCode AS CHAR,
    INPUT iiLanguage AS INT,
    INPUT icRepText  AS CHAR):

   FIND FIRST RepText WHERE RepText.Brand      = "1"        AND
                      RepText.TextType = iiTextType AND
                      RepText.LinkCode = icLinkCode AND
                      RepText.Language = iiLanguage AND
                      RepText.ToDate   > TODAY      NO-LOCK NO-ERROR.
   IF NOT AVAIL RepText THEN
   DO:
      CREATE RepText.
      ASSIGN
          RepText.Brand    = "1"
          RepText.TextType = iiTextType
          RepText.LinkCode = icLinkCode
          RepText.Language = iiLanguage
          RepText.FromDate = TODAY
          RepText.ToDate   = DATE(12,31,2049)
          RepText.RepText  = icRepText.
   END.

   RETURN TRUE.

END FUNCTION.


FUNCTION fCopySLG RETURNS LOGICAL
   (INPUT icFromType AS CHAR,
    INPUT icToType AS CHAR):
   DEF BUFFER slganalyse for slganalyse.
   DEF BUFFER bnew_slganalyse for slganalyse.

   FOR EACH slganalyse NO-LOCK WHERE
            slganalyse.brand EQ "1" AND
            slganalyse.clitype EQ icFromType AND
            SLGAnalyse.ValidTo GE TODAY:

      FIND FIRST bnew_slganalyse WHERE
                 bnew_SLGAnalyse.Brand    EQ SLGAnalyse.Brand         AND
                 bnew_SLGAnalyse.BelongTo EQ SLGAnalyse.BelongTo      AND
                 bnew_SLGAnalyse.Clitype  EQ icToType                 AND
                 bnew_SLGAnalyse.BillCode EQ SLGAnalyse.BillCode      AND
                 bnew_SLGAnalyse.CCN      EQ SLGAnalyse.CCN           AND
                 bnew_SLGAnalyse.BDest    EQ SLGAnalyse.BDest         AND
                 bnew_SLGAnalyse.ValidTo  GE TODAY                    AND
                 bnew_SLGAnalyse.ServiceLimitGroup EQ SLGAnalyse.ServiceLimitGroup AND
                 bnew_SLGAnalyse.SLGAType EQ SLGAnalyse.SLGAType NO-ERROR.
      IF NOT AVAIL bnew_SLGAnalyse THEN DO:
         CREATE bnew_SLGAnalyse.
         BUFFER-COPY slganalyse EXCEPT  clitype validfrom TO bnew_SLGAnalyse.

      END.
      


   END.

END FUNCTION.


/*DATA FIXING PART STARTS */
/*mxitem*/
/*Manual copying for matrix*/
FOR EACH mxitem NO-LOCK WHERE
         mxitem.mxvalue matches "*contfh69_300*":

   IF mxitem.mxvalue MATCHES  "*,contfh69_300*" THEN DO:
       mxitem.mxvalue =  mxitem.mxvalue + ",CONTFH89_1000".
   END.
   ELSE DO:
       FIND FIRST matrix where
                  matrix.mxseq = mxitem.mxseq no-error.
       IF AVAIL matrix and matrix.mxname eq "CONTFH69_300" then next.
       fCreateMXItem(mxitem.mxseq, mxitem.mxname, "CONTFH89_1000").
   END.
END.

/*requestaction*/
DEF BUFFER brequestaction FOR requestaction.
FOR EACH brequestaction NO-LOCK WHERE
         brequestaction.actionkey eq "contfh69_300":
   fCreateRequestAction(brequestaction.reqtype,
                        brequestaction.clitype,
                        brequestaction.action,
                        brequestaction.actiontype,
                        "CONTFH89_1000").
END.


/*FMItem*/
fCreateFMItem("CONTPROMF","CONTFHMF","PRO_CONTFH89_1000",10.29). /*value valvulated 12.45 / 1.21*/

/*pro pricelist*/
fCreatePriceList("PRO_CONTFH89_1000").


/*pro reptext*/
fCreateRepText(1, /*text type */
               "CONTFH89_1000PRO", /*LinkCode*/
               1, /*Language */
               "Ventajas PRO"). /* text */
fCreateRepText(1, /*text type */
               "CONTFH89_1000PRO", /*LinkCode*/
               2, /*Language */
               "Avantatges PRO"). /* text */
fCreateRepText(1, /*text type */
               "CONTFH89_1000PRO", /*LinkCode*/
               3, /*Language */
               "Abantaila PRO"). /* text */
fCreateRepText(1, /*text type */
               "CONTFH89_1000PRO", /*LinkCode*/
               5, /*Language */
               "PRO benefits"). /* text */


/*slganalyse*/
fCopySLG("CONTFH69_300","CONTFH89_1000").

/*
DPSubject 

DPTarget 
*/

