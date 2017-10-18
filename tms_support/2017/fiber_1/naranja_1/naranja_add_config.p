/*Additional configs to Azul 1GB*/
DEF VAR lcFromType AS CHAR NO-UNDO. 
DEF VAR lcToType AS CHAR NO-UNDO. 
DEF VAR lcFromBundle AS CHAR NO-UNDO. 
DEF VAR lcToBundle AS CHAR NO-UNDO. 
DEF VAR ldeProFee AS DEC NO-UNDO. 

ASSIGN
   lcFromType = "CONTFH49_300"
   lcToType   = "CONTFH69_1000"
   lcFromBundle = "CONTFH300"
   lcToBundle  = "CONTFH1000"
   ldeProFee = 3.98.   

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
FUNCTION fcreateRequestAction RETURNS LOGICAL (INPUT iiRequestActionID AS INT,
                                               INPUT iireqtype AS INT,
                                               INPUT icclitype AS CHAR,
                                               INPUT iiAction  AS INT,
                                               INPUT icActType AS CHAR,
                                               INPUT icKey     AS CHAR):
   DEF VAR liActionID AS INT NO-UNDO.
   def buffer brequestactionrule for RequestActionrule.

   FIND FIRST RequestAction WHERE
              RequestAction.brand      EQ "1" AND
              RequestAction.clitype    EQ icclitype AND
              RequestAction.reqtype    EQ iireqtype AND
              RequestAction.validto    GE TODAY AND
              RequestAction.actiontype EQ icActType AND
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
   ELSE liActionID = requestaction.requestactionid.
      
   FOR EACH requestactionrule NO-LOCK where
            requestactionrule.requestactionid = iiRequestActionID and
            requestactionrule.todate > today:
      create brequestactionrule.
      buffer-copy requestactionrule except requestactionid to brequestactionrule.
      brequestactionrule.requestactionid = liActionID.
   end.

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
      fmitem.pricelist = icPricelist and
      fmitem.todate >= today
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
                 bnew_SLGAnalyse.ServiceLimitGroup EQ 
                     REPLACE(slganalyse.servicelimitgroup,
                             lcFromBundle,lcToBundle) AND
                 bnew_SLGAnalyse.SLGAType EQ SLGAnalyse.SLGAType NO-ERROR.
      IF NOT AVAIL bnew_SLGAnalyse THEN DO:
         CREATE bnew_SLGAnalyse.
         BUFFER-COPY slganalyse EXCEPT  clitype validfrom TO bnew_SLGAnalyse.
         ASSIGN
            bnew_slganalyse.clitype = icToType
            bnew_slganalyse.servicelimitgroup = 
               REPLACE(bnew_slganalyse.servicelimitgroup,
                       lcFromBundle,lcToBundle)
            bnew_slganalyse.validfrom = today.
      END.

   END.

END FUNCTION.

FUNCTION fCreateDPTarget RETURNS LOGICAL
   (icBase AS CHAR,
    icNew AS CHAR):

   def buffer bDPTarget FOR dptarget.

    /*TODO: needs for each */
   FOR EACH dptarget NO-LOCK WHERE
            dptarget.targetkey eq icBase AND
            dptarget.validto ge TODAY:
      
      if lookup(string(dptarget.dpid),"36,41,44,50,51") > 0 then next.

      FIND FIRST bDPTarget NO-LOCK where
                 bDPTarget.DPId = dptarget.dpid and
                 bDPTarget.TargetTable = dptarget.TargetTable and
                 bDPTarget.TargetKey  = icNew and
                 bDPTarget.ValidTo = dptarget.validto no-error.
      IF AVAIL bDPTarget then next.

      CREATE bdptarget.
      BUFFER-COPY dptarget EXCEPT dpid validfrom targetkey TO bdptarget.
      ASSIGN
         bdptarget.validfrom = TODAY
         bdptarget.dpid = dptarget.dpid
         bdptarget.targetkey = icNew.
      

/* TODO : Create DPSubject */
   END.

END.

FUNCTION fCreateDPsubjects RETURNS LOGICAL
   (icBase AS CHAR,
    icNew AS CHAR):

   def buffer bDPSubject FOR dpsubject.

   FOR EACH dpsubject NO-LOCK WHERE
            dpsubject.dpsubject EQ icBase AND
            dpsubject.validto GE TODAY:

      if lookup(string(dpsubject.dpid),"36,41,44,50,51") > 0 then next.

      FIND FIRST bDPSubject NO-LOCK where
                 bDPSubject.dpid = dpsubject.dpid and
                 bDPSubject.dpsubject = icNew and
                 bDPSubject.validto GE TODAY NO-ERROR.
      IF AVAIL bDPSubject THEN NEXT.

      CREATE bDPSubject.
      BUFFER-COPY dpsubject EXCEPT validfrom dpsubject TO bDPSubject.

      assign
        bDPSubject.validfrom = TODAY
        bDPSubject.dpsubject = icNew. 

   END.
END.

/*DATA FIXING PART STARTS */
/*mxitem*/
/*Manual copying for matrix*/
FOR EACH mxitem EXCLUSIVE-LOCK  WHERE
         mxitem.mxvalue matches SUBST("*&1*",lcFromType):

   IF mxitem.mxvalue MATCHES  SUBST("*,&1*",lcFromType) THEN DO:
       mxitem.mxvalue =  mxitem.mxvalue + "," + lcToType.
   END.
   ELSE DO:
       FIND FIRST matrix where
                  matrix.mxseq = mxitem.mxseq no-error.
       IF AVAIL matrix and matrix.mxname eq lcFromType then next.
       fCreateMXItem(mxitem.mxseq, mxitem.mxname, lcToType).
   END.
END.

/*requestaction*/
DEF BUFFER brequestaction FOR requestaction.
FOR EACH brequestaction NO-LOCK WHERE
         brequestaction.clitype eq lcFromType:
   fCreateRequestAction(brequestaction.requestactionid,
                        brequestaction.reqtype,
                        lcToType,
                        brequestaction.action,
                        brequestaction.actiontype,
                        REPLACE(brequestaction.actionkey,
                                lcFromBundle,lcToBundle)).
END.


/*FMItem*/
fCreateFMItem(lcToType + "PRO",lcToBundle + "MF","PRO_" + lcToType, ldeProFee). /*value valvulated 12.45 / 1.21*/ 

/*pro pricelist*/
fCreatePriceList("PRO_" + lcToType).
/*
/*pro reptext*/
fCreateRepText(1, /*text type */
               lcToType + "PRO", /*LinkCode*/
               1, /*Language */
               "Ventajas PRO"). /* text */
fCreateRepText(1, /*text type */
               lcToType + "PRO", /*LinkCode*/
               2, /*Language */
               lcToType + "PRO"). /* text */
fCreateRepText(1, /*text type */
               lcToType + "PRO", /*LinkCode*/
               3, /*Language */
               "Abantaila PRO"). /* text */
fCreateRepText(1, /*text type */
               lcToType + "PRO", /*LinkCode*/
               5, /*Language */
               "PRO benefits"). /* text */
*/
/*slganalyse*/
fCopySLG(lcFromType,lcToType).

/*DPTarget */
/* TODO: check CONTH1000 DPTargets*/
fCreateDPTarget(lcFromType + "PRO",lcToType + "PRO").
fCreateDPTarget(lcFromType,lcToType).

/*DPSubject */
fCreateDPSubjects(lcFromType,lcToType).

FOR EACH tmsparam EXCLUSIVE-LOCK  WHERE
         tmsparam.charval matches SUBST("*&1*",lcFromType):

   if tmsparam.charval eq lcFromType then next. 

   if not tmsparam.charval MATCHES SUBST("*,&1",lcToType) then
      tmsparam.charval =  tmsparam.charval + "," + lcToType. 
END.
 
