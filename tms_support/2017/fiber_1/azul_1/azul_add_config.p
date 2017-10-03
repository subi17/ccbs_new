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

/*
pro pricelist

pro reptext


slganalyse

reptext

DPSubject 

DPTarget 
*/

