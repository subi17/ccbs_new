

DEF BUFFER bCustCat FOR CustCat.
DEF BUFFER bTMSCodes FOR TMSCodes.
DEF BUFFER bservicelimitTarget FOR ServiceLimitTarget.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER tempSLGAnalyse FOR SLGAnalyse.
DEF BUFFER breqstatus FOR requeststatus.

DEF VAR ldaFrom AS DATE INIT TODAY.
DEF VAR limode AS INT INIT 1.
DEF VAR laskuri AS INT.

DEF TEMP-TABLE ttDiscountPlan NO-UNDO LIKE DiscountPlan.
DEF TEMP-TABLE ttDPTarget NO-UNDO LIKE DPTarget.
DEF TEMP-TABLE ttFeeModel NO-UNDO LIKE FeeModel.
DEF TEMP-TABLE ttDaycampaign NO-UNDO LIKE Daycampaign.

DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE giSlSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE liSimulate AS INTEGER NO-UNDO.
DEFINE VARIABLE llerror AS LOGICAL NO-UNDO.



liSimulate = 1.    /* UPDATE MODE, Discount plan */


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
      ELSE  MESSAGE "DPTarget exists / " + icMFFeeModel VIEW-AS ALERT-BOX.
      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.billcode = icDp AND
                 DiscountPlan.ValidTo > TODAY NO-ERROR.
      IF NOT AVAIL DiscountPlan THEN DO:
         CREATE DiscountPlan.
         BUFFER-COPY ttDiscountPlan TO Discountplan.
         DELETE ttDiscountPlan. /*for safety reasons*/
      END.
      ELSE  MESSAGE "DiscountPlan exists / " + icDp VIEW-AS ALERT-BOX.
      FIND FIRST DPRate WHERE
                 DPRate.dpid = liDpId NO-LOCK NO-ERROR.
      IF NOT AVAIL DPRate THEN DO:
         CREATE DPRate.
         DPRate.dpid = liDpId.
         DPRate.discValue = idAmt.
         DPRate.ValidTo = 12/31/49.
         DPRate.ValidFrom = idaValidFrom.
      END.
      ELSE  MESSAGE "DPRate exists / " + STRING(liDpId) VIEW-AS ALERT-BOX.
   END.
   IF AVAIL DPTarget THEN RELEASE DPTarget.
   RETURN TRUE.
END FUNCTION.

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
      FIND FIRST FeeModel WHERE
                 Feemodel.feeModel EQ icMFFeeModel NO-LOCK NO-ERROR.
      IF NOT AVAIL FeeModel THEN DO:
         CREATE FeeModel.
         BUFFER-COPY ttFeeModel TO FeeModel.
         DELETE ttFeeModel. /*for safety reasons*/
      END.
      ELSE  MESSAGE "FeeModel exists / " + icMFFeeModel VIEW-AS ALERT-BOX.
   END.
   IF AVAIL FeeModel THEN RELEASE FeeModel.
   RETURN TRUE.
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


FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

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

/* Verde */
fCreateMatrix("Convergent 5GB  mobile", "PERCONTR", 1, 40).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* Azul */
fCreateMatrix("CONTDSL59", "PERCONTR", 1, 44).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH59_50", "PERCONTR", 1, 45).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH69_300", "PERCONTR", 1, 46).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* Naranda */
fCreateMatrix("CONTDSL39", "PERCONTR", 1, 47).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH39_50", "PERCONTR", 1, 49).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH49_300", "PERCONTR", 1, 50).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* Morada */
fCreateMatrix("CONTDSL52", "PERCONTR", 1, 48).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH52_50", "PERCONTR", 1, 51).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

fCreateMatrix("CONTFH62_300", "PERCONTR", 1, 52).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* La infinita 5GB */
fCreateMatrix("CONT26", "PERCONTR", 1, 42).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* La del cerp 1,5GB */
fCreateMatrix("CONT10", "PERCONTR", 1, 43).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* La Sinfin */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 41).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").

/* La del cero 5GB */
fCreateMatrix("Per.contract usage", "PERCONTR", 1, 29).
fCreateMXItem(giMXSeq, "PerContract", "ANGELTECH").
fCreateMXItem(giMXSeq, "PerContract", "MANITAS").


fcreateDaycampaign("AGILETV","ANGELTECH","Angel Tecnológico","7",limode,0).
fcreateDaycampaign("AGILETV","MANITAS","Manitas Oficina","7",limode,0).


llError = fcreateDiscountPlan ("CONT15MF", "ANGELTECHMF", 12/01/17,
                           "CONT15DISC", "ANGELTECHDISC_1",
                           "Angel technology SVA discount for 3 months", 100,
                           liSimulate).

llError = fcreateDiscountPlan ("DATA6MF", "ANGELTECHMF", 12/01/17,
                           "BONO6DISC", "ANGELTECHDISC_2", 
                           "Angel technology SVA discount for next 9 months", 1, 
                           liSimulate).


llError = fcreateDiscountPlan ("CONT15MF", "MANITASMF", 12/01/17,
                           "CONT15DISC", "MANITASDISC",
                           "Manitas Oficina SVA discount", 100,
                           liSimulate).

/* SVA configs */

fcreateFeeModel("OFFICE365MF","ANGELTECHMF","Angel Tecnológico",lisimulate).
fcreateFeeModel("OFFICE365MF","MANITASMF","Manitas Oficina",lisimulate).

fCreateFMItem("ANGELTECHMF","ANGELTECHMF","COMMON",3.00).
fCreateFMItem("MANITASMF","MANITASMF","COMMON",5.00).
