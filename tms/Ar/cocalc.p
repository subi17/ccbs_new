/* ------------------------------------------------------
  MODULE .......: cocalc.p
  FUNCTION .....: commission calculation
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 22.01.03
  MODIFIED .....: 14.11.03/aam CoBasis,
                               CoRule.BasisType,OpenDays,ParentRule,
                               new structures
                  23.02.03 kl fixes after index changes
                  29.03.04/aam check CoBasis also for basistype 3
                  14.05.04/aam delete CoEvent when recalculated
                  01.06.04/aam CLIType,
                               CoAmt and CoPerc moved from CoTarg -> CoBasis
                  23.06.04/aam check MsOwner.ms-minotype             
                  29.06.04/aam new input; EventCust & CoRuleId
                  30.08.04/aam brand
                  26.11.04/aam special handling for reseller "RF" 
                  30.06.05/aam CoRule.CLIType can contain a list,
                               use previous operator as basis 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
{Func/fprevoper.i}

DEF INPUT  PARAMETER icInvGroup   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiRuleCust1  AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiRuleCust2  AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiEventCust1 AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiEventCust2 AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCoRuleID   AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtDate1     AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2     AS DATE NO-UNDO.
DEF INPUT  PARAMETER icReseller   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icSalesman   AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER ilRecalc     AS LOG  NO-UNDO. 
DEF INPUT  PARAMETER idtDelete    AS DATE NO-UNDO. 
DEF OUTPUT PARAMETER oiEvents     AS INT  NO-UNDO.

DEF VAR llFound    AS LOGIC NO-UNDO. 
DEF VAR ldAmt      AS DEC   NO-UNDO.
DEF VAR lcSalesman AS CHAR  NO-UNDO. 
DEF VAR liCustNum  AS INT   NO-UNDO. 
DEF VAR lcParent   AS CHAR  NO-UNDO. 
DEF VAR ldtDate    AS DATE  NO-UNDO.
DEF VAR ldtTime    AS INT   NO-UNDO.
DEF VAR ldBase     AS DEC   NO-UNDO.
DEF VAR liCount    AS INT   NO-UNDO. 
DEF VAR ldCommBase AS DEC   NO-UNDO. 
DEF VAR ldArpuBase AS DEC   NO-UNDO. 
DEF VAR lcCLIKey   AS CHAR  NO-UNDO. 
DEF VAR lcMonthKey AS CHAR  NO-UNDO. 
DEF VAR liPrevMth  AS INT   NO-UNDO. 
DEF VAR liPeriod   AS INT   NO-UNDO EXTENT 2. 
DEF VAR liInvPer   AS INT   NO-UNDO EXTENT 2. 
DEF VAR ldtBegDate AS DATE  NO-UNDO.
DEF VAR ldtEndDate AS DATE  NO-UNDO. 
DEF VAR liCLIQTy   AS INT   NO-UNDO. 
DEF VAR liPercID   AS INT   NO-UNDO. 
DEF VAR liBasisID  AS INT   NO-UNDO. 
DEF VAR llCalc     AS LOG   NO-UNDO. 
DEF VAR liQty      AS INT   NO-UNDO. 
DEF VAR liCrea     AS INT   NO-UNDO. 
DEF VAR llTake     AS LOG   NO-UNDO. 

DEF BUFFER bEvent    FOR CoEvent.
DEF BUFFER bSalesman FOR Salesman. 
DEF BUFFER bInvoice  FOR Invoice.
DEF BUFFER bOwner    FOR MsOwner.

/* first make temp-table on events, and then on target level check possible 
   subscription qty limits etc. and update events to db */
DEF TEMP-TABLE ttEvent NO-UNDO
   LIKE CoEvent
   FIELD PercID AS INT.
   
DEF TEMP-TABLE ttSalesman NO-UNDO
   FIELD Salesman AS CHAR
   FIELD Reseller AS CHAR
   FIELD CustNum  AS INT
   FIELD CoTargId AS INT
   INDEX Salesman Salesman
   INDEX CustNum  CustNum.

DEF TEMP-TABLE ttBasis NO-UNDO
   FIELD CoPerc   AS DEC
   FIELD CoAmt    AS DEC
   FIELD SubsQty  AS INT
   FIELD CoLimit  AS DEC
   FIELD CCN      AS INT
   FIELD BillCode AS CHAR
   FIELD PrevOper AS CHAR 
   FIELD CommBase AS DEC
   FIELD PercID   AS INT
   INDEX BillCode BillCode CCN 
   INDEX PercID PercID.
   
DEF TEMP-TABLE ttCust NO-UNDO
   FIELD EventCust AS INT
   FIELD Contract  AS CHAR
   FIELD ttSlsID   AS INT.

DEF TEMP-TABLE ttMonths NO-UNDO
   FIELD Year     AS INT
   FIELD Month    AS INT
   FIELD FromDate AS DATE
   FIELD ToDate   AS DATE
   FIELD CLIQty   AS INT
   INDEX YEAR YEAR MONTH.

DEF TEMP-TABLE ttCLI NO-UNDO
   FIELD CLI     AS CHAR
   FIELD TsBeg   AS DEC
   FIELD Year    AS INT
   FIELD Month   AS INT
   INDEX CLI CLI TsBeg.

DEF TEMP-TABLE ttARPU NO-UNDO
   FIELD BaseAmt AS DEC
   FIELD YEAR    AS INT
   FIELD MONTH   AS INT
   FIELD PercID  AS INT 
   INDEX YEAR Year Month.
  
DEF BUFFER bBasis FOR ttBasis.

FORM CoRule.CORuleId LABEL "Rule ...." SKIP
     liQty           LABEL "Customers" FORMAT ">>>>>>>9"
WITH OVERLAY ROW 10 CENTERED SIDE-LABELS 
     TITLE " Collecting " FRAME fCollect. 

FORM CoRule.CORuleId LABEL "Rule ........." SKIP
     liQty           LABEL "Customers ...." FORMAT ">>>>>>>9" SKIP
     liCrea          LABEL "Events Created" FORMAT ">>>>>>>9"
WITH OVERLAY ROW 10 CENTERED SIDE-LABELS 
     TITLE " Calculating " FRAME fCreate. 

FUNCTION fCollRules RETURNS LOGICAL
   (icSalesman AS CHAR,
    icReseller AS CHAR,
    iiCustNum  AS INT).

   /* does this target already exist */
   FIND FIRST ttSalesman WHERE
              ttSalesman.Salesman = icSalesman AND
              ttSalesman.CustNum  = iiCustNum  NO-ERROR.
   IF NOT AVAILABLE ttSalesman THEN DO:
      CREATE ttSalesman.
      ASSIGN ttSalesman.Salesman = icSalesman
             ttSalesman.Reseller = icReseller
             ttSalesman.CustNum  = iiCustNum
             ttSalesman.CoTargId = CoTarg.CoTargId.
   END.
   
   RETURN TRUE.

END FUNCTION.

/* determine targets in commission sharing */
FUNCTION fCoShareTarget RETURNS LOGICAL.

   ASSIGN lcSalesman = ""
          liCustNum  = 0
          lcParent   = "". 

   CASE CoShare.TargType:
   WHEN "S" THEN lcSalesman = CoShare.CoTarg.
   WHEN "R" THEN DO:

         /* find salesman's parent (go through middle levels if necessary) */
         FIND Salesman NO-LOCK WHERE
              Salesman.Brand    = gcBrand  AND
              Salesman.Salesman = ttSalesman.Salesman NO-ERROR.
         IF AVAILABLE Salesman THEN lcParent = Salesman.Parent.

         REPEAT:
            IF lcParent = "" THEN LEAVE. 

            FIND bSalesman NO-LOCK WHERE 
                 bSalesman.Brand    = gcBrand  AND
                 bSalesman.Salesman = lcParent NO-ERROR.

            IF NOT AVAILABLE bSalesman THEN LEAVE.

            /* match */
            IF bSalesman.Reseller = CoShare.CoTarg AND
               bSalesman.RsLevel  = CoShare.RsLevel
            THEN DO:
               lcSalesman = bSalesman.Salesman.
               LEAVE.
            END. 

            /* next level */
            lcParent = bSalesman.Parent.
         END. 

      END.
   WHEN "C" THEN liCustNum = INTEGER(CoShare.CoTarg). 
   END CASE. 

END FUNCTION.

/* create event */
FUNCTION fMakettEvent RETURNS LOGICAL
   (icHostTable AS CHAR,
    icHostKey   AS CHAR,
    iiPercID    AS INT,
    idBaseAmt   AS DEC,   /* base for percentage commission */
    idtFrom     AS DATE,  /* event dates (commission base) */
    idtTo       AS DATE). /* event dates */

   IF iiPercID = -1 THEN DO:
      FIND FIRST ttBasis NO-ERROR.
      IF NOT AVAILABLE ttBasis THEN RETURN FALSE.
      iiPercID = ttBasis.PercID.
   END.
   
   IF icHostTable NE "CoEvent" THEN 
   FIND FIRST ttEvent EXCLUSIVE-LOCK WHERE
              ttEvent.HostTable = icHostTable         AND
              ttEvent.HostKey   = icHostKey           AND
              ttEvent.CoRuleID  = CoRule.CoRuleID     AND
              ttEvent.Salesman  = ttSalesman.Salesman AND
              ttEvent.CustNum   = ttSalesman.CustNum  AND
              ttEvent.PercID    = iiPercID 
   NO-ERROR.

   IF NOT AVAILABLE ttEvent OR icHostTable = "CoEvent" THEN DO:
      CREATE ttEvent.
      ASSIGN ttEvent.HostTable = icHostTable         
             ttEvent.HostKey   = icHostKey           
             ttEvent.CoRuleID  = CoRule.CoRuleID     
             ttEvent.Salesman  = ttSalesman.Salesman 
             ttEvent.CustNum   = ttSalesman.CustNum
             ttEvent.CoTargId  = ttSalesman.CoTargId 
             ttEvent.PercID    = iiPercID
             ttEvent.CoEventID = NEXT-VALUE(CommSeq).
   END.

   ttEvent.BaseAmt = ttEvent.BaseAmt + idBaseAmt.

   IF ttEvent.CommFrom = ? OR
      ttEvent.CommFrom > idtFrom 
   THEN ttEvent.CommFrom = idtFrom.

   IF ttEvent.CommTo = ? OR
      ttEvent.CommTo < idtTo
   THEN ttEvent.CommTo = idtTo. 

   RETURN TRUE. 

END FUNCTION.

FUNCTION fCoShare RETURNS LOGICAL.

   /* remove possible old ones */
   FOR EACH bEvent EXCLUSIVE-LOCK WHERE
            bEvent.Brand     = gcBrand   AND
            bEvent.HostTable = "CoEvent" AND
            bEvent.HostKey   = STRING(CoEvent.CoEventID):

      DELETE bEvent.
   END. 

   FOR EACH CoShare NO-LOCK WHERE
            CoShare.CoTargId = CoEvent.CoTargID:

      /* target for commission sharing */
      fCoShareTarget(). 

      CREATE bEvent.
      ASSIGN bEvent.Brand     = gcBrand  
             bEvent.HostTable = "CoEvent"         
             bEvent.HostKey   = STRING(CoEvent.CoEventID)           
             bEvent.CoRuleID  = CoRule.CoRuleID     
             bEvent.CoTargId  = CoEvent.CoTargId
             bEvent.Salesman  = lcSalesman
             bEvent.CustNum   = liCustNum
             bEvent.CoEventID = NEXT-VALUE(CommSeq)
             bEvent.CommFrom  = CoEvent.CommFrom
             bEvent.CommTo    = CoEvent.CommTo
             bEvent.CoAmt     = CoShare.CoAmt
             bEvent.CoPerc    = CoShare.CoPerc
             bEvent.BaseAmt   = CoEvent.BaseAmt
             bEvent.CalcDate  = TODAY 
             bEvent.CommAmt   = CoShare.CoAmt +
                                IF CoRule.BasisType NE 3 
                                THEN (CoShare.CoPerc * CoEvent.BaseAmt / 100)
                                ELSE 0
             oiEvents         = oiEvents + 1
             liCrea           = liCrea + 1.

      /* if based on arpu -> multiply with cliqty */
      IF CoRule.BasisType = 2 AND liCLIQty > 0 
      THEN bEvent.CommAmt = bEvent.CommAmt * liCLIQty.
                   
   END. 
   
END FUNCTION.

/* has commission already been calculated */
FUNCTION fEventExists RETURNS LOGICAL    
   (icHostTable AS CHAR,
    icHostKey   AS CHAR). 

   /* already calculated */
   IF CAN-FIND(FIRST CoEvent WHERE
                     CoEvent.Brand     = gcBrand             AND
                     CoEvent.HostTable = icHostTable         AND
                     CoEvent.HostKey   = icHostKey           AND
                     CoEvent.CoRuleID  = CoRule.CoRuleID     AND
                     CoEvent.Salesman  = ttSalesman.Salesman AND
                     CoEvent.CustNum   = ttSalesman.CustNum)
   THEN DO:

      /* if recalculation then delete old (if not already paid) */
      IF ilRecalc THEN DO:
         FOR EACH CoEvent EXCLUSIVE-LOCK WHERE
                  CoEvent.Brand      = gcBrand             AND  
                  CoEvent.HostTable  = icHostTable         AND
                  CoEvent.HostKey    = icHostKey           AND
                  CoEvent.CoRuleID   = CoRule.CoRuleID     AND
                  CoEvent.Salesman   = ttSalesman.Salesman AND
                  CoEvent.CustNum    = ttSalesman.CustNum:

            IF CoEvent.PaymDate NE ? THEN RETURN TRUE.
         
            DELETE CoEvent.
         END.
         
         RETURN FALSE.        
      END.

      ELSE RETURN TRUE. 

   END.

   ELSE RETURN FALSE.

END FUNCTION.


FUNCTION fChkCommPoint RETURNS LOGICAL
   (iiCustNum AS INT,
    idtFromDate AS DATE,
    idtToDate   AS DATE).
    
   DEF VAR llTake AS LOG NO-UNDO. 

   llTake = TRUE.

   /* check that there is a bill for this month */
   FIND FIRST Invoice NO-LOCK WHERE
              Invoice.CustNum    = iiCustNum   AND
              Invoice.FirstCall <= idtToDate   AND
              Invoice.ToDate    >= idtFromDate NO-ERROR.
   IF NOT AVAILABLE Invoice OR
     (CoRule.CommPoint = 2 AND Invoice.PaymState NE 2)
   THEN llTake = FALSE.
            
   RETURN llTake.
   
END FUNCTION.

FUNCTION fCLIDates RETURNS LOGICAL
   (idBeg AS DEC,
    idEnd AS DEC).
    
   fSplitTS(idBeg,
            OUTPUT ldtBegDate,
            OUTPUT ldtTime). 
   
   IF idEnd >= 999999 
   THEN ldtEndDate = 12/31/2050.
   ELSE fSplitTS(idEnd,
                 OUTPUT ldtEndDate,
                 OUTPUT ldtTime). 
    
END FUNCTION.
    
FUNCTION fCLIQty RETURNS LOGICAL
   (icCLI   AS CHAR,
    idBeg   AS DEC).
    
   /* cli qty is needed for calculating arpu and 
      choosing correct %/fixed amt from CoTarg 
   */
   FOR EACH ttMonths:
   
      IF ldtBegDate > ttMonths.ToDate OR
         ldtEndDate < ttMonths.FromDate
      THEN NEXT.
      
      IF NOT CAN-FIND(FIRST ttCLI WHERE 
                            ttCLI.CLI   = icCLI AND
                            ttCLI.TsBeg = idBeg AND
                            ttCLI.Year  = ttMonths.Year AND
                            ttCLI.Month = ttMonths.Month)
      THEN DO:
         CREATE ttCLI.
         ASSIGN ttCLI.CLI   = icCLI
                ttCLI.TsBeg = idBeg
                ttCLI.Year  = ttMonths.Year
                ttCLI.Month = ttMonths.Month.
      END.
   END.  
   
END FUNCTION.    

FUNCTION fCheckCLIDays RETURNS LOGICAL
   (iiMsSeq   AS INT,
    ilActDate AS LOG).

   DEF VAR llValid    AS LOG  NO-UNDO. 
   DEF VAR ldtActDate AS DATE NO-UNDO.
   DEF VAR liTime     AS INT  NO-UNDO. 
   
   ldtActDate = ?.
   
   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF AVAILABLE MobSub AND MobSub.ActivationTS > 0 THEN DO:
      fSplitTS(MobSub.ActivationTS,
               OUTPUT ldtActDate,
               OUTPUT liTime).               
   END. 
   ELSE FOR FIRST Solog NO-LOCK WHERE
                  Solog.MsSeq = iiMsSeq AND
                  Solog.Stat  = 2       AND
                  Solog.CommLine BEGINS "CR":
      fSplitTS(Solog.CompletedTS,
               OUTPUT ldtActDate,
               OUTPUT liTime).               
   END.

   IF ldtActDate = ? THEN ldtActDate = ldtBegDate.
   
   llValid = TRUE.
   
   IF ilActDate THEN DO:
      /* activated during given time period */         
      IF ldtActDate < idtDate1 OR 
         ldtActDate > idtDate2
      THEN llValid = FALSE.
   END.
   
    /* minimum open days defined */
   IF llValid AND CoRule.OpenDays > 0 THEN DO:
      IF TODAY - ldtActDate < CoRule.OpenDays OR
         ldtEndDate - ldtActDate < CoRule.OpenDays
      THEN llValid = FALSE.
   END.
   
   RETURN llValid.
    
END FUNCTION.   

FUNCTION fCheckCLI RETURNS LOGICAL
   (icCLI     AS CHAR,
    ilActDate AS LOG).

   DEF VAR llSure AS LOG NO-UNDO.
   
   IF icCLI = "" THEN RETURN TRUE.
   
   llSure = FALSE.
   
   /* first from mob */
   IF CoRule.CLIType NE "fixed" THEN 
   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.Brand  = gcBrand     AND
             MsOwner.CLI    = icCLI       AND
             MsOwner.TSBeg <= liInvPer[2] AND
             MsOwner.TSEnd >= liInvPer[1] AND
            (IF ttCust.Contract NE "" AND ttSalesman.Reseller NE "RF"
             THEN MsOwner.Contract = ttCust.Contract
             ELSE TRUE)                    AND
            (IF CoRule.CLIType > ""
             THEN LOOKUP(MsOwner.CLIType,CoRule.CLIType) > 0
             ELSE TRUE):
       
      fCLIDates(MsOwner.TSBeg,MsOwner.TSEnd).
      
      IF NOT fCheckCLIDays(MsOwner.MsSeq,
                           ilActDate) THEN NEXT.
      
      llSure = TRUE.       
      
      fCLIQty(MsOwner.CLI,
              MsOwner.TSBeg).
                                  
   END.

   RETURN llSure.
   
END FUNCTION.

FUNCTION fCollInvSum RETURNS LOGICAL
   (icBillCode AS CHAR,
    iiCCN      AS INT,
    idAmt      AS DEC,
    ilVatIncl  AS LOG).

   FOR EACH ttBasis:

      IF (ttBasis.CCN > 0 AND iiCCN = ttBasis.CCN) OR
         /* commission is not based on particular product or CCN */
         ttBasis.BillCode = "ALL" OR 
         (ttBasis.BillCode > "" AND icBillCode = ttBasis.BillCode) 
      THEN DO:

         /* remove vat */
         IF ilVatIncl THEN DO:
            IF Invoice.VatUsage < 3 THEN 
            FOR FIRST InvRow OF Invoice NO-LOCK WHERE
                      InvRow.BillCode = icBillCode:
               idAmt = idAmt / (1 + InvRow.VatPerc / 100).
            END.
            ELSE 
            FOR FIRST BillItem NO-LOCK WHERE 
                      BillItem.Brand    = gcBrand  AND
                      BillItem.BillCode = icBillCode,
                FIRST VatCode NO-LOCK WHERE
                      VatCode.VatCode = BillItem.VatCode:
               idAmt = idAmt / (1 + VatCode.VatPerc / 100).       
            END.
         END.
      
         ttBasis.CommBase = ttBasis.CommBase + idAmt.      
      
         /* use first one */
         LEAVE.
      END.
         
   END.

   RETURN TRUE. 
   
END FUNCTION.


/* check if basis-products are used in contract fees */
FUNCTION fCheckContractBasis RETURNS INTEGER
   (icContract AS CHAR,
    iiCustNum  AS INT,
    iiMsSeq    AS INT,
    idtFrom    AS DATE,
    idtTo      AS DATE).

   DEF VAR liBasisFound AS INT  NO-UNDO.
   DEF VAR lcPrevOper   AS CHAR NO-UNDO.
    
   /* nothing to check */
   IF NOT CAN-FIND(FIRST ttBasis WHERE ttBasis.BillCode > "")
   THEN RETURN -1. 

   liBasisFound = 0.

   /* previous operator is first in priority */
   FOR EACH ttBasis WHERE 
            ttBasis.CCN      = -1 AND
            ttBasis.BillCode > "":
      
      /* operator where cli was transferred from */
      lcPrevOper = fPrevOperator(iiMsSeq).
   
      IF lcPrevOper = ttBasis.BillCode THEN DO:
         liBasisFound = ttBasis.PercID.
         LEAVE.
      END. 
   END. 
   
   IF liBasisFound = 0 THEN 
   FOR EACH FixedFee NO-LOCK WHERE  
            FixedFee.Brand   = gcBrand   AND
            FixedFee.CustNum = iiCustNum:
         
         /* referee commission */
         IF ttSalesman.Reseller = "RF" OR ttSalesman.Reseller BEGINS "AC"
         THEN DO:
            IF FixedFee.HostTable NE "MobSub" OR
               FixedFee.KeyValue  NE STRING(iiMsSeq) 
            THEN NEXT.
         END.
         
         ELSE IF icContract > "" AND FixedFee.Contract NE icContract 
         THEN NEXT. 
         
         FIND FIRST ttBasis WHERE ttBasis.BillCode = "ALL" NO-ERROR.
         IF NOT AVAILABLE ttBasis THEN
         FIND FIRST ttBasis WHERE 
                    ttBasis.BillCode = FixedFee.BillCode NO-ERROR.
         IF NOT AVAILABLE ttBasis THEN NEXT.

         CASE CoRule.CommPoint:
         WHEN 0 THEN liBasisFound = ttBasis.PercID.
         WHEN 1 OR
         WHEN 2 THEN DO:
         
            FOR EACH FFItem OF FixedFee NO-LOCK WHERE
                     FFItem.Billed = TRUE,
               FIRST bInvoice NO-LOCK WHERE
                     bInvoice.InvNum   = FFItem.InvNum AND
                     (IF idtTo NE ?
                      THEN bInvoice.FirstCall <= idtTo AND
                           bInvoice.ToDate    >= idtFrom
                      ELSE TRUE):
                   
               IF CoRule.CommPoint = 1 OR
                  (CoRule.CommPoint = 2 AND bInvoice.PaymState = 2)
               THEN DO:
                  liBasisFound = ttBasis.PercID.
                  LEAVE.
               END.
            END. 

         END.
         END CASE.
         
         IF liBasisFound > 0 THEN LEAVE.       
         
   END.

   IF liBasisFound = 0 THEN
   FOR EACH SingleFee NO-LOCK WHERE  
            SingleFee.Brand   = gcBrand   AND
            SingleFee.CustNum = iiCustNum AND
            (IF icContract > ""  AND ttSalesman.Reseller NE "RF"
             THEN SingleFee.Contract  = icContract 
             ELSE TRUE):

         /* referee commission */
         IF ttSalesman.Reseller = "RF" THEN DO:
            IF SingleFee.HostTable NE "MobSub" OR
               SingleFee.KeyValue  NE STRING(iiMsSeq) 
            THEN NEXT.
         END.
         
         FIND FIRST ttBasis WHERE ttBasis.BillCode = "ALL" NO-ERROR.
         IF NOT AVAILABLE ttBasis THEN
         FIND FIRST ttBasis WHERE 
                    ttBasis.BillCode = SingleFee.BillCode NO-ERROR.
         IF NOT AVAILABLE ttBasis THEN NEXT.

         CASE CoRule.CommPoint:
         WHEN 0 THEN liBasisFound = ttBasis.PercID.
         WHEN 1 OR
         WHEN 2 THEN DO:
         
            IF SingleFee.Billed THEN 
            FOR FIRST bInvoice NO-LOCK WHERE
                      bInvoice.InvNum = SingleFee.InvNum AND
                     (IF idtTo NE ?
                      THEN bInvoice.FirstCall <= idtTo AND
                           bInvoice.ToDate    >= idtFrom
                      ELSE TRUE):
                   
               IF CoRule.CommPoint = 1 OR
                  (CoRule.CommPoint = 2 AND bInvoice.PaymState = 2)
               THEN DO:
                  liBasisFound = ttBasis.PercID.
                  LEAVE.
               END.
            END. 

         END.
         END CASE.
         
         IF liBasisFound > 0 THEN LEAVE.       
         
   END.
       
   RETURN liBasisFound.
   
END.


/* get calendar months that are included in given time period */
liPrevMth = 0.
DO ldtDate = idtDate1 TO idtDate2:

   IF MONTH(ldtDate) NE liPrevMth THEN DO:
      CREATE ttMonths.
      ASSIGN ttMonths.Year     = YEAR(ldtDate)
             ttMonths.Month    = MONTH(ldtDate)
             ttMonths.FromDate = DATE(ttMonths.Month,1,ttMonths.Year)
             ttMonths.ToDate   = IF ttMonths.Month = 12
                                 THEN DATE(12,31,ttMonths.Year)
                                 ELSE DATE(ttMonths.Month + 1,1,ttMonths.Year)
                                      - 1 
             liPrevMth         = MONTH(ldtDate).
   END.
END.

ASSIGN liPeriod[1] = fHMS2TS(idtDate1,"00:00:00")
       liPeriod[2] = fHMS2TS(idtDate2,"23:59:59").

/* delete all previous events if wanted */
IF idtDelete NE ? THEN 
FOR EACH CoEvent EXCLUSIVE-LOCK WHERE
         CoEvent.Brand    = gcBrand   AND
         CoEvent.CalcDate = idtDelete AND
         CoEvent.PaymDate = ?:

   IF iiCoRuleID > 0 AND CoEvent.CoRuleID NE iiCoRuleID THEN NEXT.
         
   DELETE CoEvent.      
END.


RuleLoop:
FOR EACH CoRule NO-LOCK WHERE
         CoRule.Brand    = gcBrand     AND
         CoRule.RuleType = 0           AND  /* 1 = template */
         CoRule.CoFrom  <= idtDate2    AND
         CoRule.CoTo    >= idtDate1    AND
         CoRule.CustNum >= iiRuleCust1 AND
         CoRule.CustNum <= iiRuleCust2:

   IF iiCoRuleID > 0 AND CoRule.CoRuleID NE iiCoRuleID THEN NEXT.
   
   /* Customer defined  */
   IF CoRule.CustNum > 0 THEN DO:
      FIND Customer NO-LOCK WHERE
           Customer.CustNum = CoRule.CustNum NO-ERROR.
      IF NOT AVAILABLE Customer OR
         (icInvGroup NE "" AND Customer.InvGroup NE icInvGroup)
      THEN NEXT RuleLoop.
   END.


   EMPTY TEMP-TABLE ttSalesman.

   FOR EACH CoTarg OF CoRule NO-LOCK:

      /* salesman defined */
      IF CoTarg.TargType = "S" THEN DO:
         IF icSalesman NE "" AND  
            CoTarg.CoTarg NE icSalesman
         THEN NEXT. 

         FIND Salesman NO-LOCK WHERE 
              Salesman.Brand    = gcBrand  AND
              Salesman.Salesman = CoTarg.CoTarg NO-ERROR.

         IF NOT AVAILABLE Salesman OR
            (icReseller NE "" AND Salesman.Reseller NE icReseller)
         THEN NEXT. 

         fCollRules(Salesman.Salesman,
                    Salesman.Reseller,
                    0).

      END.         

      /* reseller & level defined */
      ELSE IF CoTarg.TargType = "R" THEN DO:

         IF icReseller NE "" AND
            CoTarg.CoTarg NE icReseller 
         THEN NEXT. 

         /* take all salesmen from defined level */
         FOR EACH Salesman NO-LOCK WHERE
                  Salesman.Brand    = gcBrand       AND
                  Salesman.Reseller = CoTarg.CoTarg AND
                  Salesman.RsLevel  = CoTarg.RsLevel:

            IF icSalesman NE "" AND
               Salesman.Salesman NE icSalesman
            THEN NEXT. 

            fCollRules(Salesman.Salesman,
                       Salesman.Reseller,
                       0).

         END.

      END.

      /* Customer */
      ELSE IF CoTarg.TargType = "C" THEN DO:
         
         fCollRules("",
                    "",
                    INTEGER(CoTarg.CoTarg)).
      END.

   END.   /* targets */      

   EMPTY TEMP-TABLE ttCust. 

   /* get event customers
     - go through salesmen and their contracts 
     - get directly defined customers 
   */
   FOR EACH ttSalesman WHERE
            ttSalesman.Salesman > "",
       EACH Contract NO-LOCK WHERE
            Contract.Brand      = gcBrand             AND
            Contract.Salesman   = ttSalesman.Salesman AND
            Contract.CustNum   >= iiEventCust1        AND
            Contract.CustNum   <= iiEventCust2        AND
            Contract.CommPerm  = TRUE                 AND
            (IF CoRule.CustNum > 0 
             THEN Contract.CustNum = CoRule.CustNum
             ELSE TRUE),
      FIRST Customer OF Contract NO-LOCK:

      /* invgroup selected */
      IF icInvGroup NE "" AND Customer.InvGroup NE icInvGroup
      THEN NEXT.

      CREATE ttCust.
      ASSIGN ttCust.EventCust = Customer.CustNum
             ttCust.Contract  = Contract.Contract
             ttCust.ttSlsID   = RECID(ttSalesman)
             liQty            = liQty + 1.
             
      IF liQty < 100 OR liQty MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY CoRule.CoRuleID liQty WITH FRAME fCollect.
      END.
      
   END. 

   /* if Customer is defined as target -> also rule must be Customer based */
   IF CoRule.CustNum > 0 THEN 
   FOR EACH ttSalesman WHERE
            ttSalesman.CustNum > 0:
      CREATE ttCust.
      ASSIGN ttCust.EventCust = CoRule.CustNum
             ttCust.ttSlsID   = RECID(ttSalesman)
             liQty            = liQty + 1.
             
      PAUSE 0.
      DISPLAY CoRule.CoRuleID liQty WITH FRAME fCollect.
   END.

   EMPTY TEMP-TABLE ttBasis.
   
   /* basis data */
   FOR EACH CoBasis OF CoRule NO-LOCK:
      
      FIND FIRST ttBasis WHERE
                 ttBasis.BillCode = CoBasis.BillCode AND
                 ttBasis.CCN      = CoBasis.CCN NO-ERROR.

      /* same id if only subscription qty differs */
      IF NOT AVAILABLE ttBasis 
      THEN liPercID = liPercID + 1.
      ELSE liPercID = ttBasis.PercID.
      
      CREATE ttBasis.
      ASSIGN ttBasis.CCN      = CoBasis.CCN
             ttBasis.BillCode = CoBasis.BillCode
             ttBasis.CoAmt    = CoBasis.CoAmt   
             ttBasis.CoPerc   = CoBasis.CoPerc  
             ttBasis.CoLimit  = CoBasis.CommLimit
             ttBasis.SubsQty  = CoBasis.SubsQty 
             ttBasis.PercID   = liPercID.
   END.

   HIDE FRAME fCollect NO-PAUSE.
   ASSIGN liQty  = 0
          liCrea = 0.
   
   IF NOT CAN-FIND(FIRST ttBasis) THEN NEXT.
                
   EventLoop:
   FOR EACH ttCust,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = ttCust.EventCust,
      FIRST ttSalesman WHERE
            RECID(ttSalesman) = ttCust.ttSlsID
   BREAK BY ttSalesman.Salesman
         BY ttSalesman.CustNum:

      liQty = liQty + 1.
      IF liQty < 100 OR liQty MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY CoRule.CoRuleID liQty liCrea WITH FRAME fCreate.
      END.

      IF FIRST-OF(ttSalesman.CustNum) THEN DO:

         /* events are taken into db on target level */
         EMPTY TEMP-TABLE ttEvent.
         FOR EACH ttBasis:
            ttBasis.CommBase = 0.
         END.
         
         /* arpu is calculated on target level */
         EMPTY TEMP-TABLE ttCLI.
         EMPTY TEMP-TABLE ttArpu.

      END.
      
      llCalc = TRUE.
      
      /* are there minimum limits that must be reached */
      IF CoRule.AmtBilled > 0 OR 
         CoRule.QtyPaidInv > 0
      THEN DO:

         ldAmt = 0.  
         /* calculate customer's invoices (except credit/credited) */
         FOR EACH Invoice OF Customer NO-LOCK WHERE
                  Invoice.CrInvNum = 0:

            ACCUMULATE Invoice.InvAmt (TOTAL).

            IF Invoice.PaymState = 2 THEN ldAmt = ldAmt + 1.
         END.

         /* minimum billed amount */
         IF CoRule.AmtBilled > 0 AND
           (ACCUM TOTAL Invoice.InvAmt) < CoRule.AmtBilled
         THEN llCalc = FALSE.

         /* minimum quantity of paid Invoices */
         ELSE IF CoRule.QtyPaidInv > 0 AND
                 ldAmt < CoRule.QtyPaidInv
         THEN llCalc = FALSE.

      END.                

      /* basis can be amount (value), arpu or subscription and
         commission point can be order, billing or payment */

      IF llCalc AND 
         /* 1=% from billed amount,2=% from arpu */
         (CoRule.BasisType = 1 OR CoRule.BasisType = 2) 
      THEN DO:
      
         FOR EACH Invoice OF Customer NO-LOCK WHERE
                  Invoice.InvDate >= idtDate1 AND
                  Invoice.InvDate <= idtDate2 AND
                  Invoice.CrInvNum = 0:

            IF Invoice.InvDate > CoRule.CoTo OR
               Invoice.InvDate < CoRule.CoFrom THEN NEXT. 
               
            /* should be paid */
            IF CoRule.CommPoint = 2 AND
               Invoice.PaymState NE 2
            THEN NEXT. 

            /* check if already handled */
            IF fEventExists("Invoice",
                            STRING(Invoice.InvNum))
            THEN NEXT.

            ASSIGN liInvPer[1] = fHMS2TS(Invoice.FirstCall,"00:00:00")
                   liInvPer[2] = fHMS2TS(Invoice.ToDate,"23:59:59").
                   
            FOR EACH ttBasis:
                ttBasis.CommBase = 0.
            END.
                    
            /* usage fees */         
            FOR EACH SubInvoice OF Invoice NO-LOCK:
                     
               /* check that CLI 'belongs' to this salesman */             
               IF NOT fCheckCLI(SubInvoice.CLI,FALSE) THEN NEXT.
                
               FOR EACH InvRowCounter NO-LOCK WHERE
                        InvRowCounter.InvNum = Invoice.InvNum AND
                        InvRowCounter.SubInvNum = SubInvoice.SubInvNum:
                  fCollInvSum(InvRowCounter.BillCode,
                              InvRowCounter.CCN,
                              InvRowCounter.Amount,
                              InvRowCounter.VatIncl).
               END.               
            END. 

            /* contract & single fees */
            FOR EACH FFItem NO-LOCK WHERE
                     FFItem.CustNum = Invoice.CustNum and
                     FFItem.InvNum  = Invoice.InvNum,
               FIRST FixedFee OF FFItem NO-LOCK WHERE
                     (IF ttCust.Contract NE ""
                      THEN FixedFee.Contract = ttCust.Contract
                      ELSE TRUE):
                      
               IF (FixedFee.HostTable = "MobSub" OR FixedFee.HostTable = "CLI")
               THEN DO:
                  IF NOT fCheckCLI(FixedFee.KeyValue,FALSE) THEN NEXT.
               END.
               
               fCollInvSum(FFItem.BillCode,
                           0,
                           FFItem.Amt,
                           FixedFee.VatIncl).
            END.
            
            FOR EACH SingleFee NO-LOCK WHERE
                     SingleFee.InvNum  = Invoice.InvNum  AND
                     (IF ttCust.Contract NE ""
                      THEN SingleFee.Contract = ttCust.Contract
                      ELSE TRUE):
                                   
               IF (SingleFee.HostTable = "MobSub" OR 
                   SingleFee.HostTable = "CLI")
               THEN DO:
                  IF NOT fCheckCLI(SingleFee.KeyValue,FALSE) THEN NEXT.
               END.
               
               fCollInvSum(SingleFee.BillCode,
                           0,
                           SingleFee.Amt,
                           SingleFee.VatIncl).
            END.

            /* create actual commission event (always on Invoice level) */
            FOR EACH ttBasis WHERE ttBasis.CommBase NE 0:

               IF CoRule.BasisType = 1 THEN DO:

                  fMakettEvent("Invoice",
                               STRING(Invoice.InvNum),
                               ttBasis.PercID,
                               ttBasis.CommBase,
                               Invoice.FromDate,
                               Invoice.ToDate).
               END.                     
         
               /* arpu is calculated on target level */
               ELSE IF CoRule.BasisType = 2 THEN DO:
                  FIND FIRST ttARPU WHERE
                             ttARPU.Year   = YEAR(Invoice.InvDate)  AND
                             ttARPU.Month  = MONTH(Invoice.InvDate) AND
                             ttARPU.PercID = ttBasis.PercID NO-ERROR.
                  IF NOT AVAILABLE ttARPU THEN DO:
                     CREATE ttARPU.
                     ASSIGN ttARPU.Year   = YEAR(Invoice.InvDate)
                            ttARPU.Month  = MONTH(Invoice.InvDate)
                            ttARPU.PercID = ttBasis.PercID.
                  END.
                  ttARPU.BaseAmt = ttARPU.BaseAmt + ttBasis.CommBase.
               END.   
            END.
            
         END.  /* invoices */

      END.  /* billed amount based */
      
      /* commission is a fixed amount per subscription */
      ELSE IF llCalc AND CoRule.BasisType = 3 THEN DO:

         IF CoRule.CLIType NE "fixed" THEN
         FOR EACH MsOwner NO-LOCK WHERE
                  MsOwner.Brand    = gcBrand          AND
                  MsOwner.CustNum  = Customer.CustNum AND
                  MsOwner.TSEnd    > liPeriod[1]      AND /* valid */
                  (IF ttCust.Contract NE "" AND ttSalesman.Reseller NE "RF"
                   THEN MsOwner.Contract = ttCust.Contract
                   ELSE TRUE)                       AND
                  (IF CoRule.CLIType > ""
                   THEN LOOKUP(MsOwner.CLIType,CoRule.CLIType) > 0
                   ELSE TRUE):
 
            /* referee commission; check if this cli was the one that
               had this referee*/
            IF ttSalesman.Reseller = "RF" THEN DO:
               llTake = FALSE.
               FOR FIRST Salesman NO-LOCK WHERE
                         Salesman.Brand    = gcBrand AND
                         Salesman.Salesman = ttSalesman.Salesman,
                   FIRST bOwner NO-LOCK WHERE
                         bOwner.MsSeq   = INTEGER(
                                          SUBSTRING(Salesman.Salesman,3)) AND
                         bOwner.CustNum = Salesman.CustNum,
                   FIRST Order NO-LOCK WHERE
                         Order.MsSeq   = MsOwner.MsSeq AND
                         Order.Referee = bOwner.CLI:
                  llTake = TRUE.
               END.
                         
               IF NOT llTake THEN NEXT.
            END.
            
            fCLIDates(MsOwner.TSBeg,MsOwner.TSEnd).
            
            IF ldtBegDate > CoRule.CoTo OR
               ldtBegDate < CoRule.CoFrom THEN NEXT. 
 
            /* check for open days */
            IF NOT fCheckCLIDays(MsOwner.MsSeq,
                                 TRUE) 
            THEN NEXT. 
            
            lcCLIKey = MsOwner.CLI + "/" +
                       STRING(MsOwner.CustNum) + "/" +
                       STRING(MsOwner.MsSeq).
                   
            /* cliqty for subs.qty limits */
            fCLIQty(MsOwner.CLI,
                    MsOwner.TSBeg).
                                   
            /* if based on order, commission is paid only once
               per each cli */
            IF CoRule.CommPoint = 0 THEN DO:
            
               /* already handled */  
               IF fEventExists("MCLI",
                               lcCLIKey)
               THEN NEXT.

               /* check if basis defined */
               liBasisID = fCheckContractBasis(ttCust.Contract,
                                               MsOwner.CustNum,
                                               MsOwner.MsSeq,
                                               ?,
                                               ?).
               IF liBasisID = 0 THEN NEXT.                             
               
               fMakettEvent("MCLI",
                            lcCLIKey,
                            liBasisID,
                            0.00,
                            ldtBegDate,
                            ldtBegDate).
            END.
            
            /* if based on billing then commission per each month */
            ELSE FOR EACH ttMonths:

               /* check that cli has been valid */
               IF ldtBegDate > ttMonths.ToDate OR
                  ldtEndDate < ttMonths.FromDate
               THEN NEXT. 
               
               /* check commission point */
               IF NOT fChkCommPoint(MsOwner.CustNum,
                                    ttMonths.FromDate,
                                    ttMonths.ToDate)
               THEN NEXT.

               /* check if basis defined */
               liBasisID = fCheckContractBasis(ttCust.Contract,
                                               MsOwner.CustNum,
                                               MsOwner.MsSeq,
                                               ttMonths.FromDate,
                                               ttMonths.ToDate).
               IF liBasisID = 0 THEN NEXT.
                
               lcMonthKey = lcCLIKey + "/" +
                            STRING(ttMonths.Year MOD 100,"99") +
                            STRING(ttMonths.Month,"99").
                            
               /* already handled */  
               IF fEventExists("MCLI",
                               lcMonthKey)
               THEN NEXT.

               fMakettEvent("MCLI",
                            lcMonthKey,
                            liBasisID,
                            0.00,
                            ttMonths.FromDate,
                            ttMonths.ToDate).
                    
            END.
            
         END.
         
      END. /* based on subscriptions */

       /* events to db on target level */
      IF LAST-OF(ttSalesman.CustNum) THEN DO:
     
          /* get monthly cli qtys */
         FOR EACH ttMonths:
            
            FOR EACH ttCLI WHERE
                     ttCLI.Year  = ttMonths.Year AND
                     ttCLI.Month = ttMonths.Month:
               ACCUMULATE ttCLI.CLI (COUNT).
            END.

            ttMonths.CliQty = (ACCUM COUNT ttCLI.CLI).
            
         END.
         
         /* arpu is calculated on target level */
         IF CoRule.BasisType = 2 THEN DO:
         
            /* make monthly events */
            FOR EACH ttARPU,
            FIRST ttMonths WHERE
                  ttMonths.Year  = ttARPU.Year  AND
                  ttMonths.Month = ttARPU.Month AND
                  ttMonths.CLIQty > 0:
                  
               lcMonthKey = STRING(ttARPU.Year,"9999") +
                            STRING(ttARPU.Month,"99").
                            
               /* already handled */  
               IF fEventExists("ARPU",
                               lcMonthKey)
               THEN NEXT.
  
               ldAmt = ttARPU.BaseAmt / ttMonths.CLIQty.
     
               fMakettEvent("ARPU",
                            lcMonthKey,
                            ttARPU.PercID,
                            ldAmt,
                            ttMonths.FromDate,
                            ttMonths.ToDate).
                    
            END.
         END.  /* arpu */
        
         /* save events to db, determine correct %/fixed amt based on 
            subscription qty */ 
         FOR EACH ttEvent:
          
            CREATE CoEvent.
            BUFFER-COPY ttEvent TO CoEvent.
            ASSIGN CoEvent.CalcDate = TODAY
                   CoEvent.Brand    = gcBrand.
                          
            FIND FIRST ttMonths WHERE 
                       ttMonths.YEAR  = YEAR(CoEvent.CommTo) AND
                       ttMonths.MONTH = MONTH(CoEvent.CommTo) NO-ERROR.
            liCLIQTY = IF AVAILABLE ttMonths 
                       THEN ttMonths.CLIQty
                       ELSE 0.
                
            FOR EACH ttBasis WHERE
                     ttBasis.PercID = ttEvent.PercID
            BY ttBasis.SubsQty DESC:
            
               IF liCLIQTy >= ttBasis.SubsQty THEN DO:
               
                  ASSIGN
                  /* possible minimum limit is reduced from base amt */     
                  CoEvent.BaseAmt  = MAX(0,CoEvent.BaseAmt - ttBasis.CoLimit)
                  /* commission */
                  CoEvent.CommAmt  =  /* fixed amt */
                                      ttBasis.CoAmt +     
                                      /* percentage */
                                     (IF CoRule.BasisType NE 3
                                      THEN ttBasis.CoPerc * CoEvent.BaseAmt 
                                           / 100
                                      ELSE 0)
                  CoEvent.CoAmt    = ttBasis.CoAmt
                  CoEvent.CoPerc   = ttBasis.CoPerc.

                  /* if based on arpu -> multiply with cliqty */
                  IF CoRule.BasisType = 2 AND liCLIQty > 0 
                  THEN CoEvent.CommAmt = CoEvent.CommAmt * liCLIQty.
                   
                  /* commission sharing */  
                  fCoShare(). 
                  
                  /* use highest % */      
                  LEAVE.
               END.
            END.

            /* if no commission -> delete unnecessary event */
            IF CoEvent.CommAmt = 0 THEN DO:

               /* delete also related, shared commissions */
               FOR EACH bEvent EXCLUSIVE-LOCK WHERE
                        bEvent.Brand     = gcBrand   AND
                        bEvent.HostTable = "CoEvent" AND
                        bEvent.HostKey   = STRING(CoEvent.CoEventID):
                  DELETE bEvent.
               END. 
               
               DELETE CoEvent.
            END.
               
            ELSE DO:
               ASSIGN oiEvents = oiEvents + 1
                      liCrea   = liCrea + 1.
               
            END.
            
            DELETE ttEvent.
         END.
         
      END. /* last-of target */
            
   END. /* EventLoop */

END.  /* RuleLoop */

HIDE FRAME fQty NO-PAUSE. 

