/* ------------------------------------------------------
  MODULE .......: VOLDISC.P
  FUNCTION .....: Calculate volume discounts
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 05.09.02 
  MODIFIED .....: 26.09.02/aam customer balances in CustCount
                  10.10.02/jp  CLI      balances in Callcount
                  11.09.03/aam brand
                  23.10.03/aam StepUsage
                  18.11.03/aam update MobCDR.TotDisc
  Version ......: M15
  ------------------------------------------------------ */

{Func/fcustcnt.i}

/* configurations */
DEF TEMP-TABLE ttConf NO-UNDO
   FIELD VolID      AS INT
   FIELD BillTarget AS INT
   FIELD BasisType  AS INT
   FIELD VFrom      AS DATE
   FIELD VTo        AS DATE 
   FIELD VolKey     AS CHAR
   FIELD DPConf     AS INT
   FIELD StartFee   AS LOG 
   INDEX BillTarget BillTarget BasisType DESC
   INDEX DPConf DpConf BillTarget.

/* collected amounts */
DEF TEMP-TABLE ttVAmt NO-UNDO
   FIELD VolID     AS INT
   FIELD Month     AS INT
   FIELD CLI       AS CHAR
   FIELD CallAmt   AS DEC
   FIELD CallDur   AS INT
   FIELD CallQty   AS INT
   INDEX Month VolID Month CLI.

/* for actual discount calculation */
DEF TEMP-TABLE ttDisc NO-UNDO
   FIELD Month     AS INT
   FIELD CLI       AS CHAR
   FIELD DiscPrcnt AS DEC
   FIELD MinLimit  AS DEC
   FIELD MaxLimit  AS DEC
   FIELD LimitAmt  AS DEC
   FIELD Order     AS INT
   FIELD OldDisc   AS DEC
   FIELD NewDisc   AS DEC
   FIELD DiscType  AS INT
   FIELD BasisType AS INT
   FIELD VolKey    AS CHAR
   FIELD StartFee  AS LOG
   INDEX Month Month CLI BasisType DESC Order ASC.

DEF BUFFER bUpdMob   FOR MobCDR.
DEF BUFFER bttConf   FOR ttConf.
DEF BUFFER bttVAmt   FOR ttVAmt.

DEF VAR liMth       AS INT  NO-UNDO.
DEF VAR liLimit     AS INT  NO-UNDO. 
DEF VAR lcBillCode  AS CHAR NO-UNDO.
DEF VAR lcCCN       AS INT  NO-UNDO.
DEF VAR ldColl      AS DEC  NO-UNDO. 
DEF VAR ldDiscPrcnt AS DEC  NO-UNDO. 
DEF VAR liVolID     AS INT  NO-UNDO.
DEF VAR llColl      AS LOG  NO-UNDO. 

FUNCTION fLimitAmt RETURNS LOGICAL
   (iiDuration AS INT,
    idtAmt     AS DEC,
    iiQty      AS INT).

   /* DiscType; 1 = duration,
                2 = value,
                3 = qty 
   */
   CASE ttDisc.DiscType:
   WHEN 1 THEN ttDisc.LimitAmt = ttDisc.LimitAmt + (iiDuration / 60).
   WHEN 2 THEN ttDisc.LimitAmt = ttDisc.LimitAmt + idtAmt.
   WHEN 3 THEN ttDisc.LimitAmt = ttDisc.LimitAmt + iiQty.
   END CASE. 
   
END FUNCTION.

FUNCTION fCollDisc RETURNS LOGICAL
   (icCLI   AS CHAR,
    idAmt   AS DEC,
    iiDur   AS INT). 

   FIND FIRST ttVAmt WHERE
              ttVAmt.VolID   = ttConf.VolID AND  
              ttVAmt.Month   = liMth       AND
              ttVAmt.CLI     = icCLI          
   NO-ERROR.

   IF NOT AVAIL ttVAmt THEN DO:
      CREATE ttVAmt.
      ASSIGN ttVAmt.VolID = ttConf.VolID
             ttVAmt.Month = liMth        
             ttVAmt.CLI   = icCLI.
   END.

   ASSIGN ttVAmt.CallQty = ttVAmt.CallQty + 1
          ttVAmt.CallAmt = ttVAmt.CallAmt + idAmt
          ttVAmt.CallDur = ttVAmt.CallDur + iiDur. 

END FUNCTION.

FUNCTION fCreateTMQueue RETURNS LOGIC
   (BUFFER ibUpdCDR FOR MobCDR,
    iiAgrCust AS INT,
    iiQty AS INT):

   CREATE TMQueue.
   BUFFER-COPY ibUpdCDR TO TMQueue.
            
   ASSIGN
      TMQueue.Qty     = iiQty
      TMQueue.EventID = ibUpdCDR.DtlSeq
      TMQueue.AgrCust = iiAgrCust
      TmQueue.Source  = ibUpdCDR.MSCID
      TmQueue.PayType = 1 + INT(ibUpdCDR.PPFlag > 0)
      TMQueue.ReportingID = ibUpdCDR.ServRid + "," + ibUpdCDR.MPMRid
      TMQueue.ExtraAmount = ibUpdCDR.MPMAmt
      TMQueue.AccumTarget = "InvRow".
   RELEASE TMQueue.

END FUNCTION.

FUNCTION fVolDiscMob RETURNS LOGICAL
   (BUFFER VMobCDR FOR MobCDR,
    iiAgrCust AS INT):

   ASSIGN liMth = YEAR(VMobCDR.DateSt) * 100 + MONTH(VMobCDR.DateSt).

   /* if user has made overlapping definitions we'll take the first one
      using reverse basis order (which is also priority order) 
   */
   ASSIGN ldDiscPrcnt = 0. 

   FOR EACH ttDisc USE-INDEX Month WHERE
            ttDisc.Month  = liMth   AND
            ttDisc.CLI    = VMobCDR.CLI:

      /* if each step% is to be used then check if this step is already used 
         (amount of one call is not divided into two steps) */
      IF ttDisc.MaxLimit > 0 AND ttDisc.LimitAmt >= ttDisc.MaxLimit 
      THEN NEXT.
 
      /* is discount granted for everything, for billitems or for ccns */  
      CASE ttDisc.BasisType:
      WHEN 0 THEN ldDiscPrcnt = ttDisc.DiscPrcnt.
      WHEN 1 
         THEN DO:
            IF VMobCDR.BillCode = ttDisc.VolKey 
            THEN ldDiscPrcnt = ttDisc.DiscPrcnt.
         END.
      WHEN 2 
         THEN DO:
            IF STRING(VMobCDR.CCN) = ttDisc.VolKey
            THEN ldDiscPrcnt = ttDisc.DiscPrcnt.
         END.
      END CASE.

      /* discount was found */
      IF ldDiscPrcnt > 0 THEN DO:

         /* update step counter */
         IF ttDisc.MaxLimit > 0
         THEN DO:
            fLimitAmt(vMobCDR.BillDur,
                      vMobCDR.Amount + vMobCDR.DiscValue, /* old discount */
                      1).            
                       
            /* is min limit reached (one call is not divided between two 
               steps) */
            IF ttDisc.MinLimit > 0 AND 
               ttDisc.LimitAmt < ttDisc.MinLimit
            THEN DO:
               ldDiscPrcnt = 0.
               LEAVE.
            END.
               
         END.
  
          /* calculate new discount */
         IF VMobCDR.Disc% NE ldDiscPrcnt THEN DO:

            FIND bUpdMob WHERE RECID(bUpdMob) = RECID(VMobCDR) EXCLUSIVE-LOCK.

            fCreateTMQueue(BUFFER bUpdMob,
                           iiAgrCust,
                           -1).
                           
            ASSIGN ttDisc.OldDisc    = ttDisc.OldDisc + bUpdMob.DiscValue
                   bUpdMob.Disc%     = ldDiscPrcnt
                   /*
                   bUpdMob.Rated     = TRUE 
                   */
                   bUpdMob.Amount    = bUpdMob.Amount + bUpdMob.DiscValue
                   bUpdMob.DiscValue = ROUND(bUpdMob.Amount * 
                                             ldDiscPrcnt / 100,2)
                   bUpdMob.TotDisc   = bUpdMob.DiscValue
                   bUpdMob.Amount    = bUpdMob.Amount - bUpdMob.DiscValue
                   ttDisc.NewDisc    = ttDisc.NewDisc + bUpdMob.DiscValue. 
 
            fCreateTMQueue(BUFFER bUpdMob,
                           iiAgrCust,
                           1).
 
         END.

         /* first one found is accepted */
         LEAVE.

      END.
   END. 

   /* volume discount has been removed */
   IF ldDiscPrcnt = 0 AND VMobCDR.Disc% > 0 THEN DO:
      /* add old amount for counters (no matter what basis is found) */
      FIND FIRST ttDisc WHERE
                 ttDisc.Month = liMth      AND
                 ttDisc.CLI   = VMobCDR.CLI NO-ERROR.
      IF NOT AVAILABLE ttDisc THEN DO:
         CREATE ttDisc.
         ASSIGN ttDisc.Month     = liMth
                ttDisc.CLI       = VMobCDR.CLI
                ttDisc.BasisType = 99.
      END.

      ttDisc.OldDisc = ttDisc.OldDisc + VMobCDR.DiscValue.

      FIND bUpdMob WHERE RECID(bUpdMob) = RECID(VMobCDR) EXCLUSIVE-LOCK.
   
      fCreateTMQueue(BUFFER bUpdMob,
                            iiAgrCust,
                            -1).
      
      ASSIGN bUpdMob.Disc%     = 0
             bUpdMob.Amount    = bUpdMob.Amount + bUpdMob.DiscValue
             bUpdMob.DiscValue = 0
             bUpdMob.TotDisc   = 0.

      fCreateTMQueue(BUFFER bUpdMob,
                     iiAgrCust,
                     1).
   END.

END FUNCTION.

PROCEDURE pGetVolDiscDefinition:

   DEF INPUT PARAMETER iiCustNum   AS INT NO-UNDO.
   DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.

   EMPTY TEMP-TABLE ttConf.
   EMPTY TEMP-TABLE ttVAmt.
   EMPTY TEMP-TABLE ttDisc.

   FIND FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
   
   /* collect volume discount; basis and call values */
   FOR EACH BillTarg NO-LOCK WHERE
            BillTarg.CustNum = iiCustNum AND
            BillTarg.DiscPlan NE "":

      FOR EACH DPConf NO-LOCK WHERE
               DPConf.Brand      = Customer.Brand    AND
               DPConf.DiscPlan   = BillTarg.DiscPlan AND 
               DPConf.ValidFrom <= idaToDate     AND
               DPConf.ValidTo   >= idaFromDate   AND
               /* DiscType 0 = fixed % */
               DPConf.DiscType      > 0,

          EACH DpBasis OF DPConf NO-LOCK:

         CREATE ttConf.
         ASSIGN liVolID           = liVolID + 1
                ttConf.VolID      = liVolID
                ttConf.BillTarget = BillTarget.BillTarget
                ttConf.DpConf     = DPConf.DPConfNum
                ttConf.VFrom      = DPConf.ValidFrom
                ttConf.VTo        = DPConf.ValidTo
                ttConf.BasisType  = DPBasis.BasisType
                ttConf.VolKey     = IF DPBasis.BasisType = 1
                                    THEN DPBasis.BillCode
                                    ELSE IF DPBasis.BasisType = 2
                                         THEN STRING(DPBasis.CCN)
                                         ELSE "" 
                ttConf.StartFee   = DPConf.StartFee.
      END.
   END.

END PROCEDURE.

PROCEDURE pInitializeVolDisc:

   DEF INPUT PARAMETER iiInvSeq AS INT NO-UNDO.

   /* nothing defined */
   IF NOT CAN-FIND(FIRST ttConf) THEN RETURN "". 

   FIND FIRST InvSeq WHERE InvSeq.InvSeq = iiInvSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE InvSeq THEN RETURN "ERROR:Unknown invseq".

   FOR EACH  MobCDR NO-LOCK WHERE
             MobCDR.InvCust  = InvSeq.CustNum AND
             MobCDR.InvSeq   = iiInvSeq AND
             MobCDR.DiscType = 2:

      llColl = FALSE.

      FOR EACH ttConf USE-INDEX BillTarget WHERE
               ttConf.BillTarget = MobCDR.BillTarget:

         /* valid */
         IF ttConf.VFrom > MobCDR.DateSt  OR
            ttConf.VTo   < MobCDR.DateSt
         THEN NEXT. 


         /* BasisType (index is descending -> priority) : 
            0 = generic (all calls),
            1 = billitem
            2 = CCN 
         */
         CASE ttConf.BasisType:
         WHEN 0 THEN llColl = TRUE.
         WHEN 1 
            THEN DO:
               IF MobCDR.BillCode = ttConf.VolKey THEN llColl = TRUE.
            END.
         WHEN 2
            THEN DO:
               IF STRING(MobCDR.CCN) = ttConf.VolKey THEN llColl = TRUE.
            END.
         END CASE. 

         IF llColl THEN DO:   
            ASSIGN liMth = YEAR(MobCDR.DateSt) * 100 + MONTH(MobCDR.DateSt).

            fCollDisc(MobCDR.CLI,
                      MobCDR.Amount + MobCDR.DiscValue, /* possible old disc.*/
                      MobCDR.BillDur). 

            /* only first found is used */
            LEAVE.
         END. 

      END.  /* ttConf */

   END.   /* MobCDR */

   /* get discount percentages */
   FOR EACH ttConf,
   EACH ttVAmt WHERE
         ttVAmt.VolID = ttConf.VolID
   BREAK BY ttConf.BillTarget
         BY ttConf.DPConf
         BY ttVAmt.Month:

      ACCUMULATE ttVAmt.CallAmt (TOTAL BY ttVAmt.Month)
                 ttVAmt.CallQty (TOTAL BY ttVAmt.Month)
                 ttVAmt.CallDur (TOTAL BY ttVAmt.Month). 

      IF LAST-OF(ttVAmt.Month) THEN DO:

         FIND FIRST DPConf NO-LOCK WHERE
                    DPConf.DPConfNum = ttConf.DPConf.

                
         /* DiscType; 1 = duration,
                      2 = value,
                      3 = qty 
         */
         ASSIGN ldColl = 0. 
         CASE DPConf.DiscType:
         WHEN 1 THEN ldColl = (ACCUM TOTAL BY ttVAmt.Month ttVAmt.CallDur) / 60.
         WHEN 2 THEN ldColl = (ACCUM TOTAL BY ttVAmt.Month ttVAmt.CallAmt).
         WHEN 3 THEN ldColl = (ACCUM TOTAL BY ttVAmt.Month ttVAmt.CallQty).
         END CASE. 

         /* StepUsage determines whether only highest exceeded step is noted 
            (i.e. % from that step), or should sum be divided according to 
            steps and use %s from each step */

         DO liLimit = 10 to 1 by -1:

            IF DPConf.Limit[liLimit] NE 0 THEN DO:

               IF ldColl >= DPConf.Limit[liLimit]
               THEN DO:
                  FOR EACH bttConf WHERE
                           bttConf.DPConf     = ttConf.DPConf     AND
                           bttConf.BillTarget = ttConf.BillTarget,
                      EACH bttVAmt WHERE  
                           bttVAmt.VolID  = bttConf.VolID AND
                           bttVAmt.Month  = ttVAmt.Month:

                     CREATE ttDisc.
                     ASSIGN ttDisc.Month     = bttVAmt.Month
                            ttDisc.CLI       = bttVAmt.CLI
                            ttDisc.BasisType = bttConf.BasisType
                            ttDisc.DiscType  = DPConf.DiscType
                            ttDisc.VolKey    = bttConf.VolKey
                            ttDisc.StartFee  = bttConf.StartFee
                            ttDisc.DiscPrcnt = DPConf.DiscPrcnt[liLimit]
                            ttDisc.Order     = IF DPConf.StepUsage > 0
                                               THEN liLimit
                                               ELSE 0.
                            
                     /* use %s from each step */
                     IF DPConf.StepUsage > 0 THEN DO:
                        ttDisc.MaxLimit = IF liLimit < 10 AND
                                             DPConf.Limit[liLimit + 1] > 0
                                          THEN DPConf.Limit[liLimit + 1] -
                                               DPConf.Limit[liLimit]
                                          ELSE 0.
                        IF liLimit = 1 
                        THEN ttDisc.MaxLimit = ttDisc.MaxLimit + 
                                               DPConf.Limit[liLimit].

                        /* amounts below first limit don't get discount 
                           for usage type 1 */     
                        IF DPConf.StepUsage = 1 AND liLimit = 1 
                        THEN ttDisc.MinLimit = DPConf.Limit[liLimit].          
      
                     END.                                         

                  END.

                  /* use only last (highest) exceeded step */
                  IF DPConf.StepUsage = 0 THEN liLimit = 0. 
               END. 

            END.  /* limit > 0 */

         END.
                  
      END.   /* month changes */
   END.

   RETURN "".  
END PROCEDURE.

/* actual calculation of discount */
PROCEDURE pCalcVolDisc.

   DEF INPUT PARAMETER iiInvSeq AS INT NO-UNDO.

   FIND FIRST InvSeq WHERE
              InvSeq.InvSeq = iiInvSeq
   NO-LOCK NO-ERROR.

   /* 'rerate' calls; go through all calls that have been marked as 
      base for volume discount */
   FOR EACH MobCDR NO-LOCK WHERE
            MobCDR.InvCust  = InvSeq.CustNum AND
            MobCDR.InvSeq   = iiInvSeq AND
            MobCDR.DiscType = 2:

      fVolDiscMob(BUFFER MobCDR,
                  InvSeq.AgrCust).
   END.

END PROCEDURE.

PROCEDURE pUpdateMthCounter:

   DEF INPUT PARAMETER iCustNum AS INT NO-UNDO.

   /* update unbilled balance */
   FOR EACH ttDisc 
   BREAK BY ttDisc.Month:

      ACCUMULATE ttDisc.OldDisc (TOTAL BY ttDisc.Month)
                 ttDisc.NewDisc (TOTAL BY ttDisc.Month).

      IF LAST(ttDisc.Month) AND
         (ACCUM TOTAL ttDisc.OldDisc) NE
         (ACCUM TOTAL ttDisc.NewDisc)
      THEN DO:

         fCustCount(iCustNum,
                    "UB",
                   (ACCUM TOTAL ttDisc.OldDisc) -
                   (ACCUM TOTAL ttDisc.NewDisc)). 
      END.      
   END.

END PROCEDURE.

PROCEDURE pUpdateCallCounter:

   DEF INPUT PARAMETER iInvSeq AS INT  NO-UNDO.

   /* update unbilled balance and monthly counters */
   FOR EACH ttDisc
   BREAK BY ttDisc.CLI:

      ACCUMULATE ttDisc.OldDisc (TOTAL BY ttDisc.CLI)
                 ttDisc.NewDisc (TOTAL BY ttDisc.CLI).

      IF LAST-OF(ttDisc.CLI) AND
         (ACCUM TOTAL BY ttDisc.CLI ttDisc.OldDisc) NE
         (ACCUM TOTAL BY ttDisc.CLI ttDisc.NewDisc)
      THEN DO:
      END.

   END.

END PROCEDURE.


