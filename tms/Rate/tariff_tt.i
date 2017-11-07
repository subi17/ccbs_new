/* --------------------------------------------------------------------------
  MODULE .......: TARIFF.I
  FUNCTION .....: FIND correct Tariff FOR pricing
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 01.03.01 kl
  MODIFIED .....: 07.04.01 kl RateBSub & Tariff tables modified
                  05.09.01/aam set rc TO 9 IF RateBSub is NOT found
                  01.11.01 kl: remember previously analysed customer
                  20.05.02 kl: DEFAULT Price list FOR Tariff records
                  11.09.02/aam DpBasis instead of CustDiscProd and 
                               Tariff.DiscPerc 
                  20.09.02/aam new logic with PListConf and liCCN
                  21.02.03/aam use of RatePref
                  05.03.03 kl: use rate-plcode with PNP calls
                  19.03.03/aam BDest and CustNum into use again
                  23.03.03 kl: tuning only
                  25.03.03 kl: Better RowId checking
                  26.03.03/aam llCurrUnit
                  09.09.03/aam lcRateBrand
                  07.11.03/jp  find tariff with lcratebrand
                  31.05.04/aam ServRid & MPMRid
                  05.04.05 jp&tp tariff.custnum 0 when common tariff

  VERSION.......: M15
--------------------------------------------------------------------------- */

{Func/fcustpl.i}

/* variables FOR calculation results */
DEF VAR discpr      AS dec  NO-UNDO. /* discount percentage         */
DEF VAR discval     AS dec  NO-UNDO. /* discount VALUE              */
DEF VAR genprice    AS dec  NO-UNDO. /* IF general Price was used   */
DEF VAR disctype    AS INT  NO-UNDO. /* discount RepType               */
DEF VAR priced      AS LOG  NO-UNDO. /* is the Result final Price   */
DEF VAR rc          AS INT  NO-UNDO. /* RETURN code IF error        */

DEF BUFFER gen_rate FOR Tariff.

DEF VAR netto    AS LOG NO-UNDO INIT FALSE.
DEF VAR debug    AS LOG NO-UNDO INIT FALSE.
DEF VAR sekvel   AS dec NO-UNDO.
DEF VAR beg-zone AS INT NO-UNDO.  /* beginning time zone */
DEF VAR lFound   AS i   NO-UNDO.
DEF VAR dround   AS i   NO-UNDO.

/* customer & discount related booleans */
DEF VAR cu-stchrg AS lo NO-UNDO INIT TRUE.
DEF VAR di-stchrg AS lo NO-UNDO INIT TRUE.
DEF VAR cu-vdisc  AS lo NO-UNDO INIT FALSE.
DEF VAR prevAcust AS i  NO-UNDO.
DEF VAR prevRcust AS i  NO-UNDO.
DEF VAR bARate    AS lo NO-UNDO.
DEF VAR bRRate    AS lo NO-UNDO.

DEF VAR liXCCN       AS INT  NO-UNDO.
DEF VAR lcXBDest     AS CHAR NO-UNDO.

{Rate/price_tt.i}

FUNCTION fRateCCN RETURNS INTEGER
  (iBDest    AS CHAR,
   iiDestType AS INT,
   iDialType AS INT).

    /* first with explicit dialling type */
    FIND FIRST RateCCN WHERE
               RateCCN.Brand    = lcRateBrand AND
               RateCCN.BDest    = iBDest      AND
               RateCCN.DestType = iiDestType  AND
               RateCCN.DialType = iDialType 
    NO-LOCK NO-ERROR.

    /* then with general if explicit was not found */
    IF NOT AVAILABLE RateCCN THEN
       FIND FIRST RateCCN WHERE
                  RateCCN.Brand    = lcRateBrand AND
                  RateCCN.BDest    = iBDest      AND
                  RateCCN.DestType = iiDestType  AND
                  RateCCN.DialType = 0 
       NO-LOCK NO-ERROR.

    IF AVAILABLE RateCCN THEN RETURN RateCCN.CCN.
    ELSE RETURN 0. 

END FUNCTION.


FUNCTION fTariff RETURNS INTEGER:

   /* reset RETURN values */
   ASSIGN
     bsub-prod = ""    
     lFound    = 0
     rc        = 0
     base      = 0
     bprice    = 0
     discpr    = 0
     discval   = 0
     lcServRid = ""
     lcMPMRid  = ""
     genprice  = 0
     rowid     = 0
     dround    = 3 
     disctype  = 0     /* default: no %-based discount */
     priced    = TRUE. /* default: pricing is final, 
                          volume Discount. modifies this */
   ASSIGN
      liXCCN   = liCCN
      lcXBDest = bsubs.

   /* 1. check if there are prices defined explicitly for this customer 
      2. if not then find tariff using predetermined pricelist (PNP)
      3. if not found then get rateplan from billtarget
      3.1 go through rateplan's plistconf
      3.2 check each valid pricelist by priority if tariff using CCN
          can be found 
   */

   FIND FIRST BillTarget WHERE
              BillTarget.CustNum    = rate-cust AND
              BillTarget.BillTarget = liBillTarget
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE BillTarget THEN DO:
      rc = 9. 
      RETURN rc.
   END.

   IF c_bppref = "" THEN lcRatePref = "DIR". 
   ELSE                  lcRatePref = c_bppref.  

   /* get the appropiate dialling type for choosing pricelist */
   FIND FIRST ttRatePref WHERE 
              ttRatePref.Brand    = lcRateBrand AND
              ttRatePref.Prefix   = lcRatePref  AND
              ttRatePref.DialType = liDialType
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttRatePref THEN DO: 
      rc = 7. 
      RETURN rc.
   END.

   /* has customer got dedicated prices */
   IF rate-plcode = "" AND ttRatePref.CustRate THEN DO:

      FOR FIRST ttTariff NO-LOCK WHERE
                ttTariff.Brand      = lcRateBrand AND
                ttTariff.CustNum    = rate-cust   AND
                ttTariff.CCN        = liXCCN      AND 
                ttTariff.BDest      = lcXBDest    AND
                ttTariff.ValidFrom <= c_day       AND
                ttTariff.ValidTo   >= c_day:
         ASSIGN
            Rowid       = ttTariff.TariffNum
            bsub-prod   = ttTariff.BillCode
            rate-plcode = ttTariff.PriceList
            llCurrUnit  = ttTariff.CurrUnit.

      END.          

      IF Rowid = 0 THEN 
      FOR FIRST ttTariff NO-LOCK WHERE
                ttTariff.Brand      = lcRateBrand AND
                ttTariff.CustNum    = rate-cust   AND
                ttTariff.CCN        = liXCCN      AND 
                ttTariff.BDest      = ""          AND 
                ttTariff.ValidFrom <= c_day       AND
                ttTariff.ValidTo   >= c_day:

         ASSIGN
            Rowid       = ttTariff.TariffNum
            bsub-prod   = ttTariff.BillCode
            rate-plcode = ttTariff.PriceList
            llCurrUnit  = ttTariff.CurrUnit.

      END.

      IF Rowid > 0 THEN DO:
         IF rate-plcode = "" 
         THEN rate-plcode = fCallPriceList(BillTarget.CustNum,
                                           BillTarget.BillTarget,
                                           "",
                                           ttRatePref.RatePref,
                                           0,
                                           c_day).

         IF rate-plcode NE "" THEN DO:
            FIND FIRST ttPriceList WHERE
                       ttPriceList.Brand     = lcRateBrand AND
                       ttPriceList.PriceList = rate-plcode
            NO-LOCK NO-ERROR.

            IF NOT AVAILABLE ttPriceList THEN Rowid = 0.
            ELSE ASSIGN
               /* with how many decimals should prices be rounded */
               dround     = ttPriceList.Rounding
               /* VAT included in prices */
               llVATIncl  = ttPriceList.InclVAT.
         END.
         ELSE Rowid = 0. 

      END.

   END.

   /* pricelist has been determined already (PNP) */
   IF Rowid = 0 AND rate-plcode NE "" THEN DO:

      /* first get tariff using both CCN and BDest */
      FOR FIRST ttTariff NO-LOCK WHERE
                ttTariff.Brand      = lcRateBrand AND
                ttTariff.CCN        = liXCCN      AND 
                ttTariff.PriceList  = rate-plcode AND
                ttTariff.BDest      = lcXBDest    AND
                ttTariff.ValidFrom <= c_day       AND
                ttTariff.ValidTo   >= c_day:
         ASSIGN
            Rowid      = ttTariff.TariffNum
            bsub-prod  = ttTariff.BillCode
            llCurrUnit = ttTariff.CurrUnit.
      END.

      IF Rowid = 0 THEN 
      FOR FIRST ttTariff NO-LOCK WHERE
                ttTariff.Brand      = lcRateBrand AND
                ttTariff.CCN        = liXCCN      AND 
                ttTariff.PriceList  = rate-plcode AND
                ttTariff.Bdest      = ""          AND
                ttTariff.ValidFrom <= c_day       AND
                ttTariff.ValidTo   >= c_day:
         ASSIGN
            Rowid      = ttTariff.TariffNum
            bsub-prod  = ttTariff.BillCode
            llCurrUnit = ttTariff.CurrUnit.
      END.


      IF Rowid > 0 THEN DO:

         FIND FIRST ttPriceList WHERE
                    ttPriceList.Brand     = lcRateBrand AND
                    ttPriceList.PriceList = rate-plcode 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ttPriceList THEN Rowid = 0.

         ELSE ASSIGN
            /* with how many decimals should prices be rounded */
            dround    = ttPriceList.Rounding
            /* VAT included in prices */
            llVATIncl = ttPriceList.InclVAT.
      END.

   END.

   ELSE IF RowId = 0 THEN DO:
      /* go through pricelists by effective date and priority */

      FOR EACH ttPListConf NO-LOCK WHERE
               ttPListConf.Brand    = lcRateBrand         AND
               ttPListConf.RatePlan = BillTarget.RatePlan AND
               ttPListConf.dFrom   <= c_day               AND
               ttPListConf.dTo     >= c_day,
          EACH ttPriceList OF ttPListConf NO-LOCK WHERE
               ttPriceList.Prefix = ttRatePref.RatePref AND
              /* are customer dedicated price lists allowed for this prefix */
              (IF ttRatePref.CustRate   = FALSE THEN 
                  ttPriceList.DedicList = FALSE
               ELSE TRUE)
      BY ttPListConf.Prior:
         /* first get tariff using both CCN and BDest */
         FOR FIRST ttTariff NO-LOCK WHERE
                   ttTariff.Brand      = lcRateBrand         AND
                   ttTariff.CCN        = liXCCN              AND 
                   ttTariff.PriceList  = ttPListConf.PriceList AND
                   ttTariff.BDest      = lcXBDest            AND
                   ttTariff.Custnum    = 0                   AND 
                   ttTariff.ValidFrom <= c_day               AND
                   ttTariff.ValidTo   >= c_day:
            ASSIGN
               Rowid      = ttTariff.TariffNum
               bsub-prod  = ttTariff.BillCode
               llCurrUnit = ttTariff.CurrUnit.

    
         END.

         IF Rowid = 0 THEN 
         FOR FIRST ttTariff NO-LOCK WHERE
                   ttTariff.Brand      = lcRateBrand         AND
                   ttTariff.CCN        = liXCCN              AND 
                   ttTariff.PriceList  = ttPListConf.PriceList AND
                   ttTariff.BDest      = ""                  AND
                   ttTariff.CustNum    = 0                   AND 
                   ttTariff.ValidFrom <= c_day               AND
                   ttTariff.ValidTo   >= c_day:


            ASSIGN
               Rowid      = ttTariff.TariffNum
               bsub-prod  = ttTariff.BillCode
               llCurrUnit = ttTariff.CurrUnit.

         END.

         /* first one found is accepted */
         IF Rowid > 0 THEN DO:

            ASSIGN  
               /* with how many decimals should prices be rounded */
               dround      = ttPriceList.Rounding
               /* VAT included in prices */
               llVATIncl   = ttPriceList.InclVAT
               rate-plcode = ttPriceList.PriceList.

            LEAVE.

         END. 

      END. 

   END.

   /* discounts */
   IF BillTarg.DiscPlan NE "" THEN DO:

      FOR EACH DPConf NO-LOCK WHERE
               DPConf.Brand       = lcRateBrand       AND
               DPConf.DiscPlan    = BillTarg.DiscPlan AND
               DPConf.ValidFrom  <= c_day             AND
               DPConf.ValidTo    >= c_day:

         /* DiscTypeType (descending order -> priority): 
            2 = CCN 
            1 = billitem
            0 = generic (all calls)
         */
         FOR FIRST DpBasis OF DPConf NO-LOCK USE-INDEX DPConfNum WHERE
                   DpBasis.BasisType = 2     AND
                   DpBasis.CCN      = liXCCN: 
            discpr = 1.
         END.
         IF discpr = 0 THEN
            FOR FIRST DpBasis OF DPConf NO-LOCK USE-INDEX DPConfNum WHERE
                      DpBasis.BasisType = 1         AND 
                      DpBasis.BillCode = bsub-prod:
               discpr = 1.
            END.
         IF discpr = 0 THEN
            FOR FIRST DpBasis OF DPConf NO-LOCK USE-INDEX DPConfNum WHERE
                      DpBasis.BasisType = 0:
               discpr = 1.
            END. 

         /*  discount found */
         IF discpr = 1 THEN DO:

            /* fixed% */
            IF DPConf.DiscType = 0 THEN ASSIGN
               disctype  = 1
               discpr    = DPConf.DiscPrcnt[1]
               di-stchrg = DPConf.StartFee.

            /* volume based */
            ELSE ASSIGN 
               disctype = 2    
               discpr   = 0.    /* final % is calculated later */

            /* first one found is used */        
            LEAVE.   

         END.

      END.     

   END. /* discounts */

   /* NO RATE was found > error flag */
   IF Rowid = 0 THEN ASSIGN rc = 5. 

   ELSE DO:  
      /*  OK, let us proceed .....  
          - either a private OR a general Price is found
          - VARIABLE discpr includes the disc-%, IF any
          - IF a volume discount, DirDiscPerc is 0 but disctype is 2
      */


      FIND ttTariff WHERE 
           ttTariff.Brand     = lcRateBrand AND 
           ttTariff.TariffNum = Rowid NO-LOCK.

      ASSIGN lcServRid = ttTariff.ServRid
             lcMPMRid  = ttTariff.MPMRid.
      /* special cases */
      IF lcServRid = "1201000" AND bsub-prod = "I" THEN DO:
          IF c_bppref > "" AND LOOKUP(c_bppref,lcOwnPrefix) > 0 
          THEN lcServRid = "1202000".
      END.
 
      bPrice = round(dec(fPrice()),dRound).
            /* fixed% discount */
      IF disctype = 1 THEN ASSIGN 
         discval = round((bprice - IF di-stchrg THEN 0 ELSE base)
                         * discpr / 100,dRound). 

   END. /* price was found */

   RETURN rc.

END. /* fPricing  */


