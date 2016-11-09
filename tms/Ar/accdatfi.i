/* accdatfi.i   14.03.01/aam
   get accounting data from different sources into a TEMP-TABLE 

   callers: accdatli.p
            accdatex.p

   GLOBAL-DEFINE:
   GetOnlySums  YES: get amounts AS positive values, otherwise get
                     sales AS negative figures FOR accounting
   UseEventDate YES: accumulate sums FOR EACH Event Date separately

   10.05.01/aam periodize invoice line's coitems
   20.02.02/aam NEW logic FOR collecting data
                (InvSeq AND other NEW features of TMS),
                NEW logic FOR periodizing (FUNCTION fPeriodize)
   07.06.02/aam use Invoice.OverPaym FOR overpayment,
                VAT handling FOR advance payment 
   17.06.02/aam skip invoices defined in parameter InvTypeDenied
   01.08.02/aam day for unbilled separately from situation date
                (i.e. billing date) 
   06.08.02/aam check InvRow.VATCode when removing VAT 
   12.08.02/aam don't round sums of individual calls
   12.09.02/aam divide unbilled into month-level
   01.10.02/aam use InvRow.VatPerc and fGetCustPriceList()
   14.10.02/aam Currency handling 
   26.02.03/aam use of PriceList.CurrUnit
   09.09.03/aam brand 
   02.12.03/aam periodize also when 'concerns' begins after end of rep.period
   31.12.03/aam bitem&coinv.VatIncl, asiakas.VatUsage, fVatFactor()
   25.05.04/aam RevVatIncl determines whether vat is posted separately or not,
                AccRid,
                account sums divided per vatcode
   04.08.05/aam use MobCDR.MPMAmt for mpm
   02.12.05/aam use InvAsub.GenPrice (=MPMAmt) for billed section
   20.12.05/aam unbilled cdrs with pCurrentMonthCDRs
   30.03.06/aam post difference within one invoice caused by the use of
                3 decimals in invrows into rounding acc,
                collect VatPercent
   17.05.06/aam status frames to the left
   28.08.06/aam ilCalls and ilFees to FindUnbilled

*/

{commali.i}
{coinv.i}
{fcustpl.i}
{fcurrency.i}
{fvatfact.i}
{cparam2.i}
{accdata.i}

DEF STREAM slog.

DEF TEMP-TABLE wAccData NO-UNDO
    FIELD EType      AS CHAR 
    FIELD Month      AS INT
    FIELD Igcode     AS CHAR
    FIELD Category   AS CHAR
    FIELD BillCode   AS CHAR 
    FIELD AccNum     AS INT
    FIELD CostCentre AS CHAR
    FIELD AccRid     AS CHAR
    FIELD VatPerc    AS DEC
    FIELD VatUsage   AS INT 
    FIELD VatCode    AS INT
    FIELD Amount     AS DEC
    INDEX AccData EType Month Igcode Category BillCode AccNum.

DEF TEMP-TABLE ttDebt NO-UNDO
    FIELD Category AS CHAR
    FIELD Month    AS INT
    FIELD BillCode AS CHAR
    FIELD AccNum   AS INT
    FIELD Amount   AS DEC
    INDEX Category Category Month BillCode AccNum.

DEF TEMP-TABLE ttCDRColl NO-UNDO
    FIELD BillCode AS CHAR
    FIELD VatIncl  AS LOG
    FIELD CDate    AS DATE
    FIELD Rid      AS CHAR
    FIELD Amt      AS DEC
    INDEX BillCode BillCode CDate Rid.

DEF TEMP-TABLE ttMCDR NO-UNDO
    FIELD Category AS CHAR
    FIELD InvGroup AS CHAR
    FIELD BillCode AS CHAR
    FIELD Rid      AS CHAR
    FIELD VatIncl  AS LOG 
    FIELD VatUsage AS INT 
    FIELD TaxZone  AS CHAR 
    FIELD Amt      AS DEC
    INDEX Category Category InvGroup BillCode Rid.
    
DEF TEMP-TABLE ttInvoice NO-UNDO
    FIELD InvNum AS INT
    INDEX InvNum InvNum.
    
DEF BUFFER bwAccData  FOR wAccData.

DEF VAR xCount     AS INT  NO-UNDO.
DEF VAR xSign      AS INT  NO-UNDO INIT -1.
DEF VAR xGetPeriod AS INT  NO-UNDO.
DEF VAR xBalAcct   AS INT  NO-UNDO.

DEF VAR xValidFrom   AS Date NO-UNDO.
DEF VAR xValidTo   AS Date NO-UNDO. 
DEF VAR xPerDays   AS INT  NO-UNDO.
DEF VAR xAllDays   AS INT  NO-UNDO.
DEF VAR xRemDays   AS INT  NO-UNDO. 
DEF VAR xFrom      AS INT  NO-UNDO.
DEF VAR xFromDate  AS Date NO-UNDO. 
DEF VAR xMonth     AS INT  NO-UNDO.
DEF VAR xPerFrom   AS INT  NO-UNDO. 
DEF VAR xPerDate   AS Date NO-UNDO.
DEF VAR xDate      AS Date NO-UNDO. 
DEF VAR xDays      AS INT  NO-UNDO. 
DEF VAR xFeb       AS LOG  NO-UNDO. 
DEF VAR xToFeb     AS LOG  NO-UNDO. 
DEF VAR xProcDate  AS Date NO-UNDO.     
DEF VAR xMonthChg  AS LOG  NO-UNDO. 
DEF VAR liPer1     AS INT  NO-UNDO.
DEF VAR liPer2     AS INT  NO-UNDO. 

DEF VAR xAmt        AS DEC  NO-UNDO.
DEF VAR xRemAmt     AS DEC  NO-UNDO. 
DEF VAR xVatAmt     AS DEC  NO-UNDO.
DEF VAR xVatIncl    AS LOG  NO-UNDO.
DEF VAR xVatTot     AS DEC  NO-UNDO.
DEF VAR xVatPrcnt   AS DEC  NO-UNDO.
DEF VAR xQty        AS INT  NO-UNDO. 
DEF VAR ldCustRate  AS DEC  NO-UNDO.
DEF VAR liMonth     AS INT  NO-UNDO. 
DEF VAR liVatUsage  AS INT  NO-UNDO.
DEF VAR ldVatFactor AS DEC  NO-UNDO.
DEF VAR llSepVat    AS LOG  NO-UNDO.
DEF VAR lcTypeDen   AS CHAR NO-UNDO.
DEF VAR ldGross     AS DEC  NO-UNDO. 
DEF VAR ldInvTot    AS DEC  NO-UNDO.
DEF VAR liDiffAcc   AS INT  NO-UNDO. 

&IF "{&GetOnlySums}" = "YES"
&THEN ASSIGN xSign = 1.
&ENDIF

       /* is vat posted separately or included in sales postings */
ASSIGN llSepVat  = (fCParamI("RevVatIncl") NE 1) 
       /* invoices that are excluded */
       lcTypeDen = fCParamC("InvTypeDenied").

/* file FOR errors in the RUN */
OUTPUT STREAM slog TO 
    VALUE("/tmp/rev_err_" + STRING(TODAY,"999999") + 
          "_" + STRING(TIME) + ".txt") APPEND.

/* collect accounting data TO a TEMP-TABLE */
FUNCTION MakeTempData RETURNS LOGIC
    (iType       AS CHAR,    /* RepType, U=unbilled, B=billed, PD=periodized */
     iDate       AS DATE,    /* period                                    */
     iIgcode     AS CHAR,    /* invoicing group                           */
     iKatkod     AS CHAR,    /* customer category                         */
     iProduct    AS CHAR,    /* BillCode code                             */
     iAccount    AS INT,     /* AccNum                                    */
     iCostCentre AS CHAR,    /* cost centre                               */
     iAccRid     AS CHAR,    /* reporting id                              */
     iVatUsage   AS INT,     /* vat usage 1-4                             */
     iVatCode    AS INT,     /* vat                                       */
     iAmount     AS DEC).    /* posted sum                                */

    DEF VAR ldVatPerc AS DEC NO-UNDO.
    
    IF iAmount = 0 THEN RETURN FALSE.

    IF iDate = ? THEN liMonth = 0.
    ELSE ASSIGN liMonth = YEAR(iDate) * 100 + MONTH(iDate).

    FIND FIRST wAccData WHERE
        wAccData.EType      = iType     AND
        wAccData.Month      = liMonth   AND 
        wAccData.Igcode     = iIgcode   AND
        wAccData.Category   = iKatkod   AND
        wAccData.BillCode   = iProduct  AND
        wAccData.AccNum     = iAccount  AND
        wAccData.AccRid     = iAccRid   AND
        wAccData.VatCode    = iVatCode  AND
        wAccData.VatUsage   = iVatUsage
        NO-ERROR.

    IF NOT AVAILABLE wAccData THEN DO:
        ldVatPerc = 0.
        IF iVatCode > 0 THEN DO:
           FIND VatCode WHERE
                VatCode.VatCode = iVatCode NO-LOCK NO-ERROR.
           IF AVAILABLE VatCode THEN ldVatPerc = VatCode.VatPerc.
        END.
           
        CREATE wAccData.
        ASSIGN wAccData.EType      = iType  
               wAccData.Month      = liMonth   
               wAccData.Igcode     = iIgcode
               wAccData.Category   = iKatkod
               wAccData.BillCode   = iProduct
               wAccData.AccNum     = iAccount
               wAccData.CostCentre = iCostCentre
               wAccData.AccRid     = iAccRid
               wAccData.VatCode    = iVatCode
               wAccData.VatUsage   = iVatUsage
               wAccData.VatPerc    = ldVatPerc.
    END.

    ASSIGN wAccData.Amount = wAccData.Amount + iAmount.

END FUNCTION.

/* periodized fees */
FUNCTION fPeriodize RETURNS DECIMAL  /* amount that belongs TO the period */
    (iFromDate AS Date,              /* beginning of accounting period    */
     iToDate   AS Date,              /* END of accounting period          */
     iPer1     AS INT,               /* beginning of fee's period         */
     iPer2     AS INT,               /* END of fee's period               */
     iAmt      AS DEC,               /* amount that should be periodized  */
     OUTPUT oRemAmt AS DEC).         /* amount that remains AS adv.payment debt
                                        in the END of accounting period   */

    IF iPer2 = 0 OR iPer2 = ? 
    THEN iPer2 = iPer1.

    /* undefined period */
    IF iPer1 = 0 THEN DO:
        ASSIGN oRemAmt = 0.
        RETURN 0.00.
    END.

    ASSIGN xValidFrom = fInt2Date(iPer1,1)
           xValidTo  = fInt2Date(iPer2,2)
           xMonth    = MONTH(xValidFrom)
           xFrom     = DAY(xValidFrom) - 1
           xFromDate = xValidFrom
           xPerFrom  = IF xValidFrom GE iFromDate AND
                          xValidFrom LE iToDate
                       THEN xFrom
                       ELSE IF iFromDate GE xValidFrom AND
                               iFromDate LE xValidTo AND
                               YEAR(xValidFrom)  = YEAR(iFromDate) AND
                               MONTH(xValidFrom) = MONTH(iFromDate)
                            THEN DAY(MAXIMUM(xValidFrom,iFromDate)) - 1
                            ELSE -1
           xPerDate  = MAXIMUM(xValidFrom,iFromDate)
           xPerDays  = 0
           xAllDays  = 0
           xRemDays  = 0
           NO-ERROR.

    /* error in the dates */
    IF ERROR-STATUS:ERROR      OR
       xValidFrom GT iToDate   OR
       /* ended before chosen accounting period -> no need to periodize */
       xValidTo LE iToDate     OR
       xValidTo LT xValidFrom  OR
       xValidFrom = ?          OR
       xValidTo = ?
    THEN DO:

       oRemAmt = IF xValidFrom NE ? AND xValidFrom > iToDate
                 THEN iAmt
                 ELSE 0.
                                     
       /* does total amount belong to accounting period */
       IF ERROR-STATUS:ERROR OR
          (xValidTo NE ? AND xValidTo LE iToDate)
       THEN RETURN iAmt.
       ELSE RETURN 0.00.
    
    END.

    /* go THROUGH period DAY BY DAY */
    DO xDate = xValidFrom TO xValidTo:

        /* MONTH changes */
        IF MONTH(xDate) NE xMonth OR
           xDate = xValidTo 
        THEN DO:

            /* Date that is Processed */
            ASSIGN xMonthChg = (MONTH(xDate) NE xMonth)
                   xProcDate = xDate - 
                               IF xMonthChg
                               THEN 1
                               ELSE 0.

            /* periodizing is done using a 360 DAY YEAR 
               -> 30 days in EACH MONTH 
            */

            /* february needs special attention 
            */
            ASSIGN xFeb = (MONTH(xValidTo) = 2 AND
                           DAY(xValidTo) GE 28). 

            /* this period is NOT the FIRST match */                            
            IF xPerFrom GE 0 THEN DO:
                /* how many days belongs TO the desired period */
                ASSIGN xDays = IF NOT xMonthChg AND NOT xFeb
                               THEN MINIMUM(30,DAY(xValidTo)) - xPerFrom
                               ELSE 30 - xPerFrom.

                IF xProcDate LE iToDate THEN ASSIGN 
                    xPerDays = xPerDays + xDays.
                ELSE IF xPerDate LE iToDate THEN ASSIGN 
                    xToFeb   = (MONTH(iToDate) = 2 AND
                                DAY(iToDate) GE 28)
                    xPerDays = xPerDays + 
                               (IF xToFeb 
                                THEN 30 
                                ELSE MINIMUM(30,DAY(iToDate))
                               ) - DAY(xPerDate) + 1.

                ASSIGN xPerFrom = 0
                       xPerDate = xDate. 
            END.

            /* FIRST hit TO the desired period */
            ELSE DO:
                IF YEAR(xDate)  = YEAR(iFromDate) AND
                   MONTH(xDate) = MONTH(iFromDate)
                THEN ASSIGN xPerFrom = DAY(iFromDate) - 1   
                            xPerDate = iFromDate. 
            END.

            /* get also ALL days that the fee covers */
            ASSIGN xDays     = IF NOT xMonthChg AND NOT xFeb
                               THEN MINIMUM(30,DAY(xValidTo)) - xFrom 
                               ELSE 30 - xFrom
                   xAllDays  = xAllDays + xDays
                   xMonth    = MONTH(xDate)
                   xFromDate = xDate
                   xFrom     = 0.

            /* AND days after accounting period */
            IF xProcDate GT iToDate THEN DO:
                IF YEAR(xProcDate)  = YEAR(iToDate) AND
                   MONTH(xProcDate) = MONTH(iToDate)
                THEN ASSIGN xRemDays = xRemDays +
                                       IF NOT xMonthChg AND NOT xFeb
                                       THEN MINIMUM(30,DAY(xValidTo)) - 
                                            DAY(iToDate)
                                       ELSE 30 - DAY(iToDate).
                ELSE ASSIGN xRemDays = xRemDays + xDays.                    
            END.
        END.

    END.

    /* amount that remains AS advance payment debt in the END of acc period */
    ASSIGN oRemAmt = IF xAllDays GT 0
                     THEN iAmt * (xRemDays / xAllDays)
                     ELSE 0.

    /* amount that belongs TO the period in question */
    IF xAllDays GT 0 THEN 
      RETURN iAmt * (xPerDays / xAllDays).
    ELSE RETURN 0.00. 

END FUNCTION.

FUNCTION fCalcVatFactor RETURNS LOGICAL
   (idVatPerc  AS DEC,
    icBillCode AS CHAR).
   
    /* if vat is posted separately (llSepVat=TRUE) -> calculate factor so 
       that vat is removed from 'vat included' cases, 
       if vat should be included in sales (llSepVat=FALSE) -> calculate factor 
       so that vat is added to 'vat excluded' cases 
    */
    ldVatFactor = 1.
    
    IF (xVatIncl AND llSepVat) OR 
       (NOT xVatIncl AND NOT llSepVat)
    THEN DO:
       
       ldVatFactor = 1 + idVatPerc / 100.
       
       IF NOT xVatIncl AND NOT llSepVat THEN ldVatFactor = 1 / ldVatFactor.
    END.

END FUNCTION.

FUNCTION fGetVatData RETURNS LOGICAL
    (icPriceList AS CHAR,
     icTaxZone   AS CHAR,   
     icBillCode  AS CHAR).

    DEF VAR ldItemVat  AS DEC NO-UNDO.
    DEF VAR lcTaxClass AS CHAR NO-UNDO.

    DEF BUFFER bVatItem FOR BillItem.
    
    IF icPriceList NE "" THEN DO:
       FIND PriceList WHERE         
            PriceList.Brand     = gcBrand  AND
            PriceList.PriceList = icPriceList NO-LOCK NO-ERROR.
       /* pricelist determines whether VAT is included OR excluded  */
       ASSIGN xVatIncl  = IF AVAILABLE PriceList 
                          THEN PriceList.InclVAT 
                          ELSE FALSE.
    END. 

    lcTaxClass = "1".
    FOR FIRST bVatItem NO-LOCK WHERE
              bVatItem.Brand    = gcBrand AND
              bVatItem.BillCode = icBillCode:
       lcTaxClass = bVatItem.TaxClass.
    END.

    ldItemVat = fTaxPerc(icTaxZone,lcTaxClass, TODAY).

    fCalcVatFactor(ldItemVat,
                   icBillCode).

END FUNCTION.


/* remove vat from amount IF it includes vat or
   add vat to amount if it excludes vat, according to llSepVat */
FUNCTION fManipVat RETURNS DECIMAL
    (iAmount AS DEC).

    RETURN iAmount / ldVatFactor.

END FUNCTION.

FUNCTION fMobCDRAcc RETURNS DECIMAL.
 
   DEF VAR ldCdrAmt AS DEC  NO-UNDO. 
   DEF VAR ldRidAmt AS DEC  NO-UNDO EXTENT 2.
   DEF VAR lcRid    AS CHAR NO-UNDO EXTENT 2.
   DEF VAR ldtCDR   AS DATE NO-UNDO. 
   
   /* call can be divided into two reporting ids */
   IF MobCDR.MpmRid NE MobCDR.ServRid AND 
      MobCDR.MpmRid > "" AND MobCDR.ServRid > ""
   THEN ASSIGN ldRidAmt[1] = MobCDR.MPMAmt
               lcRid[1]    = MobCDR.MpmRid
               ldRidAmt[2] = MobCDR.Amount - MobCDR.MPMAmt
               lcRid[2]    = MobCDR.ServRid.
   ELSE ASSIGN ldRidAmt[1] = MobCDR.Amount
               lcRid[1]    = IF MobCDR.MpmRid > "" 
                             THEN MobCDR.MpmRid
                             ELSE MobCDR.ServRid
               ldRidAmt[2] = 0
               lcRid[2]    = "".
          
   ldCdrAmt = 0.
   
   ldtCDR = DATE(MONTH(MobCDR.DateSt),1,YEAR(MobCDR.DateSt)).
   
   DO xCount = 1 TO 2:
           
      IF ldRidAmt[xCount] = 0 THEN NEXT.
           
      /* currency unit of call */
      fCurrUnit(ldRidAmt[xCount],
                ldRidAmt[xCount],
                MobCDR.CurrUnit,
                "",           
                MobCDR.TariffNum,
                gcBrand,
                OUTPUT xAmt,
                OUTPUT ldGross).

      FIND FIRST ttCDRColl WHERE
                 ttCDRColl.BillCode = MobCDR.BillCode AND
                 ttCDRColl.CDate    = ldtCDR          AND
                 ttCDRColl.Rid      = lcRid[xCount]   AND
                 ttCDRColl.VatIncl  = MobCDR.VatIncl NO-ERROR.
      
      IF NOT AVAILABLE ttCDRColl THEN DO:
         CREATE ttCDRColl.
         ASSIGN ttCDRColl.BillCode = MobCDR.BillCode
                ttCDRColl.CDate    = ldtCDR
                ttCDRColl.Rid      = lcRid[xCount]
                ttCDRColl.VatIncl  = MobCDR.VatIncl.
      END.
      
      ttCDRColl.Amt = ttCDRColl.Amt + xAmt.
     
      ldCdrAmt = ldCdrAmt + xAmt.
   END.

   RETURN ldCdrAmt.
      
END FUNCTION.

FUNCTION fInvASubAcc RETURNS DECIMAL.
 
   DEF VAR ldCdrAmt AS DEC  NO-UNDO. 
   DEF VAR ldRidAmt AS DEC  NO-UNDO EXTENT 2.
   DEF VAR lcRid    AS CHAR NO-UNDO EXTENT 2.
   DEF VAR ldtCDR   AS DATE NO-UNDO. 
   
   /* call can be divided into two reporting ids */
   IF InvASub.MpmRid NE InvASub.ServRid AND 
      InvASub.MpmRid > "" AND InvASub.ServRid > ""
   THEN ASSIGN ldRidAmt[1] = InvASub.GenPrice
               lcRid[1]    = InvASub.MpmRid
               ldRidAmt[2] = InvASub.Amt - InvASub.GenPrice
               lcRid[2]    = InvASub.ServRid.
   ELSE ASSIGN ldRidAmt[1] = InvASub.Amt
               lcRid[1]    = IF InvASub.MpmRid > "" 
                             THEN InvASub.MpmRid
                             ELSE InvASub.ServRid
               ldRidAmt[2] = 0
               lcRid[2]    = "".
          
   ldCdrAmt = 0.
   
   ldtCDR = DATE(MONTH(InvASub.FromDate),1,YEAR(InvASub.FromDate)).
   
   DO xCount = 1 TO 2:
           
      IF ldRidAmt[xCount] = 0 THEN NEXT.
           
      FIND FIRST ttCDRColl WHERE
                 ttCDRColl.BillCode = InvASub.BillCode AND
                 ttCDRColl.CDate    = ldtCDR           AND
                 ttCDRColl.Rid      = lcRid[xCount]    AND
                 ttCDRColl.VatIncl  = Invoice.VatIncl NO-ERROR.
      
      IF NOT AVAILABLE ttCDRColl THEN DO:
         CREATE ttCDRColl.
         ASSIGN ttCDRColl.BillCode = InvASub.BillCode
                ttCDRColl.CDate    = ldtCDR
                ttCDRColl.Rid      = lcRid[xCount]
                ttCDRColl.VatIncl  = Invoice.VatIncl.
      END.
      
      ttCDRColl.Amt = ttCDRColl.Amt + ldRidAmt[xCount].
     
      ldCdrAmt = ldCdrAmt + ldRidAmt[xCount].
   END.

   RETURN ldCdrAmt.
      
END FUNCTION.

FUNCTION fUnbilledCDRColl RETURNS LOGIC
   (icTaxZone AS CHAR):
        
   FOR EACH ttCDRColl:

      xVatIncl = ttCDRColl.VatIncl.
        
      fGetVatData("",
                  icTaxZone,  
                  ttCDRColl.BillCode).

      /* remove/add possible VAT */
      xAmt = fManipVat(ttCDRColl.Amt).
      /* currency */
      xAmt = fToHomeCurr(xAmt,ldCustRate).

      /* error in the amount */
      IF xAmt = ? THEN DO:
         PUT STREAM slog UNFORMATTED
            "Mcdr (U)"         CHR(9)
            ttCDRColl.CDate    CHR(9)
            Customer.Custnum   CHR(9)
            ttCDRColl.BillCode CHR(9)
            xAmt               SKIP.
         NEXT.
      END.

      MakeTempData("U",
                   ttCDRColl.CDate,
                   Customer.InvGroup,
                   Customer.Category,
                   ttCDRColl.BillCode,
                   -1,
                   "",
                   ttCDRColl.Rid, 
                   liVatUsage,
                   -1,
                   xSign * xAmt).
   END.

END FUNCTION.


FUNCTION fInvRowCDRColl RETURNS LOGIC
   (idTotalAmt AS DEC).
      
   FOR EACH ttCDRColl,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            /* billcode-level is enough */
            InvRow.BillCode = ttCDRColl.BillCode AND
            InvRow.RowType  = 2:

      /* vat method of cdr may differ from that of invoice's */
      IF xVatIncl NE ttCDRColl.VatIncl THEN DO:
          IF xVatIncl THEN DO:
             IF llSepVat THEN ldVatFactor = 1.
             ELSE ldVatFactor = 1 / (1 + InvRow.VatPerc / 100).
          END.    
          ELSE DO:
             IF NOT llSepVat THEN ldVatFactor = 1.
             ELSE ldVatFactor = 1 + InvRow.VatPerc / 100.
         END.
      END.

      /* vat method is the same -> use invoice's vat data */
      ELSE DO:
         fCalcVatFactor(InvRow.VatPerc,
                        "").
      END.

      /* remove/add possible VAT */
      xAmt = fManipVat(ttCDRColl.Amt).
      /* currency */
      xAmt = fToHomeCurr(xAmt,ldCustRate).

      /* error in the amount */
      IF xAmt = ? THEN DO:
         PUT STREAM slog UNFORMATTED
            "Mcdr (B)"         CHR(9)
            ttCDRColl.CDate    CHR(9)
            Customer.Custnum   CHR(9)
            ttCDRColl.BillCode CHR(9)
            xAmt               SKIP.
         NEXT.
      END.

      MakeTempData("B",
                   ttCDRColl.CDate,
                   Customer.InvGroup,
                   Customer.Category,
                   ttCDRColl.BillCode,
                   InvRow.SlsAcc,
                   "",
                   ttCDRColl.Rid, 
                   Invoice.VatUsage,
                   InvRow.VatCode,
                   xSign * xAmt).
                   
      idTotalAmt = idTotalAmt - xAmt.             
   END.

   /* possible leftover */
   IF idTotalAmt NE 0 THEN DO:
    
      /* dump the amount to first available cdr-row */     
      FIND FIRST InvRow OF Invoice NO-LOCK WHERE 
                 InvRow.RowType = 2.
                  
      MakeTempData("B",
                   ?,
                   Customer.InvGroup,
                   Customer.Category,
                   InvRow.BillCode,
                   InvRow.SlsAccNum,
                   InvRow.CostCentre,
                   "",
                   Invoice.VatUsage,
                   InvRow.VatCode,
                   fToHomeCurr(xSign * idTotalAmt,
                               Invoice.ExchRate)
                   ).
   END.
  
END FUNCTION.

/* get accounting data from invoice lines */
FUNCTION fInvLineAcc RETURNS LOGICAL.

    DEF VAR ldRowAmt AS DEC NO-UNDO.
    DEF VAR ldMobAmt AS DEC NO-UNDO. 
    
    xVatTot = 0.
    DO xCount = 1 TO 10:
       xVatTot = xVatTot + Invoice.VatAmount[xCount].
    END. 

    ldMobAmt = 0.
    
    FOR EACH InvRow OF Invoice NO-LOCK 
    BREAK BY InvRow.VatPerc
          BY InvRow.Amt:

        ldRowAmt = InvRow.Amt.
        
        /* according to llSepVat remove or add vat */
        ASSIGN xVatAmt = 0.
        IF ((xVatIncl AND llSepVat) OR 
            (NOT xVatIncl AND NOT llSepVat)) AND 
           InvRow.VatPerc > 0                
        THEN DO:

            IF last(InvRow.Amt) THEN 
                ASSIGN xVatAmt = xVatTot
                       xVatTot = 0.
            ELSE DO:
               IF xVatIncl THEN
                    xVatAmt = ROUND(InvRow.Amt * InvRow.VATPerc
                                    / (100 + InvRow.VATPerc),2).
               ELSE xVatAmt = ROUND(InvRow.Amt * InvRow.VatPerc / 100,2).
                                    
               xVatTot = xVatTot - xVatAmt.
            END.

            IF xVatIncl
            THEN ldRowAmt = ldRowAmt - xVatAmt.
            ELSE ldRowAmt = ldRowAmt + xVatAmt.
             
        END.

        ldInvTot  = ldInvTot - ldRowAmt.
        
        /* handle rows with mobcdrs separately */
        IF InvRow.RowType = 2 THEN ldMobAmt = ldMobAmt + ldRowAmt.

        ELSE DO:
           MakeTempData("B",
                        ?,
                        Customer.InvGroup,
                        Customer.Category,
                        InvRow.BillCode,
                        InvRow.SlsAccNum,
                        InvRow.CostCentre,
                        "",
                        Invoice.VatUsage,
                        InvRow.VatCode,
                        fToHomeCurr(xSign * ldRowAmt,
                                    Invoice.ExchRate)
                        ).
       END.                 
    END.

    EMPTY TEMP-TABLE ttCDRColl.
    
    /* mobcdrs may be divided into two reporting ids */
    IF CAN-FIND(FIRST InvASub OF Invoice) THEN 
    FOR EACH InvASub NO-LOCK WHERE
             InvASub.InvNum = Invoice.InvNum:
       fInvASubAcc().      
    END.
    
    ELSE 
    FOR EACH SubInvoice OF Invoice NO-LOCK,
        EACH MobCDR NO-LOCK WHERE
             MobCDR.InvCust = Invoice.CustNum AND
             MobCDR.InvSeq  = SubInvoice.InvSeq:
             
       fMobCDRAcc().
    END.

    fInvRowCDRColl(ldMobAmt).
    
    RETURN TRUE. 

END FUNCTION.    


PROCEDURE pCurrentMonthCDRs:

   DEF INPUT PARAMETER idtDate1  AS DATE NO-UNDO.  
   DEF INPUT PARAMETER idtDate2  AS DATE NO-UNDO.  
   DEF INPUT PARAMETER icInvGrp1 AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icInvGrp2 AS CHAR NO-UNDO.

   DEF VAR ldCdrAmt AS DEC  NO-UNDO. 
   DEF VAR ldRidAmt AS DEC  NO-UNDO EXTENT 2.
   DEF VAR lcRid    AS CHAR NO-UNDO EXTENT 2.
 
   DEF VAR lcTaxZone AS CHAR NO-UNDO.
   
   xQty = 0.
   
   FOR EACH MobCDR USE-INDEX Date NO-LOCK WHERE
            MobCDR.DateSt >= idtDate1 AND
            MobCDR.DateSt <= idtDate2 AND
            MobCDR.InvSeq > 0,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = MobCDR.InvCust:

      IF Customer.InvGroup < icInvGrp1 OR
         Customer.InvGroup > icInvGrp2 
      THEN NEXT. 

      lcTaxZone = fRegionTaxZone(Customer.Region).

      xQty = xQty + 1.

      IF xQty < 100 OR xQty MOD 1000 = 0 THEN DO:
        PAUSE 0.
        DISPLAY "CDRs:" xQty  SKIP
                "Date:" MobCDR.Date 
        WITH NO-LABELS OVERLAY ROW 14 COL 50 TITLE " Collecting (UB) "
            FRAME fCDRQty.
      END.
            
      /* call can be divided into two reporting ids */
      IF MobCDR.MpmRid NE MobCDR.ServRid AND 
         MobCDR.MpmRid > "" AND MobCDR.ServRid > ""
      THEN ASSIGN ldRidAmt[1] = MobCDR.MPMAmt
                  lcRid[1]    = MobCDR.MpmRid
                  ldRidAmt[2] = MobCDR.Amount - MobCDR.MPMAmt
                  lcRid[2]    = MobCDR.ServRid.
      ELSE ASSIGN ldRidAmt[1] = MobCDR.Amount
                  lcRid[1]    = IF MobCDR.MpmRid > "" 
                                THEN MobCDR.MpmRid
                                ELSE MobCDR.ServRid
                  ldRidAmt[2] = 0
                  lcRid[2]    = "".
          
      DO xCount = 1 TO 2:
           
         IF ldRidAmt[xCount] = 0 THEN NEXT.
           
         xAmt = ldRidAmt[xCount].

         FIND FIRST ttMCDR WHERE
                    ttMCDR.Category = Customer.Category AND
                    ttMCDR.InvGroup = Customer.InvGroup AND
                    ttMCDR.BillCode = MobCDR.BillCode   AND
                    ttMCDR.Rid      = lcRid[xCount]     AND
                    ttMCDR.VatIncl  = MobCDR.VatIncl    AND
                    ttMCDR.VatUsage = Customer.VatUsage AND
                    ttMCDR.TaxZone  = lcTaxZone NO-ERROR.
         IF NOT AVAILABLE ttMCDR THEN DO:
         
            CREATE ttMCDR.
            ASSIGN ttMCDR.Category = Customer.Category 
                   ttMCDR.InvGroup = Customer.InvGroup 
                   ttMCDR.BillCode = MobCDR.BillCode   
                   ttMCDR.Rid      = lcRid[xCount]     
                   ttMCDR.VatIncl  = MobCDR.VatIncl    
                   ttMCDR.VatUsage = Customer.VatUsage
                   ttMCDR.TaxZone  = lcTaxZone .
         END.
         
         ttMCDR.Amt = ttMCDR.Amt + xAmt.
      END.
   END.

   HIDE FRAME fCDRQty NO-PAUSE.
   
   xQty = 0.
   
   FOR EACH ttMCDR:
   
      xQty = xQty + 1.

      IF xQty < 100 OR xQty MOD 100 = 0 THEN DO:
        PAUSE 0.
        DISPLAY "CDRs:" xQty  SKIP
        WITH NO-LABELS OVERLAY ROW 14 COL 50 TITLE " Processing (UB) "
            FRAME fCDRProc.
      END.

      xVatIncl = ttMCDR.VatIncl.
        
      fGetVatData("",
                  ttMCDR.TaxZone,  
                  ttMCDR.BillCode).

      /* remove/add possible VAT */
      xAmt = fManipVat(ttMCDR.Amt).

      /* error in the amount */
      IF xAmt = ? THEN DO:
         PUT STREAM slog UNFORMATTED
            "Coll.Mcdr (U)"    CHR(9)
            ttMCDR.BillCode    CHR(9)
            ttMCDR.Category    CHR(9)
            ttMCDR.InvGroup    CHR(9)
            xAmt               SKIP.
         NEXT.
      END.

      MakeTempData("U",
                   idtDate1,
                   ttMCDR.InvGroup,
                   ttMCDR.Category,
                   ttMCDR.BillCode,
                   -1,
                   "",
                   ttMCDR.Rid, 
                   ttMCDR.VatUsage,
                   -1,
                   xSign * xAmt).
                   
   END.

   HIDE FRAME fCDRProc NO-PAUSE.
   
   EMPTY TEMP-TABLE ttMCDR.
   
END PROCEDURE.

/* Unbilled data */
PROCEDURE FindUnbilled:

    /* everything Unbilled until the given Date is taken */
    DEF INPUT PARAMETER iDate      AS Date NO-UNDO.  /* period's END    */
    DEF INPUT PARAMETER idtCDRDate AS DATE NO-UNDO. 
    DEF INPUT PARAMETER iBilled    AS DATE NO-UNDO.  /* when billed     */
    DEF INPUT PARAMETER ilCalls    AS LOG  NO-UNDO.  /* take calls      */
    DEF INPUT PARAMETER ilOldCalls AS LOG  NO-UNDO.  /* take also old calls */
    DEF INPUT PARAMETER ilFees     AS LOG  NO-UNDO.  /* take fees       */
    DEF INPUT PARAMETER iIgCode    AS CHAR NO-UNDO.  /* invoicing group */
    DEF INPUT PARAMETER iCustNum1  AS INT  NO-UNDO.  /* customer Limit  */
    DEF INPUT PARAMETER iCustNum2  AS INT  NO-UNDO.  /* customer Limit  */

    DEF VAR xCtPeriod  AS INT  NO-UNDO.
    DEF VAR liShortPer AS INT  NO-UNDO.
    DEF VAR ldMAmt     AS DEC  NO-UNDO. 
    DEF VAR liTotQty   AS INT  NO-UNDO.
    DEF VAR lcTaxZone  AS CHAR NO-UNDO. 

    ASSIGN xQty       = 0
           xCtPeriod  = YEAR(iDate) * 10000 +
                        MONTH(iDate) * 100  +
                        DAY(iDate)
           liShortPer = YEAR(iDate) * 100 +
                        MONTH(iDate).

    /* get ALL Unbilled data THROUGH customer AND customer's invseqs 
       -> much quicker when e.g. only one group is wanted 
    */
    FOR EACH Customer NO-LOCK WHERE
             Customer.Brand    = gcBrand   AND
             Customer.InvGroup = iIgcode   AND
             Customer.CustNum GE iCustNum1 AND
             Customer.CustNum LE iCustNum2:
       liTotQty = liTotQty + 1.
    END.


    /* older calls before period's beginning are not taken */
    IF NOT ilOldCalls AND idtCDRDate < iDate THEN ilCalls = FALSE.

    /* get ALL Unbilled data THROUGH customer AND customer's invseqs 
       -> much quicker when e.g. only one group is wanted 
    */
    FOR EACH Customer NO-LOCK WHERE
             Customer.Brand    = gcBrand   AND
             Customer.InvGroup = iIgcode   AND
             Customer.CustNum GE iCustNum1 AND
             Customer.CustNum LE iCustNum2:

      ASSIGN xQty       = xQty + 1
             liVatUsage = Customer.VatUsage.

      IF xQty < 100 OR xQty MOD 100 = 0 THEN DO:
        PAUSE 0.
        DISPLAY "Invg:" Customer.InvGroup SKIP
                "Cust:" Customer.CustNum  SKIP
                "Qty :" xQty  "/" TRIM(STRING(liTotQty,">>,>>>,>>9")) 
        WITH NO-LABELS OVERLAY ROW 14 COL 50 TITLE " Collecting (UB) "
            FRAME fQty.
      END.

      /* get rate for possible currency changes */
      ldCustRate = fCurrRate(Customer.Currency,iDate).

      lcTaxZone = fRegionTaxZone(Customer.Region).

      IF ilCalls THEN 
      /* ALL customer's invoicing batches before period's END */
      FOR EACH InvSeq NO-LOCK WHERE
               InvSeq.CustNum   = Customer.CustNum AND
               InvSeq.FromDate <= idtCDRDate:

        /* still Unbilled AT the END of this period */
        IF InvSeq.Billed THEN DO:
            IF NOT CAN-FIND(FIRST ttInvoice WHERE 
                                  ttInvoice.InvNum = InvSeq.InvNum)
            THEN NEXT. 
        END.

        EMPTY TEMP-TABLE ttCDRColl.

        /* mobile calls */
        FOR EACH MobCDR NO-LOCK WHERE
                 MobCDR.InvCust = InvSeq.CustNum AND
                 MobCDR.InvSeq  = InvSeq.InvSeq  AND
                 MobCDR.DateSt LE idtCDRDate:
            
            fMobCDRAcc().
        END.
                 
        fUnbilledCDRColl(lcTaxZone).
        

        /* fixed calls */
        FOR EACH FixCDR NO-LOCK WHERE
                 FixCDR.InvSeq = InvSeq.InvSeq  AND
                 FixCDR.Date LE iDate:

            /* is VAT included OR excluded */
            xVatIncl = FixCDR.VatIncl. 
            fGetVatData("",
                        lcTaxZone,
                        FixCDR.BillCode).


            /* currency unit of call */
            fCurrUnit(FixCDR.GrossPrice - FixCDR.DiscValue,
                      FixCDR.GrossPrice,
                      FixCDR.CurrUnit,
                      "",
                      FixCDR.TariffID,
                      gcBrand,
                      OUTPUT xAmt,
                      OUTPUT ldGross).

            /* remove/add possible VAT */
            xAmt = fManipVat(xAmt).
            /* currency */
            xAmt = fToHomeCurr(xAmt,ldCustRate).

            /* error in the amount */
            IF xAmt = ? THEN DO:
                PUT STREAM slog UNFORMATTED
                    "Fixed"       CHR(9)
                    FixCDR.Date  CHR(9)
                    FixCDR.InvCust CHR(9)
                    FixCDR.BillCode CHR(9)
                    xAmt          SKIP.
                NEXT.
            END.

            MakeTempData("U",
                         FixCDR.Date,
                         Customer.InvGroup,
                         Customer.Category,
                         FixCDR.BillCode,
                         -1,
                         "",
                         "",
                         liVatUsage,
                         -1,
                         xSign * xAmt
                         ).

        END.

      END. /* InvSeq */

      IF ilFees THEN DO: 

         /* contract fees */
         FOR EACH FixedFee NO-LOCK WHERE
                  FixedFee.Brand   = gcBrand          AND
                  FixedFee.CustNum = Customer.CustNum AND
                  FixedFee.InUse   = TRUE:

            xVatIncl = FixedFee.VatIncl.
          
            /* items of contract fee */
            FOR EACH FFItem OF FixedFee NO-LOCK WHERE
                     FFItem.Concerns[1] <= xCtPeriod AND
                     FFItem.Amt         NE 0:
          
              ASSIGN liPer1 = IF FFItem.Concerns[1] = 0 OR
                                 FFItem.Concerns[1] = ?
                              THEN FFItem.BillPeriod
                              ELSE FFItem.Concerns[1]
                     liPer2 = IF FFItem.Concerns[2] = 0 OR
                                 FFItem.Concerns[2] = ?
                              THEN liPer1
                              ELSE FFItem.Concerns[2].

              IF liPer1 < 999999 AND
                 liPer1 > liShortPer
              THEN NEXT.
         
              /* error in item's period */
              IF liPer1 = 0 OR liPer1 = ? THEN DO:
                 PUT STREAM slog UNFORMATTED
                    "Contr"             CHR(9)
                    liPer1              CHR(9)
                    FFItem.CustNum      CHR(9)
                    FFItem.BillCode     CHR(9)
                    ROUND(FFItem.Amt,2) SKIP.
                 NEXT. 
              END.

              /* still Unbilled AT the END of this period */
              IF FFItem.Billed THEN DO:
                 IF NOT CAN-FIND(FIRST ttInvoice WHERE 
                                       ttInvoice.InvNum = FFItem.InvNum)
                 THEN NEXT. 
              END.

              /* periodizing; get the sum that has been Unbilled in the END of
                 accounting period */
              IF (liPer2  > 999999 AND
                  liPer2 <= xCtPeriod) OR
                 (liPer2  < 999999 AND
                  liPer2 <= liShortPer) 
              THEN xAmt = FFItem.Amt.
              ELSE xAmt = fPeriodize(01/01/1980,
                                     iDate,
                                     liPer1,
                                     liPer2,
                                     FFItem.Amt,
                                     OUTPUT xRemAmt).
              IF xAmt = 0 THEN NEXT. 

              fGetVatData("",
                          lcTaxZone,
                          FFItem.BillCode). 

              /* remove/add possible VAT */
              xAmt = fManipVat(xAmt).
              /* currency */
              xAmt = fToHomeCurr(xAmt,ldCustRate).

              /* error in the amount */
              IF xAmt = ? THEN DO:
                  PUT STREAM slog UNFORMATTED
                      "Contr"            CHR(9)
                      liPer1             CHR(9)
                      FFItem.CustNum     CHR(9)
                      FFItem.BillCode    CHR(9)                    
                      ROUND(xAmt,2)      SKIP.
                  NEXT.
              END.

              xDate = fInt2Date(liPer1,1).

              MakeTempData("U",       
                           xDate,
                           Customer.InvGroup,
                           Customer.Category,
                           FixedFee.BillCode,
                           -1,
                           "",
                           "",
                           liVatUsage,
                           -1,
                           xSign * xAmt).
          
            END.  /* FFItem */

         END. /* FixedFee */

         /* single fees */
         FOR EACH SingleFee NO-LOCK WHERE
                  SingleFee.Brand   = gcBrand          AND 
                  SingleFee.CustNum = Customer.CustNum AND
                  SingleFee.Active  = TRUE             AND 
                  SingleFee.Concerns[1] LE xCtperiod   AND
                  SingleFee.Amt NE 0:

            ASSIGN liPer1   = IF SingleFee.Concerns[1] = 0 OR
                                 SingleFee.Concerns[1] = ?
                              THEN SingleFee.BillPeriod
                              ELSE SingleFee.Concerns[1]
                   liPer2   = IF SingleFee.Concerns[2] = 0 OR
                                 SingleFee.Concerns[2] = ?
                              THEN liPer1
                              ELSE SingleFee.Concerns[2]
                   xVatIncl = SingleFee.VatIncl.         


            IF liPer1 < 999999 AND
               liPer1 > liShortPer
            THEN NEXT.
          
            /* error in the period */
            IF liPer1 = 0 OR liPer1 = ? THEN DO:
               PUT STREAM slog UNFORMATTED
                  "S.Fee"               CHR(9)
                  liPer1                CHR(9)
                  SingleFee.CustNum     CHR(9)
                  SingleFee.BillCode    CHR(9)
                  SingleFee.Amt         SKIP.
               NEXT. 
            END.

            /* still Unbilled AT the END of this period */
            IF SingleFee.Billed THEN DO:
               IF NOT CAN-FIND(FIRST ttInvoice WHERE 
                                     ttInvoice.InvNum = SingleFee.InvNum)
               THEN NEXT. 
            END.

            /* periodizing; get the sum that has been Unbilled in the END of
               accounting period */
            IF (liPer2  > 999999 AND
                liPer2 <= xCtPeriod) OR
               (liPer2  < 999999 AND
                liPer2 <= liShortPer) 
            THEN xAmt = SingleFee.Amt.
            ELSE xAmt = fPeriodize(01/01/1980,
                                   iDate,
                                   liPer1,
                                   liPer2,
                                   SingleFee.Amt,
                                   OUTPUT xRemAmt).
            IF xAmt = 0 THEN NEXT. 

            fGetVatData("",
                        lcTaxZone,
                        SingleFee.BillCode). 

            /* remove/add VAT */
            xAmt = fManipVat(xAmt).
            /* currency */
            xAmt = fToHomeCurr(xAmt,ldCustRate).

            /* error in the amount */
            IF xAmt = ? THEN DO:
               PUT STREAM slog UNFORMATTED
                  "S.Fee"               CHR(9)
                  SingleFee.Concerns[1] CHR(9)
                  SingleFee.CustNum     CHR(9)
                  SingleFee.BillCode    CHR(9)                    
                  xAmt                  SKIP.
               NEXT.
            END.

            xDate = fInt2Date(liPer1,1). 

            MakeTempData("U",
                         xDate,
                         Customer.InvGroup,
                         Customer.Category,
                         SingleFee.BillCode,
                         -1,
                         "",
                         "",
                         liVatUsage,
                         -1,
                         xSign * xAmt).

        END.  /* SingleFee */

       END. /* ilFees */
       
    END.   /* Customer */

    EMPTY TEMP-TABLE ttDebt.

    /* get accounts AND cost centres FOR collected data */
    FOR EACH wAccData WHERE
       wAccData.Etype  = "U"     AND 
       wAccData.IgCode = iIgCode:
       
       IF wAccData.AccNum = -1 THEN DO:

          /* accounting data from BillCode */
          GetAccKeys(wAccData.Category,     
                     wAccData.VatUsage,
                     wAccData.BillCode,
                     ?).

          ASSIGN wAccData.AccNum     = xAccNum
                 wAccData.CostCentre = xCostCentre.
       END.
       
       IF wAccData.VatCode = -1 THEN DO:
          FIND BillItem NO-LOCK WHERE
               BillItem.Brand = gcBrand AND
               BillItem.BillCode = wAccData.BillCode NO-ERROR.
          IF AVAILABLE BillItem 
          THEN wAccData.VatCode = BillItem.VatCode.
          ELSE wAccData.VatCode = 0.

           IF wAccData.VatCode > 0 THEN DO:
             FIND VatCode WHERE
                  VatCode.VatCode = wAccData.VatCode NO-LOCK NO-ERROR.
             IF AVAILABLE VatCode THEN wAccData.VatPerc = VatCode.VatPerc.
           END.
        END.
       
    END.

    FOR EACH wAccData WHERE
       wAccData.Etype  = "U"     AND
       wAccData.IgCode = iIgCode:

        /* debt account is retrieved according TO customer category */
        xBalAcct = GetAccRcv(0,
                             wAccData.Category,
                             1).
        IF xBalAcct = 0 THEN xBalAcct = 9999.                     

        FIND FIRST ttDebt WHERE
           ttDebt.Category = wAccData.Category AND
           ttDebt.Month    = wAccData.Month    AND
           ttDebt.BillCode = wAccData.BillCode AND
           ttDebt.AccNum   = xBalAcct NO-ERROR.

        IF NOT AVAILABLE ttDebt THEN DO:
           CREATE ttDebt.
           ASSIGN ttDebt.Category = wAccData.Category
                  ttDebt.Month    = wAccData.Month
                  ttDebt.BillCode = wAccData.BillCode 
                  ttDebt.AccNum   = xBalAcct.
        END.                                     

        /* debt posting */
        ASSIGN ttDebt.Amount = ttDebt.Amount + wAccData.Amount.

    END.

    /* debt posting */
    FOR EACH ttDebt:

        xDate = DATE(INTEGER(SUBSTRING(STRING(ttDebt.Month),5)),
                     01,
                     INTEGER(SUBSTRING(STRING(ttDebt.Month),1,4))).

        MakeTempData("U",
                     xDate,
                     iIgcode,
                     ttDebt.Category,
                     ttDebt.BillCode,
                     ttDebt.AccNum,
                     "",
                     "",
                     0,
                     0,
                     ttDebt.Amount * -1).
        DELETE ttDebt.                      
    END.                

    HIDE FRAME fQty NO-PAUSE. 

END PROCEDURE.        


/* Billed data */
PROCEDURE FindInvoices:

   DEF INPUT PARAMETER iBilled     AS LOGIC NO-UNDO. /* Billed section     */
   DEF INPUT PARAMETER iPeriodized AS LOGIC NO-UNDO. /* periodized section */
   DEF INPUT PARAMETER iDate1      AS Date  NO-UNDO. /* period's begin     */
   DEF INPUT PARAMETER iDate2      AS Date  NO-UNDO. /* period's END       */
   DEF INPUT PARAMETER iIgcode     AS CHAR  NO-UNDO. /* invoicing group    */
   DEF INPUT PARAMETER iCustNum1   AS INT   NO-UNDO. /* customer Limit     */
   DEF INPUT PARAMETER iCustNum2   AS INT   NO-UNDO. /* customer Limit     */

   DEF VAR xAcct       AS INT   EXTENT 16 NO-UNDO.
   DEF VAR xSum        AS DEC   EXTENT 16 NO-UNDO.
   DEF VAR xOverPayAcc AS INT   NO-UNDO.
   DEF VAR xInLimits   AS LOGIC NO-UNDO.
   DEF VAR xBackwards  AS DATE  NO-UNDO.
   DEF VAR liTotQty    AS INT   NO-UNDO.

   /* default account FOR overpayments */
   xOverPayAcc = fCParamI("OverPayAcc").
           
   ASSIGN xQty       = 0
          /* invoices are taken from about 6 months backwards in case 
             there are periodized fees 
          */
          xBackwards = IF iPeriodized 
                       THEN iDate1 - 180
                       ELSE iDate1.

   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand    AND
            Customer.InvGroup = iIgcode    AND
            Customer.CustNum  GE iCustNum1 AND
            Customer.CustNum  LE iCustNum2:
      liTotQty = liTotQty + 1.
   END.
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand    AND
            Customer.InvGroup = iIgcode    AND
            Customer.CustNum  GE iCustNum1 AND
            Customer.CustNum  LE iCustNum2:
        
      ASSIGN xQty = xQty + 1.

      IF xQty < 100 OR xQty MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY "Invg:" Customer.InvGroup SKIP
                 "Cust:" Customer.CustNum  SKIP
                 "Qty :" xQty  "/" TRIM(STRING(liTotQty,">>,>>>,>>9")) 
         WITH NO-LABELS OVERLAY ROW 14 COL 50 TITLE " Collecting (B & P)"
              FRAME fQty1.
      END.

      FOR EACH Invoice NO-LOCK WHERE
               Invoice.Brand   = Customer.Brand   AND
               Invoice.CustNum = Customer.CustNum AND
               Invoice.InvDate  GE xBackwards     AND
               Invoice.InvDate  LE iDate2         AND
               /* printing not denied */
               LOOKUP(STRING(Invoice.InvType),lcTypeDen) = 0:

         /* error in the amount */
         IF Invoice.InvAmt = ? THEN DO:
            PUT STREAM slog UNFORMATTED
                "Invoice"          CHR(9)
                Invoice.InvDate    CHR(9)
                Invoice.CustNum    CHR(9)
                ""                 CHR(9)
                Invoice.InvAmt     SKIP.
            NEXT. 
         END.

               /* is VAT included OR excluded */
         ASSIGN xVatIncl  = Invoice.VatIncl

                /* invoice is within the time Limit */
                xInLimits = (Invoice.InvDate GE iDate1 AND
                             Invoice.InvDate LE iDate2 AND
                             iBilled)
                xSum      = 0.

         /* don't take invoice's accounting data 
            IF invoice doesn't belong TO the desired period 
            (OR Billed section has NOT been chosen)
         */
         IF xInLimits THEN DO:

            /* accounts from invoice header */
            ASSIGN
              xSum[1]  = Invoice.InvAmt   
              xAcct[1] = Invoice.ARAccNum /* acc. receivable */  
              xSum[2]  = 0 - Invoice.Rounding     
              xAcct[2] = Invoice.RoundAccNum /* rounding */
              xSum[3]  = 0 - Invoice.InterestAmt    
              xAcct[3] = Invoice.IntAccNum /* Interest */
              /* advance payment */   
              xSum[4]  = 0 - Invoice.AdvPaym       
              xAcct[4] = Invoice.APAccNum

              /* overpayment */
              xSum[5]  = 0 - Invoice.OverPaym
              xAcct[5] = Invoice.OPAccNum. 

            /* Default VALUE IF AccNum is missing */
            IF xAcct[5] = 0 THEN ASSIGN xAcct[6] = xOverPayAcc.

            /* VAT can be devided into several accounts */
            IF llSepVat THEN DO xCount = 1 TO 10: 
               ASSIGN xSum[xCount + 5]  = -1 * Invoice.VATAmount[xCount]
                      xAcct[xCount + 5] = Invoice.VATAccount[xCount].
            END. 

            /* total amount, 'receivable': IF a Currency rate is used */
            IF Invoice.ExchRate NE 0 AND Invoice.ExchRate NE 1
            THEN DO xCount = 1 TO 15:
               IF xSum[xCount] NE 0 THEN 
               xSum[xCount] = fToHomeCurr(xSum[xCount],Invoice.ExchRate).
            END.

            ASSIGN ldInvTot  = 0
                   liDiffAcc = 0.
            
            &IF "{&GetOnlySums}" NE "YES"
            &THEN
            DO xCount = 1 TO 15:

                IF xSum[xCount] = 0 THEN NEXT.

                ldInvTot = ldInvTot + xSum[xCount].
                
                MakeTempData("B",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             "",
                             xAcct[xCount],
                             "",
                             "",
                             0,
                             0,
                             xSum[xCount]).
            END.            
            &ENDIF

            /* invoice lines */
            fInvLineAcc().
            
            liDiffAcc = Invoice.RoundAccNum.
            
            /* if there is a difference caused by the use of 3 decimals in
               invoice row amounts then post it to rounding account */
            IF ldInvTot >= -0.005 AND ldInvTot <= 0.005 AND   
               ldInvTot NE 0 AND liDiffAcc > 0 
            THEN DO:
                MakeTempData("B",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             "",
                             liDiffAcc,
                             "",
                             "",
                             0,
                             0,
                             -1 * ldInvTot).
            END. 

         END.  /*  within period Limit */

         /* if "periodized" section has not been chosen or 
            invoice has been credited (or is a credit invoice) 
            then there is no need to continue from this on 
         */
         IF NOT iPeriodized OR
            Invoice.CrInvNum > 0
         THEN NEXT. 

         /* check IF fees are periodized, AND IF so
            THEN make a line containing the amount that remains 
            AS advance payment debt in the END of accounting period 
         */

         ASSIGN /* retrieve debt AccNum from customer category */
                xBalAcct = GetAccRcv(Invoice.CustNum,
                                     Customer.Category,
                                     2).

         /* invoice's contract fees */  
         FOR EACH FFItem NO-LOCK WHERE
                  FFItem.InvNum  = Invoice.InvNum,
            FIRST FixedFee OF FFItem NO-LOCK,      
            FIRST InvRow OF Invoice NO-LOCK WHERE
                  InvRow.BillCode = FFItem.BillCode AND
                  InvRow.RowType  = 3:

            ASSIGN liPer1   = IF FFItem.Concerns[1] = 0 OR
                                 FFItem.Concerns[1] = ?
                              THEN FFItem.BillPeriod
                              ELSE FFItem.Concerns[1]
                   liPer2   = IF FFItem.Concerns[2] = 0 OR
                                 FFItem.Concerns[2] = ?
                              THEN liPer1
                              ELSE FFItem.Concerns[2]
                   xVatIncl = FixedFee.VatIncl.         

            /* error in the period */
            IF liPer1 = 0 OR
               liPer1 = ?
            THEN DO:
                PUT STREAM slog UNFORMATTED
                    "Contr"             CHR(9)
                    FFItem.Concerns[1]  CHR(9)
                    FFItem.CustNum      CHR(9)
                    FFItem.BillCode     CHR(9)
              ROUND(FFItem.Amt,2)       SKIP.
                 NEXT. 
            END.

            ASSIGN xAmt = fPeriodize(iDate1,
                                     iDate2,
                                     liPer1,
                                     liPer2,
                                     FFItem.Amt,
                                     OUTPUT xRemAmt).

            fCalcVatFactor(InvRow.VatPerc,
                           "").

            /* amount that remains after this period */
            IF xRemAmt NE 0 THEN DO:

                /* remove/add possible VAT */
                ASSIGN xRemAmt = fManipVat(xRemAmt).

                /* error in the amount */
                IF xRemAmt = ? THEN DO: 
                    PUT STREAM slog UNFORMATTED
                       "Contr"            CHR(9)
                       FFItem.Concerns[1] CHR(9)
                       FFItem.CustNum     CHR(9)
                       FFItem.BillCode    CHR(9)
                       xRemAmt            SKIP.
                    NEXT. 
                END.

                MakeTempData("PD",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             FFItem.BillCode,
                             0,
                             "",
                             "",
                             Invoice.VatUsage,
                             InvRow.VatCode,
                             fToHomeCurr(xRemAmt,Invoice.ExchRate)
                             ).

                /* debt posting */
                &IF "{&GetOnlySums}" NE "YES"
                &THEN               

                MakeTempData("PD",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             FFItem.BillCode,
                             xBalAcct,
                             "",
                             "",
                             Invoice.VatUsage,
                             InvRow.VatCode,
                             xSign * xRemAmt
                            ).
                &ENDIF
             END.

         END.

         /* single fees */
         FOR EACH SingleFee NO-LOCK WHERE
                  SingleFee.InvNum  = Invoice.InvNum,
            FIRST InvRow OF Invoice NO-LOCK WHERE 
                  InvRow.BillCode = SingleFee.BillCode AND
                  InvRow.RowType  = 4: 

             ASSIGN liPer1   = IF SingleFee.Concerns[1] = 0 OR
                                  SingleFee.Concerns[1] = ?
                               THEN SingleFee.BillPeriod
                               ELSE SingleFee.Concerns[1]
                    liPer2   = IF SingleFee.Concerns[2] = 0 OR
                                  SingleFee.Concerns[2] = ?
                               THEN liPer1
                               ELSE SingleFee.Concerns[2]
                    xVatIncl = SingleFee.VatIncl.         

             /* error in the period */
             IF liPer1 = 0 OR
                liPer1 = ?
             THEN DO:
                PUT STREAM slog UNFORMATTED
                    "S.Fee"               CHR(9)
                    SingleFee.Concerns[1] CHR(9)
                    SingleFee.CustNum     CHR(9)
                    SingleFee.BillCode    CHR(9)
                    SingleFee.Amt         SKIP.
                 NEXT. 
             END.

             fCalcVatFactor(InvRow.VatPerc,
                            "").

             ASSIGN xAmt = fPeriodize(iDate1,
                                      iDate2,
                                      liPer1,
                                      liPer2,
                                      SingleFee.Amt,
                                      OUTPUT xRemAmt).

            /* amount that remains after this period */
            IF xRemAmt NE 0 THEN DO:

                /* remove/add possible VAT */
                ASSIGN xRemAmt = fManipVat(xRemAmt).

                /* error in the amount */
                IF xRemAmt = ? THEN DO:
                    PUT STREAM slog UNFORMATTED
                       "S.Fee"               CHR(9)
                       SingleFee.Concerns[1] CHR(9)
                       SingleFee.CustNum     CHR(9)
                       SingleFee.BillCode    CHR(9)
                       xRemAmt               SKIP.
                    NEXT.
                END.

                MakeTempData("PD",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             SingleFee.BillCode,
                             0,
                             "",
                             /* InvRow.CostCentre, */
                             "",
                             Invoice.VatUsage,
                             InvRow.VatCode,
                             fToHomeCurr(xRemAmt,Invoice.ExchRate)
                            ).

                /* debt posting */
                &IF "{&GetOnlySums}" NE "YES"
                &THEN                

                MakeTempData("PD",
                             ?,
                             Customer.InvGroup,
                             Customer.Category,
                             SingleFee.BillCode,
                             xBalAcct,
                             "",
                             /* InvRow.CostCentre,*/
                             "",
                             Invoice.VatUsage,
                             InvRow.VatCode,
                             xSign * xRemAmt
                            ).
                &ENDIF
            END.

         END.

      END.  /* invoices */

   END.  /* customers */  

   /* get accounts AND cost centres */
   FOR EACH wAccData WHERE
            wAccData.Etype  = "PD"    AND 
            wAccData.IgCode = iIgCode AND
            wAccData.AccNum = 0:

       /* accounting data from BillCode */
       GetAccKeys(wAccData.Category,
                  wAccData.VatUsage,
                  wAccData.BillCode,
                  ?).

       ASSIGN wAccData.AccNum     = xAccNum
              wAccData.CostCentre = xCostCentre.
   END.

   HIDE FRAME fQty1 NO-PAUSE.

END PROCEDURE.


