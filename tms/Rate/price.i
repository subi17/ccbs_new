/* --------------------------------------------------------------------------
  MODULE .......: PRICE.I
  FUNCTION .....: ISValue Price calculations
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 01.03.01 kl
  MODIFIED .....: 26.06.01 kl unit handling fixed
                  13.07.01 kl base / 100 & xUnit correction
                  16.07.01 jp/kl base fixed ( x * 100)
                  16.04.02 kl use h4 & lTBE FOR adding units TO h3
                              UPDATE h4 before WE & NH check
                  30.06.03 kl Tariff.DataType & Tariff.RateType
                  07.11.03 jp/aam base to subunit, bprice always in subunit
                  11.11.03 jp all startcharge must be in subunit
                  18.11.03 aam recalculate h2 if minsec used
                  04.03.03 jp currunit manipulate disabled
--------------------------------------------------------------------------- */

/* used THROUGH analyse tree */
DEF VAR xUnit  AS I  NO-UNDO EXTENT 6.
DEF VAR xAmt   AS DE NO-UNDO EXTENT 6.

/* hour calculations */
DEF VAR h1     AS I NO-UNDO.
DEF VAR h2     AS I NO-UNDO.
DEF VAR h3     AS I NO-UNDO.
DEF VAR h4     AS I NO-UNDO.

/* weekday limitations FOR Price EXTENTs */
DEF VAR d1     AS I NO-UNDO.
DEF VAR d2     AS I NO-UNDO.

/* miscellaneous */
DEF VAR iLoop  AS I  NO-UNDO.
DEF VAR dPrice AS DE NO-UNDO.
DEF VAR dTot   AS DE NO-UNDO.
DEF VAR iUnit  AS DE NO-UNDO.
DEF VAR iTime  AS I  NO-UNDO.
DEF VAR xTime  AS I  NO-UNDO.
DEF VAR wd     AS I  NO-UNDO.
DEF VAR lDay   AS I  NO-UNDO.
DEF VAR lPrAmt AS I  NO-UNDO.
DEF VAR lTLim  AS I  NO-UNDO.

/* Price band etc. parameters */
DEF VAR wd1   AS I  NO-UNDO. /* FIRST weekday  column    */
DEF VAR wd2   AS I  NO-UNDO. /* LAST  weekday  column    */
DEF VAR we1   AS I  NO-UNDO. /* FIRST weekend  column    */
DEF VAR we2   AS I  NO-UNDO. /* LAST  weekend  column    */ 
DEF VAR nh1   AS I  NO-UNDO. /* FIRST nat.hol. column    */
DEF VAR nh2   AS I  NO-UNDO. /* LAST  nat.hol. column    */
DEF VAR tbLmt AS I  NO-UNDO. /* time band limitation     */
DEF VAR wdays AS C  NO-UNDO. /* list of weekdays (1,2..) */ 
DEF VAR wel1  AS C  NO-UNDO. /* limits FOR weekend       */
DEF VAR wel2  AS C  NO-UNDO. /* limits FOR weekend       */
DEF VAR wed1  AS I  NO-UNDO. /* limits FOR weekend, WD   */
DEF VAR wed2  AS I  NO-UNDO. /* limits FOR weekend, WD   */
DEF VAR weh1  AS I  NO-UNDO. /* limits FOR weekend, hour */
DEF VAR weh2  AS I  NO-UNDO. /* limits FOR weekend, hour */
DEF VAR bFst  AS LO NO-UNDO. /* marked when FIRST Price  */
DEF VAR lTBE  AS I  NO-UNDO. /* Time Band Ends           */

FUNCTION fC2Sec RETURNS INTEGER
  (INPUT pSec AS CHAR):

   DEF VAR hh  AS I NO-UNDO.
   DEF VAR mm  AS I NO-UNDO.

   ASSIGN
      hh  = INT(substr(pSec,1,2))
      mm  = INT(substr(pSec,3)).

   RETURN hh * 3600 + 60 * mm.

END.

FUNCTION fUnits RETURNS INTEGER
  (INPUT pSec AS INT, INPUT pDiv AS INT):

   DEF VAR ret AS I NO-UNDO.

   ret = TRUNC(pSec / pDiv,0) + /* full units */  
         /* started units left ? */
         INT(TRUNC(pSec / pDiv,0) NE pSec / pDiv). 

   RETURN ret.

END.  

/* from minute into second Price */
FUNCTION fM2S RETURNS DECIMAL
  (INPUT pTar AS DECIMAL, INPUT lDataType AS INTEGER):
   CASE lDataType:
      WHEN 0 THEN RETURN ptar.
      WHEN 1 THEN RETURN pTar / 60.
      WHEN 2 THEN RETURN pTar  .
      WHEN 4 THEN RETURN ptar / 1048576.
   END.

END.
/* set up parameters */
ASSIGN 
   /* list of weekdays */
   wdays = "2,3,4,5,6"
   /* weekend starts (wd:hour) */
   wel1  = fCParamC("WeekendBegin") /* "6:18"  */ 
   /* weekend ends   (wd:hour) */
   wel2  = fCParamC("WeekendEnd")   /* "2:06"  */
   /* weekend starting DAY: WeekDay */
   wed1  = INT(ENTRY(1,wel1,":"))
   /* weekend ending   DAY: WeekDay */
   wed2  = INT(ENTRY(1,wel2,":"))
   /* weekend starting hour in seconds */
   weh1  = INT(ENTRY(2,wel1,":")) * 3600
   /* weekend ending hour in seconds */
   weh2  = INT(ENTRY(2,wel2,":")) * 3600.

FUNCTION fPrice RETURNS DECIMAL:

   ASSIGN
      /* default values FOR weekday limitations */
      wd1    = 8
      wd2    = 0
      we1    = 8
      we2    = 0
      nh1    = 8
      nh2    = 0

      /* Price & Duration */
      xUnit  = 0
      xAmt   = 0
      dTot   = 0
      lPrAmt = 0
      lTLim  = 9999
      bFst   = FALSE
      iTime  = 0
      ltBE   = 0

      /* hours FOR star & END time in seconds */
      h1     = c_time
      h2     = c_time + c_dur

      /* starting minutes of call Duration */ 
      xTime  = fUnits(c_dur,Tariff.RateType). 

   /* Check what prices are found: weekday, weekend, national Holiday ... */
   DO iLoop = 1 TO 6:

      /* how many time zones are defined */
      IF Tariff.DayType[iLoop] > 0 THEN ASSIGN
         /* calculate amount */
         lPrAmt = lPrAmt + 1.
      /* zone limitation minutes, any bigger than zero ? */
      IF SUBSTR(Tariff.TZFrom[iLoop],3) NE "00" THEN 
         lTLim  = MIN(lTLim,INT(SUBSTR(Tariff.TZFrom[iLoop],3))).

      /* check EXTENT types */
      CASE Tariff.DayType[iLoop]:
         WHEN 1 THEN ASSIGN
            wd1 = MIN(wd1,iLoop)
            wd2 = MAX(wd2,iLoop).
         WHEN 2 THEN ASSIGN
            we1 = MIN(we1,iLoop)
            we2 = MAX(we2,iLoop).
         WHEN 3 THEN ASSIGN
            nh1 = MIN(nh1,iLoop)
            nh2 = MAX(nh2,iLoop).
      END.

   END.

   /* Minutes.sec. used */
   IF NOT Tariff.Discount[4] THEN DO:
      /* check IF Minutes.sec. is needed */
      IF xTime < Tariff.MinSec THEN ASSIGN
         xTime = Tariff.MinSec
         h2    = c_time + Tariff.MinSec * Tariff.RateType
         ltBE  = -1.  /* if more than 1 zones -> use only first */
         
      /* StartCharge check NOT needed */
      bFst = TRUE.
   END.

   /* how many zones ? */
   CASE lPRAmt:
      /* ALL prices are zero: nothing TO calculate */
      WHEN 0 THEN RETURN dTot.
      /* one zone defined: use that, no further analyse needed */
      WHEN 1 THEN DO:
         ASSIGN
            dTot     = xTime * 
                       (fM2S(Tariff.Price[1],
                             Tariff.DataType) * Tariff.RateType)
            xAmt[1]  = fM2S(Tariff.Price[1],
                            Tariff.DataType) * Tariff.RateType
            xUnit[1] = xTime.
         /* add possible starting fee */
         IF Tariff.Discount[4] THEN ASSIGN
             /* always sub units per  */
            base = Tariff.StartCharge[1]  
               /* * (IF llCurrUnit THEN 100 ELSE 1) */ 

            dTot = (xTime * (fM2S(Tariff.Price[1],
                                  Tariff.DataType) * Tariff.RateType)) + base.


         RETURN dTot.
      END.
   END.

   /* limitation BY hours / minutes */
   CASE lTLim:
      /* only full hours */
      WHEN 9999 THEN tbLmt = 3600.
      /* how many pieces of smallest minute limitation is in full hour */
      OTHERWISE      tbLmt = 60 * lTLim.
   END.

   /* No weekend prices, use weekday limitation */
   IF we1 = 8 THEN ASSIGN 
      we1 = wd1
      we2 = wd2.

   /* No national Holiday prices, use weekday limitation */
   IF nh1 = 8 THEN ASSIGN 
      nh1 = wd1
      nh2 = wd2.

   /* loop from FIRST TO LAST hour */
   DO h3 = h1 TO h2:

      ASSIGN
         /* NEW DAY counter */
         lDay   = TRUNC(h3 / 86400,0)
         /* weekday of CURRENT Date */
         wd     = WEEKDAY(c_day + lDay)
         /* VARIABLE FOR time band units */
         iUnit  = 0
         /* VARIABLE FOR time band Price */
         dPrice = 0
         /* allways 0 - 23 in seconds */ 
         h4     = (h3 - lDay * 86400).

      /* national Holiday */
      IF CAN-FIND(FIRST NatHoliday WHERE
                        NatHoliday.Holiday = c_day + lDay) THEN ASSIGN 
         d1 = nh1
         d2 = nh2.
      /* weekdays MON-FRI: exception FOR weekend start & END */
      ELSE IF LOOKUP(STRING(wd),wdays) > 0 THEN DO:
         IF (wd = wed1 AND h4 >= weh1  OR           /* FIRST DAY of weekend */
             wd = wed2 AND h4 <  weh2) THEN ASSIGN  /* LAST  DAY of weekend */
             d1 = we1
             d2 = we2.
         /* 'normal' weekday */
         ELSE ASSIGN
            d1 = wd1
            d2 = wd2.
      END.
      /* weekend  SAT-SUN */   
      ELSE ASSIGN
         d1 = we1
         d2 = we2.

      /* - loop FOR WEEKDAYS OR WEEKEND
         - iLoop gets the EXTENT number of the time band  */
      DO iLoop = d1 TO d2:
         IF fC2Sec(Tariff.TZFrom[iLoop]) <= h4 AND 
            fC2Sec(Tariff.TZTo[iLoop])   >= h4 THEN DO:
            ASSIGN
               /* Price used FOR rating */
               dPrice = fM2S(Tariff.Price[iLoop],
                             Tariff.DataType).

            IF ltBE NE -1 THEN 
               /* END of this time band */
               lTBE   = fC2Sec(Tariff.TZTo[iLoop]).
               
            /* FIRST TIME: mark possible starting fee */
            IF NOT bFst THEN ASSIGN
               bFst = TRUE
               base = Tariff.StartCharge[iLoop]  /* * 
                      (IF llCurrUnit THEN 100 ELSE 1) */ .
            LEAVE.
         END.
      END.

      /* ALL seconds inside CURRENT TimeBand or minsec used */
      IF h4 + ((xTime - iTime) * Tariff.RateType) <= lTBE OR
         ltBE = -1
      THEN ASSIGN
         iUnit = iUnit + (xTime - iTime)
         iTime = iTime + (xTime - iTime)
         /* main loop ends */
         h3    = h2.
      /* seconds also outside CURRENT TimeBand */
      ELSE ASSIGN
         iUnit = iUnit + fUnits((lTBE - h4),Tariff.RateType)
         iTime = iTime + fUnits((lTBE - h4),Tariff.RateType)
         h3    = h3    + fUnits((lTBE - h4),Tariff.RateType).

      ASSIGN
         /* Price * units */
         dTot = dTot + iUnit * (dPrice * Tariff.RateType)
         /* time band units */
         xUnit[iLoop] = xUnit[iLoop] + iUnit
         /* time band costs
         xAmt[iLoop]  = xAmt[iLoop]  + iUnit * dPrice.
         */
         /* time band Price */
         xAmt[iLoop]  = dPrice.

      /* ALL units are counted: QUIT */
      IF iTime = xTime THEN h3 = h2.

   END. /* DO h3 = ... */

   /* RETURN total calculated VALUE + possible starting fee */
   
   RETURN dTot + base.

END. /* FUNCTION */

