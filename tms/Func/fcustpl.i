/* fcustpl.i      27.09.2002/aam 
   get customer's valid pricelist

                  28.02.2003/aam fCallPriceList added 
                  03.03.2003/aam fBEventPriceList added
                  09.09.2003/aam brand
                  27.11.2006 jp  dates for fFeeModelPriceList
*/

&IF "{&FCUSTPL_I}" NE "YES"
&THEN
&GLOBAL-DEFINE FCUSTPL_I YES

FUNCTION fCustPriceList RETURNS CHARACTER   
    (iiCustNum    AS INT,
     iiBillTarget AS INT,
     idtDate      AS DATE).

   DEF VAR lcCustPriceList AS CHAR NO-UNDO.

   lcCustPriceList = "". 

   IF NOT AVAILABLE BillTarget OR
      BillTarget.CustNum    NE iiCustNum OR
      BillTarget.BillTarget NE iiBillTarget
   THEN FIND BillTarget NO-LOCK WHERE
             BillTarget.CustNum    = iiCustNum AND
             BillTarget.BillTarget = iiBillTarget NO-ERROR.

   IF AVAILABLE BillTarget THEN
   FOR EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
            PListConf.Brand    = gcBrand           AND
            PListConf.RatePlan = BillTarg.RatePlan AND
            PListConf.dFrom   <= idtDate           AND
            PListConf.dTo     >= idtDate
   BY PListConf.Prior:

      lcCustPriceList = PListConf.PriceList.

      /* first one found is the also the first in priority in case
         there are several price lists with overlapping time period 
      */
      LEAVE.
   END.

   RETURN lcCustPriceList.

END FUNCTION.

FUNCTION fCallPriceList RETURNS CHARACTER   
    (iiCustNum    AS INT,
     iiBillTarget AS INT,
     icPrefix     AS CHAR,
     icRatePref   AS CHAR,
     iiDialType   AS INT,
     idtDate      AS DATE).

   DEF VAR lcCallPriceList AS CHAR NO-UNDO.
   DEF VAR lcRatePref      AS CHAR NO-UNDO. 

   ASSIGN lcCallPriceList = ""
          lcRatePref      = icRatePref.

   IF NOT AVAILABLE BillTarget OR
      BillTarget.CustNum    NE iiCustNum OR
      BillTarget.BillTarget NE iiBillTarget
   THEN FIND BillTarget NO-LOCK WHERE
             BillTarget.CustNum    = iiCustNum AND
             BillTarget.BillTarget = iiBillTarget NO-ERROR.

   IF lcRatePref = "" THEN DO:
      FIND FIRST RatePref NO-LOCK WHERE                 
                 RatePref.Brand    = gcBrand  AND
                 RatePref.Prefix   = icPrefix AND
                 RatePref.DialType = iiDialType NO-ERROR.
      IF AVAILABLE RatePref THEN lcRatePref = RatePref.RatePref.           
   END. 

   IF AVAILABLE BillTarget THEN
   FOR EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
            PListConf.Brand    = gcBrand           AND
            PListConf.RatePlan = BillTarg.RatePlan AND
            PListConf.dFrom   <= idtDate           AND
            PListConf.dTo     >= idtDate,
      FIRST PriceList OF PListConf NO-LOCK WHERE
            (IF lcRatePref NE "" 
             THEN PriceList.Prefix = lcRatePref
             ELSE TRUE)
   BY PListConf.Prior:

      lcCallPriceList = PListConf.PriceList.

      /* first one found is the also the first in priority in case
         there are several price lists with overlapping time period 
      */
      LEAVE.
   END.

   RETURN lcCallPriceList.

END FUNCTION.

FUNCTION fFeeModelPriceList RETURNS CHARACTER   
    (iiCustNum    AS INT,
     iiBillTarget AS INT,
     icFeeModel   AS CHAR,
     idtDate      AS DATE).

   DEF VAR lcFMPriceList AS CHAR NO-UNDO.

   DEF BUFFER bFMItem FOR FMItem.

   ASSIGN lcFMPriceList = "".

   IF NOT AVAILABLE BillTarget OR
      BillTarget.CustNum    NE iiCustNum OR
      BillTarget.BillTarget NE iiBillTarget
   THEN FIND BillTarget NO-LOCK WHERE
             BillTarget.CustNum    = iiCustNum AND
             BillTarget.BillTarget = iiBillTarget NO-ERROR.

   IF AVAILABLE BillTarget THEN
   FOR EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
            PListConf.Brand    = gcBrand           AND
            PListConf.RatePlan = BillTarg.RatePlan AND
            PListConf.dFrom   <= idtDate           AND
            PListConf.dTo     >= idtDate,
      FIRST PriceList OF PListConf NO-LOCK,
      FIRST bFMItem NO-LOCK WHERE
            bFMItem.Brand     = gcBrand    AND
            bFMItem.FeeModel  = icFeeModel AND
            bFMItem.PriceList = PriceList.PriceList AND 
            bFMitem.FromDate  <= idtDate             AND
            bFMitem.ToDate    >= idtDate
                         
   BY PListConf.Prior:

      lcFMPriceList = PListConf.PriceList.

      /* first one found is also the first in priority in case
         there are several price lists with overlapping time period 
      */
      LEAVE.
   END.

   RETURN lcFMPriceList.

END FUNCTION.

FUNCTION fCliTypeFeeModelPriceList RETURNS CHARACTER   
    (icCliType    AS CHAR,
     icFeeModel   AS CHAR,
     idtDate      AS DATE).

   DEF VAR lcFMPriceList AS CHAR NO-UNDO.

   DEF BUFFER bFMItem FOR FMItem.
   DEF BUFFER bCLIType FOR CLIType.

   FIND bCLIType WHERE
        bCLIType.Brand = gcBrand AND 
        bCLIType.CLIType = icCliType NO-LOCK NO-ERROR.
   IF NOT AVAIL bCLIType THEN RETURN "".

   ASSIGN lcFMPriceList = "".

   FOR EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
            PListConf.Brand    = gcBrand           AND
            PListConf.RatePlan = bCLIType.PricePlan AND
            PListConf.dFrom   <= idtDate           AND
            PListConf.dTo     >= idtDate,
      FIRST PriceList OF PListConf NO-LOCK,
      FIRST bFMItem NO-LOCK WHERE
            bFMItem.Brand     = gcBrand    AND
            bFMItem.FeeModel  = icFeeModel AND
            bFMItem.PriceList = PriceList.PriceList AND 
            bFMitem.FromDate  <= idtDate             AND
            bFMitem.ToDate    >= idtDate
                         
   BY PListConf.Prior:

      lcFMPriceList = PListConf.PriceList.

      /* first one found is also the first in priority in case
         there are several price lists with overlapping time period 
      */
      LEAVE.
   END.

   RETURN lcFMPriceList.

END FUNCTION.

&ENDIF
