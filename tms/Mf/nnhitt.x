/* nnhitt.p 

   changed:   17.09.02/aam all tariffs have a plist-code
              20.10.02/aam use tariff.frm 
              03.03.03/tk  tokens
              24.03.03/aam currency unit from pricelist
              26.03.03 kl recid as input param & disp tariff.custnum
*/


{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'tariff'}
{fcustpl.i}

DEF INPUT PARAM pRecId AS RECID NO-UNDO.

FIND FIRST FixCDR WHERE
     RECID(FixCDR) = pRecId
NO-LOCK NO-ERROR.

DEF VAR cust-name    LIKE Customer.CustName.
def var Currency     as   c    format "x(7)"   NO-UNDO EXTENT 6.
DEF VAR fr-header    AS   CHAR                 NO-UNDO.
DEF VAR lcCSeek      LIKE Tariff.CCN           NO-UNDO.
DEF VAR plseek       LIKE PriceList.PriceList  NO-UNDO.
DEF VAR liCustSeek   LIKE Tariff.CustNum       NO-UNDO. 
DEF VAR lcCName      LIKE CCN.CCNName          NO-UNDO.
DEF VAR plname       LIKE PriceList.PLName     NO-UNDO.
DEF VAR xCCN         LIKE Tariff.CCN           NO-UNDO.
DEF VAR xPriceList   LIKE PriceList.PriceList  NO-UNDO.
DEF VAR lcBDest      AS   CHAR                 NO-UNDO. 
DEF VAR xBDest       LIKE Tariff.BDest         NO-UNDO. 
DEF VAR xCustNum     LIKE Tariff.CustNum       NO-UNDO. 

fr-header = " PRICE DETAILS ". 

{tariff.frm}

FIND FIRST Tariff where
           Tariff.TariffNum = FixCDR.TariffID
no-lock no-error.

IF NOT AVAIL Tariff THEN DO:
   message "SORRY, tariff was not found in the call record !"
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.

PAUSE 0.

FIND FIRST Customer where 
           Customer.CustNum = FixCDR.InvCust
no-lock no-error.

IF Tariff.CustNum = 0 THEN
   FIND FIRST PriceList where 
              PriceList.PriceList = Tariff.PriceList
   NO-LOCK NO-ERROR.
ELSE 
   FIND FIRST PriceList where 
              PriceList.PriceList = fCustPriceList(FixCDR.CustNum,
                                                   FixCDR.BillTarget,
                                                   FixCDR.Date)
   NO-LOCK NO-ERROR.

IF AVAIL PriceList THEN DO:
   ASSIGN
      plname    = PriceList.PLName
      fr-header = fr-header + " (" + PriceList.Prefix + ")".

   FIND FIRST Currency where
              Currency.Currency = PriceList.Currency
   NO-LOCK NO-ERROR.
   IF AVAILABLE Currency THEN 
      Currency = (IF PriceList.CurrUnit THEN
                     Currency.Currency 
                  ELSE
                     Currency.SubName) + "/" +
                 (if Tariff.Discount[6]then "min"
                  else "sec").
   ELSE Currency = "". 
END.
else plname = "!! BLANK !!".

FIND FIRST CCN WHERE
           CCN.CCN = Tariff.CCN
NO-LOCK NO-ERROR.

IF AVAIL CCN THEN lcCName = CCN.CCNName.
ELSE              lcCName = "!! BLANK !!".

PAUSE 0.
VIEW frame lis.

DO WITH FRAME lis:

   FIND FIRST BillItem WHERE
              BillItem.BillCode = Tariff.BillCode 
   NO-LOCK NO-ERROR.

   IF AVAILABLE BillItem THEN
      DISPLAY BillItem.BIName.
   ELSE
      DISPLAY "Unknown" ;& BillItem.BIName. 

   DISP 
      lcCName          @ CCN.CCNName
      Tariff.CCN       @ xCCN 
      Tariff.PriceList @ xPriceList 
      plname           @ PriceList.PLName
      Currency
      Tariff.CustNum   @ xCustNum
      Tariff.ValidFrom
      Tariff.ValidTo 
      Tariff.BillCode 
      Tariff.Discount[6] 
      Tariff.RateType
      /*
      Tariff.ASubType 
      */
      Tariff.Discount[4]

      Tariff.TZName[1]
      Tariff.DayType[1] 
      Tariff.TZFrom[1]
      Tariff.TZTo[1]
      Tariff.Price[1]
      Tariff.StartCharge[1]

      Tariff.TZName[2]
      Tariff.DayType[2] 
      Tariff.TZFrom[2]
      Tariff.TZTo[2]
      Tariff.Price[2]
      Tariff.StartCharge[2]

      Tariff.TZName[3]
      Tariff.DayType[3] 
      Tariff.TZFrom[3]
      Tariff.TZTo[3]
      Tariff.Price[3]
      Tariff.StartCharge[3]

      Tariff.TZName[4]
      Tariff.DayType[4] 
      Tariff.TZFrom[4]
      Tariff.TZTo[4]
      Tariff.Price[4]
      Tariff.StartCharge[4]

      Tariff.TZName[5]
      Tariff.DayType[5] 
      Tariff.TZFrom[5]
      Tariff.TZTo[5]
      Tariff.Price[5]
      Tariff.StartCharge[5]

      Tariff.TZName[6]
      Tariff.DayType[6] 
      Tariff.TZFrom[6]
      Tariff.TZTo[6]
      Tariff.Price[6]
      Tariff.StartCharge[6]
      Tariff.MinSec.

END. 

MESSAGE "Hit ENTER".
PAUSE no-message.
HIDE FRAME lis.

