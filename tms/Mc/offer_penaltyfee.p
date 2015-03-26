/* ----------------------------------------------------------------------
  MODULE .......: offer_penaltyfee.p
  TASK .........: Get amount of penalty fee (periodical contract)
  CREATED ......: 04.03.09
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{offer.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiOrderID    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiTermMonths AS INT  NO-UNDO.
DEF OUTPUT PARAMETER odPenaltyFee AS DEC  NO-UNDO.

DEF VAR ldAmount       AS DEC  NO-UNDO.
DEF VAR ldaOfferDate   AS DATE NO-UNDO.
DEF VAR liTime         AS INT  NO-UNDO.
DEF VAR liRequest      AS INT  NO-UNDO.
DEF VAR lcResult       AS CHAR NO-UNDO.
DEF VAR lcUseOffer     AS CHAR NO-UNDO.

/******** Main start *********/

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND
           Order.OrderID = iiOrderID NO-LOCK NO-ERROR. 
IF NOT AVAILABLE Order THEN 
   RETURN "ERROR:Unknown order".

IF Order.Offer = "" THEN DO:
   
   /* backwards compatibility */
   FOR FIRST OrderAccessory OF Order NO-LOCK WHERE
             OrderAccessory.ProductCode > "" AND
             OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}):
      assign odPenaltyFee = 100
             oiTermMonths = 18.
   END.         
    
   RETURN "No offer available".
END.

fSplitTS(Order.CrStamp,
         OUTPUT ldaOfferDate,
         OUTPUT liTime).
   
/* determine correct offer using 'permanencia' code */
IF LOOKUP(Order.Offer,"1,2") > 0 THEN 
   lcUseOffer = fGetOffer(Order.Offer,
                          ldaOfferDate).
ELSE lcUseOffer = Order.Offer.
   
IF lcUseOffer = "" THEN RETURN "ERROR:Unknown offer ID".

FIND FIRST Offer WHERE 
           Offer.Brand = gcBrand AND
           Offer.Offer = lcUseOffer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Offer THEN RETURN "ERROR:Unknown offer".


FOR EACH OfferItem NO-LOCK WHERE
         OfferItem.Brand       = gcBrand       AND
         OfferItem.Offer       = Offer.Offer   AND
         OfferItem.ItemType    = "PerContract" AND
         OfferItem.EndStamp   >= Order.CrStamp AND
         OfferItem.BeginStamp <= Order.CrStamp,
   FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
         DayCampaign.Brand   = gcBrand AND
         DayCampaign.DCEvent = OfferItem.ItemKey AND
         DayCampaign.DCType  = {&DCTYPE_DISCOUNT} AND
         DayCampaign.TermFeeCalc > 0,
   FIRST FMItem NO-LOCK WHERE
         FMItem.Brand     = gcBrand AND
         FMItem.FeeModel  = DayCampaign.TermFeeModel AND
         FMItem.ToDate   >= ldaOfferDate AND
         FMItem.FromDate <= ldaOfferDate:
         
   assign odPenaltyFee = FMItem.Amount
          oiTermMonths = DayCampaign.DurMonths.
   LEAVE.
END.

RETURN "".

/******** Main end *********/

