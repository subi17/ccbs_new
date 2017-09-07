/* nnhitt.p 

   changed:   17.09.02/aam all tariffs have a plist-code
              20.10.02/aam use tariff.frm 
              03.03.03/tk  tokens
              24.03.03/aam currency unit from pricelist
              26.03.03 kl recid as input param & disp tariff.custnum
              17.11.03/aam RateType instead of Discount[6], 
                           Brand,
                           display BDest, PriceList etc. correctly 
              15.1.07 jp   Input parameter tariffnum
              10.09.15/pha Removed Customer and added Rate ID


*/


{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'tariff'}
{Func/fcustpl.i}

DEF INPUT PARAM iiTariffNum  AS INT NO-UNDO.

def var Currency     as   c    format "x(7)"   NO-UNDO EXTENT 6.
DEF VAR fr-header    AS   CHAR                 NO-UNDO.
DEF VAR lcCSeek      LIKE Tariff.CCN           NO-UNDO.
DEF VAR plseek       LIKE PriceList.PriceList  NO-UNDO.
DEF VAR lcCName      LIKE CCN.CCNName          NO-UNDO.
DEF VAR plname       LIKE PriceList.PLName     NO-UNDO.
DEF VAR xCCN         LIKE Tariff.CCN           NO-UNDO.
DEF VAR xPriceList   LIKE PriceList.PriceList  NO-UNDO.
DEF VAR lcBDest      AS   CHAR                 NO-UNDO. 
DEF VAR xBDest       LIKE Tariff.BDest         NO-UNDO. 
DEF VAR lcDataType   AS CHAR               NO-UNDO.

fr-header = " PRICE DETAILS ". 


FIND FIRST Tariff where
           Tariff.Brand = gcBrand AND 
           Tariff.TariffNum = iiTariffNum
no-lock no-error.

IF NOT AVAIL Tariff THEN DO:
   message "SORRY, tariff was not found in the call record !"
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.

PAUSE 0.

FIND FIRST PriceList where 
           PriceList.Brand     = Tariff.Brand AND
           PriceList.PriceList = Tariff.PriceList
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
                 (if Tariff.RateType = 1 
                  then "min"
                  else "sec").
   ELSE Currency = "". 
END.
else plname = "!! BLANK !!".

FIND FIRST CCN WHERE
           CCN.Brand = Tariff.Brand AND
           CCN.CCN   = Tariff.CCN
NO-LOCK NO-ERROR.

IF AVAIL CCN THEN lcCName = CCN.CCNName.
ELSE              lcCName = "!! BLANK !!".


form /* ADD */
   "Rate ID.....:" Tariff.TariffNum FORMAT ">>>>>9"
      help "Tarif ID number" SKIP
   "CCN ........:" Tariff.CCN FORMAT ">>>9"
      help "Call case number"
      CCN.CCNName  format "x(30)" AT 32 SKIP
   "BDestination:" Tariff.BDest FORMAT "x(20)"
      help "B-Destination"
      BDest.BDName format "x(30)" AT 32 SKIP
   "Pricelist ..:" Tariff.PriceList FORMAT "X(16)" 
      help "Pricelist code"
      PriceList.PLName   format "x(22)" AT 32 SKIP
   "--------------------------------------------------------------------"
   SKIP
   "Valid during ..:" 
   Tariff.ValidFrom format "99-99-9999"
      validate(input Tariff.ValidFrom NE ?, 
               "Date is mandatory")
   "-" 
   Tariff.ValidTo   format "99-99-9999"
      validate(input Tariff.ValidTo >= input Tariff.ValidFrom AND
               input Tariff.ValidTo NE ?,
               "Check the dates; price would never become valid using these")
   "Minute/Second .:" AT 48 Tariff.RateType 
      VALIDATE(INPUT Tariff.RateType > 0,"Minimum is 1") 
   SKIP         
   "Price unit ....:" Tariff.DataType
      help "Rate data type" 
      lcDataType NO-LABEL FORMAT "X(20)"
   "Start / Min.Sec:" AT 48 Tariff.Discount[4] format "Fee/Sec"
      help "Starting fee or minimum charging seconds (Fee/Sec)"    
   SKIP
   "Billing Item ..:" Tariff.BillCode
      BillItem.BIName FORMAT "X(12)" NO-LABEL
   "Currency unit .:" AT 48 Tariff.CurrUnit
   SKIP(1)
   "Name     T  From - To      Price" AT 15
   "Starting fees" TO 67              
   SKIP

   " Time zone 1:"
   Tariff.TZName[1] FORMAT "X(8)"
   Tariff.DayType[1]
   Tariff.TZFrom[1] "-" Tariff.TZTo[1] Tariff.Price[1] currency[1]
   Tariff.StartCharge[1] AT 60 SPACE(1) SKIP

   " Time zone 2:"
   Tariff.TZName[2] FORMAT "X(8)"
   Tariff.DayType[2]
   Tariff.TZFrom[2] "-" Tariff.TZTo[2] Tariff.Price[2] currency[2]
   Tariff.StartCharge[2] AT 60 SKIP

   " Time zone 3:"
   Tariff.TZName[3] FORMAT "X(8)"
   Tariff.DayType[3]
   Tariff.TZFrom[3] "-" Tariff.TZTo[3] Tariff.Price[3] currency[3]

   Tariff.StartCharge[3] AT 60 SKIP

   " Time zone 4:"
   Tariff.TZName[4] FORMAT "X(8)"
   Tariff.DayType[4]
   Tariff.TZFrom[4] "-" Tariff.TZTo[4] Tariff.Price[4] currency[4]
   Tariff.StartCharge[4] AT 60 SKIP

   " Time zone 5:"
   Tariff.TZName[5] FORMAT "X(8)"
   Tariff.DayType[5]
   Tariff.TZFrom[5] "-" Tariff.TZTo[5] Tariff.Price[5] currency[5]
   Tariff.StartCharge[5] AT 60 SKIP

   " Time zone 6:"
   Tariff.TZName[6] FORMAT "X(8)"
   Tariff.DayType[6]
   Tariff.TZFrom[6] "-" Tariff.TZTo[6] Tariff.Price[6] currency[6]
   Tariff.StartCharge[6] AT 60 SKIP
   " First Billable Sec:" Tariff.FirstBillableSec  

   "OR Minimum sec:" AT 48 Tariff.MinSec
      help "Minimum charging seconds for calls"  
   SPACE(1)
   SKIP
WITH
   WITH ROW 1 centered COLOR value(cfc) TITLE COLOR value(ctc) 
   fr-header NO-LABEL OVERLAY
FRAME lis.



PAUSE 0.
VIEW frame lis.

DO WITH FRAME lis:

   FIND FIRST BillItem WHERE
              BillItem.Brand    = Tariff.Brand AND
              BillItem.BillCode = Tariff.BillCode 
   NO-LOCK NO-ERROR.

   IF AVAILABLE BillItem THEN
      DISPLAY BillItem.BIName.
   ELSE
      DISPLAY "Unknown" ;& BillItem.BIName. 

   IF Tariff.BDest > "" THEN DO:
      FIND FIRST BDest NO-LOCK WHERE 
           BDest.Brand = Tariff.Brand AND
           BDest.BDest = Tariff.BDest AND
           BDest.ToDate >= Tariff.ValidFrom AND
           BDest.FromDate <= Tariff.ValidTo NO-ERROR.
      IF AVAILABLE BDest THEN DISPLAY BDest.BDName.
   END.
   ELSE DISPLAY "" @ BDest.BDName.
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Tariff"   AND
              TMSCodes.FieldName = "DataType" AND
              TMSCodes.CodeGroup = "Tariff"   AND
              TMSCodes.CodeValue = STRING(Tariff.DataType)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN
      lcDataType = TMSCodes.CodeName.
   ELSE lcDataType = "".
   
   DISP 
      lcCName          @ CCN.CCNName
      Tariff.TariffNum
      Tariff.CCN  
      Tariff.BDest 
      Tariff.PriceList  
      plname           @ PriceList.PLName
      Currency
      Tariff.ValidFrom
      Tariff.ValidTo 
      Tariff.BillCode 
      Tariff.RateType
      Tariff.DataType
      Tariff.CurrUnit
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
      Tariff.MinSec
      Tariff.FirstBillableSec
      lcDataType.

END. 

MESSAGE "Hit ENTER".
PAUSE no-message.
HIDE FRAME lis.

