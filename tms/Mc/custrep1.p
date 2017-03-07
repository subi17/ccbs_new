/* ------------------------------------------------------
  MODULE .......: custrep1.p
  FUNCTION .....: Print out customer report with given criteria
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 13.03.03
  MODIFIED .....: 07.04.03 tk RateCust added, utuloste
                  12.09.03/aam brand
                  08.03.05/tk Category
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/fcustbal.i}
{Syst/utumaa.i "new"}

assign 
   tuni1 = "custrep"
   tuni2 = "".

DEF TEMP-TABLE tCustNums 
   FIELD CustNum LIKE Customer.CustNum
   INDEX CustNum CustNum.

DEF INPUT PARAMETER CustNum1  AS I   NO-UNDO.
DEF INPUT PARAMETER CustNum2  AS I   NO-UNDO.
DEF INPUT PARAMETER ZipCode   AS CH  NO-UNDO.
DEF INPUT PARAMETER Salesman  AS CH  NO-UNDO.
DEF INPUT PARAMETER RateCust  AS I   NO-UNDO.  
DEF INPUT PARAMETER lCG       AS LOG NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tCustNums.
DEF INPUT PARAMETER InvGroup  AS CH  NO-UNDO.
DEF INPUT PARAMETER Reseller  AS CH  NO-UNDO.
DEF INPUT PARAMETER RatePlan  AS CH  NO-UNDO.
DEF INPUT PARAMETER DiscPlan  AS CH  NO-UNDO.
DEF INPUT PARAMETER Category  AS CH  NO-UNDO.

DEF VAR pre AS CH NO-UNDO.

assign tila = TRUE.
{Syst/utuloste.i "return"}

PUT stream tul UNFORMATTED
   "CustNum"        tab
   "Abbrev"         tab
   "Customer name"  tab
   "Addit.name"     tab
   "Contact"        tab
   "Phone"          tab
   "Fax"            tab
   "Address"        tab
   "Zipcode"        tab
   "Post office"    tab
   "Country"        tab
   "Email"          tab
   "MobEmail"       tab
   "NetEmail"       tab
   "Invgroup"       tab
   "Accgrp"         tab
   "Class"          tab
   "Invcust"        tab
   "RateCust"       tab
   "Paymcust"       tab
   "Repcust"        tab
   "CCLang"         tab
   "OrgId"          tab
   "Category"       tab
   "Start charge"   tab
   "Size"           tab
   "Conntype"       tab
   "Language"       tab
   "Contr. beg"     tab
   "Contr. end"     tab
   "Salesman"       tab
   "Reseller"       tab
   "DirMark"        tab
   "Brand"          tab
   "RepCodes"       tab
   "ChargeType"     tab
   "DeliveryType"   tab
   "InvCode"        tab
   "Currency"       tab
   "VatID"          tab
   "VatIncl"        tab
   "PaymTerm"       tab
   "ClaimPerm"      tab
   "InterestPerm"   tab
   "Deposit"        tab
   "CreditLimit"    tab
   "OverPaym"       tab
   "Debt"           tab
   "AdvPaym"        tab
   "Interest"       tab
   "PaymMethod"     tab
   "PaymQty"        tab
   "LatestInv"      skip.


FOR EACH Customer WHERE
         Customer.Brand     = gcBrand   AND 
         Customer.CustNum  >= CustNum1  AND
         Customer.CustNum  <= CustNum2  AND
        (IF ZipCode   NE "" THEN
         Customer.ZipCode   = ZipCode 
         ELSE TRUE)                     AND   
        (IF Salesman  NE "" THEN 
         Customer.Salesman  = Salesman
         ELSE TRUE)                     AND
        (IF RateCust  NE 0  THEN
         Customer.RateCust = RateCust
         ELSE TRUE)                     AND
        (IF InvGroup  NE "" THEN 
         Customer.InvGroup  = InvGroup
         ELSE TRUE)                     AND
        (IF Reseller  NE "" THEN
         Customer.Reseller  = Reseller
         ELSE TRUE)                     AND
        (IF Category  NE "" THEN 
         Customer.Category = Category
         ELSE TRUE)
NO-LOCK:

   IF RatePlan NE "" AND
     NOT CAN-FIND(BillTarget WHERE
                  BillTarget.CustNum = Customer.CustNum AND
                  BillTarget.Rateplan = RatePlan) THEN NEXT.

   IF DiscPlan NE "" AND 
     NOT CAN-FIND(BillTarget WHERE
                  BillTarget.CustNum = Customer.CustNum AND
                  BillTarget.Discplan = DiscPlan) THEN NEXT.

   IF lCG AND
     NOT CAN-FIND(tCustNums WHERE
                  tCustNums.CustNum = Customer.CustNum) THEN NEXT.


   PUT stream tul UNFORMATTED
      Customer.CustNum      tab 
      Customer.SearchName   tab
      Customer.CustName     tab
      Customer.COName       tab
      Customer.Contact      tab
      Customer.Phone        tab
      Customer.Fax          tab
      Customer.Address      tab
      Customer.ZipCode      tab
      Customer.PostOffice   tab
      Customer.Country      tab
      Customer.email        tab
      Customer.MobeMail     tab
      Customer.NeteMail     tab
      Customer.InvGroup     tab
      Customer.AccGrp       tab
      Customer.CustClass    tab
      Customer.InvCust      tab
      Customer.RateCust     tab
      Customer.PaymCust     tab
      Customer.RepCust      tab
      Customer.CCLang       tab
      Customer.OrgId        tab
      Customer.Category     tab
      Customer.Startcharge  tab
      Customer.Size         tab
      Customer.ConnType     tab
      Customer.Language     tab
      Customer.Contrbeg     tab
      Customer.Contrend     tab
      Customer.Salesman     tab
      Customer.Reseller     tab
      Customer.DirMark      tab
      Customer.Brand        tab
      Customer.RepCodes     tab
      Customer.ChargeType   tab
      Customer.DelType      tab
      Customer.InvCode      tab
      Customer.Currency     tab
      Customer.VatID        tab
      Customer.VatIncl      tab
      Customer.PaymTerm     tab
      Customer.ClaimPerm    tab
      Customer.InterestPerm tab
      fGetCustBal(Customer.Custnum,"TOTAL","DP")    tab
      Customer.CreditLimit  tab
      fGetCustBal(Customer.Custnum,"TOTAL","OP")    tab
      fGetCustBal(Customer.Custnum,"TOTAL","ARBAL") tab
      fGetCustBal(Customer.Custnum,"TOTAL","AP")    tab
      fGetCustBal(Customer.Custnum,"TOTAL","INT")   tab
      fGetCustBal(Customer.Custnum,"TOTAL","PBEH")  tab
      fGetCustBal(Customer.Custnum,"TOTAL","PQTY")  tab
      fGetCustBal(Customer.Custnum,"TOTAL","INVP")  skip.

END.

assign tila = FALSE.
{Syst/utuloste.i}





