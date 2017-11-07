/* finvoiceacc.i    13.05.09/aam
   
   invoice account handling 
*/
{Syst/commali.i}
{Func/cparam2.i}

DEF VAR lcOwnUse   AS CHAR NO-UNDO.
DEF VAR lcVipUse   AS CHAR NO-UNDO.

ASSIGN 
   lcOwnUse = fCParamC("OwnUseCategory")
   lcVipUse = fCParamC("VipUseCategory").


FUNCTION fInvRowAccount RETURNS INTEGER
   (icCategory AS CHAR,
    iiVATUsage AS INT):
    
   DEF VAR liSlsAccount AS INT  NO-UNDO.
    
   IF NOT AVAILABLE BillItem THEN RETURN 0.
       
   /* customer's vatusage-code determines final vat handling 
      and sales account, except when own usage or vip usage 
   */
   CASE icCategory:
   WHEN lcOwnUse THEN liSlsAccount = BillItem.AltAccNum.
   WHEN lcVipUse THEN liSlsAccount = BillItem.VIPAccNum.
         
   OTHERWISE DO:
      CASE iiVATUsage:
      WHEN 0 OR 
      WHEN 1 THEN liSlsAccount = BillItem.AccNum.
      WHEN 2 THEN liSlsAccount = BillItem.EUConAccNum.
      WHEN 3 THEN liSlsAccount = BillItem.EUAccNum.
      WHEN 4 THEN liSlsAccount = BillItem.FSAccNum.
      END CASE.
   END.

   END CASE.

   RETURN liSlsAccount.
   
END FUNCTION.

