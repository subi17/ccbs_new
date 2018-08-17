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
   DEFINE BUFFER bCCRule FOR CCRule.
    
   IF NOT AVAILABLE BillItem THEN RETURN 0.
       
   /* customer's vatusage-code determines final vat handling 
      and sales account, except when own usage or vip usage 
   */
   
   FIND FIRST bCCRule NO-LOCK WHERE 
              bCCRule.Brand      = BillItem.Brand    AND 
             (bCCRule.Category   = "*"      OR
              bCCRule.Category   = icCategory )      AND 
              bCCRule.BillCode   = BillItem.BillCode AND  
              bCCRule.CLIType    = ""                AND             
              bCCRule.ValidTo    >= TODAY USE-INDEX Category NO-ERROR. 
         
   IF AVAILABLE bCCRule THEN 
   DO:
          
      CASE iiVATUsage:
      WHEN 0 OR 
      WHEN 1 THEN liSlsAccount = bCCRule.AccNum.
      WHEN 2 THEN liSlsAccount = bCCRule.EUConAccNum.
      WHEN 3 THEN liSlsAccount = bCCRule.EUAccNum.
      WHEN 4 THEN liSlsAccount = bCCRule.FSAccNum.
      END CASE.
      
   END. 

   RETURN liSlsAccount.
   
END FUNCTION.

