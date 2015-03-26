/* frefnum.i        09.02.04/aam

   form reference number
   
   callers: fprintinv.i         
            nnsvti.p
            showpr.p
            
   cparam2.i and refcode.i needed 

   changes:         15.03.06/aam refnum for payment plan
            
*/

DEF VAR liRefNumForm AS INT NO-UNDO. 
   
/* format for reference nbr */
liRefNumForm = fCParamI("RefNumForm"). 

FUNCTION fFormRefNum RETURNS CHARACTER
   (iiCustNum AS INT,
    iiInvNum  AS INT,
    iiInvType AS INT).
   
   DEF VAR lcFormRefNum AS CHAR NO-UNDO.
       
   /* deposit invoice */
   IF iiInvType = 3 
   THEN lcFormRefNum = "3" + STRING(iiInvNum,"99999999").

   /* adv.payment invoice */
   ELSE IF iiInvType = 4 
   THEN lcFormRefNum = "4" + STRING(iiInvNum,"99999999").
   
   /* payment plan */
   ELSE IF iiInvType < 0 
   THEN lcFormRefNum = "2" + STRING(iiInvNum,"9999999")   +
                             STRING(iiCustNum,"99999999").
   
   /* normal invoices */
   ELSE DO:
      CASE liRefNumForm:
      WHEN 1 THEN lcFormRefNum = STRING(iiCustNum).
      WHEN 2 THEN lcFormRefNum = STRING(iiCustNum) +
                                 STRING(iiInvNum,"99999999").
      OTHERWISE   lcFormRefNum = STRING(iiInvNum).
      END CASE.
   END.
   
   /* calculate last digit */
   lcFormRefNum = lcFormRefNum + fChkNbrFIN(lcFormRefNum).

   RETURN lcFormRefNum.
   
END FUNCTION.

