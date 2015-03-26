/* fcustref.i           28.05.03/aam
   special reference number for customer

   refcode.i is needed in calling program
   callers:   climepl.p 
              nnasse.p
*/

FUNCTION fCustRefNum RETURNS CHARACTER
   (iiCustNum AS INT).

   DEF VAR lcCustRefNum AS CHAR NO-UNDO.

   ASSIGN lcCustRefNum = "1" + STRING(iiCustNum,"99999999")
          /* control number to the end */
          lcCustRefNum = lcCustRefNum + fChkNbrFIN(lcCustRefNum)
          /* divide into groups */
          lcCustRefNum = fViite(lcCustRefNum).

   RETURN lcCustRefNum.

END FUNCTION.

