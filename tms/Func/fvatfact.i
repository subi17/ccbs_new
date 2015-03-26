/* fvatfact.i       11.12.03/aam 
   calculate vat factor according to customer's vatusage definition and
   billing item's vatcode
*/

{commali.i}
{ftaxdata.i}

FUNCTION fVatFactor RETURNS DECIMAL
   (iiVatUsage AS INT,
    icTaxZone  AS CHAR,
    icBillCode AS CHAR,
    idaDate    AS DATE).

   DEF VAR ldFactor   AS DEC NO-UNDO.
   DEF VAR lcTaxClass AS CHAR NO-UNDO.
   
   DEF BUFFER bVatBillItem FOR BillItem.
   
   ldFactor = 1.

   IF iiVatUsage < 3 THEN DO:

      lcTaxClass = "1".
      FOR FIRST bVatBillItem NO-LOCK WHERE
                bVatBillItem.Brand    = gcBrand AND
                bVatBillItem.BillCode = icBillCode:
         lcTaxClass = bVatBillItem.TaxClass.
      END.

      ASSIGN
         ldFactor = fTaxPerc(icTaxZone,lcTaxClass, idaDate)
         ldFactor = 1 + ldFactor / 100.
   END.

   RETURN ldFactor. 
   
END FUNCTION.   
