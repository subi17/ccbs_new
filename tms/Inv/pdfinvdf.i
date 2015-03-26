/* pdfinvdf.i       30.04.03/aam
                    25.11.03/aam ClaimAmt etc.

*/

DEF TEMP-TABLE ttPDFInv NO-UNDO
   FIELD InvNum   AS INT
   FIELD PDFFile  AS CHAR
   FIELD BarCode  AS CHAR
   FIELD BankAcc  AS CHAR
   FIELD RefNum   AS CHAR
   FIELD MinClAmt AS DEC
   FIELD ClaimAmt AS DEC
   FIELD InvForm  AS CHAR 
   FIELD Printed  AS INT
   INDEX InvNum InvNum.
 
