/* printdoc1tt.i      20.12.06/aam
*/

DEF TEMP-TABLE ttInvoice NO-UNDO
   FIELD InvNum   AS INT
   FIELD Printed  AS INT
   FIELD ZipCode  AS CHAR
   FIELD MsRequest AS INT
   FIELD ErrMsg AS CHAR 
   FIELD InstallmentAmt AS DEC
   FIELD PenaltyAmt AS DEC
   FIELD InstallmentDiscAmt AS DEC
   INDEX InvNum IS UNIQUE InvNum
   INDEX ZipCode ZipCode InvNum.

