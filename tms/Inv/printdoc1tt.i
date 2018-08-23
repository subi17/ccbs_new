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
   FIELD PostPoned       AS LOG
   FIELD InstallmentDiscAmt AS DEC
   FIELD Q25Phase AS INT INIT 99
   FIELD GBValue AS DEC
   FIELD GBDiscValue AS DEC
   FIELD AgileTV       AS DEC
   FIELD OtherConcepts AS DEC
   INDEX InvNum IS UNIQUE InvNum
   INDEX ZipCode ZipCode InvNum.

