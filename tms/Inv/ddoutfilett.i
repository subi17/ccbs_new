/* ddoutfilett.i      23.01.07/aam
*/

DEF TEMP-TABLE ttInvoice NO-UNDO
   FIELD InvNum   AS INT
   FIELD Printed  AS INT
   FIELD ZipCode  AS CHAR
   FIELD DueDate  AS DATE
   FIELD BankCode AS CHAR
   FIELD Movable  AS LOG 
   FIELD InvAmt   AS DEC 
   INDEX InvNum IS UNIQUE InvNum
   INDEX DueDate DueDate BankCode
   INDEX InvAmt Movable BankCode InvAmt.

