/* refufile.i       30.03.04/aam
*/


DEF TEMP-TABLE ttPaym NO-UNDO
   FIELD Voucher  AS INT
   FIELD CustNum  AS INT
   FIELD Amt      AS DEC
   FIELD CustBank AS CHAR
   INDEX CustNum CustNum Voucher
   INDEX Amt Amt CustNum. 
 
