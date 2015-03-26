/* invprdf.i        16.01.04/aam
 
   changes:         07.10.05/aam zipcode 
*/


DEF TEMP-TABLE wInvoice NO-UNDO
    FIELD InvNum   AS INT
    FIELD Amt      AS DEC 
    FIELD MinAmt   AS DEC 
    FIELD Printed  AS INT
    FIELD CharCust AS CHAR
    FIELD Sheets   AS INT
    FIELD ZipCode  AS CHAR
    INDEX InvNum IS UNIQUE InvNum
    INDEX ZipCode ZipCode InvNum.

