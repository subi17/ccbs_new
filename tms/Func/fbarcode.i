/* fbarcode.i       09.10.03/aam 

   form the bar code line in numeric format 
   total length is 54 
*/


FUNCTION fBarCodeNum RETURNS CHARACTER
   (idAmt      AS DEC,
    idtDueDate AS DATE,
    icRefNum   AS CHAR,
    icBank     AS CHAR).
   
       
   DEF VAR lcBarCode AS CHAR NO-UNDO.

   IF icBank     = "" OR
      icRefNum   = "" OR
      idAmt      = 0  OR
      idtDueDate = ?  
   THEN RETURN "".
   
   IF LENGTH(icRefNum) < 20 
   THEN icRefNum = FILL("0",20 - LENGTH(icRefNum)) + icRefNum.

   IF LENGTH(icBank) < 14
   THEN icBank = FILL("0",14 - LENGTH(icBank)) + icBank.
   
   lcBarCode =  
   "2"                                     +        /* 2 = EUR       1  */
   STRING(icBank,"X(14)")                  +        /* bank account  14 */
   STRING(TRUNCATE(idAmt,0),"999999")      +        /* Euros         6  */
   STRING((idAmt - TRUNCATE(idAmt,0)) * 100
          ,"99")                           +        /* Cents         2  */
   STRING(icRefNum,"X(20)")                +        /* reference nbr 20 */ 
   STRING(ENTRY(3,STRING(idtDueDate),"/"),"99") +   /* year          2  */
   STRING(ENTRY(2,STRING(idtDueDate),"/"),"99") +   /* month         2  */
   STRING(ENTRY(1,STRING(idtDueDate),"/"),"99") +   /* day           2  */
   "0000".                                          /* fill          4  */
 
   /* calculate last digit with the same logic as for reference nbr */
   lcBarCode = lcBarCode + fChkNbrFIN(lcBarCode).

   RETURN lcBarCode.

END FUNCTION.
