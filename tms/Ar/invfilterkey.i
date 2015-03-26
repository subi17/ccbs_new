/* invfilterkey.i    22.11.07/aam
*/


DEF TEMP-TABLE ttFilter NO-UNDO
   FIELD FType     AS INT
   FIELD FIntKey   AS INT
   FIELD FCharKey  AS CHAR
   FIELD FQty      AS INT
   INDEX FChar FType FCharKey
   INDEX FInt  FType FIntKey.

