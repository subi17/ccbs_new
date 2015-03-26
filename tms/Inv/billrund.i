/* billrund.i       04.09.02/aam 

*/

DEF {1} SHARED TEMP-TABLE ttInvCust NO-UNDO
   FIELD CustNr  AS INT
   FIELD MinInv  AS DEC
   FIELD LowVal  AS LOG
   FIELD CallQty AS INT
   FIELD RunStatus AS INT
   INDEX CallQty IS PRIMARY RunStatus CallQty DESC CustNr.

