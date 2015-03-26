/* camprundf.i    18.02.04/aam 
*/

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   FIELD MsSeq   AS INT
   INDEX CustNum CustNum MsSeq.
   

