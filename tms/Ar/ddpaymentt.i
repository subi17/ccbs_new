/* ddpaymentt.i      23.04.07/aam
*/

DEF TEMP-TABLE ttInvoice NO-UNDO
   FIELD InvNum   AS INT
   FIELD Paid     AS INT
   INDEX InvNum IS UNIQUE InvNum.

