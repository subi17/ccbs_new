
DEF TEMP-TABLE ttInvoice NO-UNDO {&ttReference}
   FIELD InvNum  AS INT
   FIELD InvCust AS INT
   FIELD Order   AS INT
   INDEX Order Order.
 