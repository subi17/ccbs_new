
DEF TEMP-TABLE ttSubs NO-UNDO {&ttReference}
   FIELD MsSeq AS INT
   FIELD InvCust AS INT
   FIELD ErrorFound AS LOGICAL
   FIELD Order AS INT
   INDEX Order Order.
 
