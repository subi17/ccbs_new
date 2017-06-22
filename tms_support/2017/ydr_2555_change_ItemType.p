/* Configuration change script after YDR-2555 deployment. 
   Mark fixedline billing items to type 1 */

FOR EACH BitemGroup NO-LOCK WHERE
         BitemGroup.GroupType EQ 1:

   FOR EACH   BillItem EXCLUSIVE-LOCK WHERE
              BillItem.Brand    = "1" AND
              BillItem.BIGroup = BitemGroup.BIGroup:
      ASSIGN BillItem.ItemType = 1.
/*      DISPLAY BillItem.BIGroup BIName BillItem.BillCode. */
   END.
END.

/* These are special case for common Items */
FOR EACH BillItem EXCLUSIVE-LOCK WHERE
         BillItem.Brand EQ "1" AND
        (BillItem.BillCode EQ "DISCFH300" OR
         BillItem.BillCode EQ "DISCFH3002P" OR
         BillItem.BillCode EQ "DISCFH3002PDWN" OR
         BillItem.BillCode EQ "DISCFH300P" OR
         BillItem.BillCode EQ "DISCFH300PDWN" OR
         BillItem.BillCode EQ "FTERMPERIOD" OR
         BillItem.BillCode EQ "DISCFHDSL" OR
         BillItem.BillCode EQ "DISCFTERMPERIOD" OR
         BillItem.BillCode EQ "F905WOVAT" OR
         BillItem.BillCode EQ "F905WVAT" OR
         BillItem.BillCode EQ "CONVDISC"):

   ASSIGN BillItem.ItemType = 1.
/*      DISPLAY BillItem.BIGroup BIName BillItem.BillCode. */
END.

FOR EACH BillItem NO-LOCK:
   DISPLAY BillItem.BIGroup BillItem.BIName BillItem.BillCode BillItem.ItemType.
END.
