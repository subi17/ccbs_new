DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.

FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000M1" NO-ERROR.
IF AVAIL BillItem THEN DO:
   BUFFER-COPY BillItem TO ttBillItem.

   ttBillItem.billcode = "TS00000U1".
   ttBillItem.biName = "Prepaid Universal SIM".

   CREATE BillItem.
   BUFFER-COPY ttBillItem TO BillItem.
   DELETE ttBillItem.
END.


FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000M3" NO-ERROR.
IF AVAIL BillItem THEN DO:
   BUFFER-COPY BillItem TO ttBillItem.

   ttBillItem.billcode = "TS00000U3".
   ttBillItem.biName = "Postpaid Universal SIM".

   CREATE BillItem.
   BUFFER-COPY ttBillItem TO BillItem.
   DELETE ttBillItem.
END.

