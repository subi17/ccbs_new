DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttIfispx NO-UNDO LIKE ifiSpx.
DEF BUFFER bifiSpx FOR ifiSpx.

FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000U1" NO-ERROR.
IF NOT AVAIL BillItem THEN DO:
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
END.

FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000U3" NO-ERROR.
IF NOT AVAIL BillItem THEN DO:
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
END.

FIND FIRST SIMArt WHERE
           SimArt.brand EQ "1" AND
           SimArt.SAName EQ "Universal" NO-ERROR.
IF NOT AVAIL SimArt THEN DO:
   CREATE SimArt.
   ASSIGN
      SimArt.Brand = "1"
      SimArt.SAName = "Universal"
      SimArt.SimArt = "Universal".
END.      

FOR EACH SimArt:
   DISP SimArt.
END.


FIND FIRST ifispx WHERE
           ifispx.brand EQ "1" AND
           ifispx.version EQ "00004" NO-ERROR.
IF NOT AVAIL ifispx THEN DO:
   FOR EACH ifispx WHERE
            ifispx.brand EQ "1" AND
            ifispx.version EQ "00003":
      BUFFER-COPY ifiSpx TO ttifiSpx.
      ttifiSpx.version = "00004".
      CREATE bifiSpx.
      BUFFER-COPY ttifiSpx TO bifiSpx.
      DELETE ttifiSpx.
   END.
END.

FOR EACH ifiSpx:
   DISP ifiSpx.
END.

