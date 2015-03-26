define input parameter icOutputFile as char no-undo.
def input param ldtPaiva  as date no-undo.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST yoigo_invrep.p" skip(1).

DEF VAR liCount1   AS INT  NO-UNDO.
DEF VAR liCount2   AS INT  NO-UNDO.
DEF VAR liCount3   AS INT  NO-UNDO.
DEF VAR liCount4   AS INT  NO-UNDO.
DEF VAR liW        AS INT  NO-UNDO.
DEF VAR liCase     AS INT  NO-UNDO.
DEF VAR liSum1     AS DECI NO-UNDO.
DEF VAR liSum2     AS DECI NO-UNDO.
DEF VAR liSum3     AS DECI NO-UNDO.
DEF VAR liSum4     AS DECI NO-UNDO.
DEF VAR liQty      AS INT  NO-UNDO.

ASSIGN liCount1 = 0
       liCount2 = 0
       liCount3 = 0
       liCount4 = 0
       liCase   = 0
       liW      = 0
       liSum1   = 0
       liSum2   = 0
       liSum3   = 0
       liSum4   = 0.

PAUSE 0.
DISP liQty WITH FRAME fAll 1 DOWN.

FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand   = "1"      AND
         Invoice.InvDate = ldtPaiva AND
         Invoice.InvType = 1:

    FIND FIRST Customer WHERE Customer.CustNum = Invoice.CustNum 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL Customer THEN DO:
        liW = liW + 1.
        NEXT.
    END.
    IF Customer.InvGroup = "VAT1" THEN DO:
         ASSIGN liCount1 = liCount1 + 1
                liCase   = 1.
    END.
    IF Customer.InvGroup = "IGIC1" THEN DO:
         ASSIGN liCount2 = liCount2 + 1
                liCase   = 2.
    END.
    IF Customer.InvGroup = "IPSIC1" THEN DO:
         ASSIGN liCount3 = liCount3 + 1
                liCase   = 3.
    END.
    IF Customer.InvGroup = "IPSIM1" THEN DO:
         ASSIGN liCount4 = liCount4 + 1
                liCase   = 4.
    END.
    FOR EACH InvRow WHERE InvRow.InvNum = Invoice.InvNum NO-LOCK:
        IF liCase = 1 THEN liSum1 = liSum1 + InvRow.Amt.
        IF liCase = 2 THEN liSum2 = liSum2 + InvRow.Amt.
        IF liCase = 3 THEN liSum3 = liSum3 + InvRow.Amt.
        IF liCase = 4 THEN liSum4 = liSum4 + InvRow.Amt.
    END.

    liQty = liQty + 1.
    IF liQty MOD 1000 = 0 THEN DO:
       PAUSE 0.
       DISP liQty WITH 1 DOWN FRAME fAll.
    END.
END.

DISP liQty WITH FRAME fAll.

PAUSE 0.
DISP stream sout liCount1 FORMAT "->>>>,>>>,>>9" COLUMN-LABEL "VAT1"
     liCount2 FORMAT "->>>>,>>>,>>9" COLUMN-LABEL "IGIC1"
     liCount3 FORMAT "->>>>,>>>,>>9" COLUMN-LABEL "IPSIC1"
     liCount4 FORMAT "->>>>,>>>,>>9" COLUMN-LABEL "IPSIM1"
     liW      COLUMN-LABEL "Others" SKIP
     
     liSum1  FORMAT "->>,>>>,>>9.99" LABEL "VAT1 SUM"
     liSum2  FORMAT "->>,>>>,>>9.99" LABEL "IGIC1 SUM"
     liSum3  FORMAT "->>,>>>,>>9.99" LABEL "IPSIC1 SUM"
     liSum4  FORMAT "->>,>>>,>>9.99" LABEL "IPSIM1 SUM"
     WITH FRAME fStat.

hide frame fAll no-pause.
DISP stream sout skip(2).
output stream sout close.
