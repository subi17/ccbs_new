DEF VAR ldtdate AS DATE NO-UNDO.
DEF VAR ldtdate2 AS DATE NO-UNDO. 
DEF VAR liInvCnt AS INT NO-UNDO.

ASSIGN
   ldtdate = 08/01/2014
   ldtDate2 = ldtdate.

DISP "Logfiles will be saved to [ /store/riftp/tmp ]  folder" SKIP
     "InvoiceRowDump:       invoice_row_dump_test_#DATE.txt " SKIP
     "BillItemsTotalLog:    billing_item_totals_#DATE.txt" SKIP(1)
     "please wait ... " SKIP.

RUN billing/ccreport_test.p
         (ldtdate,
         ldtdate2,
         1,
         "",
         0,
         0,
         "",
         OUTPUT liInvCnt).

DISP "Test dump is done within: " STRING(liInvCnt) " cases." WITH FRAME a.

