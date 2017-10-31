form
    "  0: Uninvoiced, 'new' invoices             " SKIP
    "  1: Billed using Print House             " SKIP
    "  2: Billed locally                       " SKIP
    "  3: Billed locally, Sent to Print House  " SKIP
    "  9: Credited invoices                      " SKIP 
WITH
   title color value (Syst.CUICommon:ctc) " Status codes for invoices " COLOR value(Syst.CUICommon:cfc)
   OVERLAY centered ROW 13 FRAME statu.


