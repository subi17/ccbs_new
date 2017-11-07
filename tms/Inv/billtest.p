{Syst/commali.i}
DEFINE VARIABLE menuc      AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE lcOutDir   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInfo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtinv     AS DATE NO-UNDO init TODAY.
DEFINE VARIABLE ldtfrom    as date no-undo init TODAY.
DEFINE VARIABLE ldtto      as date no-undo init TODAY.
DEFINE VARIABLE lcResult   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ufkey      AS LOGICAL NO-UNDO.

DEFINE VARIABLE lcDataHeader AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcQualityHeader AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcQuantityHeader AS CHARACTER NO-UNDO. 

FORM
   menuc[1] SKIP
   menuc[2] SKIP 
   menuc[3] SKIP(1)
   lcInfo FORMAT "x(38)" SKIP(1)
WITH OVERLAY WIDTH 40 ROW 1 COL 2 FRAME choices NO-LABELS.

ASSIGN
   lcOutDir = "/scratch/reports/billing_tests/".

DO WHILE TRUE ON ENDKEY UNDO:
   ASSIGN  Syst.Var:ufk = 0 Syst.Var:ufk[8] = 8 Syst.Var:ehto = 3. RUN Syst/ufkey.p. 
   hide frame fParam1 no-pause.       
   hide frame fParam2 no-pause.       
   DISPLAY 
   "A) Data Check     "  @ menuc[1] 
   "B) Quality Check  "  @ menuc[2]  
   "C) Quantity Check "  @ menuc[3]  
   WITH FRAME choices.

   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " Billings Tests "  
   CENTERED WITH COL 1 ROW 3.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      
      lcOutFile = lcOutDir + "data_check.txt".
      IF SEARCH(lcOutFile) NE ? THEN UNIX SILENT VALUE("rm " + lcOutFile).
      lcInfo = "ONGOING DATA CHECK".
      DISPLAY lcInfo  WITH FRAME choices.
      
      RUN Inv/chk_customer_bankacc.p(lcOutFile).
      RUN Inv/chk_customer_deltype.p(lcOutFile).
      RUN Inv/chk_invseq_msseq0.p(lcOutFile).
      RUN Inv/cust_address_missing.p(lcOutFile).
      
      DISPLAY "DATA CHECK DONE" @ lcInfo WITH FRAME choices.
      MESSAGE "Data check results: " + lcOutFile VIEW-AS ALERT-BOX. 
   END.

   ELSE IF FRAME-INDEX  = 2 THEN DO:
      Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
      update 
         ldtinv label "Invoice date" skip 
         ldtfrom format "99-99-99"
            label "Period"
            help "Period from (YYYYMM)"
         "-"   
         ldtto format "99-99-99"   
            no-label
            help "Period to" 
         with side-labels title "Test parameters" 
         overlay row 10 centered frame fParam2.
      hide frame fParam2 no-pause.       
      
      lcOutFile = lcOutDir + "quality_check.txt". 
      IF SEARCH(lcOutFile) NE ? THEN UNIX SILENT VALUE("rm " + lcOutFile).
      IF KEYLABEL(lastkey) = "F4" THEN NEXT.

      RUN Inv/chk_inv_cdr.p(lcOutFile,ldtinv).
      RUN Inv/chk_inv_acc.p(lcOutFile,ldtInv).
      RUN Inv/chk_subs_invoice.p(lcOutFile,ldtInv,ldtFrom,ldtTo).
      
      DISPLAY "QUALITY CHECK DONE" @ lcInfo WITH FRAME choices.
      MESSAGE "Test results: " + lcOutFile VIEW-AS ALERT-BOX. 
   END.
   
   ELSE IF FRAME-INDEX  = 3 THEN DO:
      
      def var llDisp       as log no-undo.      
      
      Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
      update 
         ldtinv label "Invoice date" 
         llDisp
            label "Disp DP Numbers"
            format "Yes/No"
            help "Display invoices that are in printing denied state"
         with side-labels title "Test parameters" 
         overlay row 10 centered frame fParam1.
      if ldtinv = ? then next. 
      hide frame fParam1 no-pause.       
      
      lcOutFile = lcOutDir + "quantity_check.txt". 
      IF SEARCH(lcOutFile) NE ? THEN UNIX SILENT VALUE("rm " + lcOutFile).
      lcInfo = "ONGOING QUANTITY CHECK".
      DISPLAY lcInfo  WITH FRAME choices.
     
      RUN Inv/yoigo_printrep.p(lcOutFile,ldtinv,lldisp).
      RUN Inv/yoigo_0count.p(lcOutFile,ldtinv).
      RUN Inv/yoigo_invrep.p(lcOutFile,ldtinv).
      
      DISPLAY "QUANTITY CHECK DONE" @ lcInfo WITH FRAME choices.
      MESSAGE "Quantity check results: " + lcOutFile VIEW-AS ALERT-BOX. 
   END.
           
   
   ELSE IF FRAME-INDEX = 4 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.
