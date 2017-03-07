/* -----------------------------------------------
  MODULE .......: NNCUCO.P
  FUNCTION .....: Contract invoice maintenance (customer related)
  APPLICATION ..: NN
  AUTHOR .......: kal
  CREATED ......: 03-12-97
  MODIFIED .....: 01.04.98 pt DISPLAY also co-exDate
                  13.05.98 pt change HELP texts on lis FRAME
                  02.12.98 pt NEW File FixedFee substituted PaymFile nncoinv
                  08.12.98 pt check IF there are Unbilled items before DELETE
                  08.12.98 kl use INPUT PARAMETER FOR finding customer !
                  09.12.98 kl UPDATE BillMethod when NEW fee created
                  12.01.99 kl fixedfee.i into use
                  26.10.99 pt DO FOR jno ...  (solve a lockout problem)
                  07.12.99 pt expanded memo FIELD on FRAME sel
                  28.06.00 kl InUse initially TRUE
                  24.07.00 kl updating InUse fixed
                  24.08.00 PT allow change of BillCode code
                  27.10.00 ht Amt, OwnCost FORMAT (millions) in FRAME lis
                  11.02.02 jp CalcObj, BegDate
                  12.02.02 jp Show contract where is NOT any Billable item.
                  14.05.02 tk eventlogging added
                  20.05.2002 jpo: hosttable@keyvalue                  
                  04.10.2002 jpo: f7, delete unbilled items
                  10.10.02/jr Fixed ProdCode validate
                  18.10.02/aam don't delete contract if it has billed items,
                               labels for BillCode
                  15.11.02/jr  New memo              
                  03.03.03/tk  tokens
                  01.04.03/aam co-begper with long intervals and co-type 3
                  24.06.03/aam new parameter for fMakeContract(),
                               InclAmt, InclUnit, InclBillCode,
                               update HostTable directly
                  02.09.03 tk wait for f1 before updating
                  16.09.03/aam brand,   
                               contract
                  20.10.03/ jp new input parameter icmsseq             
                  14.11.03 jp hosttable,keyvalue not updated, 
                  19.11.03 jp refers to 
                  07.01.04/aam VATIncl
                  06.02.04 jp CustNum for memo
                  22.03.04/aam create Contract (fFeeContract)
                  24.01.05/aam allow change of EndPeriod
                  12.05.05/aam Contract was not updateable
                  30.05.05/aam use fMakeContract(More) when e.g. price is
                               changed
                  23.09.05/aam eventlog from deleting unbilled items           
                  22.12.05/aam new layout etc.
                  14.11.07/jt  sel label to FIXED FEE DATA
                  14.11.07/jt  FixedFee memoisplay, if memo[x] > "" show M  
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}  
{Func/fixedfee.i}
{Func/nncoit2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixedfee'}

{Syst/eventval.i}
{Syst/tmsconst.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   lhFixedFee = BUFFER FixedFee:HANDLE.
   RUN StarEventInitialize(lhFixedFee).

   DEFINE VARIABLE lhFFItem AS HANDLE NO-UNDO.
   lhFFItem = BUFFER FFItem:HANDLE.
   RUN StarEventInitialize(lhFFItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhFixedFee).
   END.

END.

{Func/ffeecont.i}

DEF  NEW shared VAR siirto AS CHAR.

DEF /* VAR */ INPUT PARAMETER CustNum LIKE FixedFee.CustNum NO-UNDO.
DEF /* VAR */ INPUT PARAMETER icMsseq LIKE FixedFee.keyvalue no-undo.


DEF VAR lcBillCode LIKE FixedFee.BillCode NO-UNDO.
DEF VAR prod-name LIKE BillItem.BIName     NO-UNDO.
DEF VAR Qty        LIKE FixedFee.Amt   NO-UNDO.
DEF VAR xrecid         AS RECID                        init ?.
DEF VAR firstline      AS INT                 NO-UNDO  init 0.
DEF VAR order          AS INT                 NO-UNDO  init 1.
DEF VAR ordercount     AS INT                 NO-UNDO  init 1.
DEF VAR ufkey          AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline        AS INT                 NO-UNDO  init 0.
DEF VAR ex-order       AS INT                 NO-UNDO.
DEF VAR memory         AS RECID               NO-UNDO.
def var line           as int format "99"     NO-UNDO.
DEF VAR must-print     AS LOG                 NO-UNDO.
DEF VAR must-add       AS LOG                 NO-UNDO.
DEF VAR fr-header      AS CHAR                NO-UNDO.
DEF VAR rtab           AS RECID EXTENT 24     NO-UNDO.
DEF VAR i              AS INT                 NO-UNDO.
DEF VAR rc             AS INT                 NO-UNDO.
def var ok             as log format "Yes/No" NO-UNDO.
DEF VAR amt-billed     AS i                   NO-UNDO.
DEF VAR amt-nonbilled  AS i                   NO-UNDO.
DEF VAR cper1          AS i                   NO-UNDO.
DEF VAR cper2          AS i                   NO-UNDO.
DEF VAR not-done       AS lo                  NO-UNDO.
DEF VAR newperyy        AS I                   NO-UNDO.
DEF VAR newpermm        AS I                   NO-UNDO.

DEF VAR xBegPeriod     AS INT                 NO-UNDO.
DEF VAR xStartDate     AS DATE                NO-UNDO.
DEF VAR xStartPeriod   AS INT                 NO-UNDO.

DEF VAR some-billed    AS lo                  NO-UNDO.
DEF VAR memocf         AS CHAR                NO-UNDO.
DEF VAR lcCode         AS CHAR                NO-UNDO. 
DEF VAR lcUnit         AS CHAR                NO-UNDO. 

DEF VAR lBelongto      AS LOG                 NO-UNDO INIT TRUE.
DEF VAR lcClitype      AS CHAR                NO-UNDO.
DEF VAR lcTarget       AS CHAR                NO-UNDO.
DEF VAR lcCustName     AS CHAR                NO-UNDO.
DEF VAR llHostTable    AS LOG                 NO-UNDO INIT TRUE.
DEF VAR lcHostTable    AS CHAR                NO-UNDO.
DEF VAR lcKeyValue     AS CHAR                NO-UNDO.
DEF VAR lcMemoAvail    AS CHAR                NO-UNDO.
DEF VAR lcFinancedResult AS CHAR NO-UNDO. 


DEF VAR llAdmin AS LOG NO-UNDO. 
IF getTMSRight("VENDOR") EQ "RW" THEN llAdmin = TRUE. 

DEF BUFFER bBillItem FOR BillItem.

form                                                          
    lcTarget              COLUMN-LABEL "Target"        FORMAT "X(8)"
    FixedFee.KeyValue     COLUMN-LABEL "TargetID"      FORMAT "X(8)" 
    FixedFee.BillCode     COLUMN-LABEL "Billing Item"  FORMAT "X(15)"
    FixedFee.Amt          column-label "Amount"        FORMAT "->>>>>9.999" 
    FixedFee.BegPeriod    column-label "From"
    FixedFee.EndPeriod    COLUMN-LABEL "To"
    FixedFee.BillMethod   column-label "BM"            format "9" 
    FixedFee.CLI          column-label "MSISDN"        format "x(12)"
    lcMemoAvail           COLUMN-LABEL "M"             FORMAT "x(1)" 
WITH centered  ROW 4 OVERLAY scroll 1 12 DOWN
    color value(cfc) title color value(ctc) 
    " Fixed Fees: " +  string(CustNum) + " " + 
        substr(lcCustName,1,24) + " "
    FRAME sel.

form
    FixedFee.CustNum  
        LABEL "Customer ........"
        lcCustName NO-LABEL FORMAT "X(45)"  SKIP
    
    FixedFee.FFNum  
         LABEL "Fixed Fee ID ...." FORMAT ">>>>>>>>9" 
         SKIP

    lbelongto 
          LABEL "Fee Belongs To .."
          HELP "Is target of the fee (s)ubscription or (c)ustomer?"
          FORMAT "Customer/Subscription"
       FixedFee.CLI AT 40 
          LABEL "MSISDN ......." FORMAT "X(15)" SKIP
           SKIP
 
    FixedFee.KeyValue  
          LABEL "Target ID ......." FORMAT "X(8)" 
    FixedFee.OrderId AT 40
          LABEL "Order ID ....." FORMAT "->>>>>>>>9" SKIP

    FixedFee.SourceTable
          LABEL "Source Table ...." FORMAT "X(15)"
    FixedFee.SourceKey AT 40
          LABEL "Source Key ..." FORMAT "X(15)"
    
    "Billing Item ....:" FixedFee.BillCode format "x(16)" NO-LABEL 
                        BillItem.BIName AT 40 NO-LABEL                 
    FixedFee.Amt 
          LABEL "Amount .........." FORMAT "->>>>>9.999"
          SKIP

    "Contract Date ...:" FixedFee.BegDate NO-LABEL 
          SKIP

    FixedFee.BegPeriod
          LABEL "Begin Period ...."
          HELP "First Period YYYYMM from which this payment is billable" 
          SKIP

    FixedFee.EndPeriod 
          LABEL "End Period ......"
          HELP "Last period YYYYMM (999999 : until further)"                 
          SKIP
    
    FixedFee.BillMethod 
          LABEL "Billing Method .."
       "Active .......:" AT 40 FixedFee.InUse NO-LABEL SKIP
   
    FixedFee.Interval 
          LABEL "Interval (months)"
          FORMAT ">>9"
        FixedFee.VatIncl AT 40 
          LABEL "VAT .........." SKIP

    FixedFee.Contract 
          LABEL "Contract ........"
        FixedFee.CalcObj AT 40 
          LABEL "Calc.Object .." FORMAT "x(21)" SKIP

    FixedFee.FinancedResult
          LABEL "Financed Result ." FORMAT "x(3)"
          HELP "Manual codes: B99 (financed by bank), Y99 (financed by Yoigo)"
    lcFinancedResult NO-LABEL FORMAT "X(12)"
    FixedFee.ServiceLimitGroup NO-LABEL 
        AT 56 FORMAT "X(20)"
WITH OVERLAY ROW 3 centered COLOR value(cfc) TITLE COLOR value(ctc)
     fr-header WITH SIDE-LABELS FRAME lis.

form /* seek Billable item  BY  keyvalue */
    "Target:" llHostTable 
       HELP "Enter target, (S)ubsc.ID / (C)ustomer"
       FORMAT "Subsc.ID/Customer" SKIP
    "ID ...:" lcKeyValue  
       HELP "Enter target ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Target "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* memo */
WITH
    OVERLAY ROW 6 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update Invoice Text "
    FRAME memo.

form
   "Changes made above shall ONLY be updated onto"
      amt-nonbilled format "zz9" "NOT YET billed items."
WITH
   overlay frame ch-info title " N O T E ! " centered ROW 17 NO-LABELS.

FORM
   SKIP
   FixedFeeTF.TFBank       COLON 20  SKIP
   FixedFeeTF.BankDate     COLON 20 SKIP
   FixedFeeTF.BankResult   COLON 20   SKIP
   FixedFeeTF.BankRespDate COLON 20  SKIP
   FixedFeeTF.Amount       COLON 20  SKIP         
   FixedFeeTF.ResidualAmount COLON 20  SKIP
   FixedFeeTF.OrderID      COLON 20   SKIP
   FixedFeeTF.OrgId        COLON 20   SKIP
   FixedFeeTF.CancelStatus COLON 20 FORMAT "x(10)"   SKIP 
   FixedFeeTF.CancelReason COLON 20 FORMAT "x(10)"   SKIP 
   FixedfeeTF.CancelResp   COLON 20 SKIP
   FixedFeeTF.CancelDate   COLON 20   SKIP 
   FixedFeeTF.CancelFile   COLON 20   SKIP 
   FixedfeeTF.CancelMemo   COLON 20 SKIP
   WITH OVERLAY ROW 4 CENTERED SIDE-LABELS   
   TITLE " INSTALLMENT FINANCING DATA " 
     FRAME fFixedFeeTF.
   

FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT).

   IF iiUnit > 0 THEN 
      lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "Tariff","DataType",STRING(iiUnit)).
   ELSE lcUnit = "".

   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")). 

END FUNCTION.

RUN local-find-first.

IF AVAILABLE FixedFee THEN ASSIGN
   memory     = recid(FixedFee)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No fixed fees available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = FALSE.
END.

FIND Customer NO-LOCK where 
     Customer.CustNum = CustNum .

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).



cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO TRANS:  /* FixedFee -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         ehto = 9. RUN Syst/ufkey.p.
         CREATE FixedFee.

         ASSIGN
            FixedFee.Brand      = Customer.Brand
            FixedFee.FFNum      = next-value(contract)
            FixedFee.CustNum    = Customer.CustNum
            FixedFee.VATIncl    = Customer.VATIncl
            FixedFee.BegPeriod  = year(pvm) * 100 + month(pvm)
            FixedFee.InUse      = TRUE
            FixedFee.BegDate    = TODAY
            FixedFee.Interval   = 1
            FixedFee.BillMethod = 1   /* before */
            lBelongTo           = FALSE.

         si-recid2 = FixedFee.CustNum.

         CLEAR FRAME lis no-pause.
         DISPLAY FixedFee.CustNum lcCustName WITH FRAME lis.
         
         UPDATE
            lBelongTo
            FixedFee.KeyValue
            FixedFee.BillCode
            FixedFee.Amt
            FixedFee.BegDate
            FixedFee.EndPeriod
            FixedFee.BillMethod
            FixedFee.Interval
            FixedFee.Contract
         WITH FRAME lis EDITING:
            READKEY.

            IF keylabel(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"InclUnit,HostTable,KeyValue") > 0
            THEN DO:
               
               IF FRAME-FIELD = "keyvalue" THEN DO:
                  ASSIGN INPUT lBelongTo.
                  IF   lBelongTo 
                  THEN RUN Mc/nnasel.p.
                  ELSE RUN Help/h-msisdn.p.

                  IF siirto NE ? THEN ASSIGN fixedfee.keyvalue = siirto.
                  DISP fixedfee.keyvalue with frame lis.
               END.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            if keylabel(lastkey) = "F4" THEN UNDO add-new, LEAVE add-new.

            IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               HIDE MESSAGE.

               if frame-field = "contract" then do:
                  if input frame lis FixedFee.contract ne "" then do:
                     find contract where 
                          Contract.Brand    = FixedFee.Brand AND
                          contract.contract = input frame lis FixedFee.contract
                     no-lock no-error.
                     if not available contract then do:
                        message "Unknown contract"
                        view-as alert-box.
                        next.
                     end.
                  end.
               end.

               ELSE if frame-field  = "BillCode" THEN DO:
                  ASSIGN FRAME lis FixedFee.BillCode.
                  if FixedFee.BillCode = "" THEN UNDO add-new, LEAVE add-new.
                  FIND FIRST BillItem where 
                             BillItem.Brand    = gcBrand AND
                             BillItem.BillCode = FixedFee.BillCode 
                  no-lock no-error.
                  IF NOT AVAIL BillItem THEN DO:
                     message "Billing code with this code does not exist !!".
                     NEXT.
                  END.
                  DISP BillItem.BIName.
               END.  /* BillCode */

               else if frame-field  = "lBelongTo" THEN DO:
                  IF INPUT FRAME lis lBelongTo 
                  THEN DO:
                     DISP string(FixedFee.CustNum) @ FixedFee.KeyValue.
                     NEXT-PROMPT FixedFee.KeyValue.
                     NEXT.
                  END. 
               END.

               else if frame-field  = "keyvalue" THEN DO:

                  IF INPUT FRAME lis lBelongTo = FALSE THEN DO:
                     FIND FIRST MobSub WHERE
                                Mobsub.Brand = gcBrand AND 
                                MobSub.msseq = INT(INPUT FRAME lis 
                                                   FixedFee.KeyValue) AND
                                MobSub.CustNum = FixedFee.CustNum
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL MobSub THEN DO:
                        BELL.
                        MESSAGE
                        "Unknown Mobile Subscription"
                        VIEW-AS ALERT-BOX.
                        NEXT.
                     END.
                   
                     IF INPUT FRAME lis FixedFee.Contract = "" THEN DO:
                        FOR FIRST MsOwner NO-LOCK WHERE
                                  MsOwner.MsSeq = MobSub.MsSeq:
                           DISPLAY MsOwner.Contract @ FixedFee.Contract
                              WITH FRAME lis.
                        END.
                     END.
                  END.

                  ELSE DO:
                     IF INPUT FRAME lis FixedFee.KeyValue NE  
                        STRING(FixedFee.CustNum) 
                     THEN DO:
                        BELL.
                        MESSAGE "Unknown Customer".
                        NEXT.
                     END.
                        
                  END.

               END.  

               else if frame-field  = "Begperiod" THEN DO:
                  ASSIGN FRAME lis BegPeriod.
                  RUN Syst/uperch.p(BegPeriod,output rc).
                  IF rc > 0 THEN NEXT.
               END.  /* BegPeriod */

               else if frame-field  = "EndPeriod" THEN DO:
                  ASSIGN FRAME lis EndPeriod.
                  IF EndPeriod NE 999999 THEN RUN Syst/uperch.p(BegPeriod,output rc).
                  ELSE rc = 0.
                  IF rc > 0 THEN NEXT.
               END.  /* BegPeriod */

               if frame-field  = "Interval" THEN DO:
                  ASSIGN FRAME lis FixedFee.Interval.
                  IF FixedFee.Interval = 0 THEN DO:
                     NEXT-PROMPT BegPeriod.
                     NEXT.
                  END.
               END.  /* Interval */

               ELSE IF frame-field = "BegDate" THEN DO:


                  IF INPUT BegDate > 01/01/2100 THEN DO:
                     BELL.
                     MESSAGE
                     "NOTE: "
                     "Contract beginning on next century!"
                     VIEW-AS ALERT-BOX TITLE "INFORMATION".
                  END.

                  IF INPUT FixedFee.BillMethod = 3 THEN DO:    /* after */                
                     ASSIGN
                     newperyy = year(INPUT BegDate)  
                     newpermm =  month(INPUT BegDate) + FixedFee.Interval + 1.

                     if newpermm > 12 then 
                       ASSIGN newperyy = newperyy + 
                                         TRUNCATE(newpermm / 12,0) -
                                         (if newpermm mod 12 = 0
                                          then 1
                                          else 0)
                              newpermm = if newpermm mod 12 = 0
                                         then 12
                                         else newpermm MOD 12.

                     BegPeriod = newperyy * 100 + newpermm .
                  END.
                  ELSE IF INPUT FixedFee.BillMethod = 2 THEN DO:
                     ASSIGN
                     newperyy = year(INPUT BegDate)
                     newpermm =  month(INPUT BegDate)
                     BegPeriod = newperyy * 100 + newpermm .
                  END.
                  ELSE IF INPUT FixedFee.BillMethod = 1 THEN DO:
                     ASSIGN
                     newperyy = year(INPUT BegDate)  
                     newpermm =  month(INPUT BegDate) -   1.

                     IF newpermm < 1 THEN 
                     ASSIGN newpermm = 12 + newpermm 
                            newperyy = newperyy - 1.
                     BegPeriod = newperyy * 100 + newpermm .
                  END.

                  IF BegPeriod > year(INPUT BegDate) * 100 + 
                                 month(INPUT BegDate)
                  THEN DISP BegPeriod WITH FRAME lis.
                  ELSE DISP year(INPUT BegDate) * 100 + month(INPUT BegDate)
                          @ BegPeriod WITH FRAME lis.   
                END.
             END.   /* IF LOOKUP */
             APPLY LASTKEY.
          END. /* UPDATE EDITING */

          si-recid2 = 0.

          IF lBelongTo 
          THEN FixedFee.HostTable = "Customer".  
          ELSE FixedFee.HostTable = "MobSub".
          
          rc = fMakeContract (INPUT FixedFee.FFNum,
                              0).

          message "Totally" rc "individual items created !".
          PAUSE 1 no-message.

          /* create contract */ 
          IF FixedFee.Contract = "" THEN DO:
             FIND CURRENT FixedFee EXCLUSIVE-LOCK.
             FixedFee.Contract = fFeeContract(FixedFee.Brand,  
                                              FixedFee.CustNum,
                                              "",
                                              FixedFee.BegDate,
                                              "FixedFee").
          END.
          
          IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFixedFee).

          ASSIGN
            memory = recid(FixedFee)
            xrecid = memory
            must-print = TRUE.

          LEAVE.
      END. /* repeat */
      HIDE FRAME lis no-pause.

      /* any records AVAILABLE ? */
      RUN local-find-first.
      IF NOT AVAILABLE FixedFee THEN LEAVE loop.
      ELSE NEXT LOOP.

   END.  /* must-add */

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND FixedFee where recid(FixedFee) = memory    no-lock no-error.
        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE FixedFee THEN DO:

              RUN local-disp-row.

              rtab[FRAME-LINE] = recid(FixedFee).

              RUN local-find-next.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE.
        PAUSE 0 no-message.

        /* one page of data has been Printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 183 ufk[2]= 927  ufk[3]= 2150 ufk[4]= 609
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= (IF lcRight = "RW" THEN 187 ELSE 0)   
        ufk[8]= 8 ufk[9]= 1
        ehto  = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW FixedFee.KeyValue {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) FixedFee.KeyValue WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND FixedFee where recid(FixedFee) = memory.
        DO i = 1 TO FRAME-LINE - 1:
              
           RUN local-find-prev.    
           IF AVAILABLE FixedFee THEN
              ASSIGN firstline = i memory = recid(FixedFee).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND FixedFee where recid(FixedFee) = rtab[1] no-lock.
           
           RUN local-find-prev.

           IF NOT AVAILABLE FixedFee THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              RUN local-disp-row.

              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(FixedFee)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND FixedFee where recid(FixedFee) = rtab[FRAME-DOWN] no-lock .
           
           RUN local-find-next.
           
           IF NOT AVAILABLE FixedFee THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.

              RUN local-disp-row.

              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(FixedFee).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FixedFee where recid(FixedFee) = memory no-lock no-error.
        
        RUN local-find-prev.

        IF AVAILABLE FixedFee THEN DO:
           memory = recid(FixedFee).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
        
              RUN local-find-prev.

              IF AVAILABLE FixedFee THEN memory = recid(FixedFee).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND FixedFee where recid(FixedFee) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku sarakk. 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.

       lcKeyValue = "". 
       PAUSE 0.
       UPDATE llHostTable 
              lcKeyValue WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       DO:
          
          IF llHostTable 
          THEN lcHostTable = "MobSub".
          ELSE lcHostTable = "Customer".
                
          FIND FIRST FixedFee USE-INDEX CustNum WHERE 
                     FixedFee.Brand     = gcBrand     AND 
                     FixedFee.CustNum   = CustNum     AND
                     FixedFee.HostTable = lcHostTable AND
                     FixedFee.KeyValue  = lcKeyValue 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE FixedFee THEN 
          FIND FIRST FixedFee USE-INDEX CustNum WHERE 
                     FixedFee.Brand     = gcBrand     AND 
                     FixedFee.CustNum   = CustNum     AND
                     FixedFee.HostTable = lcHostTable AND
                     FixedFee.KeyValue >= lcKeyValue 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE FixedFee THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
       
          /*  FixedFee/prod-code was found */
          ASSIGN order = 1 memory = recid(FixedFee) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN
     DO: /* MEMO */
        FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
        NO-LOCK.
        RUN Mc/memo.p(INPUT FixedFee.CustNum,
                 INPUT "FixedFee",
                 INPUT STRING(FixedFee.FFNum),
                 INPUT "FixedFee").
        ufkey = TRUE. 
        ehto = 9.
        NEXT LOOP.
     END.

     ELSE if lookup(nap,"3,f3") > 0 THEN 
     DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:   /* memo */

       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey.p. RUN Syst/ufcolor.p.
       FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
       exclusive-lock.

       /* calculate count of billed/nonbilled items */
       RUN Mc/nnccit.p(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).

       PAUSE 0.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).

       UPDATE text(FixedFee.Memo [1 FOR 5]) WITH FRAME memo 1 col.
       /* UNDO WITH F4 -key */
       if keylabel(lastkey) = "F4" THEN DO:
          HIDE FRAME memo no-pause.
          UNDO LOOP.
       END.

       IF 
       FixedFee.Memo[1] entered OR
       FixedFee.Memo[2] entered OR
       FixedFee.Memo[3] entered OR
       FixedFee.Memo[4] entered OR
       FixedFee.Memo[5] entered THEN DO:
          rc = 0.
          FOR EACH FFItem of FixedFee where NOT Billed exclusive-lock:
             rc = rc + 1.
             DO i = 1 TO 5.
                FFItem.Memo[i] = FixedFee.Memo[i].
             END.   
          END.
          message "Memo text updated onto" rc "items - press enter !".
          PAUSE no-message.
       END.
       HIDE FRAME memo no-pause.
       PAUSE 0 .
       DISP FixedFee.Cli WITH FRAME sel.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).

     END.


     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* items */
        FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE] no-lock.
        RUN Mc/nncobi.p(FixedFee.FFNum).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"7,f7") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* del unbilled */
        FIND FixedFee WHERE RECID(FixedFee) = rtab[FRAME-LINE] NO-LOCK.
        ok = FALSE.

        ASSIGN 
        xStartPeriod = 999999
        i            = 0 
        some-billed  = FALSE.

        /* count no. of billable items */

        FOR EACH FFItem OF FixedFee NO-LOCK:
           IF NOT billed THEN ASSIGN 
              i = i + 1
              xStartPeriod = MIN(xStartPeriod,FFItem.BillPeriod).
           ELSE some-billed = TRUE.
        END.   

        IF NOT some-billed THEN DO:
           MESSAGE
           "None of billable items is billed."  SKIP
           "Erase whole contract instead (F6) !"
           VIEW-AS ALERT-BOX.
           NEXT.
        END.   

        IF i = 0 THEN MESSAGE
        "There are NO billable items anymore on this contract !"
        VIEW-AS ALERT-BOX.

        ELSE DO:

           PAUSE 0.
           UPDATE xStartPeriod 
            format "999999"
            label "From period"
            help  "First billing period to be deleted (0 = ALL)" 
            WITH OVERLAY ROW 10 centered side-labels
                 title " Delete unbilled items "
                 FRAME fStartPeriod.
           HIDE FRAME fStartPeriod NO-PAUSE.

           ASSIGN i = 0.

           FOR EACH FFItem OF FixedFee NO-LOCK WHERE 
             NOT billed AND
             FFItem.BillPeriod ge xStartPeriod:
                ASSIGN i = i + 1.
           END.

           MESSAGE 
           "DO YOU REALLY WANT TO DELETE" i "billable items" SKIP
           "from this contract, starting from period"
           xStartPeriod
           VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

           IF ok THEN DO:
              FOR EACH FFItem OF FixedFee WHERE 
                NOT billed AND
                FFItem.BillPeriod ge xStartPeriod:
                  IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFFItem).
                  DELETE FFItem.
              END.
              MESSAGE i "billable items deleted"
              VIEW-AS ALERT-BOX INFORMATION.

              /* set the expiring period */
              FIND LAST FFItem OF FixedFee NO-LOCK.
              FIND CURRENT FixedFee EXCLUSIVE-LOCK.

              MESSAGE
              "The expiration period of this contract"    SKIP
              "was" FixedFee.EndPeriod "and it was replaced" SKIP
              "by" FFItem.BillPeriod "which is the billing" SKIP
              "period of the last item on this contract."
              VIEW-AS ALERT-BOX.              

              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).

              ASSIGN
              FixedFee.EndPeriod = FFItem.BillPeriod
              FixedFee.InUse     = FALSE.

              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).
              
              DISP FixedFee.EndPeriod WITH FRAME sel.
           END.   
           
           RUN Syst/ufkey.p.
           
        END.
     END.

     else if lookup(nap,"6,f6") > 0 AND ufk[6] > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       
       delline = FRAME-LINE.
       FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE] no-lock.

       /* calculate count of billed/nonbilled items */
       RUN Mc/nnccit.p(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).

       /* if something has already been billed -> don't delete */
       IF amt-billed > 0 THEN DO:
          BELL.
          MESSAGE 
          "Contract has" amt-billed "billed items."
          "It can not be deleted. "
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT.
       END.   

       IF NOT FixedFee.BillCode BEGINS "PAYTERM" AND
          NOT llAdmin THEN DO:
          MESSAGE 
          "Contract deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT.
       END.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          FixedFee.BillCode FixedFee.EndPeriod
          FixedFee.Amt FixedFee.BegPeriod  FixedFee.Cli.

       RUN local-find-next.

       IF AVAILABLE FixedFee THEN memory = recid(FixedFee).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN local-find-prev.
          IF AVAILABLE FixedFee THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(FixedFee).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          FixedFee.BillCode FixedFee.EndPeriod
          FixedFee.Amt FixedFee.BegPeriod FixedFee.Cli.
       IF ok THEN DO:

           FOR EACH FFItem of FixedFee:
              DELETE FFItem.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFixedFee).

           DELETE FixedFee.

           /* in the LAST record was deleted ? */
           RUN local-find-first.
           
           IF NOT AVAILABLE FixedFee THEN DO: 
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION ON ENDKEY UNDO, NEXT LOOP:
       /* change */

       assign fr-header = " FIXED FEE DATA " cfc = "lis".  RUN Syst/ufcolor.p.

       FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
       no-lock.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).

       /* calculate count of billed/nonbilled items */
       RUN Mc/nnccit.p(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).

       IF amt-nonbilled = 0 THEN DO:
          ehto = 5.
          RUN Syst/ufkey.p.
          BELL.
          message "There are NONE Unbilled items left" SKIP
          VIEW-aS ALERT-BOX title " NOTE ".         
          assign fr-header = " VIEW ".
       END.   

       FIND BillItem   where 
            BillItem.Brand    = FixedFee.Brand AND
            BillItem.BillCode = FixedFee.BillCode no-lock no-error.

       IF FixedFee.InclBillCode > "" THEN 
          FIND bBillItem WHERE 
               bBillItem.Brand    = FixedFee.Brand AND
               bBillItem.BillCode = FixedFee.InclBillCode NO-LOCK NO-ERROR.

       CLEAR FRAME lis NO-PAUSE.
       
       IF Fixedfee.Hosttable = "MOBSUB" THEN DO:
          FIND FIRST mobsub where 
                     mobsub.msseq = INT(fixedfee.keyvalue) NO-LOCK NO-ERROR.
          IF AVAIL mobsub THEN DO:
             FIND first clitype WHERE 
                        clitype.clitype = mobsub.clitype NO-LOCK NO-ERROR.
                    
             IF  AVAIL clitype THEN lcclitype = clitype.cliname.           
             ELSE  lcclitype = "".
          ENd.
          IF not avail mobsub then lcclitype = "".
       END.

       lbelongto = (FixedFee.HostTable = "Customer" OR 
                    FixedFee.HostTable = "").
       
       FIND Customer WHERE Customer.CustNum = FixedFee.CustNum NO-LOCK.
       lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                     BUFFER Customer).

       IF FixedFee.TFBank > "" THEN 
         lcFinancedResult = "(" + FixedFee.TFBank + ")".
       else lcFinancedResult = "".

       DISPLAY
          FixedFee.CustNum lcCustName
          lBelongTo  
          FixedFee.Contract
          FixedFee.FFNum
          FixedFee.BillCode
          BillItem.BIName when AVAIL BillItem
          "!! UNKNOWN !!" when NOT AVAIL BillItem @ BillItem.BIName
          FixedFee.BegPeriod
          FixedFee.Interval
          FixedFee.EndPeriod
          FixedFee.BegDate
          FixedFee.CalcObj 
          FixedFee.ServiceLimitGroup
          FixedFee.FinancedResult lcFinancedResult
          FixedFee.VATIncl
          FixedFee.Amt
          FixedFee.BillMethod
          FixedFee.InUse
          FixedFee.KeyValue
          FixedFee.CLI
          FixedFee.SourceTable
          FixedFee.SourceKey
          FixedFee.OrderID
       WITH FRAME lis.

       PAUSE 0.
       ASSIGN not-done = TRUE  ufkey = TRUE.

       IF lcRight = "RW" THEN DO:         

          ASSIGN 
             ufk = 0 
             ufk[1] = 7 
             ufk[2] = 9845 when CAN-FIND(first fixedfeetf of fixedfee)
             ufk[8] = 8 
             ehto = 0 
             ufkey = true.
          RUN Syst/ufkey.p.

          if toimi = 8 then do:
             hide frame lis no-pause.
             next.
          end.

          else if toimi = 2 then do:

            FIND fixedfeetf NO-LOCK WHERE
                 fixedfeetf.ffnum = fixedfee.ffnum NO-ERROR.

            ehto = 3. ufk = 0. RUN Syst/ufkey.p.
            ufkey = true.
            DISP
               FixedFeeTF.BankDate     
               FixedFeeTF.BankRespDate 
               FixedFeeTF.BankResult   
               FixedFeeTF.CancelDate   
               FixedFeeTF.OrderId
               FixedFeeTF.OrgId        
               FixedFeeTF.ResidualAmount
               FixedFeeTF.Amount            
               FixedFeeTF.CancelStatus
               FixedFeeTF.CancelReason
               FixedFeeTF.CancelMemo
               FixedFeeTF.CancelResp
               FixedFeeTF.CancelFile
               FixedFeeTF.TFBank WITH FRAME fFixedFeeTF.
            PAUSE.
            HIDE FRAME fFixedFeeTF NO-PAUSE.
            NEXT.
          end.
          
          ehto = 9.
          RUN Syst/ufkey.p.

          si-recid2 = FixedFee.CustNum.

          PROMPT
          FixedFee.Amt            WHEN amt-nonbilled > 0
          FixedFee.EndPeriod      WHEN amt-nonbilled > 0
          FixedFee.BillMethod     WHEN amt-nonbilled > 0
          FixedFee.Contract       WHEN amt-nonbilled > 0
          FixedFee.FinancedResult WHEN amt-nonbilled > 0 AND 
                                       (FixedFee.BillCode BEGINS "PAYTERM" OR
                                        FixedFee.BillCode BEGINS "RVTERM")
          FixedFee.InUse
          FixedFee.VATIncl        WHEN amt-nonbilled > 0
          WITH FRAME lis EDITING:

            READKEY.

            IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               HIDE MESSAGE.

               if frame-field = "contract" then do:
                  if input frame lis FixedFee.contract ne "" then do:
                  end.
               end.

               IF FRAME-FIELD EQ "FinancedResult" THEN DO:
                  IF (INPUT FixedFee.FinancedResult NE 
                            FixedFee.FinancedResult) AND
                     INPUT FixedFee.FinancedResult > "" THEN DO:
                     IF LOOKUP((INPUT FixedFee.FinancedResult),
                               {&TF_STATUSES_MANUAL}) = 0 OR
                        (INPUT FixedFee.FinancedResult = "B99" AND
                         LOOKUP(FixedFee.TFbank,{&TF_BANK_CODES}) = 0) THEN DO:
                        MESSAGE 
                           "Incorrect or not allowed value"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT-PROMPT FixedFee.FinancedResult.
                        NEXT.
                     END.
                  END.
               END.
               
             END.

             APPLY LASTKEY.

          END. /* EDITING */   

          FIND CURRENT FixedFee NO-LOCK.
             
          IF CURRENT-CHANGED FixedFee THEN DO:
             
             MESSAGE ({&MSG_RECORD_CHANGED})
             VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
       
             UNDO, NEXT LOOP.

          END. 
          ELSE DO:
          
            FIND CURRENT FixedFee EXCLUSIVE-LOCK.
          
            ASSIGN
             FixedFee.Amt            WHEN amt-nonbilled > 0
             FixedFee.EndPeriod      WHEN amt-nonbilled > 0
             FixedFee.BillMethod     WHEN amt-nonbilled > 0
             FixedFee.Contract       WHEN amt-nonbilled > 0
             FixedFee.InUse
             FixedFee.VATIncl        WHEN amt-nonbilled > 0
             FixedFee.FinancedResult WHEN amt-nonbilled > 0.
            FIND CURRENT FixedFee NO-LOCK.
          END.

          si-recid2 = 0.

          IF FixedFee.Amt entered          OR
             FixedFee.BillMethod  entered  OR
             FixedFee.EndPeriod ENTERED
          THEN DO:

             ok = FALSE.
             rc = 0.
             message "Do You want to change the Billable " + 
                  "(not Billed) items (Y/N) ?" UPDATE ok.
             IF ok THEN DO:

                FOR EACH FFItem of FixedFee WHERE
                         FFItem.Billed = FALSE:
                   IF llDoEvent THEN RUN StarEventMakeDeleteEventWithMemo(
                                           lhFFItem,
                                           katun,
                                           "ManualCUI").
                   DELETE FFItem.
                END. 

                FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.

                /* create items from scratch */
                IF NOT AVAIL FFItem THEN DO:
                   fMakeContract(FixedFee.FFNum,
                                 0).
                END.
                
                /* add new ones after last billed one */
                ELSE DO:
                   fMakeContractMore(FixedFee.FFNum, 
                                     FFItem.Concerns[2]).
                END.

                not-done = FALSE.
                
                message "Non-billed items were changed."
                VIEW-AS ALERT-BOX. 
                
                RUN Mc/nncobi.p(FixedFee.FFNum).
             END.
             ELSE not-done = TRUE.
          END.
          ELSE not-done = FALSE.
       END.
       ELSE PAUSE MESSAGE "PRESS ENTER TO CONTINUE !".

       HIDE FRAME lis no-pause.

       IF not-done THEN UNDO, NEXT LOOP.

       DISPLAY 
          FixedFee.BillCode 
          FixedFee.Amt 
          FixedFee.BillMethod 
       WITH FRAME sel.
       xrecid = recid(FixedFee).

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:

       RUN local-find-first.

       ASSIGN memory = recid(FixedFee) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN local-find-last.

       ASSIGN memory = recid(FixedFee) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
  LEAVE LOOP.
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   RUN local-find-others.
    
   DISPLAY  lcTarget 
            FixedFee.KeyValue
            FixedFee.BillCode 
            FixedFee.Amt 
            FixedFee.BegPeriod 
            FixedFee.EndPeriod 
            FixedFee.BillMethod
            FixedFee.CLI
            lcMemoAvail
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
   
   DEF VAR liLoop AS INTEGER NO-UNDO.
   
   lcMemoAvail = "".
   CASE FixedFee.HostTable:
   WHEN "MobSub" THEN lcTarget = "Subsc.ID".
   OTHERWISE lcTarget = FixedFee.HostTable.
   END CASE. 
   /* Check if fixed fee memos has writing */
   DO liLoop = 1 TO 5:
      IF FixedFee.Memo[liLoop] > "" THEN DO:
         lcMemoAvail = "M".
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE local-find-next:

   IF icMsSeq > "" THEN DO:
      FIND NEXT FixedFee USE-INDEX CustNum WHERE 
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum AND 
                FIxedFee.HostTable = "Mobsub" AND
                FixedFee.KeyValue  = icMsseq
      NO-LOCK NO-ERROR.
   END.
 
   ELSE DO:
      IF order = 1 THEN 
      FIND NEXT FixedFee USE-INDEX CustNum WHERE
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-prev:

   IF icMsSeq > "" THEN DO:
      FIND PREV FixedFee USE-INDEX CustNum WHERE 
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum AND 
                FIxedFee.HostTable = "Mobsub" AND
                FixedFee.KeyValue  = icMsseq
      NO-LOCK NO-ERROR.
   END.
 
   ELSE DO:
      IF order = 1 THEN 
      FIND PREV FixedFee USE-INDEX CustNum WHERE
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-first:

   IF icMsSeq > "" THEN DO:
      FIND FIRST FixedFee USE-INDEX CustNum WHERE 
                 FixedFee.Brand   = gcBrand AND
                 FixedFee.CustNum = CustNum AND 
                 FIxedFee.HostTable = "Mobsub" AND
                 FixedFee.KeyValue  = icMsseq
      NO-LOCK NO-ERROR.
   END.
 
   ELSE DO:
      IF order = 1 THEN 
      FIND FIRST FixedFee USE-INDEX CustNum WHERE
                 FixedFee.Brand   = gcBrand AND
                 FixedFee.CustNum = CustNum NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-last:

   IF icMsSeq > "" THEN DO:
      FIND LAST FixedFee USE-INDEX CustNum WHERE 
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum AND 
                FIxedFee.HostTable = "Mobsub" AND
                FixedFee.KeyValue  = icMsseq
      NO-LOCK NO-ERROR.
   END.
 
   ELSE DO:
      IF order = 1 THEN 
      FIND LAST FixedFee USE-INDEX CustNum WHERE
                FixedFee.Brand   = gcBrand AND
                FixedFee.CustNum = CustNum NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
