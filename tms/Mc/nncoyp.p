/* -----------------------------------------------
  MODULE .......: NNCOYP.P
  FUNCTION .....: Contract invoice maintenance
  APPLICATION ..: NN
  AUTHOR .......: kal
  CREATED ......: 03-12-97
  MODIFIED .....: 01.04.98 pt DISPLAY also co-exDate
                  13.05.98 pt change HELP texts on lis FRAME
                  01.12.98 pt NEW File coyp substituted PaymFile nncoyp
                  03.12.98 pt changes are allowed
                  08.12.98 pt check IF there are Unbilled items before DELETE
                  27.01.99 pt IF not-done ...
                  17.05.99 jp uright1 & uright2 added  
                  28.06.00 kl InUse initially TRUE
                  24.07.00 kl updating InUse fixed
                  27.10.00 ht Amt, OwnCost FORMAT (millions) in FRAME lis
                  05.06.01 pt FFNum retrieved now from sequence "contract"
                  28.12.01 jp BegDate
                  12.02.02 jp Show contract where is NOT any Billable item.
                  14.05.02 tk Event logging added
                  20.05.2002 jpo: hosttable@keyvalue
                  22.07.2002 tk print full page on "end"
                  04.10.2002 jpo: f7, delete unbilled items
                  18.10.02/aam don't delete contract if it has billed items,
                               labels for BillCode
                  15.11.02/jr New memo
                  05.03.03 tk changed F1 label to FIND, tokens
                  01.04.03/aam co-begper with long intervals and co-type 3
                  24.06.03/aam new parameter for fMakeContract(),
                               InclAmt, InclUnit, InclBillCode,
                               update HostTable directly
                  02.09.03 tk wait for f1 before updating
                  16.09.03/aam brand,
                               Contract
                  14.11.03 fixedfee.cli , hosttable,keyvalue not updated       
                  07.01.04/aam VATIncl
                  06.03.04 jp  custnum for memo
                  22.03.04/aam create Contract (fFeeContract)
                  09.08.04/jp  show name of clitype      
                  30.05.05/aam use fMakeContract(More) when e.g. price is
                               changed
                  23.09.05/aam eventlog from deleting unbilled items              Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable FixedFee

{Syst/commali.i} 
{Func/fixedfee.i} 
{Func/nncoit2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixedfee'}

{Syst/eventval.i}

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

DEF NEW shared VAR siirto AS CHAR.

DEF VAR CustNum         LIKE FixedFee.CustNum  NO-UNDO.
DEF VAR cust-name       LIKE Customer.CustName NO-UNDO.
DEF VAR BillCode        LIKE FixedFee.BillCode NO-UNDO.
DEF VAR prod-name       LIKE BillItem.BIName   NO-UNDO.
DEF VAR Qty             LIKE FixedFee.Amt      NO-UNDO.
DEF VAR xrecid          AS RECID                        init ?.
DEF VAR firstline       AS INT                 NO-UNDO  init 0.
DEF VAR order           AS INT                 NO-UNDO  init 1.
DEF VAR ordercount      AS INT                 NO-UNDO  init 2.
DEF VAR ufkey           AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline         AS INT                 NO-UNDO  init 0.
DEF VAR ex-order        AS INT                 NO-UNDO.
DEF VAR memory          AS RECID               NO-UNDO.
def var line            as int format "99"     NO-UNDO.
DEF VAR must-print      AS LOG                 NO-UNDO.
DEF VAR must-add        AS LOG                 NO-UNDO.
DEF VAR fr-header       AS CHAR                NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24     NO-UNDO.
DEF VAR i               AS INT                 NO-UNDO.
DEF VAR rc              AS INT                 NO-UNDO.
def var ok              as log format "Yes/No" NO-UNDO.
DEF VAR amt-billed      AS i                   NO-UNDO.
DEF VAR amt-nonbilled   AS i                   NO-UNDO.
DEF VAR cper1           AS i                   NO-UNDO.
DEF VAR cper2           AS i                   NO-UNDO.
DEF VAR not-done        AS lo                  NO-UNDO.
DEF VAR newperyy        AS I                   NO-UNDO.
DEF VAR newpermm        AS I                   NO-UNDO.
DEF VAR endloop         AS I                   NO-UNDO.


DEF VAR xBegPeriod     AS INT                 NO-UNDO.
DEF VAR xStartDate     AS DATE                NO-UNDO.
DEF VAR xStartPeriod   AS INT                 NO-UNDO.

DEF VAR some-billed    AS lo                  NO-UNDO.
DEF VAR memocf         AS CHAR                NO-UNDO.

DEF VAR lcCode         AS CHAR                NO-UNDO. 
DEF VAR lcUnit         AS CHAR                NO-UNDO. 
DEF VAR lcKeyValue     AS CHAR                NO-UNDO.
DEF VAR lcClitype      AS CHAR                NO-UNDO.

DEF BUFFER bBillItem FOR BillItem.

form
    FixedFee.Brand           column-label "Bran"          format "x(4)"
    FixedFee.CustNum      /* column-label "Cust.nr"  */
    cust-name                column-label "CName"         format "x(9)"
    FixedFee.BillCode        column-label "BillCode"      format "x(9)"
    FixedFee.Contract        
    FixedFee.Amt          /* column-label "Amount"   */    
    FixedFee.BegPeriod    /* column-label "From per" */   
    FixedFee.EndPeriod   /* column-label "Exp per"  */
    FixedFee.Memo[1]     /* column-label "Memo"     */    format "x(7)"
WITH width 80 ROW 1 OVERLAY scroll 1 15 DOWN
    color value(cfc) title color value(ctc) " " + ynimi +
    " CONTRACT FEES "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "Customer nr .......:" CustNum   Customer.CustName AT 39 SKIP
    "Contract ..........:" FixedFee.Contract skip
    "BillCode ..........:" BillCode format "x(16)"
        BillItem.BIName  AT 39 SKIP
    "VAT ...............:" FixedFee.VATIncl  skip
    "Amount ............:" FixedFee.Amt  format "->,>>>,>>9.99"  
       "Included amount .:" AT 39 FixedFee.InclAmt  SKIP
    "Our own cost ......:" FixedFee.OwnCost format "->,>>>,>>9.99"     
       "Unit of incl.amt :" AT 39 FixedFee.InclUnit 
       lcUnit FORMAT "X(15)" SKIP
    "Incl.amt concerns:" AT 39 FixedFee.InclBillCode 
       SKIP 
    
    "Type of contract ..:" FixedFee.BillMethod                             SKIP      "Contract belongs to:" FixedFee.Cli FORMAT "x(12)"
                           FixedFee.HostTable FORMAT "X(10)" 
        help "Customer, Mobsub or DataCLI"
       
       "KeyValue"  FixedFee.KeyValue  format "x(15)"               
       lcClitype NO-LABEL                                                 SKIP
    "Interval (months) .:" FixedFee.Interval                              SKIP
    "Date of contract ..:" FixedFee.BegDate                               SKIP
    "From Period .......:" FixedFee.BegPeriod
help "First Period YYYYMM from which this payment shall be invoiced" 
      FixedFee.CalcObj format "x(28)"                                     SKIP

    "Period of expiry ..:" FixedFee.EndPeriod                      
 help "Last Period YYYYMM from which this payment shall be invoiced"
 "(999999 : until further)"                                       SKIP
    "Contract is in use :" FixedFee.InUse                          SKIP

 WITH  OVERLAY ROW 1 centered
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header NO-LABEL
    FRAME lis.

{Func/brand.i}

form
   "Changes made above shall ONLY be updated onto"
      amt-nonbilled format "zz9" "NOT YET billed items."
WITH
   overlay frame ch-info title " N O T E ! " centered ROW 17 NO-LABELS.


form /*  search WITH FIELD CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" CustNum
    help "Give Customer No."
    with row 4 col 2 title color value(ctc) " FIND CUSTOMER "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD BillCode */
    "Brand ..:" lcBrand skip
    "BillItem:" BillCode
    help "Give BillItem Code"
    with row 4 col 2 title color value(ctc) " FIND BillItem CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form /* memo */
WITH
    OVERLAY ROW 6 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update Invoice Text "
    FRAME memo.


FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT).

   IF iiUnit > 0 THEN 
      lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "Tariff","DataType",STRING(iiUnit)).
   ELSE lcUnit = "".

   DISPLAY lcUnit WITH FRAME lis.

   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")). 

END FUNCTION.


    
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST FixedFee
WHERE FixedFee.Brand = lcBrand no-lock no-error.
IF AVAILABLE FixedFee THEN ASSIGN
   memory     = recid(FixedFee)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No fixedfees available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by customer Number  ".
       if order = 2 then put screen row 19 col 30 " Order by BillCode code     ".
    END.

   IF must-add THEN DO:  /* FixedFee -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         ehto = 9. RUN Syst/ufkey.
         assign CustNum = 0 BillCode = "".
         CREATE FixedFee.

         CLEAR FRAME lis no-pause.

         ASSIGN
         /* sequence */
         FixedFee.FFNum = NEXT-VALUE(contract)
         /* beginning Period */
         FixedFee.BegPeriod = year(pvm) * 100 + month(pvm)
         FixedFee.Brand     = lcBrand
         FixedFee.InUse     = TRUE
         FixedFee.BegDate   = TODAY
         .



         UPDATE CustNum
                FixedFee.Contract
                BillCode
                FixedFee.VATIncl
                FixedFee.Amt
                FixedFee.OwnCost
                FixedFee.InclAmt
                FixedFee.InclUnit
                FixedFee.InclBillCode
                FixedFee.BillMethod
                FixedFee.CLI       
                FixedFee.Interval
                FixedFee.BegDate
                FixedFee.EndPeriod
         WITH FRAME lis EDITING:
            READKEY.

            IF keylabel(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"InclUnit,HostTable,KeyValue") > 0
            THEN DO:

               IF FRAME-FIELD = "InclUnit" THEN DO:

                  RUN Help/h-tmscodes(INPUT "Tariff",    /* TableName */
                                       "DataType", /* FieldName */
                                       "Tariff",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     fDispUnit(INTEGER(lcCode)).
                     DISPLAY INTEGER(lcCode) ;& FixedFee.InclUnit 
                     WITH FRAME lis.
                  END.
               END.

               ELSE IF FRAME-FIELD = "HostTable" THEN DO:

                  RUN Help/h-tmscodes(INPUT "FixedFee",    /* TableName */
                                       "HostTable", /* FieldName */
                                       "Contract",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN 
                     DISPLAY lcCode ;& FixedFee.HostTable 
                     WITH FRAME lis.
               END.

               ELSE IF FRAME-FIELD = "keyvalue" THEN DO:
                  ASSIGN INPUT FixedFee.HostTable.
                  IF   FixedFee.HostTable = "customer" THEN RUN Mc/nnasel.
                  ELSE IF FixedFee.HostTable = "mobsub" THEN RUN Help/h-msisdn.

                  IF siirto NE ? THEN ASSIGN fixedfee.keyvalue = siirto.
                  DISP fixedfee.keyvalue with frame lis.
               END.
               

               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.


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

               else if frame-field = "CustNum" THEN DO:
                  ASSIGN FRAME lis CustNum.

                  IF CustNum = 0 THEN UNDO add-new, LEAVE add-new.
                  FIND FIRST Customer where 
                             Customer.Brand   = lcBrand AND
                             Customer.CustNum = CustNum 
                  no-lock no-error.
                  IF NOT AVAIL Customer THEN DO:
                     message "Customer Number does not exist !!".
                     NEXT.
                  END.
                  ASSIGN FixedFee.CustNum   = CustNum
                         FixedFee.VATIncl   = Customer.VATIncl
                         si-recid2          = CustNum.
                  DISP Customer.CustName FixedFee.VATIncl
                  WITH FRAME lis.
               END.   /* CustNum */

               else if frame-field  = "BillCode" THEN DO:
                  ASSIGN FRAME lis BillCode.
                  FIND FIRST BillItem where 
                             BillItem.Brand    = lcBrand AND
                             BillItem.BillCode = BillCode 
                  no-lock no-error.

                  IF NOT AVAIL BillItem THEN DO:
                     message "Billing code '" billcode "' does not exist !!".
                     NEXT.
                  END.
                  FixedFee.BillCode = BillCode.
                  DISP BillItem.BIName.
               END.  /* BillCode */

               ELSE IF FRAME-FIELD = "cli" THEN DO:
                  ASSIGN INPUT FixedFee.CLI.
                  IF FixedFee.Cli ne "" THEN DO:
                     FIND FIRST Mobsub WHERE 
                                Mobsub.CLI = FixedFee.CLI NO-LOCK NO-ERROR.
                     IF NOT AVAIL Mobsub THEN DO:
                        MESSAGE 
                        "This mobile subscription number has not activate" SKIP
                        VIEW-aS ALERT-BOX.
                        NEXT-PROMPT Fixedfee.cli. NEXT.
                     END.   
                     ASSIGN
                        FixedFee.HostTable = "Mobsub"
                        FixedFee.KeyValue  = STRING(mobsub.msseq).
                  END.
                  ELSE ASSIGN 
                       FixedFee.HostTable = "Customer"
                       FixedFee.KeyValue  = string(fixedfee.custnum). 
                             

                  DISP fixedfee.keyvalue fixedfee.hosttable 
                       fixedfee.cli with frame lis.
               END.

               ELSE IF FRAME-FIELD = "InclBillCode" THEN DO:
                   IF INPUT FixedFee.InclBillCode = "" 
                   THEN DO:
                   END.
                   ELSE DO:
                      FIND bBillItem WHERE 
                           bBillItem.Brand    = lcBrand AND
                           bBillItem.BillCode = 
                           INPUT FRAME lis FixedFee.InclBillCode 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL bBillItem THEN DO:
                         BELL.
                         MESSAGE "Unknown Billing Item !".
                         NEXT.
                      END.
                   END.   
               END.

               ELSE IF FRAME-FIELD = "InclUnit" THEN DO:
                  IF NOT fDispUnit(INPUT INPUT FixedFee.InclUnit)
                  THEN DO:
                     BELL.
                     MESSAGE "Unknown unit".
                     NEXT.
                  END.
               END.

               else if frame-field  = "HostTable" THEN DO:
                  IF INPUT FRAME lis FixedFee.HostTable NE "" THEN DO:

                     IF NOT 
                        DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                         "FixedFee",
                                         "HostTable",
                                         INPUT FRAME lis FixedFee.HostTable)
                     THEN DO:
                        BELL.
                        MESSAGE "Unknown host table".
                        NEXT.
                     END.

                     IF INPUT FRAME lis FixedFee.HostTable = "Customer" 
                     THEN DISP string(FixedFee.CustNum) @ FixedFee.KeyValue.
                  END.   
               END.

               else if frame-field  = "keyvalue" THEN DO:

                  CASE INPUT FRAME lis FixedFee.HostTable:
                  WHEN "mobsub" THEN DO:
                     IF NOT CAN-FIND(FIRST MobSub WHERE
                                MobSub.Brand   = lcBrand AND
                                MobSub.CLI     = INPUT FRAME lis 
                                                    FixedFee.KeyValue AND
                                MobSub.CustNum = FixedFee.CustNum)
                     THEN DO:
                        BELL.
                        MESSAGE
                        "Unknown Mobile Subscription"
                        VIEW-AS ALERT-BOX.
                        NEXT.
                     END.
                  END.
                  WHEN "customer" THEN DO:
                     IF NOT CAN-FIND(FIRST Customer WHERE
                             Customer.Brand   = lcBrand AND
                             Customer.Custnum = INT(INPUT FRAME lis 
                                                    FixedFee.KeyValue))
                     THEN DO:
                        BELL.
                        MESSAGE "Unknown Customer".
                        NEXT.
                     END.
                  END.
                  END CASE.

               END.  


               else if frame-field  = "EndPeriod" THEN DO:
                  ASSIGN FRAME lis EndPeriod.
                  IF EndPeriod NE 999999 THEN DO:
                     RUN Syst/uperch(EndPeriod,output rc).
                     IF rc > 0 THEN NEXT.
                  END.
               END.  /* BegPeriod */

               else if frame-field  = "InterVal" THEN DO:
                  ASSIGN FRAME lis FixedFee.Interval.
                  IF FixedFee.Interval = 0 THEN DO:
                     MESSAGE "Must be greater than 0 !".
                     NEXT.
                  END.
               END.  /* Interval */

               ELSE IF frame-field = "BillMethod" OR 
                       frame-field = "Begdate" THEN DO:

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

          /* CREATE now individual items */
          rc = fMakeContract (INPUT FixedFee.FFNum,
                              0).

          message "Totally" rc "individual items created - press ENTER !".
          PAUSE no-message.
          /* THEN we SHOW always ALL Billable items ... */
          RUN Mc/nncobi(FixedFee.FFNum).
          ASSIGN
            memory = recid(FixedFee)
            xrecid = memory.

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

         HIDE FRAME lis no-pause.
         ASSIGN must-print = TRUE.

         /* any records AVAILABLE ? */
         FIND FIRST FixedFee
         WHERE FixedFee.Brand = lcBrand no-lock no-error.
         IF NOT AVAILABLE FixedFee THEN LEAVE LOOP.
         NEXT LOOP.

      END. /* repeat */
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
              IF order = 1 THEN FIND NEXT FixedFee
              WHERE FixedFee.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT FixedFee USE-INDEX BillCode
              WHERE FixedFee.Brand = lcBrand no-lock no-error.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        IF endloop = 0 THEN up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE
               endloop = 0.
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
        ufk[1]= 816 ufk[2]= 927 ufk[3]= 2150 ufk[4]= 609
        ufk[5]= (IF lcRight = "RW" THEN 5   ELSE 0)   
        ufk[6]= (IF lcRight = "RW" THEN 4   ELSE 0) 
        ufk[7]= (IF lcRIght = "RW" THEN 187 ELSE 0)
        ufk[8]= 8 ufk[9]= 1
        ehto  = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW FixedFee.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) FixedFee.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FixedFee.BillCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) FixedFee.BillCode WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

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
           IF order = 1 THEN FIND prev FixedFee
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev FixedFee USE-INDEX BillCode
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
           IF AVAILABLE FixedFee THEN
              ASSIGN firstline = i memory = recid(FixedFee).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND FixedFee where recid(FixedFee) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev FixedFee
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev FixedFee USE-INDEX BillCode
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
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
           IF order = 1 THEN FIND NEXT FixedFee
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT FixedFee USE-INDEX BillCode
           WHERE FixedFee.Brand = lcBrand no-lock no-error.
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
        IF order = 1 THEN FIND prev FixedFee
        WHERE FixedFee.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND prev FixedFee USE-INDEX BillCode
        WHERE FixedFee.Brand = lcBrand no-lock no-error.
        IF AVAILABLE FixedFee THEN DO:
           memory = recid(FixedFee).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev FixedFee
              WHERE FixedFee.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND prev FixedFee USE-INDEX BillCode
              WHERE FixedFee.Brand = lcBrand no-lock no-error.
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
     else if lookup(nap,"f1,1") > 0 THEN repeat WITH FRAME sel:
        ASSIGN 
          ufkey  = TRUE 
          ufk    = 0 
          ehto   = 1
          ufk[1] = 702 
          ufk[2] = 703 
          ufk[3] = 0 
          ufk[4] = 0 
          ufk[5] = 0
          ufk[6] = 0 
          ufk[8] = 8.
        RUN Syst/ufkey.
        nap = keylabel(LASTKEY).
        IF toimi = 8 THEN NEXT BROWSE.

        /* Haku 1 */
        else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           CustNum = 0.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME F1.
           UPDATE lcBrand WHEN gcAllBrand
                  CustNum WITH FRAME f1.
           HIDE FRAME f1 no-pause.

           IF CustNum <> 0 THEN DO:
              FIND FIRST FixedFee where 
                         FixedFee.Brand    = lcBrand AND
                         FixedFee.CustNum >= CustNum
              no-lock no-error.

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
        END. /* Haku sar. 1 */

        /* Haku sarakk. 2 */
        else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           BillCode = "".
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME F2.
           UPDATE lcBrand WHEN gcAllBrand
                  BillCode WITH FRAME f2.
           HIDE FRAME f2 no-pause.

           if BillCode <> "" THEN DO:

              FIND FIRST FixedFee where 
                         FixedFee.Brand     = lcBrand AND
                         FixedFee.BillCode >= BillCode
              USE-INDEX BillCode no-lock no-error.

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
        END. /* Haku sar. 2 */
     END. /* End of find */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN
     DO: /* MEMO */
        FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
        NO-LOCK.
        RUN Mc/memo(INPUT FixedFee.CustNum,
                 INPUT "FixedFee",
                 INPUT STRING(FixedFee.FFNum),
                 INPUT "FixedFee").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"3,f3") > 0 THEN 

     DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:   /* memo */
       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey. RUN Syst/ufcolor.
       FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
       exclusive-lock.

       DISPLAY FixedFee.Memo [1 FOR 5] WITH FRAME memo 1 col.

       /* calculate count of billed/nonbilled items */
       RUN Mc/nnccit(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).


       DISP amt-nonbilled WITH FRAME ch-info.

       PAUSE 0.

       IF lcRight = "RW" THEN DO:

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).

          UPDATE text(FixedFee.Memo [1 FOR 5]) WITH FRAME memo 1 col.
          /* UNDO WITH F4 -key */
          if keylabel(lastkey) = "F4" THEN DO:
             HIDE FRAME memo no-pause.
             UNDO LOOP.
          END.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).

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

       END.
       ELSE PAUSE.



       HIDE FRAME memo no-pause.
       HIDE FRAME ch-info no-pause.
       DISP FixedFee.Memo[1] WITH FRAME sel.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* items */

        FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE] no-lock.
        RUN Mc/nncobi(FixedFee.FFNum).
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
       RUN Mc/nnccit(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).

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

       /* line TO be deleted is hi-lighted */
       COLOR DISPLAY value(ctc)
          FixedFee.CustNum cust-name FixedFee.BillCode FixedFee.Contract 
          FixedFee.EndPeriod FixedFee.Amt FixedFee.BegPeriod.

       IF order = 1 THEN FIND NEXT FixedFee
       WHERE FixedFee.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT FixedFee USE-INDEX BillCode
       WHERE FixedFee.Brand = lcBrand no-lock no-error.
       IF AVAILABLE FixedFee THEN memory = recid(FixedFee).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND FixedFee where recid(FixedFee) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev FixedFee
          WHERE FixedFee.Brand = lcBrand no-lock no-error.
          ELSE IF order = 2 THEN FIND prev FixedFee USE-INDEX BillCode
          WHERE FixedFee.Brand = lcBrand no-lock no-error.
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
          FixedFee.CustNum cust-name FixedFee.BillCode FixedFee.Contract    
          FixedFee.EndPeriod FixedFee.Amt FixedFee.BegPeriod
          FixedFee.Memo[1].  
       IF ok THEN DO:

           FOR EACH FFItem where
                    FFItem.FFNum = FixedFee.FFNum.
               DELETE FFItem.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFixedFee).

           DELETE FixedFee.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST FixedFee
           WHERE FixedFee.Brand = lcBrand) THEN DO:
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
       assign fr-header = " CHANGE " cfc = "lis".  RUN Syst/ufcolor.

       FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
       no-lock.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).

       /* calculate count of billed/nonbilled items */
       RUN Mc/nnccit(FixedFee.FFNum, OUTPUT amt-billed, OUTPUT amt-nonbilled).

       IF amt-nonbilled = 0 THEN DO:

          MESSAGE 
          "There are NONE Unbilled items left - there is nothing" SKIP
           "to change."    SKIP 
           "PRESS ENTER !" 
           VIEW-AS ALERT-BOX TITLE " NOTE ".
           assign fr-header = " VIEW (READ ONLY) ".       
       END.     
       FIND Customer where Customer.CustNum = FixedFee.CustNum   
          no-lock no-error.
       FIND BillItem   where 
            BillItem.Brand    = lcBrand AND
            BillItem.BillCode = FixedFee.BillCode 
          no-lock no-error.

       IF FixedFee.InclBillCode > "" THEN 
          FIND bBillItem WHERE 
               bBillItem.Brand    = lcBrand AND
               bBillItem.BillCode = FixedFee.InclBillCode NO-LOCK NO-ERROR.

       CLEAR FRAME lis NO-PAUSE.

       fDispUnit(FixedFee.InclUnit).

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



       PAUSE 0.
       DISPLAY
          FixedFee.CustNum   @ CustNum
          FixedFee.Contract
          FixedFee.BillCode @ BillCode
          Customer.CustName when AVAIL Customer
          "!! UNKNOWN !!" when NOT AVAIL Customer @ Customer.CustName
          BillItem.BIName when AVAIL BillItem
          "!! UNKNOWN !!" when NOT AVAIL BillItem @ BillItem.BIName
          FixedFee.BegPeriod
           "(a-index:" + FixedFee.CalcObj + ")" @ FixedFee.CalcObj 
          FixedFee.Interval
          FixedFee.BegDate
          FixedFee.EndPeriod
          FixedFee.VATIncl
          FixedFee.Amt
          FixedFee.OwnCost
          FixedFee.BillMethod
          FixedFee.InUse
          FixedFee.HostTable
          FixedFee.KeyValue
          FixedFee.InclAmt
          FixedFee.InclUnit
          FixedFee.InclBillCode
          fixedfee.cli
          lcclitype.


       PAUSE 0.
       DISP amt-nonbilled WITH FRAME ch-info.
       PAUSE 0.

       IF amt-nonbilled NE 0 AND lcRight = "RW" THEN DO:

          ASSIGN 
             ufk = 0 
             ufk[1] = 7 
             ufk[8] = 8 
             ehto = 0 
             ufkey = true.
          RUN Syst/ufkey.

          if toimi = 8 then do:
             hide frame lis no-pause.
             hide frame ch-info no-pause.
             next.
          end.

          ASSIGN ufkey = TRUE ehto = 9.
          RUN Syst/ufkey.

          FIND FixedFee where recid(FixedFee) = rtab[frame-line(sel)]
          exclusive-lock.

          si-recid2 = FixedFee.CustNum.

          UPDATE
          FixedFee.Contract
          FixedFee.VATIncl
          FixedFee.Amt
          FixedFee.OwnCost
          FixedFee.InclAmt
          FixedFee.InclUnit
          FixedFee.InclBillCode
          FixedFee.BillMethod
          FixedFee.InUse
          WITH FRAME lis EDITING:

            READKEY.

            IF keylabel(LASTKEY) = "F9" AND 
               FRAME-FIELD = "InclUnit" 
            THEN DO:

               RUN Help/h-tmscodes(INPUT "Tariff",    /* TableName */
                                    "DataType", /* FieldName */
                                    "Tariff",     /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  fDispUnit(INTEGER(lcCode)).
                  DISPLAY INTEGER(lcCode) ;& FixedFee.InclUnit 
                  WITH FRAME lis.
               END.

               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.


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

               else if frame-field  = "BillCode" THEN 
               DO:
                  ASSIGN FRAME lis BillCode.
                  FIND FIRST BillItem where
                             BillItem.Brand    = lcBrand AND
                             BillItem.BillCode = BillCode
                  no-lock no-error.
                  IF NOT AVAIL BillItem THEN 
                  DO:
                     message "Billing code with this code does not exist !!".
                     NEXT.
                  END.
                  DISP BillItem.BIName.
               END.  /* BillCode */

               
               ELSE IF FRAME-FIELD = "InclBillCode" THEN DO:
                   IF INPUT FixedFee.InclBillCode = "" 
                   THEN DO:
                   END.
                   ELSE DO:
                      FIND bBillItem WHERE 
                           bBillItem.Brand    = lcBrand AND
                           bBillItem.BillCode = 
                           INPUT FRAME lis FixedFee.InclBillCode 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL bBillItem THEN DO:
                         BELL.
                         MESSAGE "Unknown Billing Item !".
                         NEXT.
                      END.
                   END.   
               END.

               ELSE IF FRAME-FIELD = "InclUnit" THEN DO:
                  IF NOT fDispUnit(INPUT INPUT FixedFee.InclUnit)
                  THEN DO:
                     BELL.
                     MESSAGE "Unknown unit".
                     NEXT.
                  END.
               END.

             END.

             APPLY LASTKEY.

          END. /* EDITING */   

          si-recid2 = 0.

          IF FixedFee.Amt  entered OR
             FixedFee.VATIncl entered OR
             FixedFee.OwnCost entered OR
             FixedFee.BillMethod  entered
          THEN DO:

             ok = FALSE.
             rc = 0.
             message "Are You SURE You want to do this change (Y/N) ?" 
             UPDATE ok.
             IF ok THEN DO:

                FOR EACH FFItem of FixedFee WHERE
                         FFItem.Billed = FALSE:
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
                
                message "Non-billed items were changed." SKIP
                        "All billable items will be shown."
                VIEW-AS ALERT-BOX.

                RUN Mc/nncobi(FixedFee.FFNum).

             END.
             ELSE not-done = TRUE.   
          END.
          ELSE not-done = FALSE.
       END.   

       ELSE PAUSE MESSAGE "PRESS ENTER TO CONTINUE !".

       HIDE FRAME lis no-pause.
       HIDE FRAME ch-info no-pause.

       IF not-done THEN UNDO, NEXT LOOP.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).

       DISPLAY  FixedFee.Amt  WITH FRAME sel.
       xrecid = recid(FixedFee).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST FixedFee
       WHERE FixedFee.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST FixedFee USE-INDEX BillCode
       WHERE FixedFee.Brand = lcBrand no-lock no-error.
       ASSIGN memory = recid(FixedFee) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST FixedFee
       WHERE FixedFee.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST FixedFee USE-INDEX BillCode
       WHERE FixedFee.Brand = lcBrand no-lock no-error.

       DO endloop = 1 TO FRAME-DOWN - 1:
          IF order = 1 THEN FIND PREV FixedFee
          WHERE FixedFee.Brand = lcBrand no-lock no-error.
          ELSE IF order = 2 THEN FIND PREV FixedFee USE-INDEX BillCode
          WHERE FixedFee.Brand = lcBrand no-lock no-error.
       END.

       ASSIGN memory = recid(FixedFee) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND Customer where Customer.CustNum = FixedFee.CustNum 
      no-lock no-error.
   IF AVAIL Customer 
   THEN cust-name = CustName.
   else cust-name = "!! UNKNOWN !!!".

   DISPLAY FixedFee.Brand
           FixedFee.CustNum 
           cust-name 
           FixedFee.BillCode 
           FixedFee.Contract
           FixedFee.Amt 
           FixedFee.BegPeriod 
           FixedFee.Memo[1]  
           FixedFee.EndPeriod
   WITH FRAME sel.

END PROCEDURE.
