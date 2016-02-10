/* ----------------------------------------------------------------------
  MODULE .......: BITEMCU.P
  TASK .........: UPDATE Billable Items of One Customer
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 02-09-99
  CHANGED ......: 04.11.99 added column InvNum etc
                  10.01.00 Added BillType
                  14.05.02 tk Event logging added
                  20.05.2002 jpo: hosttable@keyvalue
                  14.10.2002 jr Replaced BillLevel with BillTarget             
                  01.11.2002 jp dont remove billed item     
                  03.03.2003 tk tokens
                  04.03.2003 aam obi -> single fee
                  16.09.2003 jp  Brand 
                  30.09.2003 aam Contract
                  20.10.2003  jp new parameter icmsseq
                  07.01.2004 aam VATIncl, Active
                  15.03.2004 aam show CLI after KeyValue
                  22.03.2004/aam create Contract (fFeeContract)
                  13.04.2004/aam index BillPeriod removed
                  14.06.2005/aam undo creation if billcode = ""
                  01.12.2005/aam hosttable is "customer" not "asiakas"
                  22.12.2005/aam new layout
                  27.01.2006/aam don't go directly into change mode with enter
                  30.08.2006/aam find msowner/mobsub also using invcust
                  28.11.2006/aam get cli from order
  Version ......: M15
  ---------------------------------------------------------------------- */


&GLOBAL-DEFINE BrTable bitem

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'singlefee'}
{Func/timestamp.i}

{Func/fcustpl.i}
{Syst/eventval.i}
{Func/dialog.i}
{Syst/tmsconst.i}
{Func/fuserright.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSingleFee).
   END.

END.

{Func/ffeecont.i}
{Func/fixedfee.i}
{Func/fcharge_comp_loaded.i}

DEF INPUT  PARAMETER  iiCustNum LIKE Customer.CustNum NO-UNDO.
DEF INPUT  PARAMETER  icMsseq LIKE Singlefee.keyvalue  NO-UNDO.

DEF   NEW  shared VAR siirto AS CHAR.
DEF VAR BillPeriod LIKE SingleFee.BillPeriod   NO-UNDO.
DEF VAR lcHostTable  AS CHAR                   NO-UNDO.
DEF VAR lcKeyValue   AS CHAR                   NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR rc           AS INT                    NO-UNDO.
DEF VAR endloop      AS I                      NO-UNDO.
DEF VAR lcCLI        AS CHAR                   NO-UNDO. 
DEF VAR ldtDate      AS DATE                   NO-UNDO. 
DEF VAR llMemo       AS LOG                    NO-UNDO. 
DEF VAR lcTarget     AS CHAR                   NO-UNDO. 
DEF VAR lcCustName   AS CHAR                   NO-UNDO. 
DEF VAR liMsSeq      AS INT                    NO-UNDO. 
DEF VAR lcMenu       AS CHAR                   NO-UNDO  EXTENT 3 FORMAT "X(30)".
DEF VAR llUseCCTool  AS LOG                    NO-UNDO.
DEF VAR llIsAdmin    AS LOG                    NO-UNDO.
DEF VAR ldFItemAmt   AS DEC                    NO-UNDO.
DEF VAR lcFeeModel    AS CHAR                   NO-UNDO.
DEF VAR lcBillItemCode AS CHAR                 NO-UNDO.
DEF VAR lcBillItemName AS CHAR                 NO-UNDO.

/* check admin user rights */
IF getTMSRight("CCSUPER,SYST") = "RW" THEN llIsAdmin = TRUE. ELSE llIsAdmin = FALSE.


DEFINE VARIABLE lcCCPostpaidPriceList AS CHARACTER NO-UNDO.
FIND TMSParam WHERE TMSParam.Brand = "1" AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "FMItemPriceListPostpaid" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN lcCCPostpaidPriceList = TMSParam.CharVal. 

 /* create records in ttable */
FOR EACH FeeModel WHERE FeeModel.Brand = gcBrand AND 
                        FeeModel.FMGroup = 1,
    FIRST FMItem OF FeeModel WHERE FMItem.ToDate >= TODAY AND 
                                   FMItem.FromDate <= TODAY AND
                                   FMItem.PriceList = lcCCPostpaidPriceList NO-LOCK :
       CREATE ttable. 
       ASSIGN ttable.ValueId = FeeModel.FeeModel
              ttable.Description = FeeModel.FeeName.
 END.


form
    lcTarget              COLUMN-LABEL "Target"        FORMAT "X(8)"
    SingleFee.KeyValue    COLUMN-LABEL "TargetID"      FORMAT "X(8)" 
    SingleFee.BillPeriod  COLUMN-LABEL "Period"
    SingleFee.BillCode    COLUMN-LABEL "Billing Item"  FORMAT "X(16)"
    SingleFee.Amt         COLUMN-LABEL "Amount"
    SingleFee.Active      COLUMN-LABEL "Active"
    llMemo                COLUMN-LABEL "M" FORMAT "M/"
    SingleFee.Billed      COLUMN-LABEL "I" FORMAT "I/" 
    SingleFee.InvNum      COLUMN-LABEL "Inv.Nbr"   
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Single Fees: " +  string(iiCustNum) + " " + 
        substr(lcCustName,1,24) + " "
    FRAME sel.

form
    SingleFee.CustNum  
        LABEL "Customer ......."
        lcCustName NO-LABEL FORMAT "X(45)"  SKIP
    
    SingleFee.FMItemID FORMAT ">>>>>>>>9"
        LABEL "Single Fee ID .." SKIP

    SingleFee.HostTable 
        LABEL "Fee Belongs To ."
        FORMAT "X(9)"
        HELP "(M)obile subscription or (O)rder"
    SingleFee.OrderId AT 40 LABEL "Order ID ....." FORMAT "->>>>>>>>9"
        SKIP
 
    SingleFee.KeyValue  
        LABEL "Target ID ......" FORMAT "X(8)" 
      lcCLI AT 40 
        LABEL "MSISDN ......." FORMAT "X(15)" 

    SingleFee.SourceTable
        LABEL "Source Table ..." FORMAT "X(15)"
    SingleFee.SourceKey AT 40
        LABEL "Source Key ..." FORMAT "X(15)"

    "Billing Item ...:" SingleFee.BillCode format "x(16)" NO-LABEL 
                        BillItem.BIName AT 40 NO-LABEL 
                        SKIP
                        
    "Amount .........:" SingleFee.Amt NO-LABEL 
       "Billing Target:" AT 40 SingleFee.BillTarget FORMAT ">>9" NO-LABEL SKIP

    "Period .........:" SingleFee.BillPeriod NO-LABEL 
       "Billing Type .:" AT 40 SingleFee.BillType
            NO-LABEL Format "x(4)" SKIP

    "Covers period(s):" SingleFee.Concerns[1] NO-LABEL format ">>>>>>>>" "-"
                        SingleFee.Concerns[2] NO-LABEL format ">>>>>>>>" 
        "Active .......:" AT 40 SingleFee.Active NO-LABEL SKIP
   
    "Contract .......:" SingleFee.Contract NO-LABEL
    SingleFee.VatIncl AT 40 
          LABEL "VAT .........." SKIP

    SingleFee.CalcObj AT 40 
       LABEL "Calc.Object .." FORMAT "x(21)" SKIP
     
       SingleFee.InvNum  AT 40 
          LABEL "Billed on Inv." 
          FORMAT ">>>>>>>>9"  
       SKIP
  
    "Info To Invoice.:" SingleFee.Memo[1]  NO-LABEL SKIP
                        SingleFee.Memo[2]  NO-LABEL AT 19
                        SingleFee.Memo[3]  NO-LABEL AT 19
                        SingleFee.Memo[4]  NO-LABEL AT 19
                        SingleFee.Memo[5]  NO-LABEL AT 19

WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


/* for for charge and compensation */
form
    iiCustNum  LABEL "Customer ......."  lcCustName NO-LABEL FORMAT "X(45)"  SKIP

    "Fee Belongs To MobSub (SubscriptionID)" SKIP
    
    "Billing Item ...:" lcBillItemCode format "x(16)" NO-LABEL
                        lcBillItemName AT 40 format "x(30)" NO-LABEL 
                        SKIP
                        
    "Amount .........:" ldFItemAmt NO-LABEL SKIP
  /*  "Period .........:" BillPeriod NO-LABEL SKIP */
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis-cctools.


form /* seek Billable item  BY  keyvalue */
    "Target:" lcHostTable 
       HELP "Enter target, (S)ubsc.ID / (C)ustomer"
       FORMAT "X(15)" SKIP
    "ID ...:" lcKeyValue  
       HELP "Enter target ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Target "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billable item  BY BillPeriod */
    BillPeriod
    HELP "Enter Billing Period"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Period "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fValidateMsSeq RETURNS LOGIC
   (iiMsSeq AS INT):

   IF CAN-FIND(FIRST MobSub WHERE
                     MobSub.MsSeq   = iiMsSeq AND
                     MobSub.InvCust = iiCustNum)
   THEN RETURN TRUE.
   
   IF CAN-FIND(FIRST MsOwner WHERE
                     MsOwner.MsSeq   = iiMsSeq AND
                     MsOwner.InvCust = iiCustNum)
   THEN RETURN TRUE.
   
   RETURN FALSE.
   
END FUNCTION.

FIND Customer WHERE Customer.CustNum  = iiCustNum  NO-LOCK NO-ERROR.
lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first. 

IF AVAILABLE SingleFee THEN ASSIGN
   memory       = recid(SingleFee)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billable items available !" VIEW-AS ALERT-BOX.
      HIDE FRAME sel NO-PAUSE.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a SingleFee  */
     
     IF icMsSeq > "" THEN DO: /* cc is possible only in for mobsub fee  */
      CHOISES:
      DO WHILE TRUE:
   
         ASSIGN
            ufk       = 0
            ufk[8]    = 8
            ehto      = 3.
      
         RUN Syst/ufkey. 

         DISPLAY
            " A) Charges and Compensations  " @ lcMenu[1]  SKIP
            " X) Manual                     " @ lcMenu[2]  SKIP
         WITH OVERLAY FRAME choices NO-LABELS.
        
         CHOOSE FIELD lcMenu AUTO-RETURN go-on (F8) WITH FRAME choices
         TITLE " Choose single fee type " CENTERED WITH COL 2 ROW 8.

         HIDE FRAME choices.

         IF  FRAME-INDEX = 1 THEN ASSIGN
            llUseCCTool = TRUE. 
         ELSE IF FRAME-INDEX = 2 THEN llUseCCTool = FALSE.

         IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN llUseCCTool = ?.

         IF llUseCCTool = ? THEN DO:
            
            ASSIGN
               must-add   = FALSE
               must-print = TRUE
               ufkey      = TRUE.
            NEXT LOOP.
         END.
         
         LEAVE CHOISES.
         
      END. /* end CHOISE */

     END. /* end if */
     ELSE llUseCCTool = FALSE.  
      
     ASSIGN
        cfc      = "lis"
        ufkey    = true
        ac-hdr   = " ADD "
        must-add = FALSE
        must-print = TRUE.

      RUN Syst/ufcolor.

      IF llUseCCTool THEN DO :
           /* using predefined charges and compensations */
           RUN CCTOOL.

           llUseCCTool = ? .
      END.
      ELSE DO:    
           /* Individual single fee*/ 
           ADD-ROW:
           REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
           PAUSE 0 NO-MESSAGE.
           ehto = 9. RUN Syst/ufkey.
        
           REPEAT TRANSACTION WITH FRAME lis:
              PAUSE 0.
              CLEAR FRAME lis NO-PAUSE.

              on CREATE of SingleFee override DO:
                 ASSIGN SingleFee.FMItemId = next-value(bi-seq).
              END.

              CREATE SingleFee.
              ASSIGN
              SingleFee.Brand      = gcBrand 
              SingleFee.CustNum    = iiCustNum
              SingleFee.VATIncl    = Customer.VATIncl
              SingleFee.BillPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
              SingleFee.HostTable  = "MobSub".

              RUN local-update-record.

              IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
                 SingleFee.BillCode = "" 
              THEN UNDO add-row, LEAVE add-row.

              /* create contract */ 
              IF SingleFee.Contract = "" THEN DO:
                  ldtDate = fInt2Date(SingleFee.BillPeriod,1).
               
                  SingleFee.Contract = fFeeContract(SingleFee.Brand,  
                                                 SingleFee.CustNum,
                                                 "",
                                                 ldtDate,
                                                 "SingleFee").
              END.
                                                             
              IF llDoEvent THEN
                 RUN StarEventMakeCreateEventWithMemo(lhSingleFee,
                                                      katun,
                                                      "ManualCUI").

              ASSIGN
              memory = recid(SingleFee)
              xrecid = memory.

              FIND CURRENT SingleFee NO-LOCK NO-ERROR.
              LEAVE ADD-ROW.
           END.

         END.  /* ADD-ROW */
           
         HIDE FRAME lis NO-PAUSE.
         
      END.
     
       ASSIGN must-print = TRUE.
      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE SingleFee THEN LEAVE LOOP.
      memory = recid(SingleFee).
      xrecid = memory.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SingleFee WHERE recid(SingleFee) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SingleFee THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SingleFee).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        IF endloop = 0 THEN up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE
               endloop = 0.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 183 ufk[2]= 771 ufk[3]= 0 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF icMsSeq > "" THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0.
           
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SingleFee.KeyValue ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SingleFee.KeyValue WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SingleFee.BillPeriod ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SingleFee.BillPeriod WITH FRAME sel.
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

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND SingleFee WHERE recid(SingleFee) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE SingleFee THEN
              ASSIGN FIRSTrow = i memory = recid(SingleFee).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE SingleFee THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(SingleFee)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE SingleFee THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(SingleFee).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND SingleFee WHERE recid(SingleFee) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE SingleFee THEN DO:
           memory = recid(SingleFee).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE SingleFee THEN memory = recid(SingleFee).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND SingleFee WHERE recid(SingleFee) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       
       CLEAR  FRAME f1.
       
       UPDATE lcHostTable 
              lcKeyValue WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       DO:
          
          FIND FIRST SingleFee USE-INDEX CustNum WHERE 
                     SingleFee.Brand     = gcBrand     AND 
                     SingleFee.CustNum   = iiCustNum     AND
                     SingleFee.HostTable = lcHostTable AND
                     SingleFee.KeyValue  = lcKeyValue 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE SingleFee THEN 
          FIND FIRST SingleFee USE-INDEX CustNum WHERE 
                     SingleFee.Brand     = gcBrand     AND 
                     SingleFee.CustNum   = iiCustNum     AND
                     SingleFee.HostTable = lcHostTable AND
                     SingleFee.KeyValue >= lcKeyValue 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE SingleFee THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some bitem/as-nro was found */
          ASSIGN order = 1 memory = recid(SingleFee) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET BillPeriod WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF BillPeriod ENTERED THEN DO:
          FIND FIRST SingleFee WHERE 
                     SingleFee.CustNum     = iiCustNum AND
                     SingleFee.BillPeriod >= BillPeriod 
          USE-INDEX CustNum_s NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SingleFee THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some bitem/bi-period was found */
          ASSIGN order = 2 memory = recid(SingleFee) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN 
     DO: /* MEMO */
        RUN local-find-this(false).
        RUN Mc/memo(INPUT SingleFee.Custnum,
                 INPUT "SingleFee",
                 INPUT STRING(FMItemId),
                 INPUT "SingleFee").
        ufkey = TRUE. 
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF SingleFee.invNum   ne 0 AND
          SingleFee.billed   = TRUE 
       THEN DO:
          MESSAGE
          "Single fee has already been billed. Deletion not allowed"
          VIEW-AS ALERT-BOX TITLE "NOT ALLOWED".
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       SingleFee.BillCode
       SingleFee.BillPeriod
       SingleFee.Amt
       SingleFee.KeyValue
       SingleFee.Billed.

       RUN local-find-NEXT.
       IF AVAILABLE SingleFee THEN memory = recid(SingleFee).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE SingleFee THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(SingleFee).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       SingleFee.BillCode
       SingleFee.BillPeriod
       SingleFee.Amt
       SingleFee.KeyValue
       SingleFee.Billed.

       IF ok THEN DO:

           IF llDoEvent THEN
              RUN StarEventMakeDeleteEventWithMemo (lhSingleFee,
                                                    katun,
                                                    "ManualCUI").

           DELETE SingleFee.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SingleFee
           WHERE SingleFee.CustNum = iiCustNum AND singlefee.Brand = gcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY SingleFee.CustNum.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN
          RUN StarEventMakeModifyEventWithMemo(lhSingleFee,
                                               katun,
                                               "ManualCUI").

       RUN local-disp-row.
       xrecid = recid(SingleFee).
       FIND CURRENT SingleFee NO-LOCK NO-ERROR.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(SingleFee) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        DO endloop = 1 to FRAME-DOWN - 1.
           RUN local-find-prev.
        END.
        ASSIGN memory = recid(SingleFee) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND SingleFee WHERE recid(SingleFee) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND SingleFee WHERE recid(SingleFee) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

     IF icMsseq > "" THEN DO:
        FIND FIRST SingleFee USE-INDEX CustNum WHERE 
                   SingleFee.CustNum   = iiCustNum  AND 
                   SingleFee.Brand     = gcBrand  AND 
                   SingleFee.HostTable = "Mobsub" AND
                   SingleFee.KeyValue  = icMsseq
        NO-LOCK NO-ERROR.
     END.

     ELSE DO. 
       IF order = 1 THEN 
       FIND FIRST SingleFee USE-INDEX CustNum WHERE 
                  SingleFee.CustNum = iiCustNum AND 
                  SingleFee.Brand   = gcBrand
       NO-LOCK NO-ERROR.
             
       ELSE IF order = 2 THEN 
       FIND FIRST SingleFee USE-INDEX CustNum_s WHERE 
                  SingleFee.CustNum = iiCustNum 
       NO-LOCK NO-ERROR.
    END.   
END PROCEDURE.

PROCEDURE local-find-LAST:
     IF icMsseq > "" THEN DO:
        FIND LAST SingleFee USE-INDEX CustNum WHERE 
                  SingleFee.CustNum   = iiCustNum  AND 
                  SingleFee.Brand     = gcBrand  AND 
                  SingleFee.HostTable = "Mobsub" AND
                  SingleFee.KeyValue  = icMsseq
        NO-LOCK NO-ERROR.
     END.

     ELSE DO. 
       IF order = 1 THEN 
       FIND LAST SingleFee USE-INDEX CustNum WHERE 
                 SingleFee.CustNum = iiCustNum AND
                 SingleFee.Brand = gcBrand 
       NO-LOCK NO-ERROR.
             
       ELSE IF order = 2 THEN 
       FIND LAST SingleFee USE-INDEX CustNum_s WHERE 
                 SingleFee.CustNum = iiCustNum 
       NO-LOCK NO-ERROR.
    END.
    
END PROCEDURE.

PROCEDURE local-find-NEXT:
     IF icMsseq > "" THEN DO:
        FIND NEXT SingleFee USE-INDEX CustNum WHERE 
                  SingleFee.CustNum   = iiCustNum  AND 
                  SingleFee.Brand     = gcBrand  AND 
                  SingleFee.HostTable = "Mobsub" AND
                  SingleFee.KeyValue  = icMsseq
        NO-LOCK NO-ERROR.
     END.

     ELSE DO. 
       IF order = 1 THEN 
       FIND NEXT SingleFee USE-INDEX CustNum WHERE 
                 SingleFee.CustNum = iiCustNum AND 
                 SingleFee.Brand   = gcBrand
       NO-LOCK NO-ERROR.
             
       ELSE IF order = 2 THEN 
       FIND NEXT SingleFee USE-INDEX CustNum_s WHERE 
                 SingleFee.CustNum = iiCustNum 
       NO-LOCK NO-ERROR.
    END.
    
END PROCEDURE.

PROCEDURE local-find-PREV:
     IF icMsseq > "" THEN DO:
        FIND PREV SingleFee USE-INDEX CustNum WHERE 
                  SingleFee.CustNum   = iiCustNum  AND 
                  SingleFee.Brand     = gcBrand  AND 
                  SingleFee.HostTable = "Mobsub" AND
                  SingleFee.KeyValue  = icMsseq
        NO-LOCK NO-ERROR.
     END.

     ELSE DO. 
       IF order = 1 THEN 
       FIND PREV SingleFee USE-INDEX CustNum WHERE 
                 SingleFee.CustNum = iiCustNum AND 
                 SingleFee.Brand   = gcBrand
       NO-LOCK NO-ERROR.
             
       ELSE IF order = 2 THEN 
       FIND PREV SingleFee USE-INDEX CustNum_s WHERE 
                 SingleFee.CustNum = iiCustNum
       NO-LOCK NO-ERROR.
    END.
    
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE. 

       DISPLAY
       SingleFee.BillPeriod
       lcTarget
       SingleFee.KeyValue
       SingleFee.BillCode
       SingleFee.Amt
       SingleFee.Active
       llMemo
       SingleFee.Billed
       SingleFee.InvNum       
       WITH FRAME sel. 

END PROCEDURE.

PROCEDURE local-find-others.

   llMemo = CAN-FIND(FIRST Memo WHERE
                           Memo.Brand     = gcBrand AND
                           Memo.HostTable = "SingleFee" AND
                           Memo.KeyValue  = STRING(SingleFee.FMItemId)).
       
   CASE SingleFee.HostTable:
   WHEN "MobSub" THEN lcTarget = "Subsc.ID".
   OTHERWISE lcTarget = SingleFee.HostTable.
   END CASE. 
   
END PROCEDURE.

PROCEDURE local-update-record:

   UPDATE_LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      FIND BillTarget WHERE BillTarget.BillTarget = SingleFee.BillTarget AND
                            BillTarget.CustNum    = SingleFee.CustNum   
      NO-LOCK NO-ERROR.

      FIND BillItem   WHERE 
           BillItem.Brand       = SingleFee.Brand AND
           BillItem.BillCode    = SingleFee.BillCode    NO-LOCK NO-ERROR.
      lcCLI  = "".

      IF SingleFee.HostTable = "mobsub" THEN DO:
         FOR FIRST MsOwner NO-LOCK WHERE
                   MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue) AND
                   MsOwner.InvCust = SingleFee.CustNum:
            lcCLI = MsOwner.CLI.       
         END.
         IF lcCLI = "" THEN 
         FOR FIRST MsOwner NO-LOCK WHERE
                   MsOwner.CustNum = SingleFee.CustNum AND
                   MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue):
            lcCLI = MsOwner.CLI.       
         END.
      END.
      ELSE IF SingleFee.HostTable = "Order" THEN DO:
         FIND Order WHERE
              Order.Brand   = gcBrand AND
              Order.OrderID = INTEGER(SingleFee.KeyValue) NO-LOCK NO-ERROR.
         IF AVAILABLE Order THEN lcCLI = Order.CLI.
      END.
       
      DISP
         SingleFee.CustNum
         lcCustName
         SingleFee.Contract
         BillItem.BIName WHEN AVAIL BillItem
         SingleFee.InvNum
         lcCLI
         SingleFee.HostTable
         SingleFee.FMItemID
         SingleFee.OrderId
         SingleFee.KeyValue
         SingleFee.BillTarget      
         SingleFee.CalcObj      
         SingleFee.BillPeriod    
         SingleFee.BillCode       
         SingleFee.VATIncl
         SingleFee.Amt
         SingleFee.BillType
         SingleFee.Concerns
         SingleFee.Memo 
         SingleFee.Active
         SingleFee.SourceTable
         SingleFee.SourceKey
      WITH FRAME lis.

      IF NOT NEW SingleFee THEN DO:
         ASSIGN ehto   = 0
                ufk    = 0            
                ufk[1] = 7 WHEN lcRight = "RW" AND 
                               (NOT SingleFee.Billed OR NEW SingleFee)
                ufk[8] = 8.
             
         RUN Syst/ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 AND lcRight = "RW" THEN DO:

         ehto = 9.
         RUN Syst/ufkey.
         
         si-recid2 = SingleFee.CustNum.
         lcCLI = "".
         
         PROMPT
            SingleFee.HostTable   WHEN NEW SingleFee
            SingleFee.keyvalue    WHEN NEW singlefee
            SingleFee.BillCode    WHEN NOT SingleFee.Billed
            SingleFee.Amt         WHEN NOT SingleFee.Billed
            SingleFee.BillPeriod  WHEN NOT SingleFee.Billed
            SingleFee.Concerns    WHEN NOT SingleFee.Billed
            SingleFee.Contract    WHEN NOT SingleFee.Billed
            SingleFee.BillTarget  WHEN NOT SingleFee.Billed AND
                                       NOT NEW SingleFee
            SingleFee.BillType    WHEN NOT SingleFee.Billed  AND
                                       NOT NEW SingleFee
            SingleFee.Active      WHEN NOT SingleFee.Billed  AND
                                       NOT NEW SingleFee
            SingleFee.VATIncl     WHEN NOT SingleFee.Billed  AND
                                       NOT NEW SingleFee
            SingleFee.CalcObj     WHEN NOT SingleFee.Billed  AND
                                       NOT NEW SingleFee
            SingleFee.Memo        WHEN NOT SingleFee.Billed
         WITH FRAME lis EDITING:
            READKEY.

            IF FRAME-FIELD = "keyvalue" and keylabel(Lastkey) = "F9"
            THEN DO:
                IF INPUT SingleFee.HostTable = "Customer" THEN RUN Mc/nnasel.
                ELSE DO:
                   MESSAGE "No help available." SKIP
                           "Check ID from subscription or order."
                   VIEW-AS ALERT-BOX INFORMATION.
                   NEXT.
                END.                       

                IF siirto NE ? THEN DISP siirto @ singlefee.keyvalue.
             END.

             IF FRAME-FIELD = "BillTarget" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
                RUN Help/h-billtarg(INPUT INPUT FRAME lis SingleFee.CustNum).
                IF siirto NE ? THEN DO:
                   ASSIGN SingleFee.BillTarget = INT(siirto).
                   DISP SingleFee.BillTarget WITH FRAME lis.
                   ASSIGN ehto = 9 ufkey = TRUE.
                   RUN Syst/ufkey.p.
                   NEXT.
                END.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                if frame-field = "contract" then do:
                   if input frame lis SingleFee.Contract ne "" then do:
                      find contract where 
                          Contract.Brand    = SingleFee.Brand AND
                          contract.contract = input frame lis SingleFee.contract
                      no-lock no-error.
                      if not available contract then do:
                         message "Unknown contract"
                         view-as alert-box.
                         next.
                      end.
                   end.
                end.

                ELSE IF FRAME-FIELD = "BillType" THEN DO:
                   IF INPUT FRAME lis SingleFee.BillType > "" THEN DO: 
                      FIND BillType WHERE 
                           BillType.BillType =
                              INPUT FRAME lis SingleFee.BillType 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL BillType THEN DO:
                         BELL.
                         MESSAGE "Unknown Billing Type !".
                         NEXT.
                      END.   
                   END.

                END.

                IF FRAME-FIELD = "BillTarget" THEN DO:
                   IF INPUT SingleFee.BillTarget > 0 THEN DO:
                      FIND BillTarget WHERE 
                           BillTarget.BillTarget = INPUT SingleFee.BillTarget  
                      AND  BillTarget.CustNum    = INPUT SingleFee.CustNum
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL BillTarget THEN DO:
                         MESSAGE "Unknown Invoicing target"
                         VIEW-AS ALERT-BOX ERROR.
                      END.
                   END.
                END.

                ELSE IF FRAME-FIELD = "BillCode" THEN DO:
                   IF INPUT FRAME  lis SingleFee.BillCode = "" THEN DO:
                      NEXT-PROMPT SingleFee.BillPeriod.
                      NEXT.
                   END.   

                   FIND BillItem WHERE 
                        BillItem.Brand    = SingleFee.Brand AND 
                        BillItem.BillCode =
                   INPUT FRAME lis SingleFee.BillCode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown BillCode !".
                      NEXT.
                   END.
                   
                   DISP BillItem.BiName
                        BillItem.BillCode @ SingleFee.BillCode.
                END.

                ELSE IF FRAME-FIELD  = "HostTable" THEN DO:
                   IF LOOKUP(INPUT SingleFee.HostTable,"Order,MobSub") = 0
                   THEN DO:
                      MESSAGE "Fee should be targeted to subscription or"
                              "order."
                      VIEW-AS ALERT-BOX INFORMATION.
                      NEXT.
                   END.
                   IF INPUT SingleFee.HostTable = "Customer" THEN DO:
                      DISP string(SingleFee.CustNum) @ SingleFee.KeyValue.
                   END.

                END. 
                
                ELSE IF FRAME-FIELD  = "keyvalue" THEN DO:
                       
                   IF INPUT FRAME lis keyvalue = "" THEN DO:
                      NEXT-PROMPT SingleFee.HostTable.
                      NEXT.
                   END.
                  
                   CASE INPUT SingleFee.HostTable:
                   WHEN "MobSub" THEN DO:
                  
                      liMsSeq = INT(INPUT FRAME lis KeyValue) NO-ERROR.
                      IF ERROR-STATUS:ERROR THEN DO:
                         MESSAGE "Invalid subscription ID"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                     
                      IF NOT fValidateMsSeq(liMsSeq) THEN DO:
                         MESSAGE
                         "Invalid subscription ID"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                    
                      FOR FIRST MsOwner NO-LOCK WHERE
                                MsOwner.MsSeq   = liMsSeq AND
                                MsOwner.InvCust = iiCustNum:
                         lcCLI = MsOwner.CLI.
                         
                         IF INPUT FRAME lis SingleFee.Contract = "" THEN
                            DISPLAY MsOwner.Contract @ SingleFee.Contract
                               WITH FRAME lis.
                         IF INPUT FRAME lis SingleFee.BillTarget = 0 THEN
                            DISPLAY MsOwner.BillTarget @ SingleFee.BillTarget
                               WITH FRAME lis.
                      END.
                   END.

                   WHEN "Customer" THEN DO:
                      IF INPUT FRAME lis SingleFee.KeyValue NE 
                         STRING(SingleFee.CustNum) THEN DO:
                         BELL.
                         MESSAGE "Invalid customer number".
                         NEXT.
                      END.
                     
                      FIND FIRST BillTarget WHERE 
                                 BillTarget.CustNum = SingleFee.CustNum
                      NO-LOCK NO-ERROR.
                      IF AVAILABLE BillTarget THEN DO:
                         SingleFee.BillTarget = BillTarget.BillTarget.
                         DISPLAY SingleFee.BillTarget WITH FRAME lis.
                      END.    
                   END.
                   
                   WHEN "Order" THEN DO:
                      FIND FIRST Order WHERE
                                 Order.Brand = gcBrand AND
                                 Order.OrderID = INT(INPUT FRAME lis
                                                     SingleFee.KeyValue)
                      NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE Order OR Order.CustNum NE iiCustNum
                      THEN DO:
                         MESSAGE "Invalid order ID"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                      lcCLI = Order.CLI.
                   END.
                   
                   END CASE.
                   
                   DISPLAY lcCLI WITH FRAME lis.
                END. /* KeyValue */
                           
                ELSE IF FRAME-FIELD = "period" THEN DO:
                   RUN Syst/uperch(INPUT FRAME lis SingleFee.BillPeriod,OUTPUT rc).
                   IF rc NE 0 THEN NEXT.
                   IF INPUT FRAME lis SingleFee.Concerns[1] = 0 THEN DISP
                      INPUT FRAME lis SingleFee.BillPeriod @ 
                           SingleFee.Concerns[1]
                      INPUT FRAME lis SingleFee.BillPeriod @ 
                           SingleFee.Concerns[2]
                   WITH FRAME lis.
                END.   

             END.
             
             ELSE IF FRAME-FIELD = "HostTable" THEN DO WITH FRAME lis:
                CASE KEYLABEL(LASTKEY):
                WHEN "M" THEN DISPLAY "MobSub" @ SingleFee.HostTable.
                WHEN "O" THEN DISPLAY "Order" @ SingleFee.HostTable.
                END CASE. 
                NEXT. 
             END.
             
             APPLY LASTKEY.
             
          END. /* EDITING */

          IF NOT NEW singlefee THEN
             FIND CURRENT SingleFee NO-LOCK. 
          
          IF CURRENT-CHANGED SingleFee THEN DO:
             
             MESSAGE ({&MSG_RECORD_CHANGED})
             VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
       
             UNDO, NEXT UPDATE_LOOP.

          END. 
          ELSE DO:
             
            IF NOT NEW SingleFee THEN
               FIND CURRENT SingleFee EXCLUSIVE-LOCK. 

            ASSIGN
               SingleFee.HostTable = INPUT FRAME lis SingleFee.HostTable
                  WHEN NEW SingleFee
               SingleFee.keyvalue = INPUT FRAME lis SingleFee.keyvalue
                  WHEN NEW SingleFee
               SingleFee.BillCode    WHEN NOT SingleFee.Billed
               SingleFee.Amt         WHEN NOT SingleFee.Billed
               SingleFee.BillPeriod  WHEN NOT SingleFee.Billed
               SingleFee.Concerns    WHEN NOT SingleFee.Billed
               SingleFee.Contract    WHEN NOT SingleFee.Billed
               SingleFee.BillTarget  WHEN NOT SingleFee.Billed AND
                                          NOT NEW SingleFee
               SingleFee.BillType    WHEN NOT SingleFee.Billed  AND
                                          NOT NEW SingleFee
               SingleFee.Active      WHEN NOT SingleFee.Billed  AND
                                          NOT NEW SingleFee
               SingleFee.VATIncl     WHEN NOT SingleFee.Billed  AND
                                          NOT NEW SingleFee
               SingleFee.CalcObj     WHEN NOT SingleFee.Billed  AND
                                          NOT NEW SingleFee
               SingleFee.Memo        WHEN NOT SingleFee.Billed.
            IF NOT NEW SingleFee THEN FIND CURRENT SingleFee NO-LOCK. 

          END.

          si-recid2 = 0.
          
      END.
      
      LEAVE.
   END.
END PROCEDURE.


PROCEDURE cctool:
      
       /*  */
      DEFINE VARIABLE lcPriceList AS CHARACTER NO-UNDO.
      DEFINE VARIABLE llChanged AS LOG  INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE llOk      AS LOG       NO-UNDO.
      CHOISE-CC:
      REPEAT:
   
         ASSIGN
            ufk       = 0
            ufk[8]    = 8
            ehto      = 3.
      
         RUN Syst/ufkey . 

          /* dialog to fetch the charge/compensation  amount using 
            ServFee, FeeModel and FMItem  */
       
          DEFINE VARIABLE lctitle AS CHARACTER INITIAL "Charge and Compensation Type " NO-UNDO.
          DEFINE VARIABLE lrecid AS RECID NO-UNDO.
          DEFINE VARIABLE loutValueId AS CHARACTER NO-UNDO. 


          RUN Help/h-dialog.p (INPUT TABLE ttable BY-REFERENCE ,
                          INPUT lctitle,
                          OUTPUT lrecid,
                          OUTPUT loutValueId).
          
          FIND ttable WHERE RECID(ttable) = lrecid NO-LOCK NO-ERROR.

          IF NOT AVAIL ttable THEN DO:
             ASSIGN
                must-add   = FALSE
                must-print = TRUE
                ufkey      = TRUE.
             LEAVE CHOISE-CC.
           END.
         FIND FeeModel WHERE FeeModel.Brand = gcBrand AND 
                             FeeModel.FeeModel  = ttable.ValueId NO-LOCK NO-ERROR.
         
         /* First catch mobsub  */   
         FIND  MobSub WHERE MobSub.MsSeq = INT(icMsSeq) NO-LOCK NO-ERROR.                
         IF NOT AVAIL MobSub  THEN DO:
             MESSAGE "Invalid subscription ID"
             VIEW-AS ALERT-BOX ERROR.
             NEXT.
         END.

         FIND BillTarget NO-LOCK WHERE
              BillTarget.CustNum    = MobSub.CustNum AND
              BillTarget.BillTarget = MobSub.BillTarget NO-ERROR.
       
         IF NOT AVAIL BillTarget  THEN DO:
             MESSAGE "Bill Target not available for this subscription ID " 
             VIEW-AS ALERT-BOX ERROR.
             NEXT.
         END.


         /* Fetch default charge */ 
        lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                         MobSub.BillTarget,
                                         FeeModel.FeeModel,
                                         TODAY).

        FIND FIRST FMItem NO-LOCK  WHERE
                   FMItem.Brand     = gcBrand       AND
                   FMItem.FeeModel  = FeeModel.FeeModel AND
                   FMItem.PriceList = lcPriceList AND
                   FMItem.FromDate <= TODAY     AND
                   FMItem.ToDate   >= TODAY NO-ERROR.
   
        IF AVAIL FMItem THEN DO:
             ASSIGN ldFItemAmt = FMItem.Amount
                    lcFeeModel = FeeModel.FeeModel
                    lcBillItemCode = FMItem.BillCode.

             FIND BillItem   WHERE 
                  BillItem.Brand       = gcBrand AND
                  BillItem.BillCode    = FMItem.BillCode    NO-LOCK NO-ERROR.
             IF AVAIL BillItem THEN lcBillItemName = BillItem.BIName. 
        END.
        ELSE DO:
          MESSAGE "Not Bill Item was found for pricelist " lcPriceList  
          VIEW-AS ALERT-BOX ERROR.
          NEXT.
        END.


        DEFINE VARIABLE llNegative AS LOGICAL NO-UNDO.
        DEFINE VARIABLE ldUpLimit AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ldUpMonthLimit AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ldeLoaded AS DECIMAL NO-UNDO.
        DEFINE VARIABLE lcSource AS CHAR NO-UNDO.
        /* check uplimit value */
        DEFINE VARIABLE liOneTimeLimitType AS INTEGER NO-UNDO.
        DEFINE VARIABLE liMonthlyLimitType AS INTEGER NO-UNDO. 
        IF  ldFItemAmt > 0 THEN DO:
            liOneTimeLimitType = {&POST_CHARGE_LIMIT_TYPE}.
            liMonthlyLimitType = {&POST_CHARGE_MONTHLY_LIMIT_TYPE}.
            llNegative = FALSE.

        END.
        ELSE DO:
            liOneTimeLimitType =  {&POST_COMP_LIMIT_TYPE}.
            liMonthlyLimitType = {&POST_COMP_MONTHLY_LIMIT_TYPE}.
            llNegative = TRUE.
        END.
        ldUpLimit = fUserLimitAmt(katun,liOneTimeLimitType).
        ldUpMonthLimit = fUserLimitAmt(katun, liMonthlyLimitType).

        IF ldUpLimit < 0 OR ldUpMonthLimit < 0  THEN DO:
           MESSAGE "One time / monthly limit is not defined in your account "
          VIEW-AS ALERT-BOX ERROR.
          LEAVE CHOISE-CC.
        END.


        BillPeriod =  YEAR(TODAY) * 100 + MONTH(TODAY).
        
        ldeLoaded =  fMonthLoaded( (IF llNegative THEN "COMP" ELSE "CHARGE" ),
                                   MobSub.CLI,
                                   FALSE).


          /* now wee going to create the charge and compensation request */
          ADD-CC: 
          REPEAT WITH FRAME lis-cctools ON ENDKEY UNDO ADD-CC, LEAVE ADD-CC:
     
           PAUSE 0 NO-MESSAGE.
           DISP
           iiCustNum 
           lcCustName
           lcBillItemCode
           lcBillItemName
           ldFItemAmt
           WITH FRAME lis-cctools.
        
           ASSIGN 
            ufk    = 0
            ufk[8] = 8
            ehto   = 0. 

           /* accept / cancel */
           IF llChanged THEN ASSIGN
              ufk[5] = 1089
              ufk[8] = 1059.

           IF NOT llChanged THEN ASSIGN 
               toimi    = 1.
           ELSE RUN Syst/ufkey. 
                          
           IF toimi = 1 AND lcRight = "RW" THEN DO:

               ehto = 9.
               RUN Syst/ufkey.
          
              REPEAT TRANSACTION WITH FRAME lis-cctools ON ENDKEY UNDO, LEAVE:
            
              
              UPDATE
                 ldFItemAmt WHEN  llIsAdmin 
              WITH FRAME lis-cctools EDITING:
 
              READKEY.
    
              nap = keylabel(lastkey).
     
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:

                  PAUSE 0. 
                  
                  IF FRAME-FIELD = "ldFItemAmt" THEN DO:
                     
                     IF INPUT FRAME lis-cctools ldFItemAmt = 0 
                     THEN DO:
                        MESSAGE "Value can not be cero !" 
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                     IF llNegative AND INPUT FRAME lis-cctools ldFItemAmt > 0 
                     THEN DO:
                        MESSAGE "Give only negative values!"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                     ELSE IF NOT llNegative AND INPUT FRAME lis-cctools ldFItemAmt < 0 
                     THEN DO:
                        MESSAGE "Give only positive values!"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.   
                     /* user charge and compensation  limit */
                     ELSE IF ABSOLUTE( INPUT ldFItemAmt) > ldUpLimit THEN DO:
                            MESSAGE "The absolute value should be less than " ldUpLimit " euros!"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     ELSE IF ABSOLUTE(ldeLoaded + INPUT ldFItemAmt) > ldUpMonthLimit  THEN DO:
                          MESSAGE
                          "Requested amount is over monthly limit!" SKIP
                          "The absolute value should be less than " ldUpMonthLimit - ABSOLUTE(ldeLoaded) "euros."
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                  END.
              END.

              APPLY LASTKEY.     

           END. /* end editing */

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0  
           THEN UNDO ADD-CC, LEAVE ADD-CC.
           
           llChanged = TRUE.
           LEAVE. 

        END. /* repeat */
       
        IF KEYLABEL(LASTKEY) = "F4" OR 
            LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN LEAVE.

        NEXT.
      END. /* end if toimi = 1*/
      
      ELSE IF toimi = 8 THEN DO:
      
         IF llChanged THEN DO:
            llOk = FALSE.
            MESSAGE "Do You want to cancel your changes?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " CANCEL "
            SET llOk.
            IF llOk THEN DO: 
              llChanged = FALSE.
              IF NOT llIsAdmin THEN LEAVE.
            END.
         END. 
         NEXT.
      END.
      
      ELSE IF toimi = 5 THEN DO: 

         /* create request */
         DEF VAR  liReqId    AS INT NO-UNDO.


         RUN Mm/create_charge_comp ({&REQUEST_SOURCE_MANUAL_TMS}, 
                                 MobSub.MsSeq,
                                 "",  /* katun, */
                                 ldFItemAmt,
                                 lcFeeModel,
                                 0,
                                 OUTPUT liReqId).

         IF ERROR-STATUS:ERROR THEN
             MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
          ELSE 
            MESSAGE "Charge/Comp has been submited with request id: " + STRING(liReqId) 
            VIEW-AS ALERT-BOX TITLE "INFO".

        LEAVE.
      END.

     END. /* ADD-CC */
     
     LEAVE.

  END. /* repeat*/
   
  ASSIGN
        must-add   = FALSE
        must-print = TRUE.

END PROCEDURE.


