/* ----------------------------------------------------------------------
  MODULE .......: Bitem.P
  TASK .........: UPDATE Billable Items
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 02-09-99
  CHANGED ......: 30.11.1999 jpo: DISPLAY fields (local-update-records)
                  20.05.2002 jpo: hosttable@keyvalue
                  09.09.2002 jpo: f9 -> billing level help
                  14.10.2002 jr Replaced BillLevel with BillTarget
                  01.11.2002 jp Dont remove billed items
                  05.11.2002 jr Eventlog
                  05.11.2002 jr New Memo
                  04.03.2003 aam obi -> single fee, 
                                 use-index custnum for first find 
                  05.03.2003 tk  tokens
                  16.09.2003 jp  Brand 
                  19.09.2003 aam order 3; billcode
                  30.09.2003 aam Contract
                  07.01.2004 aam VATIncl, Active
                  06.02.2004 jp  custnum for memo
                  15.03.2004 aam show CLI after KeyValue
                  22.03.2004/aam create Contract (fFeeContract)
                  13.04.2004/aam index BillPeriod removed
                  14.06.2005/aam undo creation if billcode = ""
                  01.12.2005/aam hosttable is "customer" not "asiakas"
                  24.01.2006/jt  DYNAMIC-FUNCTION("fDispCustName")
                  07.08.2006/jt  edded order by billcode
                  30.08.2006/aam find msowner/mobsub also using invcust
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable SingleFee

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixedfee'}

DEF new shared VAR siirto AS CHAR.

DEF VAR liCustNum    LIKE SingleFee.CustNum    NO-UNDO.
DEF VAR BillPeriod   LIKE SingleFee.BillPeriod NO-UNDO.
DEF VAR lcBillCode   LIKE SingleFee.BillCode   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lBelongto    AS LO  FORMAT "Customer/Subscription" NO-UNDO INIT TRUE.
DEF VAR endloop      AS I NO-UNDO.
DEF VAR lcCustName   AS CHARACTER              NO-UNDO.

DEF VAR new_singlefee AS LOG                   NO-UNDO INIT FALSE.
DEF VAR lcCLI         AS CHAR                  NO-UNDO. 
DEF VAR ldtDate       AS DATE                  NO-UNDO. 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSingleFee).
   END.
END.

{Func/ffeecont.i}
{Func/fixedfee.i}

form
    SingleFee.Brand       FORMAT "x(4)" 
    SingleFee.CustNum      
    SingleFee.BillTarget  format ">9" 
    lcCustName            format "x(16)" LABEL "Cust.Name"
    SingleFee.BillPeriod   
    SingleFee.BillCode    format "x(10)" /* COLUMN-LABEL FORMAT */
    SingleFee.Contract  
    SingleFee.Amt
    SingleFee.Billed        format "I/" column-label "I"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Single Fees "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}
lcBrand = "1".

form
    "Customer .......:" SingleFee.CustNum  lcCustName AT 45    SKIP
    "Contract .......:" SingleFee.Contract SKIP
    "Billing Target .:" SingleFee.BillTarget 
          "Billing Type ...:" AT 55    SingleFee.BillType Format "x(4)" SKIP
    "Calculation Obj.:" SingleFee.CalcObj                          SKIP
    "Period .........:" SingleFee.BillPeriod                       SKIP
    "Product ........:" SingleFee.BillCode format "x(16)"
                        BillItem.BIName   AT 45      SKIP
    "VAT ............:" SingleFee.VATIncl                          SKIP
    "Billable payment:" SingleFee.Amt                              SKIP
    "Our own cost ...:" SingleFee.OwnCost                          SKIP
    "Contr. belong to:" lbelongto
    help "Base of the contract fee is (s)ubscription or (c)ustomer?"
    "number"  SingleFee.KeyValue FORMAT "X(15)"                                        lcCLI FORMAT "X(15)" SKIP
    "Covers period(s):" SingleFee.Concerns[1] format "zzzzzzzz" "-"
                        SingleFee.Concerns[2] format "zzzzzzzz"   SKIP
    "Info to Invoice.:" SingleFee.Memo[1]                        SKIP
                        SingleFee.Memo[2]  AT 19
                        SingleFee.Memo[3]  AT 19
                        SingleFee.Memo[4]  AT 19
                        SingleFee.Memo[5]  AT 19 
    "Active .........:" SingleFee.Active
       "Billed on Invoice:" AT 40 SingleFee.InvNum  FORMAT "zzzzzzzzz"         

WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

form /* seek Billable item  BY  CustNum */
    "Brand ..:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Customer:" liCustNum HELP "Enter Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUST. NO "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billable item  BY BillPeriod */
    "Brand :" lcBrand  HELP "Enter Brand"
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Period:"  BillPeriod HELP "Enter Billing Period"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Period "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Billable item  BY BillCode */
    "Brand ..:" lcBrand  HELP "Enter Brand"
      VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") 
      SKIP
    "BillItem:"  lcBillCode HELP "Enter Billing Item code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BillItem "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


form
    SingleFee.Memo
    WITH OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Invoice Text: Customer No " + STRING(singlefee.custnum) + "  " + 
                     singlefee.billcode +  " " 
    WITH NO-LABELS 1 columns
    FRAME f4.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Customer No.,   By Period  ,  By BillItem , By 4".


FIND FIRST SingleFee USE-INDEX CustNum 
WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE SingleFee THEN ASSIGN
   memory       = recid(SingleFee)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No single fees available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.


LOOP:
REPEAT WITH FRAME sel:
    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a SingleFee  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR SingleFee.CustNum 
              VALIDATE(INPUT CustNum = 0 OR 
                       CAN-FIND(FIRST Customer WHERE
                                      Customer.Brand  = lcBrand AND
                                      Customer.CustNum =
                                INPUT CustNum),"Unknown Customer !").
           IF INPUT FRAME lis SingleFee.CustNum =  0 THEN 
           LEAVE add-row.

           new_singlefee = TRUE.
           CREATE SingleFee.
           ASSIGN
           SingleFee.Brand    = lcBrand
           SingleFee.FMItemId = next-value(bi-seq)
           SingleFee.CustNum  = INPUT FRAME lis SingleFee.CustNum.
           
           FIND Customer WHERE Customer.CustNum = SingleFee.CustNum NO-LOCK.
           SingleFee.VATIncl = Customer.VATIncl.

           RUN local-update-record.
           
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR,F4") > 0 OR
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
           
           ASSIGN
           memory = recid(SingleFee)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SingleFee
      WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SingleFee THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SingleFee WHERE recid(SingleFee) = memory NO-LOCK NO-ERROR.

        /* DILAY one page beginning the record 
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
        ufk[1]= 702 ufk[2]= 0 /* 771  */ ufk[3]= 703 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 2150 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SingleFee.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SingleFee.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SingleFee.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SingleFee.BillCode WITH FRAME sel.
      END.
      
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).
      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order <= 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 2 THEN DO:
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

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
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
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR  FRAME f1.
       Disp lcBrand With FRAME f1.
       SET  lcBrand WHEN gcAllBrand = TRUE 
            liCustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liCustNum ENTERED THEN DO:
          FIND FIRST SingleFee WHERE 
                     SingleFee.CustNum >= liCustNum AND 
                     Singlefee.Brand    = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 3 */
     
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       Disp lcBrand With FRAME f3.
       
       SET  lcBrand WHEN gcAllBrand = TRUE
            lcBillCode WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF lcBillCode ENTERED THEN DO:
          FIND FIRST SingleFee WHERE 
                     Singlefee.Brand = lcBrand AND
                     SingleFee.BillCode >= lcBillCode 
          NO-LOCK NO-ERROR.
          IF NOT fRecFound(2) THEN NEXT Browse.
          
          NEXT LOOP.
       
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN 
     
     DO: /* MEMO */
        RUN local-find-this(false).
        RUN Mc/memo(INPUT SingleFee.Custnum,
                 INPUT "SingleFee",
                 INPUT STRING(FMItemId),
                 INPUT "SingleFee").
        ufkey = TRUE. ehto = 9.
        NEXT LOOP.
     END.

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. 
        RUN Syst/ufkey. ufkey = TRUE.

        RUN local-find-this(TRUE).

        IF lcRight = "RW" THEN 
        UPDATE SingleFee.Memo WITH FRAME f4.
        ELSE DO:
           DISP SingleFee.Memo WITH FRAME f4.
           PAUSE.
        END.
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"6,f6") > 0  AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).
       IF SingleFee.invNum   ne 0 AND
          SingleFee.billed   = TRUE 
       THEN DO:
          MESSAGE
          "Single Fee already billed. Cant be Removed"
          VIEW-AS ALERT-BOX TITLE "REMOVE NOT ALLOWED".
          NEXT.
        END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          SingleFee.CustNum      
          SingleFee.BillTarget  
          lcCustName    
          SingleFee.BillPeriod 
          SingleFee.BillCode   
          SingleFee.Contract
          SingleFee.Amt
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
          SingleFee.CustNum      
          SingleFee.BillTarget  
          lcCustName
          SingleFee.BillPeriod 
          SingleFee.BillCode   
          SingleFee.Contract
          SingleFee.Amt
          SingleFee.Billed.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEventWithMemo(
                                    lhSingleFee,
                                    katun,
                                    "ManualCUI").
           DELETE SingleFee.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SingleFee
           WHERE Singlefee.Brand = lcBrand) THEN DO:
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
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY SingleFee.CustNum.

       new_singlefee = FALSE.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(SingleFee).
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
       IF order = 1 THEN FIND FIRST SingleFee      USE-INDEX CustNum
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN FIND FIRST SingleFee USE-INDEX BillCode
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST SingleFee USE-INDEX CustNum
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN FIND LAST SingleFee USE-INDEX BillCode
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT SingleFee USE-INDEX CustNum
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN FIND NEXT SingleFee USE-INDEX BillCode
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

       IF order = 1 THEN FIND PREV SingleFee USE-INDEX CustNum
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN FIND PREV SingleFee USE-INDEX BillCode
       WHERE Singlefee.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       
       RUN local-find-others.
       
       lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                        BUFFER Customer).
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       SingleFee.Brand 
       SingleFee.CustNum
       SingleFee.BillTarget
       SingleFee.BillPeriod
       SingleFee.Amt
       SingleFee.Billed
       SingleFee.BillCode
       SingleFee.Contract 
       lcCustName WHEN AVAIL Customer

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND Customer WHERE 
            Customer.CustNum        = SingleFee.CustNum   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      FIND BillItem   WHERE 
           BillItem.Brand          = lcBrand  AND 
           BillItem.BillCode       = SingleFee.BillCode    NO-LOCK NO-ERROR.
      FIND BillTarget WHERE 
           BillTarget.BillTarget   = SingleFee.BillTarget   AND
           BillTarget.CustNum      = SingleFee.CustNum    
      NO-LOCK NO-ERROR.
      FIND FIRST BillType WHERE
                 BillType.BillType = SingleFee.BillType           
      NO-LOCK NO-ERROR.

      ASSIGN lbelongto = SingleFee.HostTable = "Customer" OR 
                         SingleFee.HostTable = ""
             lcCLI     = "".

      IF SingleFee.HostTable = "mobsub" THEN DO:
         FOR FIRST MsOwner NO-LOCK WHERE
                   MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue) AND
                   MsOwner.CustNum = SingleFee.CustNum:
            lcCLI = MsOwner.CLI.       
         END.
         IF lcCLI = "" THEN 
         FOR FIRST MsOwner NO-LOCK WHERE
                   MsOwner.InvCust = SingleFee.CustNum AND
                   MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue):
            lcCLI = MsOwner.CLI.       
         END.
      END.
      
      DISP
         lcCustName   WHEN AVAIL Customer
         SingleFee.Contract 
         BillItem.BIName       WHEN AVAIL BillItem
         SingleFee.InvNum
         SingleFee.BillTarget      
         SingleFee.CalcObj      
         lbelongto
         lcCLI
         SingleFee.KeyValue
         SingleFee.Concerns
         SingleFee.BillPeriod    
         SingleFee.BillCode       
         SingleFee.VATIncl
         SingleFee.Amt 
         SingleFee.Owncost
         SingleFee.Memo
         SingleFee.BillType
         SingleFee.Active
      WITH FRAME lis.

      IF SingleFee.HostTable = "Order" THEN 
      DISPLAY SingleFee.HostTable @ lBelongTo WITH FRAME lis.
 
      IF lcRight = "RW" THEN DO:

         si-recid2 = SingleFee.CustNum.

         UPDATE
            SingleFee.Contract    WHEN NOT SingleFee.Billed
            SingleFee.BillTarget  WHEN NOT SingleFee.Billed
            SingleFee.BillType    WHEN NOT SingleFee.Billed
            SingleFee.CalcObj     WHEN NOT SingleFee.Billed
            SingleFee.BillPeriod  WHEN NOT SingleFee.Billed
            SingleFee.BillCode    WHEN NOT SingleFee.Billed
            SingleFee.VATIncl     WHEN NOT SingleFee.Billed
            SingleFee.Amt         WHEN NOT SingleFee.Billed
            SingleFee.OwnCost
            lbelongto             WHEN NEW SingleFee
            SingleFee.KeyValue    WHEN NEW SingleFee
            SingleFee.Concerns    WHEN NOT SingleFee.Billed
            SingleFee.Memo        WHEN NOT SingleFee.Billed
            SingleFee.Active      WHEN NOT SingleFee.Billed

         WITH FRAME lis EDITING:
             READKEY.
              IF FRAME-FIELD = "keyvalue" and keylabel(Lastkey) = "F9"
              THEN DO:
                  ASSIGN INPUT lbelongto.
                  IF    lbelongto = TRUE THEN RUN Mc/nnasel.
                  ELSE  IF lbelongto = FALSE THEN RUN Help/h-mobsub.

                  IF siirto NE ? THEN 
                  ASSIGN singlefee.keyvalue = siirto.
                  
                  DISP singlefee.keyvalue with frame lis.
             END.

             IF FRAME-FIELD = "BillTarget" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
                RUN Help/h-billtarg(INPUT INPUT FRAME lis Singlefee.CustNum).
                IF siirto NE ? THEN DO:
                   ASSIGN Singlefee.BillTarget = INT(siirto).
                   DISP Singlefee.BillTarget WITH FRAME lis.
                   ASSIGN ehto = 9 ufkey = TRUE.
                   RUN Syst/ufkey.p.
                   
                   NEXT.
                END.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CustNum" THEN DO:
                   FIND Customer WHERE 
                        Customer.Brand   = gcBrand AND 
                        Customer.CustNum =
                   INPUT FRAME lis SingleFee.CustNum NO-LOCK NO-ERROR.
                   IF NOT AVAIL Customer THEN DO:
                      BELL.
                      MESSAGE "Unknown customer !".
                      NEXT.
                   END.
                   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                 BUFFER Customer).
                   DISP lcCustName.
                END.

                else if frame-field = "contract" then do:
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

                ELSE IF FRAME-FIELD = "billType" THEN DO:

                   FIND BillType WHERE BillType.BillType =
                   INPUT FRAME lis SingleFee.BillType NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillType THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing RepType !".
                      NEXT.
                   END.
                END.

                IF FRAME-FIELD = "BillTarget" THEN DO:
                   FIND BillTarget WHERE 
                        BillTarget.BillTarget = INPUT FRAME LIS                                                                  SingleFee.BillTarget 
                   AND  BillTarget.CustNum    = INPUT FRAME LIS                                                 SingleFee.CustNum
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillTarget THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Target !".
                      NEXT.
                   END.   
                END.

                ELSE IF FRAME-FIELD  = "lbelongto" THEN DO:
                   IF FRAME lis lbelongto = TRUE  THEN DO:
                      DISP string(SingleFee.CustNum) @ SingleFee.KeyValue.
                      ASSIGN
                      SingleFee.HostTable = "Customer"
                      SingleFee.KeyValue  = string(SingleFee.CustNum).
                   END.
                   ELSE IF FRAME lis lbelongto =  FALSE THEN DO:
                      ASSIGN
                      SingleFee.HostTable = "mobsub".
                   END.
                END.  /* BillCode */

                ELSE IF FRAME-FIELD  = "keyvalue" THEN DO:
                IF SingleFee.HostTable = "mobsub" THEN DO:
                   FIND FIRST MobSub WHERE
                              Mobsub.Brand   = lcBrand AND 
                              MobSub.CLI     = INPUT FRAME lis KeyValue
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL MobSub OR 
                      (MobSub.CustNum NE SingleFee.CustNum AND
                       MobSub.InvCust NE SingleFee.CustNum)
                   THEN DO:
                      BELL.
                      MESSAGE
                      "UNknown Mobile Subscription"
                      VIEW-AS ALERT-BOX.
                      NEXT.
                   END.
                END.
                
                IF SingleFee.HostTable = "Customer" THEN DO:
                   FIND FIRST Customer WHERE
                              Customer.Brand   = lcBrand  AND 
                              Customer.CustNum = INT(INPUT FRAME lis KeyValue)
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL Customer THEN DO:
                      BELL.
                      MESSAGE
                      "Unknown Customer"
                      VIEW-AS ALERT-BOX.
                      NEXT.
                   END.
                END.   
                END. /* KeyValue */

                ELSE IF FRAME-FIELD = "BillCode" THEN DO:

                   FIND BillItem WHERE 
                        BillItem.Brand    = lcBrand AND 
                        BillItem.BillCode =
                   INPUT FRAME lis SingleFee.BillCode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown BillCode !".
                      NEXT.
                   END.
                   DISP BillItem.BIName.    
                END.

                ELSE IF FRAME-FIELD = "Billperiod" THEN DO:
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
             APPLY LASTKEY.
          END. /* EDITING */

          si-recid2 = 0.

         IF new_singlefee AND
            llDoEvent THEN RUN StarEventMakeCreateEventWithMemo(
                                 lhSingleFee,
                                 katun,
                                 "ManualCUI").

         IF NOT new_singlefee AND
            llDoEvent THEN RUN StarEventMakeModifyEventWithMemo(
                                 lhSingleFee,
                                 katun,
                                 "ManualCUI").
      END.

      ELSE PAUSE.
      new_singlefee = FALSE.
      LEAVE.
   END.
   END.
