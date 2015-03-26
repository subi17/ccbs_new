/* ----------------------------------------------------------------------
  MODULE .......: Contract
  TASK .........: UPDATEs table Contract
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 30.12.02
  CHANGED ......: 19.09.03/aam brand
                  03.10.03/aam input custnum and salesman
                  06.10.03/aam ContrType, CloseDate, FeeModel
                  13.10.03/aam tokens
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable Contract

{commali.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'contract'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhContract AS HANDLE NO-UNDO.
   lhContract = BUFFER Contract:HANDLE.
   RUN StarEventInitialize(lhContract).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhContract).
   END.

END.

DEF INPUT PARAMETER iiCustNum  AS INT NO-UNDO.
DEF INPUT PARAMETER icSalesman AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcContract  LIKE Contract.Contract      NO-UNDO. 
DEF VAR liCustNum   LIKE Contract.CustNum       NO-UNDO.
DEF VAR ldtFrom     LIKE Contract.FromDate      NO-UNDO.
DEF VAR liContrType LIKE Contract.ContrType     NO-UNDO.
DEF VAR ldtTo       LIKE Contract.CloseDate     NO-UNDO.
DEF VAR xSalesman   LIKE Contract.Salesman      NO-UNDO. 

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 5.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

DEF VAR lcCustName   AS CHAR  NO-UNDO.
DEF VAR lcSmName     AS CHAR  NO-UNDO.
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR lcContrType  AS CHAR  NO-UNDO.
DEF VAR lcCTypeLst   AS CHAR  NO-UNDO. 

form
    Contract.Brand     FORMAT "X(4)"  COLUMN-LABEL "Bran"
    Contract.Contract
    Contract.ContrType 
    Contract.CustNum    
    lcCustName         FORMAT "X(14)" COLUMN-LABEL "CName"
    Contract.Salesman
    Contract.FromDate
    Contract.ToDate
    Contract.CloseDate

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " CONTRACTS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Contract.Contract   COLON 20 SKIP
    Contract.CustNum     COLON 20   
       VALIDATE(INPUT Contract.CustNum = 0 OR 
                CAN-FIND(FIRST Customer WHERE 
                               Customer.Brand   = gcBrand AND
                               Customer.CustNum = INPUT Contract.CustNum),
                "Unknown customer")
       lcCustName FORMAT "X(30)" NO-LABEL SKIP
    Contract.Salesman     COLON 20   LABEL "Salesman"
       VALIDATE(INPUT Contract.Salesman = "" OR
                CAN-FIND(Salesman WHERE 
                         Salesman.Brand   = gcBrand AND
                         Salesman.Salesman = 
                         INPUT Contract.Salesman),
                "Unknown Salesman")
       lcSmName FORMAT "X(30)" NO-LABEL SKIP
    Contract.ContrType   COLON 20 
       VALIDATE(INPUT Contract.ContrType >= 1 AND
                INPUT Contract.ContrType <= 2,
               "Valid values are 1 - 2")
       lcContrType COLON 29 NO-LABEL FORMAT "X(30)" SKIP
    Contract.FeeModel    COLON 20
       VALIDATE(INPUT Contract.FeeModel = "" OR
                CAN-FIND(FeeModel WHERE 
                         FeeModel.Brand = gcBrand AND
                         FeeModel.FeeModel = INPUT Contract.FeeModel),
                "Unknown fee model")
       FeeModel.FeeName NO-LABEL SKIP
    Contract.FromDate    COLON 20
       VALIDATE(INPUT Contract.FromDate NE ?,
                "Date is mandatory")
    Contract.ToDate      COLON 20
       VALIDATE(INPUT Contract.ToDate NE ?,
                "Date is mandatory")
    Contract.CloseDate   COLON 20                
    Contract.CommPerm    COLON 20 
    Contract.Memo        COLON 20 
       FORMAT "X(55)"

WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{brand.i}

form /* seek  contract */
    "Brand ..:" lcBrand skip
    "Contract:" lcContract
    HELP "Enter contract ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND contract "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  Salesman */
    "Brand ..:" lcBrand skip
    "Salesman:" xSalesman
    HELP "Enter Salesman code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Salesman "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  date */
    "Brand:" lcBrand skip
    "Date :" ldtFrom
    HELP "Enter beginning date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Beg. Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

form /* seek  date */
    "Brand:" lcBrand skip
    "Type :" liContrType 
       HELP "Enter contract type"
       SKIP
    "Date :" ldtTo
       HELP "Enter end date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND End Date"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f5.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Contract  ," +
         "By Customer  ," + 
         "By Salesman  ," +
         "By Beg. Date ," +
         "By End Date   ".

DO i = 1 TO 3:
   lcCTypeLst = lcCTypeLst + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Contract","ContrType",STRING(i)) + ",".
END.                                  

IF iiCustNum > 0 
THEN ASSIGN MaxOrder = 1
            Order    = 4.
ELSE IF icSalesman > "" 
THEN ASSIGN MaxOrder = 1
            Order    = 2.

RUN local-find-first.

IF AVAILABLE Contract THEN ASSIGN
   Memory       = recid(Contract)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a Contract  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:

           IF iiCustNum > 0 THEN DISPLAY iiCustNum @ Contract.CustNum.

           PROMPT-FOR Contract.Contract
                      Contract.CustNum WHEN iiCustNum = 0.

           IF INPUT FRAME lis Contrac.Contract = ""               
           THEN LEAVE add-row.

           IF INPUT FRAME lis Contract.CustNum = 0 
           THEN NEXT.

           IF CAN-FIND(FIRST Contract USING FRAME lis Contract.Contract
                        WHERE Contract.Brand = lcBrand)
           THEN DO:
              MESSAGE 
              "Contract"
              INPUT FRAME lis Contract.Contract
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE Contract.
           ASSIGN
           Contract.Brand    = lcBrand
           Contract.Contract = INPUT FRAME lis Contract.Contract
           Contract.CustNum  = INPUT FRAME lis Contract.CustNum
           Contract.Salesman  = icSalesman.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhContract).

           ASSIGN
           Memory = recid(Contract)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Contract WHERE Contract.Brand = gcBrand 
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Contract THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Contract WHERE recid(Contract) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Contract THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Contract).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
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
        ufk[1]= 816  ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW Contract.Contract ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contract.Contract WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Contract.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contract.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW Contract.Salesman ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contract.Salesman WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
         CHOOSE ROW Contract.FromDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contract.FromDate WITH FRAME sel.
      END.
      ELSE IF order = 5 THEN DO:
         CHOOSE ROW Contract.ToDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contract.ToDate WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Contract WHERE recid(Contract) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Contract THEN
              ASSIGN FIRSTrow = i Memory = recid(Contract).
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
           IF NOT AVAILABLE Contract THEN DO:
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
                rtab[1] = recid(Contract)
                Memory  = rtab[1].
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
           IF NOT AVAILABLE Contract THEN DO:
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
              rtab[FRAME-DOWN] = recid(Contract).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Contract WHERE recid(Contract) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Contract THEN DO:
           Memory = recid(Contract).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Contract THEN Memory = recid(Contract).
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
           Memory = rtab[FRAME-DOWN].
           FIND Contract WHERE recid(Contract) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       ASSIGN
       ufk[1]= 35   ufk[2]= 714 ufk[3]= 885 ufk[4]= 1351
       ufk[5]= 1352 ufk[6]= 0   ufk[7]= 0   ufk[8]= 8 
       ehto = 0
       ufkey = TRUE.

       IF iiCustNum > 0 
       THEN ASSIGN ufk[1] = 0 
                   ufk[2] = 0
                   ufk[3] = 0.
       IF icSalesman > ""
       THEN ASSIGN ufk[1] = 0
                   ufk[3] = 0
                   ufk[4] = 0.

       RUN ufkey.

       /* Search BY column 1 */
       IF toimi = 1 THEN DO:
          cfc = "puyr". run ufcolor.
          ehto = 9. RUN ufkey. ufkey = TRUE.
          CLEAR FRAME f1.
          DISPLAY lcBrand WITH FRAME F1.
          UPDATE lcBrand WHEN gcAllBrand
                 lcContract WITH FRAME f1.
          HIDE FRAME f1 NO-PAUSE.

          IF lcContract NE "" THEN DO:
             FIND FIRST Contract WHERE 
                        Contract.Brand     = lcBrand AND
                        Contract.Contract >= lcContract
             NO-LOCK NO-ERROR.

             IF NOT fRecFound(1) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
       END. /* Search-1 */

       /* Search BY column 2 */
       ELSE IF TOIMI = 2 THEN DO ON ENDKEY UNDO, NEXT LOOP:
          cfc = "puyr". run ufcolor.
          ehto = 9. RUN ufkey. ufkey = TRUE.
          CLEAR FRAME f2.
          DISPLAY lcBrand WITH FRAME F2.
          UPDATE lcBrand WHEN gcAllBrand
                 liCustNum WITH FRAME f2.
          HIDE FRAME f2 NO-PAUSE.

          IF liCustNum > 0 THEN DO:

             IF icSalesman > "" THEN 
             FIND FIRST Contract WHERE 
                Contract.Brand    = lcBrand    AND
                Contract.Salesman = icSalesman AND
                Contract.CustNum >= liCustNum
             USE-INDEX Salesman NO-LOCK NO-ERROR.

             ELSE FIND FIRST Contract WHERE 
                             Contract.Brand    = lcBrand AND
                             Contract.CustNum >= liCustNum
             USE-INDEX CustNum NO-LOCK NO-ERROR.

             IF NOT fRecFound(2) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
       END. /* Search-2 */

       /* Search BY col 3 */
       ELSE IF toimi = 3 THEN DO ON ENDKEY UNDO, NEXT LOOP:

          cfc = "puyr". run ufcolor.
          ehto = 9. RUN ufkey. ufkey = TRUE.
          CLEAR FRAME F3.
          DISPLAY lcBrand WITH FRAME F3.
          UPDATE lcBrand WHEN gcAllBrand
                 xSalesman WITH FRAME f3.
          HIDE FRAME f3 NO-PAUSE.

          IF xSalesman NE "" THEN DO:
             FIND FIRST Contract WHERE 
                        Contract.Brand     = lcBrand AND
                        Contract.Salesman >= xSalesman
             USE-INDEX Salesman  NO-LOCK NO-ERROR.

             IF NOT fRecFound(3) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
       END. /* Search-3 */

       /* Search BY col 4 */
       ELSE IF toimi = 4 THEN DO ON ENDKEY UNDO, NEXT LOOP:

          cfc = "puyr". run ufcolor.
          ehto = 9. RUN ufkey. ufkey = TRUE.
          CLEAR FRAME F4.
          DISPLAY lcBrand WITH FRAME F4.
          UPDATE lcBrand WHEN gcAllBrand
                 ldtFrom WITH FRAME f4.
          HIDE FRAME f4 NO-PAUSE.

          IF ldtFrom NE ? THEN DO:

             IF iiCustNum > 0 THEN 
             FIND LAST Contract WHERE 
                Contract.Brand     = lcBrand   AND
                Contract.CustNum   = iiCustNum AND
                Contract.FromDate >= ldtFrom
             USE-INDEX CustNum  NO-LOCK NO-ERROR.

             ELSE
             FIND LAST Contract WHERE 
                       Contract.Brand     = lcBrand AND
                       Contract.FromDate >= ldtFrom
             USE-INDEX FromDate  NO-LOCK NO-ERROR.

             IF NOT fRecFound(4) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
       END. /* Search-4 */

       ELSE IF toimi = 5 THEN DO ON ENDKEY UNDO, NEXT LOOP:

          cfc = "puyr". run ufcolor.
          ehto = 9. RUN ufkey. ufkey = TRUE.
          CLEAR FRAME F5.
          DISPLAY lcBrand WITH FRAME F5.
          UPDATE lcBrand WHEN gcAllBrand
                 liContrType
                 ldtTo WITH FRAME f5.
          HIDE FRAME f5 NO-PAUSE.

          IF liContrType > 0 AND ldtTo NE ? THEN DO:

             FIND LAST Contract WHERE 
                       Contract.Brand     = lcBrand AND
                       Contract.ContrType = liContrType AND
                       Contract.ToDate   >= ldtTo
             USE-INDEX ContrType  NO-LOCK NO-ERROR.

             IF NOT fRecFound(5) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
       END. /* Search-5 */

     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Contract.Contract Contract.CustNum Contract.Salesman .

       RUN local-find-NEXT.
       IF AVAILABLE Contract THEN Memory = recid(Contract).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Contract THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Contract).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       /* check for links */
       FOR FIRST FixedFee OF Contract NO-LOCK:
          MESSAGE "There are fixed fees defined for this contract."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT LOOP.
       END.

       FOR FIRST SingleFee OF Contract NO-LOCK:
          MESSAGE "There are single fees defined for this contract."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT LOOP.
       END.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Contract.Contract Contract.CustNum Contract.Salesman .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhContract).

           DELETE Contract.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Contract) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhContract).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Contract.CustNum.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhContract).

       RUN local-disp-row.
       xrecid = recid(Contract).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Contract) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Contract) must-print = TRUE.
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
      FIND Contract WHERE recid(Contract) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Contract WHERE recid(Contract) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiCustNum > 0 THEN DO:
      FIND FIRST Contract WHERE 
         Contract.Brand   = lcBrand AND
         Contract.CustNum = iiCustNum
         NO-LOCK NO-ERROR.
   END.
   ELSE IF icSalesman > "" THEN DO:
      FIND FIRST Contract WHERE 
         Contract.Brand    = lcBrand AND
         Contract.Salesman = icSalesman
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
        IF order = 1 THEN FIND FIRST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Contract
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Salesman
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX FromDate
        NO-LOCK NO-ERROR.
       ELSE IF order = 5 THEN FIND FIRST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX ContrType
        NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiCustNum > 0 THEN DO:
      FIND LAST Contract WHERE 
         Contract.Brand   = lcBrand AND
         Contract.CustNum = iiCustNum
         NO-LOCK NO-ERROR.
   END.
   ELSE IF icSalesman > "" THEN DO:
      FIND LAST Contract WHERE 
         Contract.Brand    = lcBrand AND
         Contract.Salesman = icSalesman
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND LAST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Contract
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Salesman
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX FromDate
        NO-LOCK NO-ERROR.
       ELSE IF order = 5 THEN FIND LAST Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX ContrType
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiCustNum > 0 THEN DO:
      FIND NEXT Contract WHERE 
         Contract.Brand   = lcBrand AND
         Contract.CustNum = iiCustNum
         NO-LOCK NO-ERROR.
   END.
   ELSE IF icSalesman > "" THEN DO:
      FIND NEXT Contract WHERE 
         Contract.Brand    = lcBrand AND
         Contract.Salesman = icSalesman
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND NEXT Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Contract
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Salesman
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX FromDate
        NO-LOCK NO-ERROR.
       ELSE IF order = 5 THEN FIND NEXT Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX ContrType
        NO-LOCK NO-ERROR.
   END.     
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF iiCustNum > 0 THEN DO:
      FIND PREV Contract WHERE 
         Contract.Brand   = lcBrand AND
         Contract.CustNum = iiCustNum
         NO-LOCK NO-ERROR.
   END.
   ELSE IF icSalesman > "" THEN DO:
      FIND PREV Contract WHERE 
         Contract.Brand    = lcBrand AND
         Contract.Salesman = icSalesman
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND PREV Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Contract
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX Salesman
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX FromDate
        NO-LOCK NO-ERROR.
       ELSE IF order = 5 THEN FIND PREV Contract WHERE 
          Contract.Brand = lcBrand USE-INDEX ContrType
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Contract.Brand
       Contract.Contract
       Contract.ContrType
       Contract.CustNum    
       lcCustName
       Contract.Salesman
       Contract.FromDate
       Contract.ToDate
       Contract.CloseDate
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Customer WHERE Customer.CustNum = Contract.CustNum NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN lcCustName = Customer.CustName. 
   ELSE lcCustName = "Unknown".

   FIND Salesman WHERE 
        Salesman.Brand    = Contract.Brand AND
        Salesman.Salesman = Contract.Salesman NO-LOCK NO-ERROR.
   IF AVAILABLE Salesman THEN lcSmName = Salesman.SMName. 
   ELSE lcSmName = "". 

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      IF Contract.ContrType >= 1 AND
         Contract.ContrType <= 2 
      THEN lcContrType = ENTRY(Contract.ContrType,lcCTypeLst).
      ELSE lcContrType = "".

      FIND FeeModel NO-LOCK WHERE
           FeeModel.Brand    = lcBrand AND
           FeeModel.FeeModel = Contract.FeeModel NO-ERROR.

      DISP Contract.Contract
           Contract.CustNum
           lcCustName 
           lcSmName
           lcContrType
           FeeModel.FeeName WHEN AVAILABLE FeeModel
           Contract.Salesman 
           Contract.ContrType
           Contract.FeeModel
           Contract.FromDate
           Contract.ToDate
           Contract.CloseDate
           Contract.CommPerm
           Contract.Memo
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN ufkey.
         
         UPDATE
         Contract.Salesman  WHEN icSalesman = ""
         Contract.ContrType
         Contract.FeeModel
         Contract.FromDate
         Contract.ToDate
         Contract.CloseDate
         Contract.CommPerm
         Contract.Memo
         WITH FRAME lis
         EDITING:

            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               FRAME-FIELD = "ContrType"
            THEN DO:

               RUN h-tmscodes(INPUT "Contract",    /* TableName */
                                    "ContrType",   /* FieldName */
                                    "Commission",  /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ?
               THEN DO:
                  lcContrType = ENTRY(INTEGER(lcCode),lcCTypeLst).
                  DISPLAY INTEGER(lcCode) ;& Contract.ContrType 
                          lcContrType
                  WITH FRAME lis.   
               END.   

               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.


             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "Salesman" THEN DO:
                   IF INPUT FRAME lis Contract.Salesman = ""
                   THEN DISPLAY "" ;& lcSmName. 

                   ELSE DO:
                      FIND Salesman WHERE 
                           Salesman.Brand    = lcBrand AND
                           Salesman.Salesman =
                      INPUT FRAME lis Contract.Salesman NO-LOCK NO-ERROR.
                      IF NOT AVAIL Salesman THEN DO:
                         BELL.
                         MESSAGE "Unknown Salesman !".
                         NEXT.
                      END.
                      DISP Salesman.SMName ;& lcSmName.
                   END.
                END.

                ELSE IF FRAME-FIELD = "ContrType" THEN DO:

                   IF INPUT Contract.ContrType >= 1 AND
                      INPUT Contract.ContrType <= 2 
                   THEN lcContrType = ENTRY(INPUT Contract.ContrType,
                                            lcCTypeLst).
                   ELSE lcContrType = "".

                   DISPLAY lcContrType WITH FRAME lis.

                END.

                ELSE IF FRAME-FIELD = "FeeModel" THEN DO:

                   IF INPUT Contract.FeeModel = "" 
                   THEN DISPLAY "" @ FeeModel.FeeName.

                   ELSE DO:
                      FIND FeeModel NO-LOCK WHERE
                           FeeModel.Brand    = lcBrand AND
                           FeeModel.FeeModel = INPUT Contract.FeeModel 
                           NO-ERROR.
                      IF AVAILABLE FeeModel THEN DISPLAY FeeModel.FeeName.
                   END.
                END. 

             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.

