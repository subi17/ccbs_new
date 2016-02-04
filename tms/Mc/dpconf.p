/* ----------------------------------------------------------------------
  MODULE .......: DPConf.p
  TASK .........: DPConf
  APPLICATION ..: TMS
  AUTHOR .......: pt
  CREATED ......: 13.07.2001
  CHANGED ......: 20.08.02/aam tuning 
                  28.02.03/tk  tokens
                  10.09.03/tk  brand
                  23.10.03/aam StepUsage
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DPConf

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'dpconf'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDpConf AS HANDLE NO-UNDO.
   lhDPConf = BUFFER DPConf:HANDLE.
   RUN StarEventInitialize(lhDPConf).

   DEFINE VARIABLE lhDPBasis AS HANDLE NO-UNDO.
   lhDPBasis = BUFFER DPBasis:HANDLE.
   RUN StarEventInitialize(lhDPBasis).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhDPConf).
   END.

END.


DEF /* new */ SHARED VAR siirto AS CHAR.

DEF INPUT PARAMETER  DiscPlan LIKE DiscPlan.DiscPlan NO-UNDO.

DEF VAR ValidFrom    LIKE DPConf.ValidFrom     NO-UNDO.
DEF VAR DPCName      LIKE DPConf.DPCName       NO-UNDO.
DEF VAR BasName      AS C FORMAT "x(16)"       NO-UNDO.
DEF VAR BasNames     AS C                      NO-UNDO.
DEF VAR fixed        AS LO                     NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR x            AS I                      NO-UNDO.

DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init true.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR lis2-hdr     AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS LOG FORMAT "Yes/No"    NO-UNDO.

DEF VAR leave_key    AS CHAR                   NO-UNDO.

leave_key = poisnap + ",cursor-right,cursor-left".

FORM
    DPConf.ValidFrom      /* column-label format */
    DPConf.ValidTo       
    DPConf.DPCName        /* column-label format */
    DPConf.DiscType
    BasName      COLUMN-LABEL "Type Name"

WITH ROW FrmRow CENTERED OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Discounts of Discount Plan " + DiscPlan + " " 
    FRAME sel.

{Func/brand.i}

FORM
    "Valid from .:" DPConf.ValidFrom SKIP
    "Valid to ...:" DPConf.ValidTo   
       VALIDATE(INPUT DPConf.ValidTo NE ? AND 
                INPUT DPConf.ValidTo >= INPUT DPConf.ValidFrom,
                "Effective period's end cannot be before the beginning")
       SKIP
    "Description :" DPConf.DPCName   SKIP
    "Disc. Type .:" DPConf.DiscType 
       HELP "0=Fixed, 1=Duration, 2=Value, 3=Qty"
       basname  SKIP
    "Step Usage .:" DPConf.StepUsage SKIP   
    "Start Charge:" DPConf.StartFee
WITH 
    OVERLAY 
    ROW 1 
    CENTERED                            
    NO-LABEL
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
   "          MinValue     Discount"     SKIP
   "          ----------   --------"     SKIP
   " Step  1:" DPConf.Limit[ 1]  "   " DPConf.DiscPrcnt[ 1] "%" SKIP
   " Step  2:" DPConf.Limit[ 2]  "   " DPConf.DiscPrcnt[ 2] "%" SKIP
   " Step  3:" DPConf.Limit[ 3]  "   " DPConf.DiscPrcnt[ 3] "%" SKIP
   " Step  4:" DPConf.Limit[ 4]  "   " DPConf.DiscPrcnt[ 4] "%" SKIP
   " Step  5:" DPConf.Limit[ 5]  "   " DPConf.DiscPrcnt[ 5] "%" SKIP
   " Step  6:" DPConf.Limit[ 6]  "   " DPConf.DiscPrcnt[ 6] "%" SKIP
   " Step  7:" DPConf.Limit[ 7]  "   " DPConf.DiscPrcnt[ 7] "%" SKIP
   " Step  8:" DPConf.Limit[ 8]  "   " DPConf.DiscPrcnt[ 8] "%" SKIP
   " Step  9:" DPConf.Limit[ 9]  "   " DPConf.DiscPrcnt[ 9] "%" SKIP
   " Step 10:" DPConf.Limit[10]  "   " DPConf.DiscPrcnt[10] "%" SKIP

WITH 
    OVERLAY 
    ROW 6 
    CENTERED
    NO-LABEL
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) lis2-hdr
    SIDE-LABELS 
    FRAME lis2.

FORM
   " Fixed discount ....:"    DPConf.DiscPrcnt[ 1] 
   HELP "Fixed (direct) discount, regardless of any volume" "%" SKIP

WITH 
    OVERLAY 
    ROW 7 
    CENTERED
    NO-LABEL
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " FIXED DISCOUNT " 
    SIDE-LABELS 
    FRAME lis3.

FORM /* seek DPConf  by  ValidFrom */
    " " ValidFrom
    HELP "Enter Effective Date"
    WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY  FRAME f1.

FORM /* seek DPConf  by DPCName */
    DPCName
    HELP "Enter name of Discount Group"
    WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FIND DiscPlan WHERE DiscPlan.DiscPlan = DiscPlan NO-LOCK no-error.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Eff. Date  ,By Description,By 3, By 4".
BasNames = "Fixed,Duration,Value/Price,Quantity".


FIND FIRST DPConf
WHERE DPConf.DiscPlan = DiscPlan NO-LOCK NO-ERROR.
IF AVAILABLE DPConf THEN ASSIGN
   memory       = recid(DPConf)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = false.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 COL 36 
       " " + ENTRY(order,orders) + " ".
      END.

   IF must-add THEN DO:  /* Add a DPConf  */
      ASSIGN 
      cfc = "lis" 
      ufkey = true 
      ac-hdr = " ADD " 
      must-add = false.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. 
        RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           DISPLAY 12/31/2049 ;& DPConf.ValidTo. 

           /* Ask new Key values */
           PROMPT-FOR 
              DPConf.ValidFrom 
              DPConf.ValidTo 
              DPConf.DPCName
           WITH FRAME lis EDITING:
              READKEY.

              if keylabel(lastkey) = "F4" then undo add-row, leave add-row.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 HIDE MESSAGE.
                 IF FRAME-FIELD = "ValidFrom" THEN DO:
                    IF INPUT DPConf.ValidFrom = ? THEN DO:
                       HIDE FRAME lis.
                       UNDO add-row, LEAVE add-row.
                    END.   
                 END.
                 ELSE IF FRAME-FIELD = "DPCName" THEN DO:
                    IF INPUT DPConf.DPCName   = "" THEN DO:
                       NEXT-PROMPT DPConf.ValidFrom.
                       NEXT.
                    END.   
                 END.

                 IF CAN-FIND(FIRST DPConf WHERE
                                   DPConf.DiscPlan = DiscPlan AND
                                   DPConf.ValidFrom = INPUT DPConf.ValidFrom AND
                                   DPConf.DPCName  = INPUT DPConf.DPCName)
                 THEN DO:
                    MESSAGE
                    "There is already a Discount Group" SKIP
                    "'" + INPUT DPConf.DPCName "' with" SKIP
                    "Effective Date" STRING(INPUT DPConf.ValidFrom,"99.99.9999")
                    VIEW-AS ALERT-BOX ERROR.
                    NEXT.               
                  END.                  

              END.
              APPLY LASTKEY.

           END.

           CREATE DPConf.
           ASSIGN
           DPConf.Brand = lcBrand
           DPConf.DiscPlan = DiscPlan
           DPConf.DPConfNum = NEXT-VALUE(DPConfNum) 

           DPConf.ValidFrom = INPUT FRAME lis DPConf.ValidFrom
           DPConf.ValidTo   = INPUT FRAME lis DPConf.ValidTo
           DPConf.DPCName  = INPUT FRAME lis DPConf.DPCName.


           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDPConf).

           ASSIGN
           memory = recid(DPConf)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST DPConf
      WHERE DPConf.DiscPlan = DiscPlan NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DPConf THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND DPConf WHERE recid(DPConf) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DPConf THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(DPConf).
              RUN local-find-NEXT.

          END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           DOWN.
        END.
        UP FRAME-line - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 28  ufk[2]= 30  ufk[3]= 0  ufk[4]= 1723
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DPConf.ValidFrom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DPConf.ValidFrom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW DPConf.DPCName  NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DPConf.DPCName WITH FRAME sel.
      END.

      nap = keylabel(LASTkey).

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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND DPConf WHERE recid(DPConf) = memory NO-LOCK.
        DO i = 1 TO FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE DPConf THEN
              ASSIGN FIRSTrow = i memory = recid(DPConf).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE DPConf THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(DPConf)
                memory  = rtab[1].
           END.
        END.
        ELSE UP 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE DPConf THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(DPConf).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DPConf WHERE recid(DPConf) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DPConf THEN DO:
           memory = recid(DPConf).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE DPConf THEN memory = recid(DPConf).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND DPConf WHERE recid(DPConf) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDkey undo, NEXT LOOP:

ASK-F1:
       REPEAT WITH FRAME f1 
       ON ENDKEY UNDO ASK-F1, LEAVE ASK-F1.

         cfc = "puyr". RUN Syst/ufcolor.
         ehto = 9. RUN Syst/ufkey. ufkey = true.
         CLEAR FRAME f1.
         SET ValidFrom WITH FRAME f1.
         LEAVE.
       END. /* ASK-F1 */   

       HIDE FRAME f1 NO-PAUSE.
       IF ValidFrom ENTERED THEN DO:
          FIND FIRST DPConf WHERE 
                     DPConf.Brand = lcBrand AND
                     DPConf.DiscPlan = DiscPlan AND
                     DPConf.ValidFrom >= ValidFrom
          USE-INDEX ValidFrom /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE DPConf THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DpConf/BegDate was found */
          ASSIGN order = 1 memory = recid(DPConf) must-print = true.
       END.
       NEXT LOOP.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDkey UNDO, NEXT LOOP:

ASK-F2:
        REPEAT WITH FRAME f2 
        ON ENDKEY UNDO ASK-F2, LEAVE ASK-F2.

          cfc = "puyr". RUN Syst/ufcolor.
          ehto = 9. RUN Syst/ufkey. ufkey = true.
          CLEAR FRAME f2.
          SET DPCName WITH FRAME f2.
          LEAVE.
       END. /* ASK-F2 */   

       HIDE FRAME f2 NO-PAUSE.
       IF DPCName ENTERED THEN DO:
          FIND FIRST DPConf WHERE 
                     DPConf.Brand = lcBrand AND
                     DPConf.DiscPlan = DiscPlan AND
                     DPConf.DPCName >= DPCName
          USE-INDEX DPCName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE DPConf THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DpConf/DpDesc was found */
          ASSIGN order = 2 memory = recid(DPConf) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* ADD */
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DPConf.ValidFrom DPConf.DPCName .

       RUN local-find-NEXT.
       IF AVAILABLE DPConf THEN memory = recid(DPConf).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE DPConf THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(DPConf).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       DPConf.ValidFrom DpConf.ValidTo DPConf.DPCName .
       IF ok THEN DO:

           FOR EACH DPBasis OF DPConf:

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPBasis).

              DELETE DPBasis.
           END. 

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPConf).

           DELETE DPConf.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST DPConf
           WHERE DPConf.DiscPlan = DiscPlan) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"f4,4") > 0 THEN DO:
        /* read this DPConf record into record buffer, NO-LOCK */
        RUN local-find-this(false).

        RUN Mc/dpbasis(DPConf.DPConfNum).  

        ufkey = TRUE.
        NEXT loop.


     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY DPConf.ValidFrom DPConf.ValidTo.
       DISPLAY DPConf.DPCName.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPConf).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPConf).

       RUN local-disp-row.
       xrecid = recid(DPConf).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(DPConf) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(DPConf) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS LO NO-undo.

    IF exlock THEN
       FIND DPConf WHERE recid(DPConf) = rtab[frame-line(sel)] 
       EXCLUSIVE-LOCK.
    ELSE
       FIND DPConf WHERE recid(DPConf) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN 
          FIND FIRST DPConf WHERE 
                     DPConf.Brand = lcBrand AND
                     DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND FIRST DPConf USE-INDEX DPCName WHERE 
                     DPConf.Brand = lcBrand AND
                     DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN 
          FIND LAST DPConf WHERE 
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND LAST DPConf USE-INDEX DPCName WHERE 
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN 
          FIND NEXT DPConf WHERE 
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND NEXT DPConf USE-INDEX DPCName WHERE 
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN 
          FIND PREV DPConf WHERE  
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR. 
       ELSE IF order = 2 THEN 
          FIND PREV DPConf USE-INDEX DPCName WHERE
                    DPConf.Brand = lcBrand AND
                    DPConf.DiscPlan = DiscPlan 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       DPConf.ValidFrom
       DPConf.ValidTo
       DPConf.DPCName
       DPConf.DiscType
       BasName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   BasName = ENTRY(DPConf.DiscType + 1,BasNames).
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
        BasName  
        DPConf.DPCName  
        DPConf.DiscType
        DPConf.StepUsage
        DPConf.StartFee
      WITH FRAME lis.
      COLOR DISPLAY MESSAGES BasName WITH FRAME lis.
      IF lcRight = "RW" THEN DO:

         UPDATE
             DPConf.DiscType  WHEN NEW DPConf
             DPConf.StepUsage WHEN NEW DPConf    
             DPConf.StartFee 

         WITH FRAME lis
         EDITING:
                READKEY.
                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                   PAUSE 0.
                   IF FRAME-FIELD = "DiscType" THEN DO:
                      IF INPUT DPConf.DiscType > 3 THEN DO:
                         MESSAGE
                         "Invalid DiscType" SKIP(1)
                         "Correct values are:"  SKIP(1)
                         "0: Fixed                          " SKIP
                         "1: Total Duration of calls        " SKIP
                         "2: Value    (Total Price of calls)" SKIP
                         "3: Quantity (No. of calls)         "
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.   
                      DISP ENTRY(INPUT DPConf.DiscType + 1,basNames) @ basname.

                   END.  /* DiscType */
                END.
                APPLY LASTKEY.
         END. /* EDITING */

      END.    

      Fixed = (DPConf.DiscType = 0).

      /* step usage is sensible only for volume based discounts */
      IF lcRight = "RW" AND Fixed THEN DPConf.StepUsage = 0.
      
      /* ASK STEPS AND %:S in a separate UPDATE EDITING block because DiscType
         had to be asked first */

      IF NOT fixed THEN DO WITH FRAME lis2:
          lis2-hdr = " DISCOUNT STEPS (BY " +
                    ENTRY(DPConf.DiscType,"MINUTES,PRICE,NO.OF CALLS") + ") ".

         PAUSE 0.
         DISPLAY 
          DPConf.Limit[ 1]  DPConf.DiscPrcnt[ 1] 
          DPConf.Limit[ 2]  DPConf.DiscPrcnt[ 2] 
          DPConf.Limit[ 3]  DPConf.DiscPrcnt[ 3] 
          DPConf.Limit[ 4]  DPConf.DiscPrcnt[ 4] 
          DPConf.Limit[ 5]  DPConf.DiscPrcnt[ 5] 
          DPConf.Limit[ 6]  DPConf.DiscPrcnt[ 6] 
          DPConf.Limit[ 7]  DPConf.DiscPrcnt[ 7] 
          DPConf.Limit[ 8]  DPConf.DiscPrcnt[ 8] 
          DPConf.Limit[ 9]  DPConf.DiscPrcnt[ 9] 
          DPConf.Limit[10]  DPConf.DiscPrcnt[10] 
         WITH frame lis2.

         IF lcRight = "RW" THEN DO:
            UPDATE
             DPConf.Limit[ 1]  DPConf.DiscPrcnt[ 1] 
             DPConf.Limit[ 2]  DPConf.DiscPrcnt[ 2] 
             DPConf.Limit[ 3]  DPConf.DiscPrcnt[ 3] 
             DPConf.Limit[ 4]  DPConf.DiscPrcnt[ 4] 
             DPConf.Limit[ 5]  DPConf.DiscPrcnt[ 5] 
             DPConf.Limit[ 6]  DPConf.DiscPrcnt[ 6] 
             DPConf.Limit[ 7]  DPConf.DiscPrcnt[ 7] 
             DPConf.Limit[ 8]  DPConf.DiscPrcnt[ 8] 
             DPConf.Limit[ 9]  DPConf.DiscPrcnt[ 9] 
             DPConf.Limit[10]  DPConf.DiscPrcnt[10] 
         WITH FRAME lis2
         EDITING:

             READKEY.

             IF KEYLABEL(LASTKEY) = "END" THEN NEXT.

             IF LOOKUP(KEYLABEL(LASTKEY),leave_key) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                x = FRAME-INDEX.

                IF FRAME-FIELD = "Limit" THEN DO:
                   IF x = 1 AND INPUT DPConf.Limit[1] = 0 THEN DO:
                      MESSAGE " First Limit can't be 0 !" 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.

                   IF INPUT DPConf.Limit[x] = 0 AND
                      INPUT DPConf.DiscPrcnt[x] NE 0 THEN DO:
                      MESSAGE " Limit of Step " x SKIP
                      "must NOT be 0 !" 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.

                   IF INPUT DPConf.Limit[x] = 0 AND
                       INPUT DPConf.Limit[x + 1] = 0 THEN LEAVE.

                   IF INPUT DPConf.Limit[x] > 0 AND
                      INPUT DPConf.DiscPrcnt[x] = 0 AND
                      LOOKUP(KEYLABEL(LASTKEY),"cursor-down,cursor-up,f1") > 0  
                      THEN DO:
                      MESSAGE " Enter a valid  percent value !" 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.

                   IF x > 1 THEN DO:
                      IF INPUT DPConf.Limit[x] > 0 AND
                         INPUT DPConf.Limit[x] <= 
                         INPUT DPConf.Limit[x - 1] THEN DO:
                         MESSAGE 
                         "Invalid step value" 
                         INPUT DPConf.Limit[x] SKIP
                         "which is SMALLER THAN previous" SKIP
                         "step value " INPUT DPConf.Limit[x - 1] SKIP(1)
                         "Values MUST be in ascending order !"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END. /* x > 1 */
                   IF x < 10 THEN DO:
                      IF INPUT DPConf.Limit[x + 1] > 0 AND    
                         INPUT DPConf.Limit[x] >= 
                         INPUT DPConf.Limit[x + 1] THEN DO:
                         MESSAGE 
                         "Invalid step value" 
                         INPUT DPConf.Limit[x] SKIP
                         "which is BIGGER or SAME than next" SKIP
                         "step value " INPUT DPConf.Limit[x + 1] SKIP(1)
                         "Values MUST be in ascending order !"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END. /* x < 10 */
                END. /* Limit */  

                ELSE IF FRAME-FIELD = "DiscPrcnt" THEN DO:
                   IF INPUT DPConf.Limit[x] = 0 AND
                      INPUT DPConf.DiscPrcnt[x] = 0 THEN LEAVE.

                   IF INPUT DPConf.Limit[x] = 0 AND
                      INPUT DPConf.DiscPrcnt[x] NE 0 THEN DO:
                      MESSAGE 
                      "Discount percent of Step " x "NOT 0 !" SKIP
                      "BUT Limit is 0 !" SKIP
                      "Clear out value and come back !"
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.

                   IF INPUT DPConf.Limit[x] NE 0 AND
                      INPUT DPConf.DiscPrcnt[x] = 0 THEN DO:                  
                      MESSAGE 
                      "Discount Percent of Step" x SKIP
                      "must NOT be 0.00 !"
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.

                   IF x > 1 THEN DO:
                      IF INPUT DPConf.DiscPrcnt[x] <=
                         INPUT DPConf.DiscPrcnt[x - 1] THEN DO:
                         MESSAGE 
                         "Discount Percent of Step" x SKIP
                         "is SMALLER or SAME than previous !" SKIP(1)
                         "Values MUST be in ascending order !"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END. /* IF x > 1 */

                   IF x < 10 THEN DO:
                      IF INPUT DPConf.DiscPrcnt[x] >=
                         INPUT DPConf.DiscPrcnt[x + 1] AND
                         INPUT DPConf.DiscPrcnt[x + 1] NE 0 THEN DO:
                         MESSAGE 
                         "Discount Percent of Step" x SKIP
                         "is BIGGER or SAME then next !" SKIP(1)
                         "Values MUST be in ascending order !"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END. /* IF x < 10 */
                   END. /* DiscPrcnt */

             END.    /* LOOKUP    */
             APPLY LASTKEY.
          END. /* EDITING */

       END. /* lcRight */
      END. /* if not fixed */

      ELSE DO:
         PAUSE 0. 
         DISPLAY DPConf.DiscPrcnt[1] WITH OVERLAY FRAME lis3.
         IF lcRight = "RW" THEN DO:
            UPDATE DPConf.DiscPrcnt[1] WITH OVERLAY FRAME lis3.
            IF INPUT DPConf.DiscPrcnt[1] = 0 THEN DO:
               MESSAGE 
               "Fixed Discount Percent must NOT be 0.00 !"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.   
         END.
      END.

      IF lcRight NE "RW" THEN PAUSE.      

      HIDE FRAME lis.
      HIDE FRAME lis2.
      HIDE FRAME lis3.    

      LEAVE.
   END.
END PROCEDURE.
