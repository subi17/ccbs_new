/* ----------------------------------------------------------------------
  MODULE .......: DISCPLAN.P
  TASK .........: Discount Planner
  APPLICATION ..: TMS
  AUTHOR .......: pt
  CREATED ......: 11-06-99
  CHANGED ......: 21.09.99 pt  F3: Volume Discounts
                  05.10.99 jp  urights added  
                  09.08.02 tk  use DPConf instead of voldisc
                  13.09.02/aam eventlog
                  03.03.03/aam delete DPConf and DPBasis
                  10.03.03/tk  tokens
                  18.03.03/tk  new memo
                  10.09.03/tk  brand
                  06.02.04 jp  custnum for memo

  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DiscPlan

{commali.i} 
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'DiscPlan'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDiscPlan AS HANDLE NO-UNDO.
   lhDiscPlan = BUFFER DiscPlan:HANDLE.
   RUN StarEventInitialize(lhDiscPlan).

   DEFINE VARIABLE lhDPConf AS HANDLE NO-UNDO.
   lhDPConf = BUFFER DPConf:HANDLE.
   RUN StarEventInitialize(lhDPConf).

   DEFINE VARIABLE lhDPBasis AS HANDLE NO-UNDO.
   lhDPBasis = BUFFER DPBasis:HANDLE.
   RUN StarEventInitialize(lhDPBasis).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhDiscPlan).
   END.

END.


def /* new */ shared var siirto AS char.

DEF VAR DiscPlan  like discplan.DiscPlan  NO-UNDO.
DEF VAR DPName like discplan.DPName NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    discplan.Brand
    discplan.DiscPlan      /* column-label format */
    discplan.DPName     /* column-label format */
             /* column-label format */
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " Discount Plans "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    discplan.brand
    discplan.DiscPlan    /* label format */
    discplan.DPName    /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) ac-hdr 
    side-labels 
    1 columns
    FRAME lis.

form /* seek DiscPlan  by  DiscPlan */
    "Brand:" lcBrand skip
    "Code :" DiscPlan
    help "Enter Discount Plan's code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) no-labels overlay FRAME f1.

form /* seek DiscPlan  by DPName */
    "Brand:" lcBrand skip 
    "Name :" DPName 
    help "Enter Discount Plan's name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) no-labels overlay FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".


RUN local-find-first.
IF AVAILABLE discplan THEN ASSIGN
   memory       = recid(discplan)
   must-print   = true
   must-add     = false.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No discount plans available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = false
      must-add     = true.
END.      

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       put screen row FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a discplan  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, leave ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis 
        ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:

           CLEAR FRAME lis NO-PAUSE.
           DISPLAY lcBrand @ discplan.Brand.

           PROMPT-FOR discplan.DiscPlan
           validate
              (discplan.DiscPlan NOT ENTERED or
              NOT CAN-FIND(discplan using  discplan.DiscPlan where
                           discplan.brand = lcbrand),
              "DiscPlan " + string(INPUT discplan.DiscPlan) +
              " already exists !").
           IF discplan.DiscPlan NOT ENTERED THEN LEAVE add-row.
           create discplan.
           ASSIGN
           discplan.Brand = lcBrand
           discplan.DiscPlan = INPUT FRAME lis discplan.DiscPlan.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDiscPlan).

           ASSIGN
           memory = recid(discplan)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST discplan
      WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE discplan THEN leave LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND discplan WHERE recid(discplan) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE discplan THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(discplan).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN leave.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 235 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row discplan.DiscPlan ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) discplan.DiscPlan WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row discplan.DPName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) discplan.DPName WITH FRAME sel.
      END.
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND discplan WHERE recid(discplan) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-prev.
           IF AVAILABLE discplan THEN
              ASSIGN FIRSTrow = i memory = recid(discplan).
           ELSE leave.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* previous row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-prev.
           IF NOT AVAILABLE discplan THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(discplan)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE discplan THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(discplan).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND discplan WHERE recid(discplan) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE discplan THEN DO:
           memory = recid(discplan).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-prev.
              IF AVAILABLE discplan THEN memory = recid(discplan).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND discplan WHERE recid(discplan) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME f1.
       SET lcBrand WHEN gcAllBrand
           DiscPlan WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF DiscPlan ENTERED THEN DO:
          FIND FIRST discplan WHERE 
                     discplan.brand = lcBrand AND
                     discplan.DiscPlan >= DiscPlan 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f2.
       disp lcBrand with frame f2.
       SET lcBrand WHEN gcAllBrand
           DPName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF DPName ENTERED THEN DO:
          FIND FIRST discplan WHERE 
                     discplan.brand = lcBrand AND 
                     discplan.DPName >= DPName
          USE-INDEX DPName NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Update discounts  */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        run local-find-this(false).
        RUN dpconf (discplan.DiscPlan).
        ufkey = true.
        NEXT loop.
     END.

     /* Update Memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        run local-find-this(FALSE).
        RUN memo(INPUT 0,
                 INPUT "DiscPlan",
                 INPUT STRING(DiscPlan.DiscPlan),
                 INPUT "Discount Plan").
        ufkey = TRUE.
        NEXT LOOP.

     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-line.
       RUN local-find-this (false).

       IF CAN-FIND(FIRST dpconf OF discplan) THEN DO:
          MESSAGE
          "There are one or more DISCOUNTS"  SKIP
          "defined for this Discount Plan"
          VIEW-AS ALERT-BOX
          TITLE " DELETE NOT ALLOWED ".
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       discplan.brand 
       discplan.DiscPlan 
       discplan.DPName . 

       RUN local-find-NEXT.
       IF AVAILABLE discplan THEN memory = recid(discplan).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-prev.
          IF AVAILABLE discplan THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(discplan).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       discplan.brand
       discplan.DiscPlan 
       discplan.DPName .
       IF ok THEN DO:

           FOR EACH DPConf OF DiscPlan:

              FOR EACH DPBasis OF DPConf:
                 IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPBasis).
                 DELETE DPBasis.
              END.

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPConf).
              DELETE DPConf.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDiscPlan).
           DELETE discplan.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST discplan
           WHERE discplan.brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              leave LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY discplan.DiscPlan.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDiscPlan).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* If  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDiscPlan).

       RUN local-disp-row.
       xrecid = recid(discplan).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(discplan) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(discplan) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN leave LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo no-undo.

    if exlock then
      find discplan WHERE recid(discplan) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find discplan WHERE recid(discplan) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST discplan
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST discplan USE-INDEX DPName
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST discplan
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST discplan USE-INDEX DPName
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT discplan
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT discplan USE-INDEX DPName
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev discplan
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev discplan USE-INDEX DPName
       WHERE discplan.brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       discplan.brand
       discplan.DiscPlan
       discplan.DPName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          discplan.brand
          discplan.discplan
      WITH FRAME lis.
      UPDATE
          discplan.DPName

      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.
