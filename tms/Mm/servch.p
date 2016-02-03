/*--------------------------------------------------------------------
  MODULE .......: SERVCH.P
  TASK .........: choose services for order
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 04.04.03
  CHANGED ......: 16.09.03 Brand 
                  21.11.06/aam OrderServices  
  VERSION ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Order'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER iiOrderId AS INT NO-UNDO.

DEF VAR ServCom      LIKE ServCom.ServCom        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
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
DEF VAR crundo       AS LOG                    NO-UNDO INIT FALSE.
DEF VAR lSave        AS LOG format "Yes/No"    NO-UNDO.
DEF VAR lcServices   AS CHAR                   NO-UNDO.

DEF TEMP-TABLE ttService
    field ServCom like ServCom.ServCom
    field ScName  like ServCom.ScName
    field chosen    as log format "*/"
    INDEX ServCom IS PRIMARY ServCom 
    INDEX ScName ScName ServCom.


form
    ttService.ServCom  COLUMN-LABEL "Service"
    ttService.ScName   COLUMN-LABEL "Service name" 
    ttService.chosen   COLUMN-LABEL "CH"
WITH ROW FrmRow centered OVERLAY 12 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " CHOOSE SERVICES " 
    FRAME sel.

form /* seek ServCom by Code */
    ServCom
    help "Enter ServCom Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".

FIND FIRST ServCom WHERE 
           ServCom.Brand = gcBrand NO-LOCK NO-ERROR.
IF NOT AVAILABLE ServCom THEN  DO:
   MESSAGE "No Service components available !" view-as alert-box.
   return.
END.

FIND Order WHERE 
     Order.Brand = gcBrand AND 
     Orderid     = iiOrderId 
NO-LOCK NO-ERROR.

for each ServCom no-lock WHERE 
         ServCom.Brand = gcBrand :
   create ttService.
   assign
     ttService.ServCom = ServCom.ServCom
     ttService.ScName  = ServCom.ScName.

   IF CAN-FIND(ServEl WHERE 
               ServEl.Brand   = gcBrand AND 
               ServEl.ServPac = "*1"    AND
               ServEl.ServCom = ServCom.ServCom) 
   THEN ttService.chosen = TRUE.

end.

RUN local-find-first.
ASSIGN
   memory       = recid(ttService)
   must-print   = TRUE
   must-add     = FALSE
   pr-order     = order.

LOOP:
REPEAT WITH FRAME sel:

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttService WHERE recid(ttService) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttService THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttService).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 35 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 11 ELSE 0) 
        ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttService.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttService.ServCom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttService.ScName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttService.ScName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY). 

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE]
        pr-order = order.
        FIND ttService WHERE recid(ttService) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE ttService THEN
              ASSIGN FIRSTrow = i memory = recid(ttService).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE ttService THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttService)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttService THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ttService).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttService WHERE recid(ttService) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE ttService THEN DO:
           memory = recid(ttService).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE ttService THEN memory = recid(ttService).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
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
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND ttService WHERE recid(ttService) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ServCom = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE ServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServCom <> "" THEN DO:
          FIND FIRST ttService WHERE 
                     ttService.ServCom >= ServCom
           NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttService THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ServCom/vc-code was found */
          ASSIGN order = 1 memory = recid(ttService) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 AND lcRight = "RW" THEN DO:
       RUN local-find-this(TRUE).
       ttService.chosen = NOT(ttService.chosen).
       RUN local-disp-row.
       xrecid = recid(ttService).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttService) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttService) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

FOR EACH ttService WHERE
         ttService.chosen = TRUE
NO-LOCK:

    FIND FIRST OrderService OF Order EXCLUSIVE-LOCK WHERE
               OrderService.Service = ttService.ServCom NO-ERROR.
    IF NOT AVAILABLE OrderService THEN DO:
       CREATE OrderService.
       ASSIGN OrderService.Brand   = gcBrand
              OrderService.OrderID = Order.OrderID
              OrderService.Service = ttService.ServCom.
    END.
 
    OrderService.ServValue = "1".
END.

/* delete excessive */
FOR EACH OrderService OF Order EXCLUSIVE-LOCK:

   FIND FIRST ttService WHERE ttService.ServCom = OrderService.Service
      NO-ERROR.
   IF NOT AVAILABLE ttService OR ttService.Chosen = FALSE
   THEN DELETE OrderService.
END.


PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND ttService WHERE recid(ttService) = 
              rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND ttService WHERE recid(ttService) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN FIND FIRST ttService NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST ttService USE-INDEX ScName
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST ttService NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST ttService USE-INDEX ScName
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT ttService NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT ttService USE-INDEX ScName
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
   IF order = 1 THEN FIND prev ttService NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND prev ttService USE-INDEX ScName
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   DISPLAY
      ttService.ServCom
      ttService.ScName
      ttService.chosen
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.




