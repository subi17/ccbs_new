/* ----------------------------------------------------------------------
  MODULE .......: OrderSubscriptionView.p
  TASK .........: Display OrderSubscription data
  APPLICATION ..: TMS
  AUTHOR .......: Diego 
  CREATED ......: 14.6.2018
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT PARAMETER iiOrderId AS INT NO-UNDO.

DEF VAR firstrow    AS INT         NO-UNDO INITIAL 0.
DEF VAR FrmRow      AS INT         NO-UNDO INITIAL 1.
DEF VAR FrmDown     AS INT         NO-UNDO INITIAL 15.
DEF VAR order       AS INT         NO-UNDO INITIAL 1.
DEF VAR orders      AS CHAR        NO-UNDO.
DEF VAR maxOrder    AS INT         NO-UNDO INITIAL 1.
DEF VAR memory      AS RECID       NO-UNDO.
DEF VAR RowNo       AS INT         NO-UNDO.
DEF VAR must-print  AS LOG         NO-UNDO.
DEF VAR pr-order    AS INT         NO-UNDO.
DEF VAR ufkey       AS LOG         NO-UNDO INITIAL TRUE.
DEF VAR rtab        AS RECID       NO-UNDO EXTENT 24.
DEF VAR i           AS INT         NO-UNDO.
DEF VAR ldtCreateTS AS DATETIME-TZ NO-UNDO.
DEF VAR ldtUpdateTS AS DATETIME-TZ NO-UNDO.

DEF STREAM strOrdSub.

FORM
   OrderSubscription.OrderProductId   COLUMN-LABEL "Ord Prod Id" 
   OrderSubscription.cli              COLUMN-LABEL "MSISDN" FORMAT "x(10)"
   OrderSubscription.msseq            COLUMN-LABEL "SubscrID" 
   OrderSubscription.subscriptiontype COLUMN-LABEL "Type" FORMAT "X(4)"
   OrderSubscription.provisionstatus  COLUMN-LABEL "Sts" 
   ldtCreateTS                        COLUMN-LABEL "Created" FORMAT "99/99/99 HH:MM:SS"
   ldtUpdateTS                        COLUMN-LABEL "Updated" FORMAT "99/99/99 HH:MM:SS" 
   WITH ROW FrmRow WIDTH 80 OVERLAY FrmDown DOWN
   COLOR VALUE(Syst.Var:cfc)
   TITLE COLOR VALUE(Syst.Var:ctc) "Order subscription statuses for " + STRING(iiOrderId)
   FRAME sel.    
    
Syst.Var:cfc = "sel". 
RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = " By OrderProductId". /* add other sortings (and the code) if needed */

/* Pre-check */
IF iiOrderId = 0 THEN
DO:
   MESSAGE "Order ID not provided!" VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND FIRST OrderSubscription NO-LOCK WHERE
           OrderSubscription.OrderId EQ iiOrderId NO-ERROR. 
IF NOT AVAIL OrderSubscription THEN
DO:
   MESSAGE "Order Subscription statuses not found for this Order!" VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   memory      = RECID(OrderSubscription)
   must-print  = TRUE
   order       = 1. 

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
      PUT SCREEN ROW FrmRow + FrmDown + 3 COL 35 ENTRY(order,orders).
   END.


   PrintPage:
   DO:
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND OrderSubscription WHERE RECID(OrderSubscription) = memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record
            whose RECID is saved into 'memory'.
            starting from ROW 'delrow' */

         REPEAT WITH FRAME sel:
            IF AVAILABLE OrderSubscription THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = RECID(OrderSubscription).
               RUN local-find-NEXT.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
        
         UP FRAME-LINE - 1.
         DOWN firstrow.
        
         ASSIGN firstrow = 0
                must-print = FALSE.
         PAUSE 0 no-MESSAGE.

         /* Now there is one page displayed and the cursor is on the
           upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1] = 0 
            Syst.Var:ufk[2] = 0 
            Syst.Var:ufk[3] = 0 
            Syst.Var:ufk[4] = 0
            Syst.Var:ufk[5] = 0
            Syst.Var:ufk[6] = 0
            Syst.Var:ufk[7] = 0
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto = 3 
            ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      
      IF order = 1 THEN
      DO:
         CHOOSE ROW OrderSubscription.OrderProductId {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) OrderSubscription.OrderProductId WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN
      DO:
         /* add other displays if needed */
      END.
      
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = KEYLABEL(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN
      DO:
         order = order + 1. 
         IF order > maxOrder THEN order = 1.
      END.
      
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN
      DO:
         order = order - 1.
         IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN
      DO:
         ASSIGN 
            firstrow = 0 
            memory = rtab[FRAME-LINE].
         
         FIND OrderSubscription WHERE RECID(OrderSubscription) = memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE OrderSubscription THEN
               ASSIGN 
                  firstrow = i 
                  memory = RECID(OrderSubscription).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN
      DO:
         BELL.
         MESSAGE "You are on an empty row, move upwards !".
         PAUSE 1 no-MESSAGE.
         NEXT.
      END.

      Syst.Var:nap = KEYLABEL(LASTKEY).

      /* previous ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN
      DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN
         DO:
            RUN local-find-this(FALSE).
            RUN local-find-prev.
            IF NOT AVAILABLE OrderSubscription THEN
            DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. 
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE
            DO:
               /* previous was found */
               SCROLL DOWN.
               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
                  rtab[1] = RECID(OrderSubscription)
                  memory  = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN
      DO WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN
         DO:
            RUN local-find-this(FALSE).
            RUN local-find-NEXT.
            IF NOT AVAILABLE OrderSubscription THEN
            DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE
            DO:
               /* NEXT ROW was found */
               SCROLL UP.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(OrderSubscription).
               /* save RECID of uppermost ROW */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN
      DO WITH FRAME sel:
         memory = rtab[1].
         FIND OrderSubscription WHERE RECID(OrderSubscription) = memory NO-LOCK NO-ERROR.
         RUN local-find-prev.
         IF AVAILABLE OrderSubscription THEN
         DO:
            memory = RECID(OrderSubscription).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE OrderSubscription THEN memory = RECID(OrderSubscription).
               ELSE RowNo = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* is this the very FIRST record of the table ?  */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL. 
            PAUSE 1 NO-MESSAGE.
         END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN
     DO WITH FRAME sel:
        /* PUT Cursor on downmost ROW */
        IF rtab[FRAME-DOWN] = ? THEN
        DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL.
           PAUSE 1 NO-MESSAGE.
        END.
        ELSE
        DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND OrderSubscription WHERE RECID(OrderSubscription) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"CTRL-P") > 0 THEN
     DO:
        FIND FIRST OrderSubscription NO-LOCK WHERE
                   RECID(OrderSubscription) = rtab[FRAME-LINE] NO-ERROR.
        IF AVAIL OrderSubscription THEN
        DO:
           OUTPUT STREAM strOrdSub TO OrderSubscription.d APPEND.
           EXPORT STREAM strOrdSub OrderSubscription.
           OUTPUT STREAM strOrdSub CLOSE.
        END.
        APPLY LASTKEY.
     END.                           
     
     ELSE IF LOOKUP(Syst.Var:nap,"home,h") > 0 THEN
     DO:
        RUN local-find-FIRST.
        ASSIGN 
           memory = RECID(OrderSubscription) 
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,e") > 0 THEN
     DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN 
           memory = RECID(OrderSubscription) 
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.


PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS LOGICAL NO-UNDO.
    IF exlock THEN
       FIND OrderSubscription WHERE RECID(OrderSubscription) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND OrderSubscription WHERE RECID(OrderSubscription) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   FIND FIRST OrderSubscription NO-LOCK WHERE 
              OrderSubscription.OrderId = iiOrderId NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   FIND LAST OrderSubscription NO-LOCK WHERE 
             OrderSubscription.OrderId = iiOrderId NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   FIND NEXT OrderSubscription NO-LOCK WHERE 
             OrderSubscription.OrderId = iiOrderId NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
   FIND PREV OrderSubscription NO-LOCK WHERE 
             OrderSubscription.OrderId = iiOrderId NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
    
   /* set pvalue */
   ASSIGN
      ldtCreateTS = DATETIME-TZ(OrderSubscription.createdTS,TIMEZONE)
      ldtUpdateTS = DATETIME-TZ(OrderSubscription.updatedTS,TIMEZONE).
   
   DISPLAY
      OrderSubscription.OrderProductId 
      OrderSubscription.cli
      OrderSubscription.msseq 
      OrderSubscription.subscriptiontype
      OrderSubscription.provisionstatus 
      ldtCreateTS
      ldtUpdateTS
      WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.


