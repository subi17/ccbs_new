/* ----------------------------------------------------------------------
  MODULE .......: fusionmessage.p
  TASK .........: Display fusionmessage 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 10.10.2016
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */
DEF INPUT PARAMETER iiOrderID  AS INT NO-UNDO.

{Syst/commali.i}
{Func/timestamp.i}

DEF VAR xrecid         AS RECID                           init ?.
DEF VAR FIRSTrow       AS INT                    NO-UNDO  init 0.
DEF VAR order          AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder       AS INT                    NO-UNDO  init 1.
DEF VAR ufkey          AS LOG                    NO-UNDO  init TRUE.
DEF VAR pr-order       AS INT                    NO-UNDO.
DEF VAR Memory         AS RECID                  NO-UNDO.
DEF VAR RowNo          AS INT                    NO-UNDO.
DEF VAR must-print     AS LOG                    NO-UNDO.
DEF VAR must-add       AS LOG                    NO-UNDO.
DEF VAR ac-hdr         AS CHAR                   NO-UNDO.
DEF VAR rtab           AS RECID EXTENT 24        NO-UNDO.
DEF VAR i              AS INT                    NO-UNDO.
DEF VAR lcCreatedTS    AS CHAR                   NO-UNDO.
DEF VAR lcDoneTS       AS CHAR                   NO-UNDO.
DEF VAR lcUpdatedTS    AS CHAR                   NO-UNDO.

FORM
   OrderTPService.Product      FORMAT "X(22)" COLUMN-LABEL "Product"
   OrderTPService.Type         FORMAT "X(15)" COLUMN-LABEL "Type"   
   OrderTPService.Status       FORMAT "X(8)"  COLUMN-LABEL "Status"
   lcCreatedTS                 FORMAT "X(24)" COLUMN-LABEL "Created"
   lcDoneTS                    FORMAT "X(24)" COLUMN-LABEL "Done"
   lcUpdatedTS                 FORMAT "X(24)" COLUMN-LABEL "Updated"
   OrderTPService.SerialNumber FORMAT "X(25)" COLUMN-LABEL "Serial Number"
   WITH ROW 1 CENTERED OVERLAY 15  DOWN COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) "Services" FRAME sel.

FORM
    "OrderID ...........:" OrderTPService.OrderID
    SKIP
    "Product ...........:" OrderTPService.Product
    SKIP
    "Service Type ......:" OrderTPService.Type
    SKIP
    "Service Provider ..:" OrderTPService.Provider
    SKIP
    "Service Status ....:" OrderTPService.Status
    SKIP
    "Serial Number .....:" OrderTPService.SerialNumber
    SKIP
    "Cancellation Reason:" OrderTPService.TermReason
    SKIP
    "Order Date ........:" lcCreatedTime FORMAT "X(24)"   
    SKIP
    "Activation Time ...:" lcActivationTime FORMAT "X(24)"
    SKIP
    "Handled Time ......:" lcDoneTime FORMAT "X(24)"
    SKIP
    "Updated Time ......:" lcUpdatedTime FORMAT "X(24)" 
    SKIP(4)
WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) OrderTPService.Type + " service data" NO-LABELS FRAME fDetails.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE OrderTPService THEN ASSIGN
   Memory       = recid(OrderTPService)
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
   END.
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND OrderTPService WHERE recid(OrderTPService) = Memory NO-LOCK NO-ERROR.


        REPEAT WITH FRAME sel:
           IF AVAILABLE OrderTPService THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OrderTPService).
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

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk    = 0
        ufk[4] = 0
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OrderTPService.Product {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OrderTPService.Product WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND OrderTPService WHERE recid(OrderTPService) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OrderTPService THEN
              ASSIGN FIRSTrow = i Memory = recid(OrderTPService).
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
           IF NOT AVAILABLE OrderTPService THEN DO:
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
                rtab[1] = recid(OrderTPService)
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
           IF NOT AVAILABLE OrderTPService THEN DO:
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
              rtab[FRAME-DOWN] = recid(OrderTPService).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OrderTPService WHERE recid(OrderTPService) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OrderTPService THEN DO:
           Memory = recid(OrderTPService).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OrderTPService THEN Memory = recid(OrderTPService).
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
           FIND OrderTPService WHERE recid(OrderTPService) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OrderTPService) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OrderTPService) must-print = TRUE.
        NEXT LOOP.
     END.
 
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).
        PAUSE 0. 

        ASSIGN
            lcCreatedTime    = fTS2HMS(OrderTPService.CreatedTS)  
            lcActivationTime = fTS2HMS(OrderTPService.ActivationTS)
            lcUpdatedTS      = fTS2HMS(OrderTPService.UpdatedTS)
            lcDoneTime       = fTS2HMS(OrderTPService.DoneTS).

        DISP OrderTPService.OrderID
             OrderTPService.Product
             OrderTPService.Type
             OrderTPService.Provider
             OrderTPService.Status
             OrderTPService.SerialNumber
             OrderTPService.TermReason
             lcCreatedTime
             lcActivationTime
             lcUpdatedTS
             lcDoneTime
             WITH FRAME fDetails.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN 
        FIND OrderTPService WHERE recid(OrderTPService) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
        FIND OrderTPService WHERE recid(OrderTPService) = rtab[frame-line(sel)] NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST OrderTPService WHERE OrderTPService.OrderId = iiOrderID NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST OrderTPService WHERE OrderTPService.OrderId = iiOrderID NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT OrderTPService WHERE OrderTPService.OrderId = iiOrderID NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV OrderTPService WHERE OrderTPService.OrderId = iiOrderID NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
    
    CLEAR FRAME sel NO-PAUSE.
    
    ASSIGN
        lcCreatedTime    = fTS2HMS(OrderTPService.CreatedTS)  
        lcActivationTime = fTS2HMS(OrderTPService.ActivationTS)
        lcUpdatedTS      = fTS2HMS(OrderTPService.UpdatedTS)
        lcDoneTime       = fTS2HMS(OrderTPService.DoneTS).

    DISPLAY 
       OrderTPService.Product
       OrderTPService.Type
       OrderTPService.Status
       lcCreatedTS
       lcDoneTS
       lcUpdatedTS
       OrderTPService.SerialNumber
       WITH FRAME sel.

END PROCEDURE.
