/* ----------------------------------------------------------------------
  MODULE .......: orderdelivery.p 
  TASK .........: Displays order logistic history.
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 08/2008
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Func/timestamp.i}
{Func/transname.i}

DEFINE INPUT PARAMETER iiOrderId AS INTEGER NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 3.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 12.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.

DEFINE VARIABLE llAdmin      AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcLOStatus   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLO         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCourier    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncidentInfoId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMeasuresInfoId AS CHARACTER NO-UNDO. 

IF getTMSRight("CCSUPER,SYST") EQ "RW" THEN llAdmin = TRUE.
FIND FIRST order NO-LOCK.
FORM
    OrderDelivery.LOTimeStamp label "Time" format "99.99.9999 hh:mm:ss"
    lcLOStatus COLUMN-LABEL "LO Status" FORMAT "x(25)"
    lcLO       COLUMN-LABEL "LO" FORMAT "x(6)" 
    lcCourier  COLUMN-LABEL "Courier" FORMAT "x(7)"
    OrderDelivery.CourierShippingId COLUMN-LABEL
      "CourierShippingId" FORMAT "x(14)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    TITLE
    " LOGISTIC OPERATOR HISTORY OF ORDER "
    + string(iiOrderId) + " "
    FRAME sel.

FORM
    "Order ID ..........:" OrderDelivery.OrderId SKIP 
    "TimeStamp .........:" OrderDelivery.LOTimeStamp FORMAT "99-99-9999 hh:mm:ss"
    SKIP
    "LO Status .........:" OrderDelivery.LOStatusId lcLOStatus FORMAT "x(30)" SKIP
    "Logistic Operator..:" OrderDelivery.LOId lcLO FORMAT "x(30)" SKIP
    "Courier  ..........:" OrderDelivery.CourierId lcCourier FORMAT "x(30)" SKIP
    "Courier Shipping Id:" OrderDelivery.CourierShippingId FORMAT "x(30)" SKIP
    "Incident info......:" OrderDelivery.IncidentInfoId lcIncidentInfoId FORMAT "x(30)" SKIP
    "Measures info......:" OrderDelivery.MeasuresInfoId lcMeasuresInfoId FORMAT "x(30)" SKIP
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.


RUN local-find-first.
IF AVAILABLE OrderDelivery THEN ASSIGN
   Memory       = recid(OrderDelivery)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No entries available!" 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND OrderDelivery WHERE recid(OrderDelivery) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OrderDelivery THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OrderDelivery).
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
        ufk = 0
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OrderDelivery.LOTimeStamp {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OrderDelivery.LOTimeStamp WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND OrderDelivery WHERE recid(OrderDelivery) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OrderDelivery THEN
              ASSIGN FIRSTrow = i Memory = recid(OrderDelivery).
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

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE OrderDelivery THEN DO:
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
                rtab[1] = recid(OrderDelivery)
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
           IF NOT AVAILABLE OrderDelivery THEN DO:
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
              rtab[FRAME-DOWN] = recid(OrderDelivery).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OrderDelivery WHERE recid(OrderDelivery) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OrderDelivery THEN DO:
           Memory = recid(OrderDelivery).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OrderDelivery THEN Memory = recid(OrderDelivery).
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
           FIND OrderDelivery WHERE recid(OrderDelivery) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
      
    /* view */
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:

       RUN local-view-record.
       ufkey = TRUE. 
       NEXT LOOP.

     END.
     
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OrderDelivery) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OrderDelivery) must-print = TRUE.
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
      FIND OrderDelivery WHERE recid(OrderDelivery) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OrderDelivery WHERE recid(OrderDelivery) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST OrderDelivery WHERE 
         OrderDelivery.Brand = gcBrand AND
         OrderDelivery.OrderId = iiOrderID USE-INDEX OrderId NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST OrderDelivery WHERE 
         OrderDelivery.Brand = gcBrand AND
         OrderDelivery.OrderId = iiOrderID USE-INDEX OrderId NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT OrderDelivery WHERE 
         OrderDelivery.Brand = gcBrand AND
         OrderDelivery.OrderId = iiOrderID USE-INDEX OrderId NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV OrderDelivery WHERE 
         OrderDelivery.Brand = gcBrand AND
         OrderDelivery.OrderId = iiOrderID USE-INDEX OrderId NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      OrderDelivery.LOTimeStamp
      OrderDelivery.CourierShippingId
      lcLOStatus
      lcLO
      lcCourier
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
      
   lcLOStatus = fGetItemName( 
      gcBrand, 
      "LOStatusId", 
      STRING(OrderDelivery.LOStatusId),
      5,
      TODAY).
   
   lcLO = fGetItemName( 
      gcBrand, 
      "LOId", 
      STRING(OrderDelivery.LOId),
      5,
      TODAY).
   
   lcCourier = fGetItemName( 
      gcBrand, 
      "CourierId", 
      STRING(OrderDelivery.CourierId),
      5,
      TODAY).
   
   lcIncidentInfoId = fGetItemName( 
      gcBrand, 
      "IncidentInfoId", 
      STRING(OrderDelivery.IncidentInfoId),
      5,
      TODAY).
   
   lcMeasuresInfoId = fGetItemName( 
      gcBrand, 
      "MeasuresInfoId", 
      STRING(OrderDelivery.MeasuresInfoId),
      5,
      TODAY).

END PROCEDURE.

PROCEDURE local-view-record:

   ufk = 0.
   RUN Syst/ufkey.

   RUN local-find-this(FALSE).
  
   CLEAR FRAME lis NO-PAUSE.
   
   RUN local-find-others.

   PAUSE 0.

   DISP 
      OrderDelivery.OrderId
      OrderDelivery.LOTimeStamp
      OrderDelivery.LOId lcLO
      OrderDelivery.CourierId lcCourier
      OrderDelivery.LOStatusId lcLoStatus
      OrderDelivery.CourierShippingId
      /* 9 = 'Courier Incident' */
      OrderDelivery.IncidentInfoId WHEN OrderDelivery.LoStatusId EQ 9
         lcIncidentInfoId
      OrderDelivery.MeasuresInfoId WHEN OrderDelivery.LoStatusId EQ 9
         lcMeasuresInfoId
   WITH FRAME lis.

   PAUSE MESSAGE "Press ENTER to continue".
  
   HIDE FRAME lis NO-PAUSE. 

END PROCEDURE.
