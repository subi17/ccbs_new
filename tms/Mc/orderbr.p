/* ----------------------------------------------------------------------
  MODULE .......: Order
  TASK .........: UPDATEs table Order
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 30.12.02
  CHANGED ......: 19.09.03/aam brand
                  03.10.03/aam input custnum and salesman
                  06.10.03/aam ContrType, CloseDate, FeeModel
                  13.10.03/aam tokens
  Version ......: M15
  ---------------------------------------------------------------------- */
/* &GLOBAL-DEFINE BrTable Order */

{Syst/commali.i}
{Func/timestamp.i}


DEFINE TEMP-TABLE ttOrder NO-UNDO
 FIELD OrderId    AS INTEGER
 FIELD TimeStamp  AS DECIMAL
 FIELD Cli        AS CHARACTER
 FIELD Contract   AS CHARACTER
 FIELD StatusCode AS CHARACTER
 FIELD CustId     AS CHARACTER
 INDEX OrderId OrderId.

      
      
DEF INPUT PARAMETER icCustId     AS CHAR NO-UNDO.
DEF INPUT PARAMETER icCType      AS CHAR NO-UNDO.
DEF INPUT PARAMETER icStatusCode AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oOrderId AS INTEGER NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR lcTimeStamp  AS CHARACTER              NO-UNDO FORMAT "x(19)".

form
    ttOrder.OrderId    FORMAT "zzzzzzz9"
    lcTimeStamp        LABEL "Created"
    ttOrder.Cli        LABEL "MSISDN" FORMAT "x(9)"
    ttOrder.Custid     FORMAT "x(10)"
    ttOrder.Contract    
    ttOrder.StatusCode

WITH ROW 4 width 80 OVERLAY 8 DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " Customers orders "  + string(pvm,"99-99-99") + " "
    FRAME sel.

/* Create orders for browsing */ 
FOR EACH OrderCustomer NO-LOCK WHERE
         OrderCustomer.CustId     = icCustId  AND
         OrderCustomer.CustIdType = icCType   AND
         OrderCustomer.Brand      = "1" AND
         OrderCustomer.RowType = 1:
                           
   FIND FIRST Order WHERE
              Order.OrderId = OrderCustomer.OrderId AND
              (IF icStatusCode > "" THEN
              Order.StatusCode = icStatusCode ELSE TRUE) AND
              Order.Brand = "1"
   NO-LOCK NO-ERROR.
                                             
   IF AVAILABLE Order THEN DO:
      CREATE ttOrder.
      ASSIGN ttOrder.TimeStamp  = Order.CrStamp
             ttOrder.OrderId    = Order.OrderId
             ttOrder.Cli        = Order.Cli
             ttOrder.Contract   = Order.ContractId
             ttOrder.StatusCode = Order.StatusCode
             ttOrder.CustId     = OrderCustomer.CustId.
   END.
END.

RUN local-find-first.

IF AVAILABLE ttOrder THEN ASSIGN
   Memory       = recid(ttOrder)
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

PrintPage:
   DO :
      IF must-print THEN DO:
        FIND ttOrder WHERE recid(ttOrder) = Memory NO-LOCK NO-ERROR.
        
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.
        
        REPEAT WITH FRAME sel:
           IF AVAILABLE ttOrder THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttOrder).
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
        ufk[1] = 0  
        ufk[2] = 0  
        ufk[3] = 0 
        ufk[4] = 0
        ufk[5] = 0
        ufk[6] = 0
        ufk[7] = 0
        ufk[8] = 8
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttOrder.OrderId {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttOrder.OrderId WITH FRAME sel.
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

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttOrder WHERE recid(ttOrder) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttOrder THEN
              ASSIGN FIRSTrow = i Memory = recid(ttOrder).
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
           IF NOT AVAILABLE ttOrder THEN DO:
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
                rtab[1] = recid(ttOrder)
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
           IF NOT AVAILABLE ttOrder THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttOrder).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttOrder WHERE recid(ttOrder) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttOrder THEN DO:
           Memory = recid(ttOrder).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttOrder THEN Memory = recid(ttOrder).
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
           FIND ttOrder WHERE recid(ttOrder) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:
        
        RUN local-find-THIS(FALSE).
        
        oOrderId = ttOrder.OrderId.
        
        LEAVE LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttOrder) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttOrder) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */
PAUSE 0.
HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttOrder WHERE recid(ttOrder) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttOrder WHERE recid(ttOrder) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   FIND FIRST ttOrder NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-LAST:

   FIND LAST ttOrder NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   FIND NEXT ttOrder NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-PREV:

   FIND PREV ttOrder NO-LOCK.
   
END PROCEDURE.

PROCEDURE local-disp-row:

       lcTimeStamp  = fTS2HMS(ttOrder.TimeStamp).
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       
        ttOrder.OrderId
        lcTimeStamp
        ttOrder.Contract
        ttOrder.StatusCode
        ttOrder.CustId
       
       WITH FRAME sel.

END PROCEDURE.


