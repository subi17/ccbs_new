/* ----------------------------------------------------------------------
  MODULE .......: OrderTopup
  TASK .........: browse table OrderTopup
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 28.08.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{commali.i}
{timestamp.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'OrderTopup'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhOrderTopup AS HANDLE NO-UNDO.
   lhOrderTopup = BUFFER OrderTopup:HANDLE.
   RUN StarEventInitialize(lhOrderTopup).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhOrderTopup).
   END.

END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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

DEF BUFFER bOrderTopup FOR OrderTopup.
    
form
    OrderTopup.Amount   
WITH ROW FrmRow WIDTH 25 CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " TOPUPS OF ORDER " + STRING(iiOrderID) + " "
    FRAME sel.

form
    OrderTopup.Amount    COLON 22
    OrderTopup.VatAmount COLON 22 
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE OrderTopup THEN ASSIGN
   Memory       = recid(OrderTopup)
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
    
   IF must-add THEN DO:  /* Add a OrderTopup  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR OrderTopup.Amount WITH FRAME lis EDITING:
           
               READKEY. 
               nap = KEYLABEL(LASTKEY).
               APPLY LASTKEY.
           END.

           IF INPUT FRAME lis OrderTopup.Amount = ""
           THEN LEAVE add-row.

           IF CAN-FIND(OrderTopup USING OrderTopup.Amount) THEN DO:
              MESSAGE "OrderTopup already exists with code"     
                      INPUT FRAME lis OrderTopup.Amount
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           
           CREATE OrderTopup.
           OrderTopup.Amount  = INPUT OrderTopup.Amount.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderTopup).

           ASSIGN
           Memory = recid(OrderTopup)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE OrderTopup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND OrderTopup WHERE recid(OrderTopup) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OrderTopup THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OrderTopup).
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
        ufk    = 0
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OrderTopup.Amount ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OrderTopup.Amount WITH FRAME sel.
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
        FIND OrderTopup WHERE recid(OrderTopup) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OrderTopup THEN
              ASSIGN FIRSTrow = i Memory = recid(OrderTopup).
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
           IF NOT AVAILABLE OrderTopup THEN DO:
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
                rtab[1] = recid(OrderTopup)
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
           IF NOT AVAILABLE OrderTopup THEN DO:
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
              rtab[FRAME-DOWN] = recid(OrderTopup).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OrderTopup WHERE recid(OrderTopup) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OrderTopup THEN DO:
           Memory = recid(OrderTopup).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OrderTopup THEN Memory = recid(OrderTopup).
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
           FIND OrderTopup WHERE recid(OrderTopup) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       OrderTopup.Amount.

       RUN local-find-NEXT.
       IF AVAILABLE OrderTopup THEN Memory = recid(OrderTopup).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE OrderTopup THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(OrderTopup).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       OrderTopup.Amount.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOrderTopup).

           DELETE OrderTopup.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE OrderTopup THEN DO:
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
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderTopup).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderTopup).

       RUN local-disp-row.
       xrecid = recid(OrderTopup).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OrderTopup) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OrderTopup) must-print = TRUE.
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
      FIND OrderTopup WHERE recid(OrderTopup) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OrderTopup WHERE recid(OrderTopup) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST OrderTopup WHERE
          OrderTopUp.Brand   = gcBrand AND
          OrderTopUp.OrderID = iiOrderID
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST OrderTopup WHERE
          OrderTopUp.Brand   = gcBrand AND
          OrderTopUp.OrderID = iiOrderID
          NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT OrderTopup WHERE
          OrderTopUp.Brand   = gcBrand AND
          OrderTopUp.OrderID = iiOrderID
          NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV OrderTopup WHERE
          OrderTopUp.Brand   = gcBrand AND
          OrderTopUp.OrderID = iiOrderID
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       OrderTopup.Amount
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      OrderTopup.Amount
      OrderTopup.VatAmount
      WITH FRAME lis.
      
      IF lcRight = "RW" AND FALSE 
      THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN ufkey.
      
         UPDATE
         OrderTopup.VatAmount 
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "VatAmount" THEN DO:
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */

         LEAVE. 
      END.
      
      ELSE DO:
         ehto = 5.
         RUN ufkey.
         PAUSE MESSAGE "Press ENTER to continue".
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.

