/* ----------------------------------------------------------------------
  MODULE .......: ActivityCounter
  TASK .........: UPDATEs table ActivityCounter
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.08.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ActivityCounter

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CDRStreamCounter'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhActivityCounter AS HANDLE NO-UNDO.
   lhActivityCounter = BUFFER ActivityCounter:HANDLE.
   RUN StarEventInitialize(lhActivityCounter).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhActivityCounter).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ldaACDate AS DATE                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcField          AS CHAR NO-UNDO. 
DEF VAR lcCode           AS CHAR NO-UNDO. 


FORM
    ActivityCounter.ACDate          COLUMN-LABEL "Date"
    ActivityCounter.ACValue         COLUMN-LABEL "Value"     FORMAT "X(15)" 
    ActivityCounter.ACSubValue      COLUMN-LABEL "SubValue"  FORMAT "X(25)"
    ActivityCounter.DecValue        COLUMN-LABEL "Value"
    ActivityCounter.IntValue        COLUMN-LABEL "Value"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  ACTIVITY COUNTERS  "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    ActivityCounter.Brand         COLON 15
    ActivityCounter.ACDate        COLON 15  LABEL "Date"
    ActivityCounter.ACValue       COLON 15  LABEL "Value"
    ActivityCounter.ACSubValue    COLON 15  LABEL "SubValue"
    ActivityCounter.DecValue      COLON 15  LABEL "Value"
    ActivityCounter.IntValue      COLON 15  LABEL "Value"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Date :" ldaACDate FORMAT "99-99-9999" 
    HELP "Enter import date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE ActivityCounter THEN ASSIGN
   Memory       = recid(ActivityCounter)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No counters available" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ActivityCounter WHERE recid(ActivityCounter) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ActivityCounter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ActivityCounter).
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
        ufk   = 0
        ufk[1]= 816 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ActivityCounter.ACDate ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ActivityCounter.ACDate WITH FRAME sel.
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
        FIND ActivityCounter WHERE recid(ActivityCounter) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ActivityCounter THEN
              ASSIGN FIRSTrow = i Memory = recid(ActivityCounter).
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
           IF NOT AVAILABLE ActivityCounter THEN DO:
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
                rtab[1] = recid(ActivityCounter)
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
           IF NOT AVAILABLE ActivityCounter THEN DO:
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
              rtab[FRAME-DOWN] = recid(ActivityCounter).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ActivityCounter WHERE recid(ActivityCounter) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ActivityCounter THEN DO:
           Memory = recid(ActivityCounter).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ActivityCounter THEN Memory = recid(ActivityCounter).
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
           FIND ActivityCounter WHERE recid(ActivityCounter) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           ldaACDate WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF ldaACDate NE ? THEN DO:
          FIND FIRST ActivityCounter WHERE 
                     ActivityCounter.Brand = lcBrand AND
                     ActivityCounter.ACDate >= ldaACDate
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ActivityCounter.ACDate 
       ActivityCounter.ACValue.

       RUN local-find-NEXT.
       IF AVAILABLE ActivityCounter THEN Memory = recid(ActivityCounter).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ActivityCounter THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ActivityCounter).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ActivityCounter.ACDate
       ActivityCounter.ACValue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhActivityCounter).

           DELETE ActivityCounter.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ActivityCounter WHERE 
                                 ActivityCounter.Brand = lcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhActivityCounter).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ActivityCounter.ACDate.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhActivityCounter).

       RUN local-disp-row.
       xrecid = recid(ActivityCounter).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ActivityCounter) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ActivityCounter) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ActivityCounter WHERE recid(ActivityCounter) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ActivityCounter WHERE recid(ActivityCounter) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ActivityCounter 
          WHERE ActivityCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ActivityCounter
          WHERE ActivityCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ActivityCounter
          WHERE ActivityCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ActivityCounter
          WHERE ActivityCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ActivityCounter.ACDate 
       ActivityCounter.ACValue
       ActivityCounter.ACSubValue   
       ActivityCounter.DecValue
       ActivityCounter.IntValue
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bConf FOR ActivityCounter.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP ActivityCounter.Brand
           ActivityCounter.ACDate
           ActivityCounter.ACValue
           ActivityCounter.ACSubValue 
           ActivityCounter.DecValue
           ActivityCounter.IntValue
       WITH FRAME lis.

      
      IF NOT NEW ActivityCounter THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.
   
   END.
   
END PROCEDURE.

