/* ----------------------------------------------------------------------
  MODULE .......: CDRStreamCounter
  TASK .........: UPDATEs table CDRStreamCounter
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.08.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable CDRStreamCounter

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CDRStreamCounter'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCDRStreamCounter AS HANDLE NO-UNDO.
   lhCDRStreamCounter = BUFFER CDRStreamCounter:HANDLE.
   RUN StarEventInitialize(lhCDRStreamCounter).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCDRStreamCounter).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ldaImportDate AS DATE                   NO-UNDO.
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
    CDRStreamCounter.ImportDate  
    CDRStreamCounter.CounterType 
    CDRStreamCounter.MSCID        FORMAT "X(5)"
    CDRStreamCounter.OnlineStream
    CDRStreamCounter.CDRQty
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       "  CDR STREAM COUNTERS  "  + string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    CDRStreamCounter.Brand         COLON 15
    CDRStreamCounter.ImportDate    COLON 15
    CDRStreamCounter.CounterType   COLON 15
    CDRStreamCounter.MSCID         COLON 15
    CDRStreamCounter.OnlineStream  COLON 15
    CDRStreamCounter.CDRQty        COLON 15
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Date :" ldaImportDate FORMAT "99-99-9999" 
    HELP "Enter import date"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Date"
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE CDRStreamCounter THEN ASSIGN
   Memory       = recid(CDRStreamCounter)
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
        FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CDRStreamCounter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CDRStreamCounter).
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
        Syst.Var:ufk   = 0
        Syst.Var:ufk[1]= 816 
        Syst.Var:ufk[7]= 0  
        Syst.Var:ufk[8]= 8 
        Syst.Var:ehto  = 3 
        ufkey = FALSE.
        
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CDRStreamCounter.ImportDate {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) CDRStreamCounter.ImportDate WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CDRStreamCounter THEN
              ASSIGN FIRSTrow = i Memory = recid(CDRStreamCounter).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE CDRStreamCounter THEN DO:
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
                rtab[1] = recid(CDRStreamCounter)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE CDRStreamCounter THEN DO:
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
              rtab[FRAME-DOWN] = recid(CDRStreamCounter).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CDRStreamCounter THEN DO:
           Memory = recid(CDRStreamCounter).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CDRStreamCounter THEN Memory = recid(CDRStreamCounter).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN Syst.Var:gcAllBrand 
           ldaImportDate WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF ldaImportDate NE ? THEN DO:
          FIND FIRST CDRStreamCounter WHERE 
                     CDRStreamCounter.Brand = lcBrand AND
                     CDRStreamCounter.ImportDate >= ldaImportDate
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       CDRStreamCounter.ImportDate 
       CDRStreamCounter.CounterType.

       RUN local-find-NEXT.
       IF AVAILABLE CDRStreamCounter THEN Memory = recid(CDRStreamCounter).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CDRStreamCounter THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CDRStreamCounter).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       CDRStreamCounter.ImportDate
       CDRStreamCounter.CounterType.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCDRStreamCounter).

           DELETE CDRStreamCounter.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CDRStreamCounter WHERE 
                                 CDRStreamCounter.Brand = lcBrand) 
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

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCDRStreamCounter).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CDRStreamCounter.ImportDate.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCDRStreamCounter).

       RUN local-disp-row.
       xrecid = recid(CDRStreamCounter).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CDRStreamCounter) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CDRStreamCounter) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

Syst.Var:ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CDRStreamCounter WHERE recid(CDRStreamCounter) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CDRStreamCounter 
          WHERE CDRStreamCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CDRStreamCounter
          WHERE CDRStreamCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CDRStreamCounter
          WHERE CDRStreamCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CDRStreamCounter
          WHERE CDRStreamCounter.Brand = lcBrand
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       CDRStreamCounter.ImportDate 
       CDRStreamCounter.CounterType
       CDRStreamCounter.MSCID   
       CDRStreamCounter.OnlineStream
       CDRStreamCounter.CDRQty
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bConf FOR CDRStreamCounter.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP CDRStreamCounter.Brand
           CDRStreamCounter.ImportDate
           CDRStreamCounter.CounterType
           CDRStreamCounter.MSCID 
           CDRStreamCounter.OnlineStream
           CDRStreamCounter.CDRQty
       WITH FRAME lis.

      
      IF NOT NEW CDRStreamCounter THEN DO:
         ASSIGN 
            Syst.Var:ufk    = 0
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto   = 0.
         
         RUN Syst/ufkey.p.
         
         IF Syst.Var:toimi = 8 THEN LEAVE.
      END.
   
   END.
   
END PROCEDURE.

