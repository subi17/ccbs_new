/* ----------------------------------------------------------------------
  MODULE .......: SimBatch.P
  TASK .........: SimBatch
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 19-07-99
  CHANGED ......: 07-10-99 jp urights added
                  07.03.03 tk eventlog + tokens
                  09.09.03 jp brand
                  02.08.05 kl do not disp simbatch
                  03.08.05 mvi call sim.p with the batchnum as parameter

  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable SimBatch

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SimBatch'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAl-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSIMBatch AS HANDLE NO-UNDO.
   lhSIMBatch = BUFFER SIMBatch:HANDLE.
   RUN StarEventInitialize(lhSIMBatch).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhSIMBatch).
   END.


END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Mancode  LIKE SimBatch.ManCode  NO-UNDO.
DEF VAR SimArt LIKE SimBatch.SimArt NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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

form
    SimBatch.Brand       FORMAT "x(4)"
    SimBatch.SimArt    column-label "SourceFile" format "x(18)"
    SimBatch.ManCode   column-label "ManCode"
    SimMan.ManName     format "x(10)" column-label "Manuf's Name"
    SimBatch.DelDate          COLUMN-LABEL "Del.Date"
    SimArt.SAName     format "x(10)" column-label "SIMType's"
    SimBatch.TpKey     
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " SimBatch "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}                       
form
    "Code of Manufacturer ..:" SimBatch.ManCode SimMan.ManName 
    format "x(20)" AT 40                                                  SKIP
    "Day of Delivery .......:" SimBatch.DelDate                           
    VALIDATE(INPUT SimBatch.DelDate ne ?,"Missing Delivery day!")        SKIP
    "Code of SIMType .......:" SimBatch.SimArt 
    VALIDATE(INPUT SimBatch.SimArt ne "","Missing Sim Type")
    SimArt.SAName            
    format "x(20)" AT 40                                                  SKIP
    "Transport Key .........:" SimBatch.TpKey               
    VALIDATE(INPUT SimBatch.TPKey ne "","Missing Transport Key!")         SKIP

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.

form /* seek SimBatch  BY  Mancode */
    "Brand Code:" lcBrand  HELP "Enter Brand"
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
     "Man.Code..:"    mancode
    HELP "Enter Code of Manufacturer"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MANUFACTURER CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek SimBatch  BY SimArt */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "SimArticle:"   SimARt
    HELP "Enter Code of SIM Article"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ARTICLE CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* memo */
    SimBatch.Memo

    WITH OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " memo of Batch#: " + string(SimBatch.SimBatch) + " " 
    WITH NO-LABELS 1 columns
    FRAME f4.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Vendor, By Type ,By 3, By 4".


FIND FIRST SimBatch USE-INDEX Mancode WHERE 
           simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE SimBatch THEN ASSIGN
   Memory       = recid(SimBatch)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No SIM Batches available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a SimBatch  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           PROMPT-FOR 
              SimBatch.ManCode
              SimBatch.DelDate
           WITH FRAME lis EDITING:
              READKEY.
              IF lookup(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF FRAME-FIELD = "Mancode" THEN DO:
                    IF INPUT SimBatch.ManCode = "" THEN UNDO add-row, LEAVE.

                    FIND SimMan WHERE SimMan.Mancode = INPUT SimBatch.ManCode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL SimMan THEN DO:
                       BELL.
                       MESSAGE "Unknown Manufacturer !".
                       NEXT.
                    END.
                    DISP SimMan.ManName.
                 END.

                 ELSE IF frame-field = "DelDate" THEN DO:
                    IF INPUT SimBatch.DelDate = ? THEN DO:
                       NEXT-PROMPT SimBatch.ManCode.
                       NEXT.
                    END.
                    FIND SimBatch USING SimBatch.ManCode AND SimBatch.DelDate
                    NO-LOCK NO-ERROR.
                    IF AVAIL SimBatch THEN DO:
                       BELL.
                       MESSAGE
                       "There is already a Batch with these values !".
                       NEXT.
                    END.
                 END.
              END.
              APPLY LASTKEY.
           END. /* EDITING */   

           CREATE SimBatch.
           ASSIGN
           SimBatch.Brand    = gcBrand 
           SimBatch.Simbatch = NEXT-VALUE(SimBatch)
           SimBatch.ManCode  = INPUT FRAME lis SimBatch.ManCode
           SimBatch.DelDate  = INPUT FRAME lis SimBatch.DelDate.

           /* remember that SimBatch.SimBatch is created BY a trigger proc */

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSimBatch).

           ASSIGN
           Memory = recid(SimBatch)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SimBatch
      WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SimBatch THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SimBatch WHERE recid(SimBatch) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SimBatch THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SimBatch).
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
        ufk[1]= 35  ufk[2]= 35 ufk[3]= 262 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SimBatch.Brand ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimBatch.Brand WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SimBatch.SimArt ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimBatch.SimArt WITH FRAME sel.
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
        FIND SimBatch WHERE recid(SimBatch) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE SimBatch THEN
              ASSIGN FIRSTrow = i Memory = recid(SimBatch).
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
           IF NOT AVAILABLE SimBatch THEN DO:
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
                rtab[1] = recid(SimBatch)
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
           IF NOT AVAILABLE SimBatch THEN DO:
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
              rtab[FRAME-DOWN] = recid(SimBatch).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SimBatch WHERE recid(SimBatch) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE SimBatch THEN DO:
           Memory = recid(SimBatch).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE SimBatch THEN Memory = recid(SimBatch).
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
           FIND SimBatch WHERE recid(SimBatch) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
        Disp lcBrand With FRAME f1.
       SET   lcBrand WHEN gcAllBrand = TRUE
             Mancode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Mancode ENTERED THEN DO:
          FIND FIRST SimBatch WHERE 
                     SimBatch.ManCode >= mancode  AND 
                     simbatch.Brand   = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       Disp lcBrand With FRAME f2.
       SET  lcBrand WHEN gcAllBrand = TRUE
            SimArt WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF SimArt ENTERED THEN DO:
          FIND FIRST SimBatch USE-INDEX simart WHERE 
                     SimBatch.SimArt >= SimARt   AND
                     simbatch.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* INIVID. CARDS */
       ufkey = TRUE.
       RUN local-find-this(FALSE). 

       ASSIGN                                 
       rt_param[2] = string(SimBatch.SimBatch)
       rt_param[1] = "". /* ALL Stocks */
       RUN Mm/sim(SimBatch.simBatch).

       NEXT LOOP.
     END.

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. ufkey = TRUE.
        RUN local-find-this(TRUE).
        DISPLAY SimBatch.Memo WITH FRAME f4.
        IF lcRight = "RW" THEN DO:
           RUN Syst/ufkey.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSimBatch).
           UPDATE SimBatch.Memo WITH FRAME f4.
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSimBatch).
        END.   
        ELSE PAUSE.   
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       SimBatch.ManCode SimBatch.SimArt SimBatch.TpKey SimBatch.DelDate.

       RUN local-find-NEXT.
       IF AVAILABLE SimBatch THEN Memory = recid(SimBatch).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE SimBatch THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(SimBatch).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       SimBatch.ManCode SimBatch.SimArt SimBatch.TpKey SimBatch.DelDate.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSimBatch).

           DELETE SimBatch.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SimBatch
           WHERE simbatch.Brand = lcBrand) THEN DO:
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
       {Syst/uright2.i}
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY SimBatch.ManCode.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSimBatch).

       RUN local-UPDATE-record.                              

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSimBatch).

       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(SimBatch).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SimBatch) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SimBatch) must-print = TRUE.
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
      FIND SimBatch WHERE recid(SimBatch) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND SimBatch WHERE recid(SimBatch) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SimBatch
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SimBatch USE-INDEX SimArt
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SimBatch
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SimBatch USE-INDEX SimArt
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SimBatch
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SimBatch USE-INDEX SimArt
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV SimBatch
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV SimBatch USE-INDEX SimArt
       WHERE simbatch.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       SimBatch.Brand
       SimBatch.ManCode
       SimBatch.SimArt
       SimBatch.TpKey 
       SimBatch.DelDate
       SimMan.ManName WHEN AVAIL SimMan
       SimArt.SAName WHEN AVAIL SimArt
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND SimMan WHERE 
        SimMan.Brand = gcBrand AND
        SimMan.Mancode = SimBatch.ManCode NO-LOCK NO-ERROR.
   FIND SimArt WHERE 
        SimArt.Brand = gcBrand AND 
        SimArt.SimArt = SimBatch.SimArt NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          SimMan.ManName WHEN AVAIL SimMan
          SimBatch.SimArt 
          SimArt.SAName WHEN AVAIL SimArt
          SimBatch.DelDate
          SimBatch.TpKey
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
         UPDATE
             SimBatch.SimArt WHEN NEW SimBatch
             SimBatch.TpKey 
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

