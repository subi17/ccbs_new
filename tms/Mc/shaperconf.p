/* ----------------------------------------------------------------------
  MODULE .......: shaperconf.p
  TASK .........: Ema
  APPLICATION ..: nn
  AUTHOR .......: Janne Tourunen
  CREATED ......: 25-06-12
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ShaperConf'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhShaperConf AS HANDLE NO-UNDO.
   lhShaperConf = BUFFER ShaperConf:HANDLE.
   RUN StarEventInitialize(lhShaperConf).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhShaperConf).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ShaperConf  LIKE ShaperConf.ShaperConfID  NO-UNDO.
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
    ShaperConf.ShaperConfID FORMAT "X(18)"
    ShaperConf.Template FORMAT "X(20)"
    ShaperConf.TariffType FORMAT "X(12)"
    ShaperConf.Tariff FORMAT "X(18)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Shaper Configurations "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ShaperConf.Brand SKIP
    ShaperConf.ShaperConfID SKIP
    ShaperConf.Template SKIP
    ShaperConf.tariffType SKIP
    ShaperConf.tariff SKIP
    ShaperConf.LimitUnshaped SKIP
    ShaperConf.LimitShaped SKIP
    ShaperConf.Active
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek ShaperConf  BY  ShaperConf */
    ShaperConf
    HELP "Enter Code of ShaperConf"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form  /* seek ShaperConf  BY  */
    ShaperConf
    HELP "Enter Name of ShaperConf"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.
    
cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By ShaperConfID, By Template, By 4".

/*FIND FIRST ShaperConf
            NO-LOCK NO-ERROR.*/
RUN local-find-first.

IF AVAILABLE ShaperConf THEN ASSIGN
   Memory       = recid(ShaperConf)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No shaperconf available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
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

   IF must-add THEN DO:  /* Add a ShaperConf  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR ShaperConf.ShaperConfID
           VALIDATE
              (ShaperConf.ShaperConfID NOT ENTERED OR
              NOT CAN-FIND(ShaperConf using  ShaperConf.ShaperConfID),
              "ShaperConf " + string(INPUT ShaperConf.ShaperConfID) +
              " already exists !").
           IF INPUT FRAME lis ShaperConf.ShaperConfID NOT ENTERED THEN 
           LEAVE add-row.
           CREATE ShaperConf.
           ASSIGN
           ShaperConf.ShaperConfID = INPUT FRAME lis ShaperConf.ShaperConfID.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhShaperConf).

           ASSIGN
           Memory = recid(ShaperConf)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ShaperConf
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ShaperConf THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ShaperConf WHERE recid(ShaperConf) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ShaperConf THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ShaperConf).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0
        /*ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)*/
        ufk[7]= 1752 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ShaperConf.ShaperConfID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ShaperConf.ShaperConfID WITH FRAME sel.
      END.
      /*
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ShaperConf.CoName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ShaperConf.CoName WITH FRAME sel.
      END.
      */
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
        FIND ShaperConf WHERE recid(ShaperConf) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ShaperConf THEN
              ASSIGN FIRSTrow = i Memory = recid(ShaperConf).
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
           IF NOT AVAILABLE ShaperConf THEN DO:
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
                rtab[1] = recid(ShaperConf)
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
           IF NOT AVAILABLE ShaperConf THEN DO:
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
              rtab[FRAME-DOWN] = recid(ShaperConf).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ShaperConf WHERE recid(ShaperConf) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ShaperConf THEN DO:
           Memory = recid(ShaperConf).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ShaperConf THEN Memory = recid(ShaperConf).
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
           FIND ShaperConf WHERE recid(ShaperConf) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     /*ELSE IF LOOKUP(nap,"1,/1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:*/
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISP ShaperConf WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ShaperConf ENTERED THEN DO:
          FIND FIRST ShaperConf WHERE ShaperConf.ShaperConfID >= ShaperConf
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ShaperConf THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ShaperConf/ShaperConfID was found */
          ASSIGN order = 1 Memory = recid(ShaperConf) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        RUN Mc/eventsel.p("shaperconf", "#BEGIN" + chr(255) 
           + gcBrand).
        ufkey = TRUE.
        NEXT.
     END.   

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* details
       {Syst/uright2.i} */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " DETAILS " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ShaperConf.ShaperConfID.


       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhShaperConf).


       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhShaperConf).

       RUN local-disp-row.

       ufkey = TRUE.
       xrecid = recid(ShaperConf).
       LEAVE.
     END.


     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ShaperConf) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ShaperConf) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        xRecid = 0. 
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ShaperConf WHERE recid(ShaperConf) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ShaperConf WHERE recid(ShaperConf) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ShaperConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ShaperConf USE-INDEX ShaperConfID
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ShaperConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ShaperConf USE-INDEX ShaperConfID
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ShaperConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ShaperConf USE-INDEX ShaperConfID
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ShaperConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ShaperConf USE-INDEX ShaperConfID
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ShaperConf.ShaperConfID
       ShaperConf.Template
       ShaperConf.TariffType
       ShaperConf.Tariff
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
      CLEAR FRAME lis NO-PAUSE.

      /* RUN local-find-others. */
      DISP
          ShaperConf.Brand
          ShaperConf.ShaperConfID
          ShaperConf.Template
          ShaperConf.tariffType
          ShaperConf.tariff
          ShaperConf.LimitUnshaped
          ShaperConf.LimitShaped
          ShaperConf.Active
      WITH FRAME lis.
      pause 0.
         
      ASSIGN 
         ehto   = 0
         ufk    = 0
         ufk[1] = 7 WHEN lcRight = "RW" AND gcHelpParam = ""
         ufk[7] = 1752
         ufk[8] = 8.
         RUN Syst/ufkey.p.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:
         FIND CURRENT ShaperConf EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE
            ShaperConf.TariffType
            ShaperConf.Tariff
            ShaperConf.LimitUnshaped
            ShaperConf.LimitShaped
            ShaperConf.Active
         WITH FRAME lis EDITING:

            READKEY.
            APPLY LASTKEY.
         END.
         LEAVE.
     END.
     ELSE IF toimi = 7 THEN DO:
        RUN Mc/eventsel.p("ShaperConf",gcBrand + CHR(255) + ShaperConf.ShaperConfID).
     END.

      CLEAR FRAME lis NO-PAUSE.
      LEAVE.
   END.
END PROCEDURE.

