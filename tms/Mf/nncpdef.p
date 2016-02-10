/* ----------------------------------------------
  MODULE .......: NNFTDEF.P
  FUNCTION .....: Default CGR ranges
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 22-12-98
  MODIFIED .....: 18.09.2002 jp some validation
                  08.11.2002 jr Eventlog
                  06.03.2003 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MedDefTrunk'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR DefFrom   LIKE MedDefTrunk.DefFrom    NO-UNDO.
DEF VAR DefName   LIKE MedDefTrunk.DefName    NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMedDefTrunk AS HANDLE NO-UNDO.
   lhMedDefTrunk = BUFFER MedDefTrunk:HANDLE.
   RUN StarEventInitialize(lhMedDefTrunk).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMedDefTrunk).
   END.
END.

form
    MedDefTrunk.Ident
    MedDefTrunk.DefFrom 
    MedDefTrunk.DefTo
    MedDefTrunk.DefName  
    MedDefTrunk.Categ 
    MedDefTrunk.OpCode
    MedDefTrunk.Type  
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Exchange default ranges "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MedDefTrunk.Ident
    MedDefTrunk.DefFrom
    MedDefTrunk.DefTo
    MedDefTrunk.DefName
    MedDefTrunk.Categ 
    MedDefTrunk.OpCode
    MedDefTrunk.Type  
    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /*  search WITH FIELD DefFrom */
    DefFrom
    help "Give range or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND RANGE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD DefName */
    DefName
    help "Give Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MedDefTrunk
/* search condition */ no-lock no-error.
IF AVAILABLE MedDefTrunk THEN ASSIGN
   memory     = recid(MedDefTrunk)
   must-print = TRUE
   must-add   = FALSE.
ELSE ASSIGN
   memory     = ?
   must-print = FALSE
   must-add   = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by ExCode ".
       if order = 2 then put screen row 19 col 30 " Order by Name ".
    END.

   IF must-add THEN DO:  /* MedDefTrunk -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           CREATE MedDefTrunk.
           UPDATE 
              MedDefTrunk.Ident
              VALIDATE(CAN-FIND(FIRST exchange WHERE Exchange.Ident =
              INPUT medDeftrunk.ident),"Unknown Ident Code")
              MedDefTrunk.DefFrom
              MedDefTrunk.DefTo
              VALIDATE(INPUT medDefTrunk.defFrom <= INPUT MedDefTrunk.defTo,
              "Invalid order")
              MedDefTrunk.DefName
              MedDefTrunk.Categ 
              MedDefTrunk.OpCode  
              VALIDATE(CAN-FIND(first operator WHERE
                               Operator.Operator = INPUT MedDefTrunk.OpCode),
              "Unknown Operator Code")
              MedDefTrunk.Type
           WITH FRAME lis EDITING.
              READKEY. nap = keylabel(LASTKEY).
              IF lookup(nap,poisnap) > 0 THEN DO:
                 if frame-field = "ident" THEN DO:
                    if input MedDefTrunk.Ident = "" THEN UNDO, LEAVE add-new.
                    ASSIGN
                       MedDefTrunk.Ident = INPUT MedDefTrunk.Ident.

                    FIND FIRST Exchange where 
                               Exchange.Ident = MedDefTrunk.Ident
                    no-lock no-error.
                    IF AVAIL Exchange THEN
                       MedDefTrunk.ExCode = STRING(Exchange.ExNum).
                    ELSE DO:              
                       MESSAGE "Unknown exchange !".
                       NEXT.
                    END.
                 END.
              END.
              APPLY LASTKEY.
           END.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMedDefTrunk).
           ASSIGN
              memory = recid(MedDefTrunk)
              xrecid = memory.
        END.

       END.  /* add-new */

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST MedDefTrunk
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE MedDefTrunk THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MedDefTrunk where recid(MedDefTrunk) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MedDefTrunk THEN DO:
              DISPLAY 
                 MedDefTrunk.Ident
                 MedDefTrunk.DefFrom 
                 MedDefTrunk.DefTo
                 MedDefTrunk.DefName  
                 MedDefTrunk.Categ 
                 MedDefTrunk.OpCode
                 MedDefTrunk.Type.
              rtab[FRAME-LINE] = recid(MedDefTrunk).
              IF order = 1 THEN FIND NEXT MedDefTrunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MedDefTrunk USE-INDEX DefName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT MedDefTrunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT MedDefTrunk USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE.
        PAUSE 0 no-message.

        /* one page of data has been Printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MedDefTrunk.Ident ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedDefTrunk.Ident WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MedDefTrunk.DefName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedDefTrunk.DefName WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW MedDefTrunk.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedDefTrunk.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MedDefTrunk.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedDefTrunk.? WITH FRAME sel.
      END.
*/
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND MedDefTrunk where recid(MedDefTrunk) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MedDefTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedDefTrunk USE-INDEX DefName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedDefTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedDefTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MedDefTrunk THEN
              ASSIGN firstline = i memory = recid(MedDefTrunk).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND MedDefTrunk where recid(MedDefTrunk) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MedDefTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedDefTrunk USE-INDEX DefName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedDefTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedDefTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedDefTrunk THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 MedDefTrunk.Ident
                 MedDefTrunk.DefFrom 
                 MedDefTrunk.DefTo
                 MedDefTrunk.DefName  
                 MedDefTrunk.Categ 
                 MedDefTrunk.OpCode
                 MedDefTrunk.Type.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MedDefTrunk)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MedDefTrunk where recid(MedDefTrunk) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MedDefTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MedDefTrunk USE-INDEX DefName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT MedDefTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT MedDefTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedDefTrunk THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 MedDefTrunk.Ident
                 MedDefTrunk.DefFrom 
                 MedDefTrunk.DefTo
                 MedDefTrunk.DefName  
                 MedDefTrunk.Categ 
                 MedDefTrunk.OpCode
                 MedDefTrunk.Type.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MedDefTrunk).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MedDefTrunk where recid(MedDefTrunk) = memory no-lock no-error.
        IF order = 1 THEN FIND prev MedDefTrunk
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MedDefTrunk USE-INDEX DefName
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev MedDefTrunk USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev MedDefTrunk USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MedDefTrunk THEN DO:
           memory = recid(MedDefTrunk).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MedDefTrunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MedDefTrunk USE-INDEX DefName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev MedDefTrunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev MedDefTrunk USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MedDefTrunk THEN memory = recid(MedDefTrunk).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND MedDefTrunk where recid(MedDefTrunk) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       DefFrom = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE DefFrom WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF DefFrom <> ? THEN DO:
          FIND FIRST MedDefTrunk where MedDefTrunk.DefFrom >= DefFrom
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedDefTrunk THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  cdrp-def/def-from was found */
          ASSIGN order = 1 memory = recid(MedDefTrunk) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       DefName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE DefName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if DefName <> "" THEN DO:
          FIND FIRST MedDefTrunk where MedDefTrunk.DefName >= DefName
          USE-INDEX DefName /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedDefTrunk THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  cdrp-def/def-name was found */
          ASSIGN order = 2 memory = recid(MedDefTrunk) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND MedDefTrunk where recid(MedDefTrunk) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          MedDefTrunk.Ident 
          MedDefTrunk.DefFrom 
          MedDefTrunk.DefTo
          MedDefTrunk.DefName
          MedDefTrunk.Categ
          MedDefTrunk.OpCode
          MedDefTrunk.Type.
       IF order = 1 THEN FIND NEXT MedDefTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT MedDefTrunk USE-INDEX DefName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT MedDefTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT MedDefTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE MedDefTrunk THEN memory = recid(MedDefTrunk).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MedDefTrunk where recid(MedDefTrunk) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MedDefTrunk
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MedDefTrunk USE-INDEX DefName
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev MedDefTrunk USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev MedDefTrunk USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE MedDefTrunk THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(MedDefTrunk).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MedDefTrunk where recid(MedDefTrunk) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          MedDefTrunk.Ident 
          MedDefTrunk.DefFrom 
          MedDefTrunk.DefTo
          MedDefTrunk.DefName
          MedDefTrunk.Categ
          MedDefTrunk.OpCode
          MedDefTrunk.Type.

       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMedDefTrunk).
           DELETE MedDefTrunk.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MedDefTrunk
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND MedDefTrunk where recid(MedDefTrunk) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISP MedDefTrunk.Ident.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMedDefTrunk).
       UPDATE 
          MedDefTrunk.DefFrom
          MedDefTrunk.DefTo
          MedDefTrunk.DefName
          MedDefTrunk.Categ 
          MedDefTrunk.OpCode  
          MedDefTrunk.Type.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMedDefTrunk).
       HIDE FRAME lis no-pause.
       DISPLAY 
          MedDefTrunk.DefTo
          MedDefTrunk.DefName
          MedDefTrunk.Categ 
          MedDefTrunk.OpCode  
          MedDefTrunk.Type
       WITH FRAME sel.
       xrecid = recid(MedDefTrunk).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MedDefTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MedDefTrunk USE-INDEX DefName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST MedDefTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST MedDefTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MedDefTrunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MedDefTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MedDefTrunk USE-INDEX DefName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST MedDefTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST MedDefTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MedDefTrunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

