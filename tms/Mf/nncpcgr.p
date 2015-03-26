/* ----------------------------------------------
  MODULE .......: NNCPCGR.P
  FUNCTION .....: CDR preprosessor CGR ranges
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 22-12-98
  MODIFIED .....: 18.09.02/jr added operator & ident validated
                  08.11.02/jr Eventlog
                  06.03.03/tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{eventval.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'MedTrunk'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR TrFrom   LIKE MedTrunk.TrFrom    NO-UNDO.
DEF VAR TrName   LIKE MedTrunk.TrName    NO-UNDO.
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

   {lib/eventlog.i}

   DEFINE VARIABLE lhMedTrunk AS HANDLE NO-UNDO.
   lhMedTrunk = BUFFER MedTrunk:HANDLE.
   RUN StarEventInitialize(lhMedTrunk).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhMedTrunk).
   END.
END.


form
   MedTrunk.Ident
   MedTrunk.TrFrom 
   MedTrunk.TrTo
   MedTrunk.TrName  
   MedTrunk.Categ 
   MedTrunk.OpCode
   MedTrunk.Type  
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi + " Exchange CGR ranges " 
   + string(pvm,"99-99-99") + " " FRAME sel.

form
   MedTrunk.Ident
   MedTrunk.TrFrom
   MedTrunk.TrTo
   MedTrunk.TrName
   MedTrunk.Categ 
   MedTrunk.OpCode
   MedTrunk.Type  
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc) TITLE COLOR value(ctc)
   fr-header WITH side-labels 1 columns FRAME lis.

form /*  search WITH FIELD TrFrom */
    TrFrom
    help "Give range or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND RANGE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD TrName */
    TrName
    help "Give Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MedTrunk
/* search condition */ no-lock no-error.
IF AVAILABLE MedTrunk THEN ASSIGN
   memory       = recid(MedTrunk)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Medtrunks available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by Code ".
       if order = 2 then put screen row 19 col 30 " Order by Name ".
    END.

   IF must-add THEN DO:  /* MedTrunk -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           CREATE MedTrunk.
           UPDATE 
              MedTrunk.Ident
              MedTrunk.TrFrom
              MedTrunk.TrTo
              MedTrunk.TrName
              MedTrunk.Categ 
              MedTrunk.OpCode  
              MedTrunk.Type
           WITH FRAME lis EDITING.
              READKEY. nap = keylabel(LASTKEY).
              IF lookup(nap,poisnap) > 0 THEN DO:
                 if frame-field = "ident" THEN DO:
                    if input MedTrunk.Ident = "" THEN UNDO, LEAVE add-new.
                    ELSE 
                    DO:
                       FIND FIRST exchange where exchange.ident = 
                       INPUT MedTrunk.ident no-lock no-error.

                       IF NOT AVAIL exchange THEN 
                       DO:
                          bell.  
                          message "Unknown Truncate Identification !".
                          NEXT.
                       END.
                    END.

                    ASSIGN
                       MedTrunk.Ident = INPUT MedTrunk.Ident.

                    FIND FIRST Exchange where 
                               Exchange.Ident = MedTrunk.Ident
                    no-lock.
                    ASSIGN MedTrunk.ExCode = STRING(Exchange.ExNum).

                 END.

                 if frame-field = "opcode" THEN 
                 DO:
                    FIND FIRST operator where operator.operator =
                    INPUT MedTrunk.opcode no-lock no-error.

                    IF NOT AVAIL operator THEN
                    DO:
                       bell.
                       message "Unknown Operator !".
                       NEXT.
                    END.   
                 END. 
              END.
              APPLY LASTKEY.
           END.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMedTrunk).
           ASSIGN
              memory = recid(MedTrunk)
              xrecid = memory.
        END.

      END.  /* add-new */

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST MedTrunk
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE MedTrunk THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MedTrunk where recid(MedTrunk) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MedTrunk THEN DO:
              DISPLAY 
                 MedTrunk.Ident
                 MedTrunk.TrFrom 
                 MedTrunk.TrTo
                 MedTrunk.TrName  
                 MedTrunk.Categ 
                 MedTrunk.OpCode
                 MedTrunk.Type.
              rtab[FRAME-LINE] = recid(MedTrunk).
              IF order = 1 THEN FIND NEXT MedTrunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MedTrunk USE-INDEX TrName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT MedTrunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT MedTrunk USE-INDEX index-4
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
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MedTrunk.Ident ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedTrunk.Ident WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MedTrunk.TrName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedTrunk.TrName WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW MedTrunk.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedTrunk.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MedTrunk.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedTrunk.? WITH FRAME sel.
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
        FIND MedTrunk where recid(MedTrunk) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MedTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedTrunk USE-INDEX TrName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MedTrunk THEN
              ASSIGN firstline = i memory = recid(MedTrunk).
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
           FIND MedTrunk where recid(MedTrunk) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MedTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedTrunk USE-INDEX TrName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedTrunk THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 MedTrunk.Ident
                 MedTrunk.TrFrom 
                 MedTrunk.TrTo
                 MedTrunk.TrName  
                 MedTrunk.Categ 
                 MedTrunk.OpCode
                 MedTrunk.Type.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MedTrunk)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MedTrunk where recid(MedTrunk) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MedTrunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MedTrunk USE-INDEX TrName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT MedTrunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT MedTrunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedTrunk THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 MedTrunk.Ident
                 MedTrunk.TrFrom 
                 MedTrunk.TrTo
                 MedTrunk.TrName  
                 MedTrunk.Categ 
                 MedTrunk.OpCode
                 MedTrunk.Type.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MedTrunk).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MedTrunk where recid(MedTrunk) = memory no-lock no-error.
        IF order = 1 THEN FIND prev MedTrunk
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MedTrunk USE-INDEX TrName
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev MedTrunk USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev MedTrunk USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MedTrunk THEN DO:
           memory = recid(MedTrunk).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MedTrunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MedTrunk USE-INDEX TrName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev MedTrunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev MedTrunk USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MedTrunk THEN memory = recid(MedTrunk).
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
           FIND MedTrunk where recid(MedTrunk) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       TrFrom = ?.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE TrFrom WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF TrFrom <> ? THEN DO:
          FIND FIRST MedTrunk where MedTrunk.TrFrom >= TrFrom
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedTrunk THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  cdrp-cgr/cgr-from was found */
          ASSIGN order = 1 memory = recid(MedTrunk) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       TrName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE TrName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if TrName <> "" THEN DO:
          FIND FIRST MedTrunk where MedTrunk.TrName >= TrName
          USE-INDEX TrName /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedTrunk THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  cdrp-cgr/cgr-name was found */
          ASSIGN order = 2 memory = recid(MedTrunk) must-print = TRUE.
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
       FIND MedTrunk where recid(MedTrunk) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          MedTrunk.Ident 
          MedTrunk.TrFrom 
          MedTrunk.TrTo
          MedTrunk.TrName
          MedTrunk.Categ
          MedTrunk.OpCode
          MedTrunk.Type.
       IF order = 1 THEN FIND NEXT MedTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT MedTrunk USE-INDEX TrName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT MedTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT MedTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE MedTrunk THEN memory = recid(MedTrunk).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MedTrunk where recid(MedTrunk) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MedTrunk
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MedTrunk USE-INDEX TrName
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev MedTrunk USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev MedTrunk USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE MedTrunk THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(MedTrunk).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MedTrunk where recid(MedTrunk) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          MedTrunk.Ident 
          MedTrunk.TrFrom 
          MedTrunk.TrTo
          MedTrunk.TrName
          MedTrunk.Categ
          MedTrunk.OpCode
          MedTrunk.Type.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMedTrunk).
           DELETE MedTrunk.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MedTrunk
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
     DO WITH FRAME lis TRANSAction on endkey undo, leave:
       /* change */
       FIND MedTrunk where recid(MedTrunk) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       DISP MedTrunk.TrFrom.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMedTrunk).
       UPDATE 
          MedTrunk.Ident
          MedTrunk.TrTo
          MedTrunk.TrName
          MedTrunk.Categ 
          MedTrunk.OpCode  
          MedTrunk.Type.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMedTrunk).
       HIDE FRAME lis no-pause.
       DISPLAY 
          MedTrunk.TrTo
          MedTrunk.TrName
          MedTrunk.Categ 
          MedTrunk.OpCode  
          MedTrunk.Type
       WITH FRAME sel.
       xrecid = recid(MedTrunk).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MedTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MedTrunk USE-INDEX TrName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST MedTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST MedTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MedTrunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MedTrunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MedTrunk USE-INDEX TrName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST MedTrunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST MedTrunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MedTrunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

