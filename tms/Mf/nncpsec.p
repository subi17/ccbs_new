/* -----------------------------------------------
  MODULE .......: NNSPSEC
  FUNCTION .....: FIDEX confoguration sections
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 23-12-98
  MODIFIED .....: 25-05-99 jp uright1 & uright2 added
                  29-02-00 kl DISPLAY uniq ALL the time
                  20.09.02 jr added num values to help
                              and validate for it
                  08.11.02 jr Eventlog            
                  06.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MedSect'}


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Type       LIKE MedSect.Type    NO-UNDO.
DEF VAR Name       LIKE MedSect.Name    NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR Memory     AS RECID                  NO-UNDO.
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

   DEFINE VARIABLE lhMedSect AS HANDLE NO-UNDO.
   lhMedSect = BUFFER MedSect:HANDLE.
   RUN StarEventInitialize(lhMedSect).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMedSect).
   END.
END.

form
   MedSect.Type
   MedSect.Name
   MedSect.Num 
   MedSect.Resvd
   MedSect.Pref
   MedSect.Uniq
   MedSect.NPref
   MedSect.Abs
   MedSect.APref
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " FTAM sections "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
   MedSect.Type  
   MedSect.Name  
   MedSect.Num   HELP "Choose A,B OR AB" 
   VALIDATE(LOOKUP(INPUT MedSect.num,"A,B,AB") NE 0,"Unknown value") 
   MedSect.Resvd
   MedSect.Pref
   MedSect.Uniq
   MedSect.NPref
   MedSect.Abs
   MedSect.APref
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH side-labels 1 columns
   FRAME lis.

form /*  search WITH FIELD Name */
   Type
   help "Give code"
with row 4 col 2 title color value(ctc) " FIND CODE "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD Name */
   Name
   help "Give name"
with row 4 col 2 title color value(ctc) " FIND NAME "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MedSect
/* search condition */ no-lock no-error.
IF AVAILABLE MedSect THEN ASSIGN
   Memory       = recid(MedSect)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No MedSectt records available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by Type ".
       if order = 2 then put screen row 19 col 30 " Order by Name ".
    END.

   IF must-add THEN DO:  /* MedSect -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION:
           PROMPT-FOR MedSect.Type
           VALIDATE
              (MedSect.Type = ? OR
              NOT can-find(MedSect using  MedSect.Type),
              " " + string(INPUT MedSect.Type) +
              " already exists !").
           IF INPUT MedSect.Type = ? THEN LEAVE add-new.
           CREATE MedSect.
           ASSIGN
           MedSect.Type = INPUT FRAME lis MedSect.Type.
           UPDATE 
              MedSect.Name
              MedSect.Num 
              MedSect.Resvd
              MedSect.Pref
              MedSect.Uniq
              MedSect.NPref
              MedSect.Abs
              MedSect.APref.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMedSect).
           ASSIGN
           Memory = recid(MedSect)
           xrecid = Memory.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST MedSect
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE MedSect THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MedSect where recid(MedSect) = Memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = Memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MedSect THEN DO:
              DISPLAY 
                 MedSect.Type 
                 MedSect.Name 
                 MedSect.Num 
                 MedSect.Resvd
                 MedSect.Pref
                 MedSect.Uniq
                 MedSect.NPref
                 MedSect.Abs
                 MedSect.APref.
              rtab[FRAME-LINE] = recid(MedSect).
              IF order = 1 THEN FIND NEXT MedSect
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MedSect USE-INDEX Name
              /* search condition */ no-lock no-error.
         /*   ELSE IF order = 3 THEN FIND NEXT MedSect USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT MedSect USE-INDEX index-4
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
        CHOOSE ROW MedSect.Type {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedSect.Type WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MedSect.Name {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedSect.Name WITH FRAME sel.
      END.
  /*  IF order = 3 THEN DO:
        CHOOSE ROW MedSect.?? {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedSect.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MedSect.??  {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedSect.? WITH FRAME sel.
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
        ASSIGN firstline = 0 Memory = rtab[FRAME-LINE].
        FIND MedSect where recid(MedSect) = Memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MedSect
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedSect USE-INDEX Name
           /* search condition */ no-lock no-error.
       /*   ELSE IF order = 3 THEN FIND prev MedSect USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedSect USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MedSect THEN
              ASSIGN firstline = i Memory = recid(MedSect).
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
           FIND MedSect where recid(MedSect) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MedSect
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedSect USE-INDEX Name
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedSect USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedSect USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedSect THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 MedSect.Type 
                 MedSect.Name
                 MedSect.Num 
                 MedSect.Resvd
                 MedSect.Pref
                 MedSect.Uniq
                 MedSect.NPref
                 MedSect.Abs
                 MedSect.APref.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MedSect)
              Memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MedSect where recid(MedSect) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MedSect
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MedSect USE-INDEX Name
           /* search condition */ no-lock no-error.
    /*      ELSE IF order = 3 THEN FIND NEXT MedSect USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT MedSect USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedSect THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 MedSect.Type 
                 MedSect.Name 
                 MedSect.Num 
                 MedSect.Resvd
                 MedSect.Pref
                 MedSect.Uniq
                 MedSect.NPref
                 MedSect.Abs
                 MedSect.APref.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MedSect).
              /* finally LAST line's KeyValue is saved */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MedSect where recid(MedSect) = Memory no-lock no-error.
        IF order = 1 THEN FIND prev MedSect
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MedSect USE-INDEX Name
        /* search condition */ no-lock no-error.
   /*   ELSE IF order = 3 THEN FIND prev MedSect USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev MedSect USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MedSect THEN DO:
           Memory = recid(MedSect).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MedSect
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MedSect USE-INDEX Name
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev MedSect USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev MedSect USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MedSect THEN Memory = recid(MedSect).
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
           Memory = rtab[FRAME-DOWN].
           FIND MedSect where recid(MedSect) = Memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       Type = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Type WITH FRAME f1.
       HIDE FRAME f1 no-pause.
          FIND FIRST MedSect where MedSect.Type >= Type
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedSect THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  MedSect/Name was found */
          ASSIGN order = 1 Memory = recid(MedSect) must-print = TRUE.
          NEXT LOOP.
     END. /* Haku sar. 1 */

     /* Haku 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       Type = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Name WITH FRAME f2.
       HIDE FRAME f1 no-pause.
       IF Name NE "" THEN DO:
          FIND FIRST MedSect where MedSect.Name >= Name
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedSect THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  MedSect/Name was found */
          ASSIGN order = 2 Memory = recid(MedSect) must-print = TRUE.
          NEXT LOOP.
        END.  
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND MedSect where recid(MedSect) = rtab[FRAME-LINE] no-lock.
       IF MedSect.Resvd THEN DO:
          BELL.
          message "This section is reserved internally, it can't be erased !".
          message "Hit ENTER to continue ... ".
          PAUSE no-message.
          NEXT BROWSE.
       END.       
       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          MedSect.Type 
          MedSect.Name 
          MedSect.Num 
          MedSect.Resvd
          MedSect.Pref
          MedSect.Uniq.

       IF order = 1 THEN FIND NEXT MedSect
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT MedSect USE-INDEX Name
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 3 THEN FIND NEXT MedSect USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT MedSect USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE MedSect THEN Memory = recid(MedSect).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MedSect where recid(MedSect) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MedSect
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MedSect USE-INDEX Name
          /* search condition */ no-lock no-error.
     /*   ELSE IF order = 3 THEN FIND prev MedSect USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev MedSect USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE MedSect THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             Memory = recid(MedSect).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MedSect where recid(MedSect) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          MedSect.Name 
          MedSect.Type 
          MedSect.Num 
          MedSect.Resvd
          MedSect.Pref
          MedSect.Uniq.

       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMedSect).
           DELETE MedSect.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MedSect
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
     DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND MedSect where recid(MedSect) = rtab[frame-line(sel)]
       exclusive-lock.
       IF MedSect.Resvd THEN DO:
          BELL.
          message "This section is reserved internally, it can't be modified !".
          message "Hit ENTER to continue ... ".
          PAUSE no-message.
          NEXT BROWSE.
       END.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       DISPLAY 
          MedSect.Type.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMedSect).
       UPDATE 
          MedSect.Name
          MedSect.Num 
          MedSect.Resvd
          MedSect.Pref
          MedSect.Uniq
          MedSect.NPref
          MedSect.Abs
          MedSect.APref.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMedSect).   
       HIDE FRAME lis no-pause.
       DISPLAY 
          MedSect.Name
          MedSect.Num 
          MedSect.Resvd
          MedSect.Pref
          " " @ MedSect.Uniq
          MedSect.Uniq when MedSect.Pref ne ""
       WITH FRAME sel.
       xrecid = recid(MedSect).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MedSect
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MedSect USE-INDEX Name
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 3 THEN FIND FIRST MedSect USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST MedSect USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(MedSect) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MedSect
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MedSect USE-INDEX Name
       /* search condition */ no-lock no-error.
   /*  ELSE IF order = 3 THEN FIND LAST MedSect USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST MedSect USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(MedSect) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

