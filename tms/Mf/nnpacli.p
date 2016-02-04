/* -----------------------------------------------
  MODULE .......: nnpacli.p
  FUNCTION .....: Maintain partners CLIs
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 31.05.00
  MODIFIED .....: 23.10.00 BY kl: add-record added
                  05.11.02 jr Eventlog
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Pref   LIKE CLIPref.Pref  NO-UNDO.
DEF VAR CLI    LIKE CLIPref.CLI   NO-UNDO.
DEF VAR xrecid     AS RECID               NO-UNDO  init ?.
DEF VAR firstline  AS INT                 NO-UNDO  init 0.
DEF VAR order      AS INT                 NO-UNDO  init 1.
DEF VAR ordercount AS INT                 NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline    AS INT                 NO-UNDO  init 0.
DEF VAR ex-order   AS INT                 NO-UNDO.
DEF VAR Memory     AS RECID               NO-UNDO.
def var line       as int format "99"     NO-UNDO.
DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR must-add   AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
def var ok         as log format "Yes/No" NO-UNDO.

DEF VAR new_clipref AS LOG                NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCLIPref AS HANDLE NO-UNDO.
   lhCLIPref = BUFFER CLIPref:HANDLE.
   RUN StarEventInitialize(lhCLIPref).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCLIPref).
   END.
END.

form
   CLIPref.Pref
   CLIPref.CLI
   CLIPref.CLIId
   CLIPref.State
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain operator prefixes "
   + string(pvm,"99-99-99") + " " FRAME sel.

form
   "  Prefix ...:" CLIPref.Pref   SKIP
   "  CLI ......:" CLIPref.CLI    SKIP
   "  Identifier:" CLIPref.CLIId     SKIP
   "  State ....:" CLIPref.State  SKIP
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH NO-LABELS FRAME lis.

form /*  search WITH FIELD Pref */
   Pref
   help "Give prefix"
with row 4 col 2 title color value(ctc) " FIND PREFIX "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD Pref */
   CLI 
   help "Give CLI"
with row 4 col 2 title color value(ctc) " FIND CLI "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CLIPref
/* search condition */ no-lock no-error.
IF AVAILABLE CLIPref THEN ASSIGN
   Memory     = recid(CLIPref)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   MESSAGE
      "No prefixes / CLIs are defined yet !"
   VIEW-AS ALERT-BOX error.
   RETURN.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by prefix ".
       if order = 2 then put screen row 19 col 30 "   Order by CLI  ".
    END.

   IF must-add THEN DO:  /* Add a CLIPref */
      ASSIGN cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           new_clipref = TRUE.
           CREATE CLIPref.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR  
              KEYLABEL(LASTKEY) = "f4" THEN 
           UNDO add-row, LEAVE add-row.

           ASSIGN
              Memory = recid(CLIPref)
              xrecid = Memory.

           LEAVE add-row.

        END.

      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CLIPref NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CLIPref THEN LEAVE LOOP.

      NEXT LOOP.

   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND CLIPref where recid(CLIPref) = Memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = Memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CLIPref THEN DO:
              DISPLAY 
                 CLIPref.Pref 
                 CLIPref.CLI 
                 CLIPref.CLIId 
                 CLIPref.State.

              rtab[FRAME-LINE] = recid(CLIPref).
              IF order = 1 THEN FIND NEXT CLIPref
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT CLIPref USE-INDEX
              CLI no-lock no-error.
         /*   ELSE IF order = 3 THEN FIND NEXT CLIPref USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT CLIPref USE-INDEX index-4
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
        ufk[1] = 652 ufk[2] = 653 ufk[3] = 0 ufk[4] = 0
        ufk[5] = 5   ufk[6] = 4   ufk[7] = 0 ufk[8] = 8 ufk[9] = 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW CLIPref.Pref ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CLIPref.Pref WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CLIPref.CLI ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CLIPref.CLI WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW CLIPref.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CLIPref.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW CLIPref.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CLIPref.? WITH FRAME sel.
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
        FIND CLIPref where recid(CLIPref) = Memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev CLIPref
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CLIPref USE-INDEX
           CLI no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND prev CLIPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev CLIPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE CLIPref THEN
              ASSIGN firstline = i Memory = recid(CLIPref).
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
           FIND CLIPref where recid(CLIPref) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev CLIPref
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CLIPref USE-INDEX
           CLI no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND prev CLIPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev CLIPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE CLIPref THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 CLIPref.Pref 
                 CLIPref.CLI 
                 CLIPref.CLIId 
                 CLIPref.State.

              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(CLIPref)
              Memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND CLIPref where recid(CLIPref) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT CLIPref
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT CLIPref USE-INDEX
           CLI no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND NEXT CLIPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT CLIPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE CLIPref THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 CLIPref.Pref 
                 CLIPref.CLI 
                 CLIPref.CLIId 
                 CLIPref.State.

              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(CLIPref).
              /* finally LAST line's KeyValue is saved */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CLIPref where recid(CLIPref) = Memory no-lock no-error.
        IF order = 1 THEN FIND prev CLIPref
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev CLIPref USE-INDEX
        CLI no-lock no-error.
    /*  ELSE IF order = 3 THEN FIND prev CLIPref USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev CLIPref USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE CLIPref THEN DO:
           Memory = recid(CLIPref).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev CLIPref
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev CLIPref USE-INDEX
              CLI no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev CLIPref USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev CLIPref USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE CLIPref THEN Memory = recid(CLIPref).
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
           FIND CLIPref where recid(CLIPref) = Memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       Pref = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE Pref WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Pref <>  "" THEN DO:
          FIND FIRST CLIPref where CLIPref.Pref >= Pref
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE CLIPref THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  CLIPref/Pref was found */
          ASSIGN order = 1 Memory = recid(CLIPref) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
        CLI = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CLI WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if CLI <> "" THEN DO:
          FIND FIRST CLIPref where CLIPref.CLI >= CLI
          USE-INDEX CLI no-lock no-error.
          IF NOT AVAILABLE CLIPref THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  CLIPref/ was found */
          ASSIGN order = 2 Memory = recid(CLIPref) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND CLIPref where recid(CLIPref) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          CLIPref.Pref
          CLIPref.CLI 
          CLIPref.CLIId 
          CLIPref.State.

       IF order = 1 THEN FIND NEXT CLIPref
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT CLIPref USE-INDEX
       CLI no-lock no-error.
   /*  ELSE IF order = 3 THEN FIND NEXT CLIPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT CLIPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE CLIPref THEN Memory = recid(CLIPref).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND CLIPref where recid(CLIPref) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev CLIPref
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev CLIPref USE-INDEX
          CLI no-lock no-error.
     /*   ELSE IF order = 3 THEN FIND prev CLIPref USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev CLIPref USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE CLIPref THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             Memory = recid(CLIPref).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND CLIPref where recid(CLIPref) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          CLIPref.Pref 
          CLIPref.CLI 
          CLIPref.CLIId 
          CLIPref.State.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCLIPref).
           DELETE CLIPref.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST CLIPref
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

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND CLIPref where recid(CLIPref) = rtab[frame-line(sel)]
       exclusive-lock.

       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY 
          CLIPref.Pref
       WITH FRAME lis.
       new_clipref = FALSE.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCLIPref).
       RUN LOCAL-UPDATE-RECORD.

       /*
       UPDATE 
          CLIPref.CLI 
          CLIPref.CLIId 
          CLIPref.State.

       HIDE FRAME lis no-pause.
       DISPLAY 
          CLIPref.CLI 
          CLIPref.CLIId 
          CLIPref.State 
       WITH FRAME sel.
       xrecid = recid(CLIPref).
       */
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST CLIPref
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST CLIPref USE-INDEX
       CLI no-lock no-error.
   /*  ELSE IF order = 3 THEN FIND FIRST CLIPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST CLIPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(CLIPref) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST CLIPref
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST CLIPref USE-INDEX
       CLI no-lock no-error.
   /*  ELSE IF order = 3 THEN FIND LAST CLIPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST CLIPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(CLIPref) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
      UPDATE 
         CLIPref.Pref
         CLIPref.CLI 
         CLIPref.CLIId 
         CLIPref.State
      WITH FRAME lis.

     LEAVE.

   END.

   IF new_clipref AND
      llDoEvent THEN RUN StarEventMakeCreateEvent(lhCLIPref).

   IF NOT new_clipref AND
      llDoEvent THEN RUN StarEventMakeModifyEvent(lhCLIPref).

   new_clipref = FALSE.

   HIDE FRAME lis no-pause.

   DISPLAY 
      CLIPref.CLI 
      CLIPref.CLIId 
      CLIPref.State 
   WITH FRAME sel.
   xrecid = recid(CLIPref).


END PROCEDURE.



