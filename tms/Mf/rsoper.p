/* --------------------------------------------
  MODULE .......: rsoper.p
  FUNCTION .....: Maintain reseller operators
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 04-07-00
  MODIFIED .....: 27.02.2000 BY kl: bug fixing
                  01.11.2002 jr Eventlog
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustNum     LIKE rsoper.CustNum  NO-UNDO.
DEF VAR Operator    LIKE rsoper.Operator NO-UNDO.
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

   DEFINE VARIABLE lhrsoper AS HANDLE NO-UNDO.
   lhrsoper = BUFFER rsoper:HANDLE.
   RUN StarEventInitialize(lhrsoper).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhrsoper).
   END.
END.


form
   rsoper.CustNum  
   Customer.CustName
   rsoper.Operator 
   OperIndir.Prefix
   rsoper.fileid
   rsoper.Reseller rscode.RsName
WITH width 80 OVERLAY scroll 1 15 DOWN
   color value(cfc) title color value(ctc) " " + ynimi + 
   " Maintain reseller operators " + string(pvm,"99-99-99") + " "
FRAME sel.

form
   " Customer number :" rsoper.CustNum     SKIP
   " Operator code ..:" rsoper.Operator    SKIP
   " IN/OUT PaymFile ID .:" rsoper.fileid     SKIP
   " Prefix RepType ....:" rsoper.Reseller    rscode.RsName SKIP
   " Home directory .:" rsoper.rs-dir     SKIP
   " Request script .:" rsoper.req-script SKIP
   " CDR script .....:" rsoper.cdr-script SKIP
   " Response script :" rsoper.res-script SKIP
   " Preselect RepType .:" rsoper.pstype     SKIP
   " Preselect script:" rsoper.cps-script  
WITH OVERLAY ROW 4 centered
   COLOR value(cfc) TITLE COLOR value(ctc)
   fr-header WITH NO-LABELS FRAME lis.

form /*  search WITH FIELD CustNum */
   CustNum
   help "Give customer number"
with row 4 col 2 title color value(ctc) " FIND CUSTOMER number "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD Operator */
   Operator
   help "Give operator code"
with row 4 col 2 title color value(ctc) " FIND OPERATOR CODE "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST rsoper no-lock no-error.
IF AVAILABLE rsoper THEN ASSIGN
   memory       = recid(rsoper)
   must-print = TRUE
   must-add    = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " By customer number ".
       if order = 2 then put screen row 19 col 30 "  By operator code  ".
    END.

   IF must-add THEN DO:  /* rsoper -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           PROMPT-FOR rsoper.CustNum
           VALIDATE
              (rsoper.CustNum = "" OR
              NOT can-find(rsoper using  rsoper.CustNum),
              " " + string(INPUT rsoper.CustNum) +
              " already exists !").
           if input rsoper.CustNum = "" THEN LEAVE add-new.
           CREATE rsoper.
           ASSIGN
           rsoper.CustNum = INPUT FRAME lis rsoper.CustNum.
           UPDATE 
              rsoper.Operator
              rsoper.fileid 
              rsoper.Reseller
           WITH FRAME lis.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhrsoper).
           ASSIGN
              memory = recid(rsoper)
              xrecid = memory.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST rsoper no-lock no-error.
      IF NOT AVAILABLE rsoper THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND FIRST rsoper where 
             recid(rsoper) = memory
        no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE rsoper THEN DO:
              FIND FIRST Customer where
                         Customer.CustNum = rsoper.CustNum
              no-lock no-error.

              FIND FIRST rscode where
                         rscode.Reseller = rsoper.Reseller
              no-lock no-error.

              FIND FIRST OperIndir where
                         OperIndir.Operator = rsoper.Operator
              no-lock no-error.
              DISPLAY 
                 rsoper.CustNum  Customer.CustName
                 rsoper.Operator OperIndir.Prefix WHEN AVAIL OperIndir
                 rsoper.fileid
                 rsoper.Reseller rscode.RsName WHEN AVAIL rscode
              WITH FRAME sel.
              rtab[FRAME-LINE] = recid(rsoper).
              IF order = 1 THEN FIND NEXT rsoper
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT rsoper USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT rsoper USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT rsoper USE-INDEX index-4
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
        ufk[1] = 702 ufk[2] = 35 ufk[3] = 0 ufk[4] = 0
        ufk[5] = 5   ufk[6] = 4  ufk[7] = 0 ufk[8] = 8 ufk[9] = 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW rsoper.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) rsoper.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW rsoper.Operator ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) rsoper.Operator WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW rsoper.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) rsoper.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW rsoper.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) rsoper.? WITH FRAME sel.
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
        FIND FIRST rsoper where 
             recid(rsoper) = memory
        no-lock no-error.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev rsoper
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev rsoper USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev rsoper USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev rsoper USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE rsoper THEN
              ASSIGN firstline = i memory = recid(rsoper).
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
           FIND FIRST rsoper where 
                recid(rsoper) = rtab[1] 
           no-lock no-error.
           IF order = 1 THEN FIND prev rsoper
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev rsoper USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev rsoper USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev rsoper USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE rsoper THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND FIRST Customer where
                         Customer.CustNum = rsoper.CustNum
              no-lock no-error.

              FIND FIRST rscode where
                         rscode.Reseller = rsoper.Reseller
              no-lock no-error.

              FIND FIRST OperIndir where
                         OperIndir.Operator = rsoper.Operator
              no-lock no-error.

              DISPLAY 
                 rsoper.CustNum  Customer.CustName
                 rsoper.Operator OperIndir.Prefix WHEN AVAIL OperIndir
                 rsoper.fileid
                 rsoper.Reseller rscode.RsName WHEN AVAIL rscode
              WITH FRAME sel.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(rsoper)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND FIRST rsoper where 
                recid(rsoper) = rtab[FRAME-DOWN] 
           no-lock no-error.
           IF order = 1 THEN FIND NEXT rsoper
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT rsoper USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT rsoper USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT rsoper USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE rsoper THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND FIRST Customer where
                         Customer.CustNum = rsoper.CustNum
              no-lock no-error.

              FIND FIRST rscode where
                         rscode.Reseller = rsoper.Reseller
              no-lock no-error.

              FIND FIRST OperIndir where
                         OperIndir.Operator = rsoper.Operator
              no-lock no-error.
              DISPLAY 
                 rsoper.CustNum  Customer.CustName
                 rsoper.Operator OperIndir.Prefix WHEN AVAIL OperIndir                 rsoper.fileid
                 rsoper.Reseller rscode.RsName WHEN AVAIL rscode
              WITH FRAME sel.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(rsoper).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FIRST rsoper where 
             recid(rsoper) = memory 
        no-lock no-error.
        IF order = 1 THEN FIND prev rsoper
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev rsoper USE-INDEX Operator
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev rsoper USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev rsoper USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE rsoper THEN DO:
           memory = recid(rsoper).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev rsoper
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev rsoper USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev rsoper USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev rsoper USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE rsoper THEN memory = recid(rsoper).
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
           FIND FIRST rsoper where 
                recid(rsoper) = memory
           no-lock no-error.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
      CustNum = 0.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE CustNum WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF CustNum <> 0 THEN DO:
          FIND FIRST rsoper where 
                     rsoper.CustNum >= CustNum
          no-lock no-error.
          IF NOT AVAILABLE rsoper THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  rsoper/CustNum was found */
          ASSIGN order = 1 memory = recid(rsoper) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       Operator = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE Operator WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if Operator <> "" THEN DO:
          FIND FIRST rsoper where 
                     rsoper.Operator >= Operator
          USE-INDEX Operator no-lock no-error.
          IF NOT AVAILABLE rsoper THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  rsoper/op-code was found */
          ASSIGN order = 2 memory = recid(rsoper) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND FIRST rsoper where 
            recid(rsoper) = rtab[FRAME-LINE] 
       no-lock no-error.
       FIND FIRST Customer where
                  Customer.CustNum = rsoper.CustNum
       no-lock no-error.

       FIND FIRST rscode where
                  rscode.Reseller = rsoper.Reseller
       no-lock no-error.

       FIND FIRST OperIndir where
                  OperIndir.Operator = rsoper.Operator
       no-lock no-error.
       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          rsoper.CustNum  Customer.CustName
          rsoper.Operator OperIndir.Prefix WHEN AVAIL OperIndir
          rsoper.fileid
          rsoper.Reseller rscode.RsName WHEN AVAIL rscode
       WITH FRAME sel.

       IF order = 1 THEN FIND NEXT rsoper
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT rsoper USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT rsoper USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT rsoper USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE rsoper THEN memory = recid(rsoper).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND FIRST rsoper where recid(rsoper) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev rsoper
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev rsoper USE-INDEX Operator
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev rsoper USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev rsoper USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE rsoper THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(rsoper).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND FIRST rsoper where 
            recid(rsoper) = rtab[FRAME-LINE]
       exclusive-lock no-error.
       FIND FIRST Customer where
                  Customer.CustNum = rsoper.CustNum
       no-lock no-error.

       FIND FIRST rscode where
                  rscode.Reseller = rsoper.Reseller
       no-lock no-error.

       FIND FIRST OperIndir where
                  OperIndir.Operator = rsoper.Operator
       no-lock no-error.
       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ctc)
          rsoper.CustNum  Customer.CustName
          rsoper.Operator OperIndir.Prefix WHEN AVAIL OperIndir
          rsoper.fileid
          rsoper.Reseller rscode.RsName WHEN AVAIL rscode
       WITH FRAME sel.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhrsoper).
           DELETE rsoper.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST rsoper
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
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND FIRST rsoper where 
            recid(rsoper) = rtab[frame-line(sel)]
       exclusive-lock no-error.
       FIND FIRST rscode where
                  rscode.Reseller = rsoper.Reseller
       no-lock no-error.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       DISPLAY 
          rsoper.CustNum
          rsoper.pstype
          rscode.RsName WHEN AVAIL rscode
       WITH FRAME lis.
       IF rsoper.pstype > 0 THEN DISPLAY
          rsoper.cps-script
       WITH FRAME lis.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhrsoper).
       UPDATE 
          rsoper.Operator
          rsoper.fileid 
          rsoper.Reseller
          rsoper.rs-dir
          rsoper.req-script
          rsoper.cdr-script
          rsoper.res-script
          rsoper.pstype
       WITH FRAME lis EDITING.
          READKEY. nap = keylabel(LASTKEY).
             IF lookup(nap,poisnap) > 0 THEN DO:
                if frame-field = "rs-code" THEN DO:
                   ASSIGN rsoper.Reseller.
                   FIND FIRST rscode where
                              rscode.Reseller = rsoper.Reseller
                   no-lock no-error.
                   IF AVAIL rscode THEN DISP rscode.RsName WITH FRAME lis.
                   ELSE DO:
                      message "This code RepType number does NOT exist !"
                      VIEW-AS ALERT-BOX error.
                      NEXT.
                   END.
                END.
             END.
          APPLY LASTKEY.
       END.

       IF rsoper.pstype > 0 THEN UPDATE
          rsoper.cps-script
       WITH FRAME lis.
       else rsoper.cps-script = "".
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhrsoper).
       HIDE FRAME lis no-pause.

       DISPLAY 
          rsoper.Operator
          rsoper.fileid 
          rsoper.Reseller
       WITH FRAME sel.
       xrecid = recid(rsoper).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST rsoper
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST rsoper USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST rsoper USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST rsoper USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(rsoper) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST rsoper
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST rsoper USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST rsoper USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST rsoper USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(rsoper) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

