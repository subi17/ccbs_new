/* ----------------------------------------------------------------------------
  MODULE .......: NNASDE.P
  FUNCTION .....: Maintain A-Sub details
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 29.07.99
  MODIFIED .....: 30.05.00 kl CLI.Pwd added
                  02.07.02 tk return if no CLI record found
                  05.07.02 tk delete -> clstamp
                  16.10.02 lp - F1 change name
                              - show Stamps
                              - UPDATE(enter) only for active series
                              - DELETE not possible (for now)
                  03.03.03 tk tokens            
  Version ......: M15
  -------------------------------------------------------------------------- */
{Func/timestamp.i}
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'cli'}

DEF INPUT PARAM liCustNum  LIKE CLI.CustNum  NO-UNDO.
/*
DEF VAR liCustNum LIKE CLI.CustNum INIT 100003 NO-UNDO.
*/
DEF /*NEW*/ shared VAR siirto AS CHAR.

DEF VAR lcCLI      LIKE CLI.CLI           NO-UNDO.
DEF VAR xrecid     AS RECID                        INIT ?.
DEF VAR firstline  AS INT                 NO-UNDO  INIT 0.
DEF VAR ufkey      AS LOG                 NO-UNDO  INIT TRUE.
DEF VAR delline    AS INT                 NO-UNDO  INIT 0.
DEF VAR Memory     AS RECID               NO-UNDO.
DEF VAR line       AS INT FORMAT "99"     NO-UNDO.

DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR must-add   AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR ok         AS LOG FORMAT "Yes/No" NO-UNDO.

DEF VAR dstamp     AS DEC                 NO-UNDO.
DEF VAR begDay     AS DA INIT today       NO-UNDO FORMAT "99.99.99".
DEF VAR begTime    AS C  INIT "000000"    NO-UNDO.
DEF VAR endDay     AS DA INIT "12-31-49"  NO-UNDO FORMAT "99.99.99".
DEF VAR endTime    AS C  INIT "235959"    NO-UNDO.
DEF VAR begTimeI   AS INT                 NO-UNDO.
DEF VAR endTimeI   AS INT                 NO-UNDO.
DEF BUFFER bufcli FOR CLI.

dstamp = fMakeTS().

form
   CLI.CLI
   CLI.Ref
   CLI.Pwd      FORMAT "99999"
   CLI.ValueLimit
   CLI.Active
WITH OVERLAY ROW 3 SCROLL 1 12 DOWN CENTERED
   COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) " " +
   " Customer " + STRING(liCustNum) + " Maintain A-Sub details " 
   + STRING(pvm,"99-99-99") + " " 
FRAME sel.

form
   " A-Sub.No .........:" CLI.CLI                                 SKIP
   " RefNum ...........:" CLI.Ref                                 SKIP
   " Password .........:" CLI.Pwd      FORMAT "99999"             SKIP
   " A-Sub. limit .....:" CLI.ValueLimit                          SKIP
   " Open .............:" CLI.Active                              SKIP
   " Connected day ....:" begDay
   HELP "Day WHEN A-sub will be connected - DD.MM.YY" begTime  
   HELP "Time WHEN A-sub will be connected - HH:MM:SS"            SKIP
   " Disconnected day .:" endDay
   HELP "Day WHEN A-sub will be disconnected - DD.MM.YY" endTime
   HELP "Time WHEN A-sub will be disconnected - HH:MM:SS"         SKIP
WITH  OVERLAY ROW 6 CENTERED
   COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   fr-header WITH NO-LABELS 
FRAME lis.

form /*  search with field CLI */
   lcCLI
   HELP "Give A-Sub number"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND A-Sub "
   COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME f1.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST CLI WHERE 
           CLI.CustNum = liCustNum
NO-LOCK NO-ERROR.

IF AVAILABLE CLI THEN ASSIGN
   Memory     = recid(CLI)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   MESSAGE "This customer has no CLI records !"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:
   PUT SCREEN ROW 19 COL 30 " Order by A-Sub number ".

print-line:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND CLI WHERE recid(CLI) = Memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = Memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE CLI THEN DO:
               DISP 
                  CLI.CLI
                  CLI.Ref
                  CLI.Pwd
                  CLI.ValueLimit WHEN CLI.ValueLimit NE 0
                  CLI.Active.
               rtab[FRAME-LINE] = recid(CLI).
               FIND NEXT CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         UP FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline  = 0
                must-print = FALSE.
         PAUSE 0 NO-MESSAGE.

         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 701 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 0   
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      CHOOSE ROW CLI.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      IF AVAIL CLI THEN COLOR DISPLAY value(ccc) 
         CLI.CLI 
         CLI.Ref
         CLI.Pwd
         CLI.ValueLimit WHEN CLI.ValueLimit NE 0
         CLI.Active
      WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         MESSAGE "You are on a empty row, move upwards !".
         PAUSE 1 NO-MESSAGE.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* Previous line */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[1] NO-LOCK NO-ERROR.
            FIND PREV CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CLI THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a PREVious one was found */
               SCROLL DOWN.
               DISP
                  CLI.CLI
                  CLI.Ref
                  CLI.Pwd
                  CLI.ValueLimit WHEN CLI.ValueLimit NE 0
                  CLI.Active.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CLI)
               Memory = rtab[1].
            END.
         END.
         ELSE UP 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[FRAME-DOWN] NO-LOCK .
            FIND NEXT CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CLI THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               SCROLL UP.
               DISP 
                  CLI.CLI
                  CLI.Ref
                  CLI.Pwd
                  CLI.ValueLimit WHEN CLI.ValueLimit NE 0
                  CLI.Active.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CLI).
               /* finally LAST line's KeyValue is saved */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* next line */

      /* previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND CLI WHERE recid(CLI) = Memory NO-LOCK NO-ERROR.
         FIND PREV CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
         IF AVAILABLE CLI THEN DO:
            Memory = recid(CLI).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND PREV CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
               IF AVAILABLE CLI THEN Memory = recid(CLI).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the first data page */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL. PAUSE 1 NO-MESSAGE.
         END.
      END. /* previous page */

      /* NEXT page */
      ELSE IF LOOKUP(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
         /* cursor TO the downmost line */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL. PAUSE 1 NO-MESSAGE.
         END.
         ELSE DO: /* the downmost line wasn't empty */
            Memory = rtab[FRAME-DOWN].
            FIND CLI WHERE recid(CLI) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* Haku 1 */
      ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
         cfc = "puyr". RUN Syst/ufcolor.
         lcCLI = "".
         ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
         UPDATE lcCLI WITH FRAME f1.  
         HIDE FRAME f1 NO-PAUSE.
         IF lcCLI <> "" THEN DO: 
            /* 1: active CLI, perfect match */
            FIND FIRST CLI WHERE
                       CLI.CLI      = lcCLI  AND
                       CLI.CrStamp <= dStamp AND
                       CLI.ClStamp >= dStamp
            NO-LOCK NO-ERROR.
            /* 2: any CLI, perfect match */
            IF NOT AVAIL CLI THEN DO:
               FIND FIRST CLI WHERE
                          CLI.CLI = lcCLI
               NO-LOCK NO-ERROR.
            END.
            /* 3: any CLI that begins */
            IF NOT AVAIL CLI THEN DO:
               FIND FIRST CLI WHERE
                          CLI.CLI BEGINS lcCLI
               NO-LOCK NO-ERROR.
            END.
            IF NOT AVAILABLE CLI THEN DO:
               BELL.
               MESSAGE "NONE FOUND !".
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            /*  CLI.CLI was found */
            ASSIGN Memory = recid(CLI) must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* Haku sar. 1 */

      ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO:
         MESSAGE "ARE WE REAL WANT TO DO DELETING THERE ???"   SKIP
                 "If YES -> chang program " SKIP
                 VIEW-AS ALERT-BOX WARNING.
         NEXT LOOP.
      END.

/*  when and if deleting CHECK: 1.active   2.series
      ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
         delline = FRAME-LINE.
         FIND CLI WHERE recid(CLI) = rtab[FRAME-LINE] NO-LOCK.
         IF CAN-FIND(FIRST bufcli  WHERE
                           bufcli.SerNum      = CLI.SerNum AND
                           bufCLI.CLI         <> CLI.CLI)
         THEN DO:
            BELL.
            MESSAGE "You cann't delete only this one number"   SKIP
                    "because it is series. " SKIP(1)
                   VIEW-AS ALERT-BOX ERROR.
            NEXT LOOP.
         END.

         /* line TO be deleted is lightened */
         COLOR DISPLAY value(ctc) 
            CLI.CLI
            CLI.Ref
            CLI.Pwd
            CLI.ValueLimit WHEN CLI.ValueLimit NE 0
            CLI.Active.
         FIND NEXT CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
         IF AVAILABLE CLI THEN Memory = recid(CLI).
         ELSE DO:
            /* the one to be deleted is rereaden */
            FIND CLI WHERE recid(CLI) = rtab[FRAME-LINE] NO-LOCK.
            /* and then the previous one */
            FIND PREV CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
            IF AVAILABLE CLI THEN DO:
               ASSIGN
               delline = delline - 1  /* cause the LAST one is TO be deleted */
               Memory = recid(CLI).
            END.
         END.

         /* 'find' back to the row to be deleted */
         FIND CLI WHERE recid(CLI) = rtab[FRAME-LINE]
         EXCLUSIVE-LOCK.

         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
         COLOR DISPLAY value(ccc) 
            CLI.CLI
            CLI.Ref
            CLI.Pwd
            CLI.ValueLimit WHEN CLI.ValueLimit NE 0
            CLI.Active.
         IF ok THEN DO:

            CLI.ClStamp = fMakeTS().
       /*    DELETE CLI. */

            /* in the LAST record was deleted ? */
            IF NOT CAN-FIND(FIRST CLI WHERE CLI.CustNum = liCustNum)
            THEN DO:
               CLEAR FRAME sel NO-PAUSE.
               PAUSE 0 NO-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE delline = 0. /* wasn't the LAST one */
      END. /* removal */
*/
      /* change or only show */
      ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
      DO WITH FRAME lis TRANSACTION:
         FIND CLI WHERE recid(CLI) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.

         IF CLI.CrStamp <= dStamp AND CLI.ClStamp >= dStamp
         AND lcRight = "RW"
            THEN ASSIGN ok = TRUE fr-header = " CHANGE ".
            ELSE ASSIGN ok = FALSE fr-header = " SHOW ".
         ASSIGN ufkey = TRUE ehto = 9.
         RUN Syst/ufkey.
         cfc = "lis". RUN Syst/ufcolor.

         fSplitTS(INPUT CLI.CrStamp, OUTPUT begDay, OUTPUT begTimeI).
         begTime = STRING(begtimeI,"hh:mm:ss").
         fSplitTS(INPUT CLI.ClStamp, OUTPUT endDay, OUTPUT endTimeI).
         endTime = STRING(endTimeI,"hh:mm:ss").

         DISP 
            CLI.Ref
            CLI.Pwd
            CLI.ValueLimit
            CLI.Active
            CLI.CLI
            begDay
            begTime
            endDay
            endTime
         WITH FRAME lis. 
         IF ok THEN
         UPDATE 
            CLI.Ref
            CLI.Pwd
            CLI.ValueLimit
            CLI.Active        
         WITH FRAME lis. 
         HIDE FRAME lis.
         xrecid = recid(CLI).
      END.

      ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:  /* First record */
         FIND FIRST CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
         ASSIGN Memory = recid(CLI) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* Last record */
         FIND LAST CLI WHERE CLI.CustNum = liCustNum NO-LOCK NO-ERROR.
         ASSIGN Memory = recid(CLI) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

   END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

