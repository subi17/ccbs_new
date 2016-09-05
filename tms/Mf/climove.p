/* -----------------------------------------------
  MODULE .......: climove.p
  FUNCTION .....: Move customers CLIs TO someone ELSE
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 29.01.02
  MODIFIED  ....: 27.03.03 kl M1.0

  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}

DEF INPUT PARAMETER CustNum LIKE Customer.CustNum.

DEF NEW shared VAR siirto AS c.

def var ok         AS lo format "Yes/No"  NO-UNDO.
DEF VAR memory     AS RECID               NO-UNDO.
def var line       AS int format "99"     NO-UNDO.
DEF VAR delline    AS INT                 NO-UNDO.
DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR ufkey      AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR asub       LIKE CLISer.CLIFrom    NO-UNDO.
def var presel     AS lo format "PRESEL/" NO-UNDO.
DEF VAR movecust   LIKE CLISer.CustNum    NO-UNDO.
DEF VAR movebt     LIKE CLISer.BillTarget NO-UNDO.
DEF VAR movedate   AS DATE                NO-UNDO FORMAT "99-99-99".
DEF VAR movetime   AS CHAR                NO-UNDO FORMAT "99:99:99".
DEF VAR lTimeStamp AS DEC                 NO-UNDO FORMAT "99999999.99999".
DEF VAR movestate  AS lo                  NO-UNDO.
DEF VAR dCLI1      AS dec                 NO-UNDO.
DEF VAR dCLI2      AS dec                 NO-UNDO.
DEF VAR dTmp       AS dec                 NO-UNDO.

DEF VAR xrecid AS RECID.

DEF BUFFER bufCLISer FOR CLISer.
DEF BUFFER bufCLI    FOR CLI.

DEF TEMP-TABLE ttCLISer NO-UNDO LIKE CLISer
   field move  as lo format "MOVE/"
   field cust  like CLISer.CustNum    column-label "Move to"
   field bt    like CLISer.BillTarget column-label "BT"
   field valid  as da                 column-label "ValidFrom" 
      FORMAT "99-99-99"
   field vtime  as char               column-label "Time"
      FORMAT "99:99:99"
   field tstamp as dec format "99999999.99999".

FUNCTION fCheckTS RETURNS LOGICAL:

   IF lTimeStamp = 0 THEN DO:
      MESSAGE
         "Starting time for new customer has not been set !"
      VIEW-AS ALERT-BOX.
   END.

   RETURN (lTimeStamp NE 0).

END FUNCTION.

FOR EACH CLISer NO-LOCK WHERE
         CLISer.CustNum = CustNum:
   CREATE ttCLISer.
   buffer-copy CLISer TO ttCLISer.
END.         

RUN pReset(1,output ok).
IF NOT ok THEN RETURN.

FIND FIRST Customer WHERE
           Customer.CustNum = CustNum
NO-LOCK NO-ERROR.

form
   ttCLISer.move       column-label "Move"
   presel              column-label "Presel"
   ttCLISer.BillTarget column-label "BT"
   ttCLISer.CLIFrom    column-label "A-Sub No. From"
   ttCLISer.CLITo      column-label "A-Sub No. Till"
   ttCLISer.Valid
   ttCLISer.vTime
      help "A Text for Customer Care"        
WITH OVERLAY scroll 1 15 DOWN ROW 1 centered WIDTH 80
   COLOR value(cfc) TITLE COLOR value(ctc)
   " " + string(Customer.CustNum) + " " + 
   substr(Customer.CustName,1,16) + ":  A-sub. nos. "
FRAME sel.

FORM 
   SKIP(1)
   "   Date :" movedate SKIP
   "   Time :" movetime
   SKIP(1)
WITH 
   OVERLAY CENTERED NO-LABELS TITLE " SET TIME STAMP " WIDTH 24 ROW 7 
FRAME frmSetTime.

form /* FIND asub */
   asub help "Enter A-Number OR beginning of it"
with row 4 col 2 title color value(ctc) " FIND ASUB No. "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME search-1.

cfc = "kline". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

ASSIGN
   xrecid  = ?
   delline = 0
   ufkey   = TRUE.

LOOP:
repeat WITH FRAME sel:

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND ttCLISer WHERE recid(ttCLISer) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose keyvalue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE ttCLISer THEN DO:

               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(ttCLISer).
               FIND NEXT ttCLISer WHERE ttCLISer.CustNum = CustNum
               USE-INDEX CustNum NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         up FRAME-LINE - 1.
         ASSIGN must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been printed AND
         mAllA linellA choosea odotellen. */

      END. /* must-print = TRUE */

   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:

         ASSIGN
            ufk[1] = 701
            ufk[2] = 702
            ufk[3] = 509
            ufk[4] = 507
            ufk[5] = 56
            ufk[6] = 508
            ufk[8] = 8 
            ufk[9] = 1
            ehto   = 3
            ufkey  = FALSE.

         RUN Syst/ufkey.p.

      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW ttCLISer.CLIFrom {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
      COLOR DISPLAY value(ccc) ttCLISer.CLIFrom WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on an empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* Search 1 */
      if lookup(nap,"F1,1") > 0 THEN DO:
         cfc = "puyr". RUN Syst/ufcolor.p.
         assign
            asub  = ""
            ehto  = 9
            ufkey = TRUE.
         RUN Syst/ufkey.p. 
         UPDATE asub WITH FRAME search-1.
         HIDE FRAME search-1 no-pause.
         if asub <> "" THEN DO:
            FIND FIRST ttCLISer WHERE 
                       ttCLISer.CustNum = Customer.CustNum AND
                       ttCLISer.CLIFrom >= asub
            USE-INDEX CustNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Customer THEN DO:
               bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
            END.
            /*  Customer  was found */
            ASSIGN memory = recid(ttCLISer) must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* Search col. 1 */

      if lookup(nap,"F2,2") > 0 THEN DO:

         FIND ttCLISer WHERE recid(ttCLISer) = rtab[FRAME-LINE] EXCLUSIVE-LOCK.
         IF ttCLISer.move = FALSE THEN DO:
            MESSAGE 
               "Number is NOT defined as moved !"
            VIEW-AS ALERT-BOX MESSAGE.
            NEXT BROWSE.
         END.

         RUN pGetCustNum.

      END.

      ELSE IF LOOKUP(nap,"F3,3") > 0 THEN DO:

         FIND ttCLISer WHERE recid(ttCLISer) = rtab[FRAME-LINE] EXCLUSIVE-LOCK.

         IF ttCLISer.move = FALSE THEN DO:
            MESSAGE 
               "Number is NOT defined as moved !"
            VIEW-AS ALERT-BOX MESSAGE.
            NEXT BROWSE.
         END.

         IF movecust = 0 THEN DO:
            MESSAGE 
               "New customer number is NOT defined !"
            VIEW-AS ALERT-BOX MESSAGE.
            NEXT BROWSE.
         END.

         ASSIGN
            ttCLISer.cust   = movecust
            ttCLISer.bt     = movebt
            ttCLISer.valid  = movedate
            ttCLISer.vtime  = movetime
            ttCLISer.tstamp = lTimeStamp.

         DISP
            ttCLISer.cust
            ttCLISer.bt
            ttCLISer.valid
            ttCLISer.vtime
         WITH FRAME sel.

      END.
      ELSE IF LOOKUP(nap,"F4,4") > 0 THEN DO:

         FIND ttCLISer WHERE recid(ttCLISer) = rtab[FRAME-LINE] EXCLUSIVE-LOCK.

         ASSIGN
            ehto  = 9
            ufkey = TRUE.

         RUN Syst/ufkey.p.

         ASSIGN
            movedate = today
            movetime = "000000".

         UPDATE
            movedate
            movetime
         WITH FRAME frmSetTime EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF LOOKUP(nap,poisnap) > 0 THEN DO:
               IF FRAME-FIELD = "movetime" AND
                  NOT fCheckTime(INPUT FRAME frmSetTime movetime) THEN DO:
                  MESSAGE 
                     "Invalid time: " + INPUT FRAME frmSetTime movetime 
                  VIEW-AS ALERT-BOX error.
                  NEXT-PROMPT movetime WITH FRAME frmSetTime.
                  NEXT.
               END.
            END.

            APPLY LASTKEY.

         END.

         lTimeStamp = fHMS2TS(movedate,movetime).

         IF ttCLISer.move = TRUE THEN DO:

            ASSIGN
               ttCLISer.valid  = movedate
               ttCLISer.vtime  = movetime
               ttCLISer.tstamp = lTimeStamp.

            DISP
               ttCLISer.valid
               ttCLISer.vtime
            WITH FRAME sel.

         END.

         HIDE FRAME frmSetTime.

      END.
      ELSE IF LOOKUP(nap,"F5,5") > 0 THEN DO:

         IF NOT fCheckTS() THEN NEXT BROWSE.

         FIND ttCLISer WHERE recid(ttCLISer) = rtab[FRAME-LINE] EXCLUSIVE-LOCK.
         ASSIGN movestate = ttCLISer.move.

         IF movestate = TRUE AND movecust = 0 THEN RUN pGetCustNum.
         ELSE IF movestate = FALSE THEN movecust = 0.

         IF movecust NE ? THEN DO:

            FOR EACH ttCLISer:

               ASSIGN
                  ttCLISer.move   = movestate
                  ttCLISer.cust   = movecust
                  ttCLISer.bt     = movebt
                  ttCLISer.valid  = movedate
                  ttCLISer.vtime  = movetime
                  ttCLISer.tstamp = lTimeStamp.

            END.

         END.

         ASSIGN
            must-print = TRUE 
            ufkey      = TRUE.

         NEXT LOOP.

      END.
      ELSE IF LOOKUP(nap,"F6,6") > 0 THEN DO:

         IF NOT fCheckTS() THEN NEXT BROWSE.

         FIND FIRST ttCLISer WHERE
                    ttCLISer.move = TRUE AND
                    ttCLISer.cust NE 0
         NO-LOCK NO-ERROR.

         IF NOT AVAIL ttCLISer THEN DO:
            message "Nothing to do !" VIEW-AS ALERT-BOX MESSAGE.
            NEXT BROWSE.
         END.

         MESSAGE
            "New starting time stamp is set as:" +
            STRING(movedate) + " " + movetime    SKIP
            "Are You sure You want to start ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.

         IF ok THEN DO TRANSACTION:

            FOR EACH ttCLISer NO-LOCK WHERE
                     ttCLISer.move = TRUE AND
                     ttCLISer.cust > 0:

               FIND FIRST CLISer WHERE
                          CLISer.CLIFrom = ttCLISer.CLIFrom AND
                          CLISer.CLITo   = ttCLISer.CLITo   AND
                          CLISer.CustNum = ttCLISer.CustNum
               NO-LOCK NO-ERROR.

               CREATE bufCLISer.
               BUFFER-COPY CLISer EXCEPT 
                  CLISer.CustNum
                  CLISer.SerNum
               TO bufCLISer.

               ASSIGN
                  bufCLISer.CustNum = ttCLISer.CustNum
                  bufCLISer.SerNum  = NEXT-VALUE(CLISeries)
                  dCLI1 = dec(CLISer.CLIFrom)
                  dCLI2 = dec(CLISer.CLITo).

               DO dTmp = dCLI1 TO dCLI2:

                  FIND FIRST CLI WHERE
                             CLI.CustNum = CLISer.CustNum AND
                         dec(CLI.CLI)    = dTMp
                  EXCLUSIVE-LOCK NO-ERROR.

                  CREATE bufCLI.
                  BUFFER-COPY CLI EXCEPT
                     CLI.CustNum
                     CLI.SerNum
                  TO bufCLI.

                  ASSIGN
                     bufCLI.CustNum = bufCLISer.CustNum.
                     bufCLI.SerNum  = bufCLISer.SerNum.

               END.

            END.

            empty TEMP-TABLE ttCLISer.

         END.

         FOR EACH CLISer NO-LOCK WHERE
                  CLISer.CustNum = CustNum:
            CREATE ttCLISer.
            buffer-copy CLISer TO ttCLISer.
         END.         

         ASSIGN
            must-print = TRUE
            ufkey      = TRUE.

         RUN pReset(2,output ok).
         IF NOT ok THEN RETURN.
         ELSE NEXT LOOP.

      END.

      /* previous line */
      ELSE IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND ttCLISer WHERE recid(ttCLISer) = rtab[1] NO-LOCK.
            FIND prev ttCLISer WHERE ttCLISer.CustNum = CustNum
            USE-INDEX CustNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCLISer THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(ttCLISer)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND ttCLISer WHERE recid(ttCLISer) = rtab[FRAME-DOWN] NO-LOCK .
            FIND NEXT ttCLISer WHERE ttCLISer.CustNum = CustNum
            USE-INDEX CustNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCLISer THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(ttCLISer).
               /* finally LAST line's keyvalue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND ttCLISer WHERE recid(ttCLISer) = memory NO-LOCK NO-ERROR.
         FIND prev ttCLISer WHERE ttCLISer.CustNum = CustNum
         USE-INDEX CustNum NO-LOCK NO-ERROR.

         IF AVAILABLE ttCLISer THEN DO:
            memory = recid(ttCLISer).

            /* mennaan tiedostoa taaksepain 1 sivun verran */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev ttCLISer WHERE ttCLISer.CustNum = CustNum
               USE-INDEX CustNum NO-LOCK NO-ERROR.
               IF AVAILABLE ttCLISer THEN memory = recid(ttCLISer).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:

        /* cursor TO the downmost line */

        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND ttCLISer WHERE recid(ttCLISer) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO TRANSACTION:
        /* change */
        FIND ttCLISer WHERE recid(ttCLISer) = rtab[frame-line(sel)] 
        EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN 
           ttCLISer.move = NOT    ttCLISer.move
           ttCLISer.cust = 0 when ttCLISer.move.
        DISP 
           "" @ ttCLISer.cust
           ttCLISer.cust when ttCLISer.cust NE 0
           ttCLISer.move 
        WITH FRAME sel.
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO: /* ensimmainen tietue */
        FIND FIRST ttCLISer WHERE 
                   ttCLISer.CustNum = CustNum 
        USE-INDEX CustNum NO-LOCK NO-ERROR.
        ASSIGN
           memory     = recid(ttCLISer)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        FIND LAST ttCLISer WHERE
                  ttCLISer.CustNum = CustNum 
        USE-INDEX CustNum NO-LOCK.
        ASSIGN
           memory     = recid(ttCLISer)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

    /* check IF this number has a PRESELECTION TRANSACTION record */
    IF CAN-FIND(FIRST PreSel WHERE PreSel.CLI >= ttCLISer.CLIFrom AND
                                   PreSel.CLI <= ttCLISer.CLITo)
    THEN ASSIGN presel = TRUE. 
    ELSE        presel = FALSE.

    DISPLAY
       ttCLISer.move
       ttCLISer.BillTarget
       ttCLISer.CLIFrom
       ttCLISer.CLITo
       presel
       "" @ ttCLISer.cust
       "" @ ttCLISer.bt
       "" @ ttCLISer.valid
       "" @ ttCLISer.vtime
       ttCLISer.cust  when ttCLISer.cust NE 0
       ttCLISer.bt    when ttCLISer.cust NE 0
       ttCLISer.valid when ttCLISer.cust NE 0
       ttCLISer.vtime when ttCLISer.cust NE 0
    WITH FRAME  sel.

END. /* PROCEDURE */

PROCEDURE pReset:

   DEF INPUT  PARAMETER pNum AS i  NO-UNDO.
   DEF OUTPUT PARAMETER pRet AS lo NO-UNDO.

   DEF VAR msg AS c NO-UNDO.

   FIND FIRST ttCLISer WHERE 
              ttCLISer.CustNum = CustNum USE-INDEX CustNum 
   NO-LOCK NO-ERROR.

   IF AVAILABLE ttCLISer THEN ASSIGN
      memory     = recid(ttCLISer)
      must-print = TRUE
      pRet       = TRUE.
   ELSE DO:
      case pNum:
         when 1 then msg = "No A-numbers found !".
         when 2 then msg = "All A-numbers were moved !".
      END.
      HIDE FRAME sel.
      MESSAGE msg VIEW-AS ALERT-BOX MESSAGE.
      ASSIGN pRet = FALSE.
   END.

END PROCEDURE.

PROCEDURE pGetCustNum:

   ASSIGN
      siirto = ?
      ufkey  = TRUE.

   RUN Mc/nnasel.p.

   IF int(siirto) NE ? THEN DO:
      i = 0.
      FOR EACH BillTarget NO-LOCK WHERE
               BillTarget.CustNum = int(siirto):
         i = i + 1.
      END.
      ASSIGN
         ttCLISer.cust = int(siirto)
         movecust      = ttCLISer.cust
         ttCLISer.bt   = 1 WHEN i = 1
         movebt        = ttCLISer.bt.
   END.
   ttCLISer.valid = movedate.
   DISP
      "" @ ttCLISer.cust
      "" @ ttCLISer.bt
      "" @ ttCLISer.valid
      ttCLISer.cust  when ttCLISer.cust NE 0
      ttCLISer.bt    when ttCLISer.cust NE 0
      ttCLISer.valid when ttCLISer.cust NE 0
   WITH FRAME sel.

   IF ttCLISer.bt = 0 THEN DO:

      MESSAGE
         "You have to choose correct billing target." SKIP
         "Customer " + STRING(ttCLISer.cust) + " has " + STRING(i) +
         " billing targets."
      VIEW-AS ALERT-BOX.

      RUN Help/h-billtarg.p(ttCLISer.cust).

      IF siirto NE ? THEN DO:
         ASSIGN
            ttCLISer.bt = INT(siirto)
            movebt      = ttCLISer.bt.
         DISP ttCLISer.bt WITH FRAME sel.
      END.

   END.

END PROCEDURE.
