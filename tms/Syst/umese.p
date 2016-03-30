/* ------------------------------------------------------
  MODULE .......: UMESE.P
  KUTSUVAMODULI :
  FUNCTION .....: MENUTEKSTIEN LOOP JA VALINTA
  SOVELLUTUS ...: TS
  AUTHOR .......: TT
  CREATED ......: 05.03.91
  MODIFIED .....: 25.06.98 kl vast => ok
                  25.05.99 jp uright1 & uright2 added
                  11.11.02 jr Eventlog  
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF shared VAR siirto AS CHAR.

def var ok          as lo format "Yes/No"   NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR order       AS INT                  NO-UNDO.
DEF VAR ex-order    AS INT                  NO-UNDO.
DEF VAR delline     AS INT                  NO-UNDO.

DEF VAR pois        AS LOG.
DEF VAR firstline   AS INT                  NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 15      NO-UNDO.
DEF VAR ufkey       AS LOG                  NO-UNDO.
DEF VAR must-add    AS LOG                  NO-UNDO.
DEF VAR must-print  AS LOG                  NO-UNDO.
def var yksityinen  as log format "K/E".

DEF VAR memory      AS RECID.
def var line        as int format "99".
DEF VAR ed-menro    LIKE MenuText.MenuNum.
DEF VAR ed-metex    LIKE MenuText.MenuText.
DEF VAR ha-menro    LIKE MenuText.MenuNum.
DEF VAR ha-metex    LIKE MenuText.MenuText.
DEF VAR fr-header   AS CHAR.
def var ens         as char format "x(8)".
def var toi         as char format "x(8)".

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMenuText AS HANDLE NO-UNDO.
   lhMenuText = BUFFER MenuText:HANDLE.
   RUN StarEventInitialize(lhMenuText).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMenuText).
   END.
END.


form /* pAAruutu, scroll */
    MenuText.MenuNum MenuText.MenuText
    WITH ROW 1 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " CHOOSE MENUTEXT "
    FRAME sel.

form
    MenuText.MenuNum label "Number .........." skip(1)
    ens        label "Text's 1.line ..."
    toi        label "       2.line ..."
    WITH  OVERLAY ROW 8 col 5
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header side-labels 1 columns
    FRAME lis.

form /* menunron hakua varten */
    ha-menro
    help "Give a menu number or beginning of it"
    WITH ROW 4 col 2 TITLE
    color value(ctc) " SEEK MENUNUMBER "
    NO-LABELS COLOR value(cfc) OVERLAY FRAME puYR.

form /* menunimen hakua varten */
    ha-metex
    help "Give a menu name or beginning of it"
    WITH ROW 4 col 2 TITLE
    color value(ctc) " SEEK MENUTEXT "
    NO-LABELS COLOR value(cfc) OVERLAY FRAME puHE.

/* Haetaan Company */
FIND FIRST Company no-lock no-error.
IF AVAILABLE Company THEN ASSIGN yvari = TRUE.
ELSE ASSIGN yvari = FALSE.

 cfc = "mese". RUN Syst/ufcolor. ccc = cfc.
 view FRAME sel.

 IF siirto <> ? THEN DO:
    order = 1.
    FIND FIRST MenuText where MenuNum = integer(siirto) USE-INDEX MenuNum
         no-lock no-error.
    IF NOT AVAILABLE MenuText THEN DO:
       FIND FIRST MenuText where
            string(MenuNum) BEGINS siirto USE-INDEX MenuNum
                no-lock no-error.
       IF NOT AVAILABLE MenuText THEN DO:
          FIND FIRST MenuText where MenuNum ge integer(siirto) USE-INDEX MenuNum
               no-lock no-error.
          IF NOT AVAILABLE MenuText THEN DO:
                 BELL.
                 message "NONE FOUND".
                 PAUSE 1 no-message.
                 siirto = ?.
                 RETURN.
          END.
      END.
    END.
END.
ELSE FIND FIRST MenuText USE-INDEX MenuNum no-lock no-error.

 IF AVAILABLE MenuText THEN DO:
    ASSIGN
    must-add = FALSE
    must-print = TRUE
    memory = recid(MenuText).
 END.
 ELSE DO:
    ASSIGN
    must-add = TRUE
    must-print = FALSE.
 END.

 ASSIGN
 ufkey = TRUE
 delline = 0
 firstline = 0
 order = 1.


LOOP:
    repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

       IF order <> ex-order THEN DO:
          ex-order = order.
          if order = 1 then put screen row 19 col 34 "  By MenuNo  ".
          if order = 2 then put screen row 19 col 34 " By MenuText ".
       END.

       IF must-add THEN DO:
          cfc = "lis". RUN Syst/ufcolor.
          fr-header = " ADD MENUTEXT ".
add-new:
          repeat WITH FRAME lis:
             PAUSE 0 no-message.
             CLEAR FRAME lis no-pause.

             FIND LAST MenuText no-lock no-error.
             DISP MenuText.MenuNum + 1 @ MenuText.MenuNum WITH FRAME lis.

             PROMPT-FOR MenuText.MenuNum
             VALIDATE
                (MenuNum = 0 OR
                NOT can-find(MenuText using  MenuNum),
                "MENUTEXT " + string(input MenuNum) + " already exist !").
            IF INPUT MenuNum = 0 THEN LEAVE add-new.
            CREATE MenuText.
            ASSIGN
            MenuNum = INPUT FRAME lis MenuNum.
            ASSIGN
               ens = ""
               toi = "".
             UPDATE ens toi WITH FRAME lis.
             ASSIGN
             MenuText = caps(string(ens,"x(8)")) + caps(string(toi,"x(8)")).
             IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMenuText).
             ASSIGN
             memory = recid(MenuText)
             ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
          END.  /* add-new */

          HIDE FRAME lis no-pause.
          ASSIGN
          must-print = TRUE
          must-add = FALSE.

          FIND FIRST MenuText no-lock no-error.
          IF NOT AVAILABLE MenuText THEN DO:
             PAUSE 0 no-message.
             LEAVE LOOP.
          END.
       END. /* lisAttAvA */


       IF must-print THEN DO:
          FIND MenuText where recid(MenuText) = memory no-lock.
          up FRAME-LINE - 1.
          rtab = ?.
          /* IF a line has just been deleted, THEN ... */
          IF delline > 0 THEN DOWN delline - 1.

          repeat WITH FRAME sel:
             IF AVAILABLE MenuText THEN DO:
                DISPLAY MenuNum MenuText.
                rtab[FRAME-LINE] = recid(MenuText).

                IF order = 1 THEN FIND NEXT MenuText no-lock no-error.
                ELSE FIND NEXT MenuText USE-INDEX MenuText no-lock no-error.
             END.
             ELSE CLEAR FRAME sel no-pause.
             IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
             DOWN 1.
          END.  /* repeat */
          up FRAME-LINE - 1.
          DOWN firstline.
          ASSIGN
          firstline = 0
          must-print = FALSE.
       END. /* must-print */

       /* IF lastly a line has been deleted */
       IF delline > 0 THEN DOWN delline - 1.
       delline=0.

       IF ufkey THEN DO:
          ASSIGN
          siirto = ?
          ufk[1] = 133  ufk[2] = 134  ufk[3] = 0    ufk[4] = 7
          ufk[5] = 5    ufk[6] = 4    ufk[7] = 11   ufk[8] = 8
          ufkey = FALSE ehto = 3.
          {Syst/uright1.i '"4,5,6"'}
          RUN Syst/ufkey.p.
       END.

       PAUSE 0 no-message.

       IF order = 1 THEN DO:
          CHOOSE ROW MenuNum {Syst/uchoose.i} no-error WITH FRAME sel.
          COLOR DISPLAY value(ccc) MenuNum WITH FRAME sel.
       END.
       ELSE DO:
          CHOOSE ROW MenuText {Syst/uchoose.i}    no-error WITH FRAME sel.
          COLOR DISPLAY value(ccc) MenuText WITH FRAME sel.
       END.

       IF rtab[FRAME-LINE] = ? THEN NEXT.

       nap = keylabel(LASTKEY).

       if nap = "cursor-right" THEN DO:
          order = order + 1.
          IF order = 3 THEN order = 1.
       END.
       if nap = "cursor-left" THEN DO:
          order = order - 1.
          IF order = 0 THEN order = 2.
       END.

       IF order <> ex-order THEN DO:
          ASSIGN
          firstline = 0
          memory = rtab[FRAME-LINE].
          FIND MenuText where recid(MenuText) = memory.
          DO i = 1 TO FRAME-LINE - 1:

             IF order = 1 THEN FIND prev MenuText no-lock no-error.
             IF order = 2 THEN FIND prev MenuText USE-INDEX MenuText no-lock no-error.

             IF AVAILABLE MenuText THEN DO:
                ASSIGN
                firstline = i
                memory = recid(MenuText).
             END.
             ELSE LEAVE.
          END.
          must-print = TRUE.
          NEXT LOOP.
       END.

       /* haku */
       if nap = "f1"  or nap = "1" THEN DO:  /* menunron haku */
          PAUSE 0 no-message.
          cfc = "puyr". RUN Syst/ufcolor.
          ha-menro = 0.
          ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
          UPDATE ha-menro WITH FRAME puyr.
          HIDE FRAME puYR.
          IF ha-menro <> 0 THEN DO:
             order = 1.

             FIND FIRST MenuText where MenuNum = ha-menro no-lock no-error.

             IF NOT AVAILABLE MenuText THEN DO:
                FIND FIRST MenuText where string(MenuNum) BEGINS
                                            string(ha-menro) no-lock no-error.

                IF NOT AVAILABLE MenuText THEN DO:
                   FIND FIRST MenuText where MenuNum ge ha-menro no-lock no-error.

                   IF NOT AVAILABLE MenuText THEN DO:
                      message "NONE FOUND".
                      NEXT LOOP.
                   END.
                END.
             END.
             /*  MenuNum was found */
             ASSIGN
             memory = recid(MenuText)
             must-print = TRUE
             order = 1.
             NEXT LOOP.
          END. /* haku menronimellA */
       END. /* f1 */

       else if nap = "f2" or nap = "2" THEN DO: /* menutekstin haku */
          PAUSE 0 no-message.
          cfc = "puhe". RUN Syst/ufcolor.
          ha-metex = "".
          ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
          UPDATE ha-metex WITH FRAME puhe.
          HIDE FRAME puhe.
          if ha-metex <> "" THEN DO:
             /* haetaan henkiloa */

             FIND FIRST MenuText where MenuText = ha-metex
             USE-INDEX MenuText no-lock no-error.
             IF NOT AVAILABLE MenuText THEN DO:
                FIND FIRST MenuText where MenuText BEGINS ha-metex
                USE-INDEX MenuText no-lock no-error.
                IF NOT AVAILABLE MenuText THEN DO:
                   FIND FIRST MenuText where MenuText ge ha-metex
                   USE-INDEX MenuText  no-lock no-error.
                   IF NOT AVAILABLE MenuText THEN DO:
                      message "NONE FOUND".
                      NEXT LOOP.

                   END.
                END.
             END.
             /*  henkilO was found */
             memory = recid(MenuText).
             must-print = TRUE.
             order = 2.
             NEXT LOOP.
          END. /* haku menunimellA */
       END. /* f2 */

       /* previous line */
       else if nap = "cursor-up" THEN DO:
          IF FRAME-LINE = 1 THEN DO:
             FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] no-lock.

             IF order = 1 THEN FIND prev MenuText no-lock no-error.
             ELSE FIND prev MenuText USE-INDEX MenuText no-lock no-error.

             IF NOT AVAILABLE MenuText THEN DO:
                BELL.
                message "You are on the first row !".
                NEXT LOOP.
             END.

             scroll DOWN.
             DO i = FRAME-DOWN TO 2 BY -1:
                rtab[i] = rtab[i - 1].
             END.
             rtab[1] = recid(MenuText).
             DISPLAY MenuNum MenuText.

          END.
          ELSE up 1.
       END.

       /* NEXT line */
       else if nap = "cursor-down" THEN DO:
          IF FRAME-LINE = FRAME-DOWN THEN DO:

             FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] no-lock.
             IF order = 1 THEN FIND NEXT MenuText no-lock no-error.
             ELSE FIND NEXT MenuText USE-INDEX MenuText no-lock no-error.
             IF NOT AVAILABLE MenuText THEN DO:
                BELL.
                message "You are on the last row !".
                PAUSE 1 no-message.
                NEXT LOOP.
             END.
             scroll up.
             DO i=1 TO FRAME-DOWN - 1:
                rtab[i] = rtab[i + 1].
             END.
             rtab[FRAME-DOWN] = recid(MenuText).
             DISPLAY MenuNum MenuText.
          END.
          ELSE DOWN 1.
       END.

       /* previous page */
       else if lookup(nap,"-,page-up,prev-page") > 0 THEN DO:
          memory = rtab[1].
          FIND MenuText where recid(MenuText) = memory no-lock.

          /* peruutetaan edell. sivulle */
          DO i = 1 TO frame-down(sel).
             IF order = 1 THEN FIND prev MenuText no-lock no-error.
             ELSE             FIND prev MenuText USE-INDEX MenuText no-lock no-error.

             IF AVAILABLE MenuText THEN memory = recid(MenuText).
             ELSE DO:
                /* jos ei lOytynyt yhtAAn */
                IF i = 1 THEN DO:
                   BELL.
                   message "You are on the first page !".
                   PAUSE 1 no-message. HIDE MESSAGE.
                   NEXT LOOP.
                END.
                ELSE LEAVE.
             END. /* NOT AVAILABLE */
          END.
          must-print = TRUE.
          NEXT LOOP.
       END.

       /* NEXT page */
       else if lookup(nap,"+,page-down,next-page") > 0 THEN DO WITH FRAME sel:
          IF rtab[FRAME-DOWN] = ? THEN DO:
             BELL.
             message "You are on the last page !".
             NEXT LOOP.
          END.
          ELSE DO:
             memory = rtab[FRAME-DOWN].
             must-print = TRUE.
             NEXT LOOP.
          END.
       END.

       else if nap = "5" or nap = "f5" THEN DO :  /* lisAys */
          {Syst/uright2.i}
          must-add = TRUE.
          NEXT LOOP.
       END.

     else if lookup(nap,"6,f6") > 0 THEN DO:  /* removal */
        {Syst/uright2.i}
        delline = FRAME-LINE.
        FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(cfc) MenuText.MenuNum MenuText.

        FIND NEXT MenuText  no-lock no-error.
        IF AVAILABLE MenuText THEN memory = recid(MenuText).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           FIND prev MenuText  no-lock no-error.
           IF AVAILABLE MenuText THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(MenuText).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) MenuText.MenuNum MenuText.
        IF ok THEN DO:
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMenuText).
            DELETE MenuText.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST MenuText) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.

            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

       else  if nap = "4" or nap = "f4" THEN DO:  /* change */
          {Syst/uright2.i}
          FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] exclusive-lock.
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMenuText).
          ASSIGN
            ens = substring(MenuText,1,8)
            toi = substring(MenuText,9,16).
          assign fr-header = " change fixmenuTEXT " ufkey = TRUE ehto = 9.
          RUN Syst/ufkey.
          cfc = "lis". RUN Syst/ufcolor.
          PAUSE 0 no-message.
          DISPLAY MenuText.MenuNum WITH FRAME lis.
          UPDATE ens toi WITH FRAME lis.
          MenuText = caps(string(ens,"x(8)")) + caps(string(toi,"x(8)")).
          ASSIGN
          ed-metex = MenuText.MenuText.
          ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
          HIDE FRAME lis no-pause.

          IF MenuText <> ed-metex THEN DO:
             ASSIGN
             memory = recid(MenuText)
             must-print = TRUE.
             NEXT LOOP.
          END.
          PAUSE 0 no-message.
          DISPLAY MenuText WITH FRAME sel.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMenuText).
       END.

       else if lookup(nap,"7,f7,enter,return") > 0 THEN DO: /* valinta */
          FIND MenuText where recid(MenuText) = rtab[FRAME-LINE] no-lock.
          siirto = string(MenuNum).
          LEAVE LOOP.
       END.

       else if lookup(nap,"home,h") > 0 THEN DO:
          IF order = 1 THEN FIND FIRST MenuText no-lock.
          ELSE FIND FIRST MenuText USE-INDEX MenuText no-lock.
          ASSIGN
          memory = recid(MenuText)
          must-print = TRUE.
       END.


       else if lookup(nap,"end,e") > 0 THEN DO:
          IF order = 1 THEN FIND LAST MenuText no-lock.
          ELSE FIND LAST MenuText USE-INDEX MenuText no-lock.
          ASSIGN
          memory = recid(MenuText)
          must-print = TRUE.
       END.

       else if nap = "8" or nap = "f8" THEN LEAVE LOOP.
    END.  /* LOOP */


HIDE FRAME sel no-pause.

