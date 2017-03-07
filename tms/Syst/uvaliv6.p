/* -----------------------------------------------
  MODULE .......: UVALIV6.P
  FUNCTION .....: Maintain MenuText tree
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 19.05.98
  MODIFIED .....: 27.04.99 pt MenuClass
                  25.05.99 jp uright1 & uright2 added  
                  24.08.99 pt MenuNum expanded into "zzz9"
                  07.08.02 tk find with module
                  06.11.02 jr Eventlog
                  24.02.03 tk update for tokencode
                  06.03.03 tk tokens
                  07.04.04 jp length of module name
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MenuTree'}

DEF shared VAR siirto AS CHAR.

DEF BUFFER xMenuTree FOR MenuTree.
DEF VAR haku        LIKE MenuTree.Level             NO-UNDO.
DEF VAR haku2       LIKE MenuTree.MenuId            NO-UNDO.
DEF VAR haku3       LIKE MenuTree.Module            NO-UNDO.
DEF VAR firstline   AS INT                          NO-UNDO.
DEF VAR order       AS INT                          NO-UNDO.
DEF VAR ex-order    AS INT                          NO-UNDO.
DEF VAR memory      AS RECID                        NO-UNDO.
def var line        as int format "99"              NO-UNDO.
DEF VAR delline     AS INT                          NO-UNDO.
DEF VAR must-print  AS LOG                          NO-UNDO.
DEF VAR must-add    AS LOG                          NO-UNDO.
DEF VAR ufkey       AS LOG                          NO-UNDO.
def var tx1         as char format "x(8)"           NO-UNDO.
def var tx2         as char format "x(8)"           NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24              NO-UNDO.
def var mt          as char format "x(8)" EXTENT 16 NO-UNDO.
DEF VAR i           AS INT                          NO-UNDO.
DEF VAR xrecid      AS RECID.
DEF VAR pois        AS LOG                          NO-UNDO.
DEF VAR ed-taso     AS CHAR                         NO-UNDO.
DEF VAR ed-pka      AS INT                          NO-UNDO.
def var mista       as char format "x(8)"           NO-UNDO.
def var minne       as char format "x(8)"           NO-UNDO.
def var ke          as log format "Yes/No"          NO-UNDO.
def var ok          as log format "Yes/No"          NO-UNDO.

DEF STREAM fixmenu1.
DEF STREAM fixmenu2.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMenuTree AS HANDLE NO-UNDO.
   lhMenuTree = BUFFER MenuTree:HANDLE.
   RUN StarEventInitialize(lhMenuTree).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMenuTree).
   END.
END.

form
    skip(1)
    " Instr: You can move one MenuText Level to a new place with one " SKIP
    "        move by determining Level THAT is to be moved and a "  SKIP
    "        new Level WHERE submenus are to be moved.           "  skip(2)
    mista  label "Menulevel that is moved ..........."
    help "Level of the MenuText that is to be moved " SKIP
    minne  label "New Level for submenu ............."
    help "New Level for moved fixmenus " SKIP

    with color value(cfc) title color value(ctc) " MOVE SUBMENU "
         OVERLAY centered ROW 4 FRAME siirto side-labels.


form
   mt[1 FOR 4] space(8) mt[5 FOR 4]  SKIP
   mt[9 FOR 4] space(8) mt[13 FOR 4]
   with centered title " fixmenuLEVEL " + MenuTree.Level + " " OVERLAY
   ROW 18 NO-LABEL FRAME meteks.

form
    MenuTree.Level  label "Menu Level ....."
             format "x(8)"
             help  "Level 1 ... 8 and 0: functions outside MenuText "
             SKIP
    MenuTree.Position   label "Menu place ....." 
             help  "Place 1 ... 8 or  0 if Level is 0"
             SKIP
    MenuTree.MenuNum   label "MENU- number ..." format "zzz9" tx1 AT 50 NO-LABEL
             SKIP
    MenuTree.MenuType  label "Function type .." tx2 AT 50 NO-LABEL
             SKIP
    MenuTree.Module  FORMAT "X(40)" label "Module ........."
             help "The name of the module Called if function = 3, 
                   otherwise empty"
             SKIP
    MenuTree.MenuId   label "Function code .."
             SKIP
    MenuTree.MenuTitle label "Function name .." 
             help "Func. name (lowercase) or MenuText HEADER (uppercase)"
    MenuTree.MenuClass    label "Program Class .."
    MenuClass.MCName NO-LABEL
             SKIP
    MenuTree.State[1] label "Deny usage ....."
             help "If YES the this MenuText can't be used !" SKIP
    MenuTree.tokencode label "Token code ....."
             help "Token code for this menu item"

    WITH OVERLAY ROW 6 centered side-labels
    TITLE COLOR value(ctc) fr-header COLOR value(cfc)
    FRAME lis.

form
    MenuTree.Level   /* column-label "Level"         */ format "x(8)"
    MenuTree.Position    /* column-label "P"             */
    MenuTree.MenuNum    /* column-label "Mno"           */ format "zzz9"
    tx1                 column-label "Key-"
    tx2                 column-label "Label"
    MenuTree.MenuType   /* column-label "F"             */
    MenuTree.Module   /* column-label "Module"        */
    MenuTree.MenuId    /* column-label "Code"          */
    MenuTree.MenuTitle  /* column-label "Function name" */ format "x(15)" 
    MenuTree.State[1]   column-label "S"    format  "*/"
    MenuTree.MenuClass     /* column-label "PgCl"          */
 WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi + " MenuText TREE "
    + string(pvm,"99-99-99") + " "
 FRAME sel.


form /* tason hakua varten */
    haku
    help "Enter Level No"                           
    with row 4 col 1 title color value(ctc) " FIND Level "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /*  tunnuksen hakua varten */
    haku2
    help "Enter Function Code"                          
    with row 4 col 47 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /*  Search by module  */
    haku3
    help "Enter Module Name"                          
    with row 4 col 35 title color value(ctc) " FIND MODULE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.
   FIND FIRST MenuTree no-lock no-error.
   IF AVAILABLE MenuTree THEN ASSIGN memory = recid(MenuTree)
      must-print = TRUE must-add    = FALSE.
   ELSE DO:
      IF lcRight NE "RW" THEN DO:
         MESSAGE "No menutree available !" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ELSE ASSIGN memory = ? must-print = FALSE must-add    = TRUE.
   END.  
ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 "  Order by Level ".
       if order = 2 then put screen row 19 col 30 "  Order by code  ".
       if order = 2 then put screen row 19 col 30 "  Order by code  ".
    END.

   IF must-add THEN DO:  /* valikon lisays  */
      assign cfc = "lis" ufkey = true fr-header = " LISAYS " must-add = FALSE.
      RUN Syst/ufcolor.p.

add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         repeat TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE add-new:

            CREATE MenuTree.
            ufkey = TRUE.
            ehto = 9. RUN Syst/ufkey.p.

            UPDATE
                MenuTree.Level 
                MenuTree.Position
                MenuTree.MenuNum 
                MenuTree.MenuType 
                MenuTree.Module
                MenuTree.MenuId when INPUT FRAME lis 
                MenuTree.MenuType NE 3
                MenuTree.MenuTitle 
                MenuTree.MenuClass 
                MenuTree.State[1] 
                MenuTree.tokencode
            WITH FRAME lis  EDITING:

               READKEY.
               nap = keylabel(LASTKEY).
               pois = lookup(nap,poisnap) > 0.
               IF pois THEN DO WITH FRAME lis:
                  HIDE MESSAGE.

                  if frame-field = "Level" THEN DO:
                     if input frame lis MenuTree.Level = ""
                     THEN UNDO add-new, LEAVE add-new.
                  END.

                  else if frame-field = "Position" THEN DO:

                     /* 0-tasolla saa olla "orpoja" toimintoja */
                     if input frame lis MenuTree.Level = "0" THEN DO:
                        APPLY LASTKEY.
                        NEXT.
                     END.

                     /* onko duplikaattia valikossa */
                     FIND FIRST xMenuTree where recid(xMenuTree) <> recid(MenuTree)
                     AND xMenuTree.Level = INPUT FRAME lis MenuTree.Level
                     AND xMenuTree.Position  = INPUT FRAME lis MenuTree.Position
                     no-lock no-error.
                     IF AVAIL xMenuTree THEN DO:
                        BELL.
                        message "This level/slot is already in use !".
                        NEXT.
                     END.
                  END.

                  else if frame-field = "MenuNum" THEN DO:
                     if input frame lis MenuTree.Level = "0" THEN DO:
                        APPLY LASTKEY.
                        NEXT.
                     END.

                     /* MenuText pakollinen jos taso <> "0" */
                     FIND FIRST MenuText where 
                                MenuText.MenuNum = INPUT FRAME lis
                     MenuTree.MenuNum no-lock no-error.
                     IF AVAILABLE MenuText THEN DO:

                        ASSIGN
                        tx1 = substring(MenuText,1,8)
                        tx2 = substring(MenuText,9).
                        DISPLAY tx1 tx2 WITH FRAME lis.
                     END.
                     ELSE DO:
                        BELL.
                        message "Menu text is not found !".
                        NEXT.
                     END.
                  END.

                  else if frame-field = "MenuId" THEN DO:
                     if input frame lis MenuTree.MenuId <> "" THEN DO:
                        FIND FIRST xMenuTree where xMenuTree.MenuId = INPUT FRAME
                        lis MenuTree.MenuId AND recid(MenuTree) <> recid(xMenuTree)
                        no-lock no-error.
                        IF AVAIL xMenuTree THEN DO:
                           BELL.
                           message "Function Code '" + INPUT
                           FRAME lis MenuTree.MenuId
                           + " already exists: level" + string(xMenuTree.Level)
                           + " slot " + string(xMenuTree.Position) + " !".
                           NEXT.
                        END.
                     END.

                     else if frame-field = "MenuClass" THEN DO:
                        FIND MenuClass where 
                             MenuClass.MenuClass = 
                             INPUT FRAME lis MenuTree.MenuClass
                        no-lock no-error.
                        IF NOT AVAIL MenuClass THEN DO:
                           BELL.
                           message "Unknown Program Class !".
                           NEXT.
                        END.
                        DISP MenuClass.MCName.
                     END.   

                     ELSE IF frame-field = "tokencode" THEN DO:
                         FIND FIRST token WHERE
                                    token.tokencode = input MenuTree.tokencode
                         NO-LOCK NO-ERROR.
                         IF NOT AVAIL token THEN DO:
                            MESSAGE "Unknown token !".        
                            NEXT.
                         END.           
                     END.

                  END.
               END.
               APPLY LASTKEY.

            END. /* EDITING */
            memory = recid(MenuTree).

            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMenuTree).
         END.
      END.  /* add-new */


      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST MenuTree no-lock no-error.
      IF NOT AVAILABLE MenuTree THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND MenuTree where recid(MenuTree) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE MenuTree THEN DO:
               FIND MenuText where MenuNum = MenuTree.MenuNum no-lock no-error.
               IF AVAILABLE MenuText THEN
                  ASSIGN tx1 = substring(MenuText,1,8)
                         tx2 = substring(MenuText,9).
               else assign tx1 = "?" tx2 = "".
               if MenuTree.Level = "0" then assign tx1 = "" tx2 = "".
               DISPLAY
               MenuTree.Level MenuTree.Position MenuTree.MenuNum tx1 tx2
               MenuTree.MenuType MenuTree.Module MenuTree.MenuId
               MenuTree.MenuTitle
               MenuTree.State[1] MenuTree.MenuClass.

               rtab[FRAME-LINE] = recid(MenuTree).
               IF order = 1 THEN FIND NEXT MenuTree no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT MenuTree USE-INDEX Module
               no-lock no-error.
               ELSE IF order = 3 THEN FIND NEXT MenuTree USE-INDEX MenuId
               no-lock no-error.
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
         malla linella choosea varten. */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 22  ufk[2]= 1150 ufk[3]= 35 ufk[4]= 301
         ufk[5]= (IF lcRight = "RW" THEN 5   ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4   ELSE 0)
         ufk[7]= (IF lcRight = "RW" THEN 140 ELSE 0)
         ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW MenuTree.Level {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) MenuTree.Level WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW MenuTree.Module {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) MenuTree.Module WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW MenuTree.MenuId {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) MenuTree.MenuId WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 4 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 3.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND MenuTree where recid(MenuTree) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev MenuTree no-lock no-error.
            ELSE IF order = 2 THEN FIND prev MenuTree use-index Module
            no-lock no-error.
            ELSE IF order = 3 THEN FIND prev MenuTree USE-INDEX MenuId
            no-lock no-error.
            IF AVAILABLE MenuTree THEN
               ASSIGN firstline = i memory = recid(MenuTree).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on an empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND MenuTree where recid(MenuTree) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev MenuTree no-lock no-error.
            ELSE IF order = 2 THEN FIND prev MenuTree use-index Module
            no-lock no-error.
            ELSE IF order = 3 THEN FIND prev MenuTree USE-INDEX MenuId
            no-lock no-error.
            IF NOT AVAILABLE MenuTree THEN DO:
               message "This is the first row !".                
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               FIND MenuText where MenuNum = MenuTree.MenuNum no-lock no-error.
               IF AVAILABLE MenuText THEN
                  ASSIGN tx1 = substring(MenuText,1,8)
                         tx2 = substring(MenuText,9).
               else assign tx1 = "?" tx2 = "".
               if MenuTree.Level = "0" then assign tx1 = "" tx2 = "".
               DISPLAY
               MenuTree.Level MenuTree.Position MenuTree.MenuNum tx1 tx2
               MenuTree.MenuType MenuTree.Module MenuTree.MenuId MenuTree.MenuTitle
               MenuTree.State[1] MenuTree.MenuClass.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(MenuTree)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND MenuTree where recid(MenuTree) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT MenuTree no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT MenuTree use-index Module
            no-lock no-error.
            ELSE IF order = 3 THEN FIND NEXT MenuTree USE-INDEX MenuId
            no-lock no-error.
            IF NOT AVAILABLE MenuTree THEN DO:
               message "This is the last row !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               FIND MenuText where MenuNum = MenuTree.MenuNum no-lock no-error.
               IF AVAILABLE MenuText THEN
                  ASSIGN tx1 = substring(MenuText,1,8)
                         tx2 = substring(MenuText,9).
               else assign tx1 = "?" tx2 = "".
               if MenuTree.Level = "0" then assign tx1 = "" tx2 = "".
               DISPLAY
               MenuTree.Level MenuTree.Position MenuTree.MenuNum tx1 tx2
               MenuTree.MenuType MenuTree.Module MenuTree.MenuId MenuTree.MenuTitle
               MenuTree.State[1] MenuTree.MenuClass.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(MenuTree).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND MenuTree where recid(MenuTree) = memory no-lock no-error.
         IF order = 1 THEN FIND prev MenuTree no-lock no-error.
         ELSE IF order = 2 THEN FIND prev MenuTree use-index Module
         no-lock no-error.
         ELSE IF order = 3 THEN FIND prev MenuTree USE-INDEX MenuId
         no-lock no-error.
         IF AVAILABLE MenuTree THEN DO:
            memory = recid(MenuTree).

            /* mennaan tiedostoa taaksepain 1 sivun verran */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev MenuTree no-lock no-error.
               ELSE IF order = 2 THEN FIND prev MenuTree use-index Module
               no-lock no-error.
               ELSE IF order = 3 THEN FIND prev MenuTree USE-INDEX MenuId
               no-lock no-error.
               IF AVAILABLE MenuTree THEN memory = recid(MenuTree).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "THIS IS THE FIRST ROW !". 
            BELL. 
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE !".
            BELL. 
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND MenuTree where recid(MenuTree) = memory no-lock.
            must-print = TRUE. NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN Syst/ufcolor.p. haku = "". ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        FIND MenuTree where recid(MenuTree) = rtab[FRAME-LINE] no-lock.
        if MenuTree.Level ne "0" THEN DO:
           IF MenuTree.MenuType = 2 THEN haku = MenuTree.Level + string(
           MenuTree.Position).
           IF MenuTree.MenuType = 3 THEN haku = substring(MenuTree.Level,1,
           length(MenuTree.Level) - 1).
        END.
        UPDATE haku WITH FRAME hayr. HIDE FRAME hayr no-pause.
        if haku <> "" THEN DO:
           FIND FIRST MenuTree where MenuTree.Level >= haku
           no-lock no-error.
           IF NOT AVAILABLE MenuTree THEN DO:
                 BELL.
                 message "NOT FOUND !".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
           END.
           /*    was found */
           ASSIGN
           order = 1 memory = recid(MenuTree) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 3 */
     if lookup(nap,"3,f3") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN Syst/ufcolor.p.
        haku2 = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST MenuTree where MenuTree.MenuId >= INPUT haku2
           use-index MenuId no-lock no-error.
           IF NOT AVAILABLE MenuTree THEN DO:
                 bell. message "NOT FOUND !". 
                 PAUSE 1 no-message.
                 NEXT BROWSE.
           END.
           /*    was found */
           ASSIGN order = 3 memory = recid(MenuTree) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */

     /* Search by module */
     if lookup(nap,"2,f2") > 0 THEN DO: 
        cfc = "puyr". RUN Syst/ufcolor.p.
        haku3 = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku3 WITH FRAME hayr3.
        HIDE FRAME hayr3 no-pause.
        if haku3 <> "" THEN DO:
           FIND FIRST MenuTree where MenuTree.Module >= INPUT haku3
           use-index Module no-lock no-error.
           IF NOT AVAILABLE MenuTree THEN DO:
                 bell. message "NOT FOUND !". 
                 PAUSE 1 no-message.
                 NEXT BROWSE.
           END.
           /*    was found */
           ASSIGN order = 2 memory = recid(MenuTree) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Search by module */

     else if nap = "7" or nap = "f7" AND lcRight = "RW" THEN DO: /* siirto */
        assign ufkey = true ehto = 9. cfc = "kline". RUN Syst/ufcolor.p. RUN Syst/ufkey.p.
        DO TRANSACTION ON ENDKEY UNDO, NEXT LOOP:
           assign mista = "" minne = "".
           UPDATE mista minne WITH FRAME siirto EDITING:
              READKEY.
              ASSIGN nap = keylabel(LASTKEY) pois = lookup(nap,poisnap) > 0.
              IF pois THEN DO:
                 HIDE MESSAGE.
                 if frame-field = "mista" THEN DO:
                    ASSIGN mista.
                    if mista = "" THEN UNDO, LEAVE.
                    IF length(mista) = 1 THEN DO:
                       BELL.
                       MESSAGE
                       "Menu to be moved has to have at least 2 " +
                       " digits !". NEXT.
                    END.
                    IF NOT can-find(FIRST MenuTree where MenuTree.Level
                    BEGINS mista) THEN DO:
                       BELL.
                       message "No fixmenus found in this Level: " mista "!".
                       NEXT.
                    END.
                 END.
                 else if frame-field = "minne" THEN DO:
                    ASSIGN minne.
                    if minne = "" THEN UNDO, LEAVE.
                    ke = FALSE.
                    message "Are You sure You want to move (Y/N) ? " UPDATE ke.
                    IF NOT ke THEN LEAVE.
                    message "Moving, just a moment ... ".
                    PAUSE 0 no-message.

                    IF minne BEGINS mista THEN FIND FIRST MenuTree where
                    MenuTree.Level = mista no-error.
                    ELSE FIND FIRST MenuTree where MenuTree.Level BEGINS mista
                    no-error.
siirto:             repeat:
                       IF NOT AVAILABLE MenuTree THEN LEAVE.
                       FIND FIRST xMenuTree where
                       xMenuTree.Position = MenuTree.Position AND
                       xMenuTree.Level = minne + substring(MenuTree.Level,
                       length(mista) + 1) no-lock no-error.
                       IF AVAIL xMenuTree THEN DO:
                          message "Level " + MenuTree.Level + ", place " +
                          string(Position) + " doesn't fit - press enter !".
                          PAUSE no-message.
                       END.
                       ELSE DO:

                          IF llDoEvent 
                          THEN RUN StarEventSetOldBuffer(lhMenuTree).

                          ASSIGN
                          MenuTree.Level = minne +
                          substring(MenuTree.Level, length(mista) + 1)
                          memory = recid(MenuTree)
                          must-print = TRUE.

                          IF llDoEvent 
                          THEN RUN StarEventMakeModifyEvent(lhMenuTree).
                       END.

                       IF minne BEGINS mista THEN FIND NEXT MenuTree where
                       MenuTree.Level = mista no-error.
                       ELSE FIND NEXT MenuTree where MenuTree.Level
                       BEGINS mista no-error.
                    END. /* siirto */
                    LEAVE.
                 END.
              END. /* pois */
              APPLY LASTKEY.
           END. /* EDITING */
           /* viela sisaantulonappaimen change */
           FIND MenuTree where 
                MenuTree.Level = substring(mista,1,length(mista)- 1) AND 
                MenuTree.Position = integer(substring(mista,length(mista)))
           no-error.
           IF AVAIL MenuTree THEN DO:
              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMenuTree).
              ASSIGN 
                MenuTree.Level = substring(minne,1,length(minne) - 1)
                MenuTree.Position = integer(substring(minne,length(minne)))
                memory = recid(MenuTree) must-print = TRUE.
              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMenuTree).  
           END.
        END. /* TRANSACTION */
        HIDE FRAME siirto no-pause.
        NEXT LOOP.
     END. /* siirto */

     else if nap = "4" or nap = "f4" THEN DO:
        FIND MenuTree where recid(MenuTree) = rtab[FRAME-LINE] no-lock.
        if MenuTree.Level = "0" or MenuTree.Level > "8" THEN DO:
           bell. message "LEvel " + MenuTree.Level + " is not a UFKEY-menu !".
           PAUSE 2 no-message. NEXT.
        END.
        mt = "". PAUSE 0 no-message.
        FOR EACH xMenuTree where xMenuTree.Level = MenuTree.Level AND
                                 xMenuTree.Position > 0 AND 
                                 xMenuTree.Position < 9:
           IF xMenuTree.MenuNum  <> 0 THEN DO:
              FIND FIRST MenuText where 
                         MenuText.MenuNum = xMenuTree.MenuNum 
              no-lock no-error.
              IF AVAILABLE MenuText THEN
                 ASSIGN mt[xMenuTree.Position]     = substring(MenuText,1,8)
                        mt[xMenuTree.Position + 8] = substring(MenuText,9).
              ELSE
                 assign mt[xMenuTree.Position]     = 
                           string(xMenuTree.MenuNum) + " ?"
                        mt[xMenuTree.Position + 8] = "".
           END.
        END.
        /* tulostetaan valintatekstit ruudulle */
        DISPLAY mt WITH FRAME meteks.
        COLOR DISPLAY messages mt WITH FRAME meteks.
        pause message "Press any Key to continue !".
        HIDE FRAME meteks no-pause.
     END.

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
         must-add = TRUE. NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */
        delline = FRAME-LINE.
        FIND MenuTree where recid(MenuTree) = rtab[FRAME-LINE] no-lock.
        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) MenuTree.Level MenuTree.MenuId
        MenuTree.Position MenuTree.MenuNum tx1 tx2
        MenuTree.MenuType MenuTree.Module MenuTree.MenuTitle MenuTree.State[1]
        MenuTree.MenuClass.
        IF order = 1 THEN FIND NEXT MenuTree no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT MenuTree use-index Module
        no-lock no-error.
        ELSE IF order = 3 THEN FIND NEXT MenuTree USE-INDEX MenuId
        no-lock no-error.
        IF AVAILABLE MenuTree THEN memory = recid(MenuTree).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND MenuTree where recid(MenuTree) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev MenuTree no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MenuTree use-index Module
           no-lock no-error.
           ELSE IF order = 3 THEN FIND prev MenuTree USE-INDEX MenuId
           no-lock no-error.
           IF AVAILABLE MenuTree THEN
           ASSIGN delline = delline - 1 memory = recid(MenuTree).
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND MenuTree where recid(MenuTree) = rtab[FRAME-LINE]
        exclusive-lock.
        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO ERASE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) MenuTree.Level MenuTree.MenuId
        MenuTree.Position MenuTree.MenuNum tx1 tx2
        MenuTree.MenuType MenuTree.Module MenuTree.MenuTitle MenuTree.State[1]
        MenuTree.MenuClass.
        IF ok THEN DO:
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMenuTree).
            DELETE MenuTree.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST MenuTree) THEN DO: CLEAR FRAME sel no-pause.
               PAUSE 0 no-message. LEAVE LOOP.
            END.
            must-print = TRUE. NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else  if lookup(nap,"return,enter") > 0  THEN DO TRANSACTION
     WITH FRAME lis ON ENDKEY UNDO, NEXT LOOP: /* change */
        FIND MenuTree where recid(MenuTree) = rtab[frame-line(sel)]
        exclusive-lock.

        FIND MenuClass of MenuTree no-lock no-error.

        ASSIGN ed-taso = MenuTree.Level ed-pka = MenuTree.Position.
        assign ufkey = true ehto = 9 fr-header = " CHANGE MenuText "
        cfc = "lis". RUN Syst/ufcolor.p.
        PAUSE 0 no-message.

        FIND FIRST MenuText where 
                   MenuText.MenuNum = MenuTree.MenuNum 
        no-lock no-error.
        IF AVAILABLE MenuText THEN
             ASSIGN tx1 = substring(MenuText,1,8) tx2 = substring(MenuText,9).
        else assign tx1 = "<tx not>" tx2 = "<avail.>".
        DISPLAY 
          tx1 tx2 
          MenuClass.MCName when AVAIL MenuClass
          "" when NOT AVAIL MenuClass @ MenuClass.MCName
          MenuTree.Level 
          MenuTree.Position 
          MenuTree.MenuNum
          MenuTree.MenuType 
          MenuTree.Module
          MenuTree.MenuId when MenuTree.MenuType NE 3
          MenuTree.MenuTitle 
          MenuTree.MenuClass 
          MenuTree.State[1]
          MenuTree.tokencode
        WITH FRAME lis.

        IF lcRight = "RW" THEN DO:

           RUN Syst/ufkey.p.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMenuTree).
           UPDATE 
               MenuTree.Level 
               MenuTree.Position 
               MenuTree.MenuNum
               MenuTree.MenuType 
               MenuTree.Module
               MenuTree.MenuId when MenuTree.MenuType NE 3
               MenuTree.MenuTitle 
               MenuTree.MenuClass 
               MenuTree.State[1]
               MenuTree.tokencode
           WITH FRAME lis EDITING:
              READKEY.
              nap = keylabel(LASTKEY).
              pois = lookup(nap,poisnap) > 0.
              IF pois THEN DO WITH FRAME lis:
                 HIDE MESSAGE.

                 if frame-field = "Level" AND
                 input MenuTree.Level = "" THEN DO:
                    HIDE FRAME lis no-pause. UNDO, LEAVE.
                 END.

                 else if frame-field = "Position" THEN DO:
                    /* 0-tasolla saa olla duplikaatteja */
                    if input frame lis MenuTree.Level = "0" THEN DO:
                       APPLY LASTKEY. NEXT.
                    END.

                    /* onko duplikaattia valikossa */
                    FIND FIRST xMenuTree where 
                         recid(xMenuTree) <>  recid(MenuTree) AND
                         xMenuTree.Level = INPUT FRAME lis MenuTree.Level AND
                         xMenuTree.Position  = 
                           INPUT FRAME lis MenuTree.Position
                    no-lock no-error.
                    IF AVAIL xMenuTree THEN DO:
                       bell. 
                       message "This level/place is already in use !".
                       NEXT.
                    END.
                 END.

                 else if frame-field = "MenuNum" THEN DO:
                    if input frame lis MenuTree.Level = "0" THEN DO:
                       assign tx1 = "" tx2 = "". APPLY LASTKEY. NEXT.
                    END.

                    /* MenuText pakollinen jos taso <> "0" */
                    FIND FIRST MenuText where 
                               MenuText.MenuNum = 
                               INPUT FRAME lis MenuTree.MenuNum
                    no-lock no-error.
                    IF AVAILABLE MenuText THEN DO:
                       ASSIGN 
                          tx1 = substring(MenuText,1,8) 
                          tx2 = substring(MenuText,9).
                       DISPLAY tx1 tx2 WITH FRAME lis.
                    END.
                    ELSE DO:
                       bell. message "Menutext wasn't found !". NEXT.
                    END.
                 END.

                 else if frame-field = "MenuId" AND
                 input frame lis MenuTree.MenuId <> "" THEN DO:
                    FIND FIRST xMenuTree where xMenuTree.MenuId = INPUT FRAME
                    lis MenuTree.MenuId AND recid(MenuTree) <> recid(xMenuTree)
                    no-lock no-error.
                    IF AVAIL xMenuTree THEN DO:
                       BELL.
                       message "Function '" + INPUT FRAME lis MenuTree.MenuId
                       + "' is already in use: Level " + string(xMenuTree.Level)
                       + " place " + string(xMenuTree.Position) + " !". NEXT.
                    END.
                 END.

                 else if frame-field = "MenuClass" THEN DO:
                    FIND MenuClass where 
                         MenuClass.MenuClass = 
                         INPUT FRAME lis MenuTree.MenuClass
                    no-lock no-error.
                    IF NOT AVAIL MenuClass THEN DO:
                       BELL.
                       message "Unknown Program Class !".
                       NEXT.
                    END.
                    DISP MenuClass.MCName.
                 END.   
                 ELSE IF frame-field = "tokencode" THEN DO:
                    FIND FIRST token WHERE
                               token.tokencode = input MenuTree.tokencode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL token THEN DO:
                       MESSAGE "Unknown token !".        
                       NEXT.
                    END.           
                 END.

              END.
              APPLY LASTKEY.
           END. /* EDITING */

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMenuTree).

        END.
        ELSE DO:
           PAUSE.   
           if MenuTree.Level = "0" THEN 
           assign tx1 = "" tx2 = "".
        END.   
        CLEAR FRAME lis no-pause.
        HIDE FRAME lis no-pause.


        IF ed-taso <> MenuTree.Level OR ed-pka <> MenuTree.Position THEN DO:
           ASSIGN memory = recid(MenuTree) must-print = TRUE. NEXT LOOP.
        END.
        ELSE DISPLAY
        MenuTree.Level MenuTree.Position MenuTree.MenuNum tx1 tx2
        MenuTree.MenuType MenuTree.Module MenuTree.MenuId MenuTree.MenuTitle
        MenuTree.State[1] MenuTree.MenuClass
        WITH FRAME sel.
     END.

     if lookup(nap,"CTRL-P") > 0 THEN DO:
        FIND FIRST MenuTree where
                   recid(MenuTree) = rtab[FRAME-LINE]
        no-lock no-error.
        IF AVAIL MenuTree THEN DO:
           FIND FIRST MenuText where
                      MenuText.MenuNum = MenuTree.MenuNum
           no-lock no-error.
           OUTPUT STREAM fixmenu1 TO MenuTree.d append.
           EXPORT STREAM fixmenu1 MenuTree.
           OUTPUT STREAM fixmenu1 CLOSE.
           IF AVAIL MenuText THEN DO:
              OUTPUT STREAM fixmenu2 TO MenuText.d append.
              EXPORT STREAM fixmenu2 MenuText.MenuNum MenuText.MenuText.
              OUTPUT STREAM fixmenu2 CLOSE.
           END.
        END.
        APPLY LASTKEY.
     END.                           

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST MenuTree no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST MenuTree use-index Module
        no-lock no-error.
        ELSE IF order = 3 THEN FIND FIRST MenuTree USE-INDEX MenuId
        no-lock no-error.
        ASSIGN memory = recid(MenuTree) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST MenuTree no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST MenuTree use-index Module
        no-lock no-error.
        ELSE IF order = 3 THEN FIND LAST MenuTree USE-INDEX MenuId
        no-lock no-error.
        ASSIGN memory = recid(MenuTree) must-print = TRUE.
        NEXT LOOP.
     END.
     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

