/* -----------------------------------------------
  MODULE .......: UTOSE.P
  FUNCTION .....: Browse menu FUNCTION shortcuts
  APPLICATION ..: NN
  AUTHOR .......: PT / KL
  CREATED ......: 19.05.98
  MODIFIED  ....: 08.09.95 /tt --> Saatettu F5 myOs toimimaan VALITSEN TAMAN
                  11.08.98 /pt --> DISP Level WITH FRAME sel
                  19.98.98 /pt --> f7: RUN nninfo
                  28.04.99 /pt --> show only those functions where this user
                                   has some rights
                  11.11.03/aam tokens instead of userrights                                     
  Version ......: M15
  shared .......: siirto: (in)  mistA alkaen haetaan
                          (out) valittu toiminto tai ""
                  order  : (in)  1: toimintoBROWSE 2: nimiBROWSE
  ------------------------------------------------------ */

{commali.i} 

DEF shared VAR siirto AS CHAR.
DEF shared VAR order   AS INT.

DEF VAR haku        LIKE MenuTree.MenuId       NO-UNDO.
DEF VAR haku2       LIKE MenuTree.MenuTitle     NO-UNDO.
DEF VAR Level       AS c                        NO-UNDO.
DEF VAR firstline   AS INT                      NO-UNDO.
DEF VAR ex-order    AS INT                      NO-UNDO.
DEF VAR memory      AS RECID                    NO-UNDO.
def var line        as int format "99"          NO-UNDO.
DEF VAR delline     AS INT                      NO-UNDO.
DEF VAR must-print  AS LOG                      NO-UNDO.
DEF VAR ufkey       AS LOG                      NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24          NO-UNDO.
DEF VAR i           AS INT                      NO-UNDO.
DEF VAR xrecid      AS RECID.
DEF VAR mc          AS LOG.
DEF VAR lctokens    AS CHAR                     NO-UNDO.

form
    MenuTree.MenuId 
    Level format "x(16)"  column-label "Level"
    MenuTree.MenuTitle

    WITH OVERLAY scroll 1 ROW 4 10 DOWN centered
    color value(cfc) title color value(ctc) " BROWSE FUNCTION SHORTCUTS "
    FRAME sel.

form /* toimintotunnuksella hakua varten */
    haku
    help "Give function code or beginning of it ..."
    with row 4 col 2 title color value(ctc) " FIND FUNCTION CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* toimintonimella hakua varten */
    haku2
    help "Give function name or beginning of it ..."
    with row 4 col 2 title color value(ctc) " FIND FUNCTION name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

/* allowed tokens */
FOR FIRST TMSUser NO-LOCK WHERE
          TMSUser.UserCode = katun,
    FIRST UserGrp OF TMSUser NO-LOCK:
    
    lctokens = UserGrp.ShowTokens + "," + UserGrp.ModifyTokens.
END.

cfc = "rajat". RUN ufcolor.
assign ccc = cfc mc = index(siirto,"*") > 0.

view FRAME sel.

memory = ?.
   IF order = 1 THEN DO:

      /* FIND FIRST menu item where this user has any right */

      FOR EACH  MenuTree no-lock USE-INDEX MenuId where 
                MenuTree.MenuId ge siirto AND 
                MenuTree.MenuType < 3     AND
                LOOKUP(MenuTree.TokenCode,lcTokens) > 0:

          memory = recid(MenuTree).
          LEAVE.
      END.           
   END.


   ELSE IF order = 2 THEN DO:
      /* IF there vas '*' in the serach VALUE */
      IF mc THEN 


      /* FIND FIRST menu item where this user has some right */

      FOR EACH  MenuTree no-lock USE-INDEX MenuTitle where 
                MenuTree.MenuType < 3 AND
                MenuTree.MenuTitle matches siirto AND
                LOOKUP(MenuTree.TokenCode,lcTokens) > 0:
         memory = recid(MenuTree).
         LEAVE.
      END.

      /* IF no '*' in the search Key : */
      ELSE 
         FOR EACH  MenuTree no-lock USE-INDEX MenuTitle where 
                   MenuTree.MenuType < 3 AND
                   MenuTree.MenuTitle ge siirto AND
                   LOOKUP(MenuTree.TokenCode,lcTokens) > 0:

         memory = recid(MenuTree).
         LEAVE.          

      END.
   END.




   IF memory = ? THEN DO:
      BELL.
      message "None found !".
      RETURN.
   END.
   ASSIGN must-print = TRUE xrecid = ? delline = 0 ufkey = TRUE firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 17 col 30 " Order by code ".
       if order = 2 then put screen row 17 col 30 " Order by name ".
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

               Level = substr(MenuTree.Level,2) + string(MenuTree.Position).
               DISPLAY MenuTree.MenuId MenuTree.MenuTitle Level.
               rtab[FRAME-LINE] = recid(MenuTree).

               IF order = 1 THEN FIND NEXT MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
               USE-INDEX MenuId no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT MenuTree where MenuType < 3 AND
               (IF mc THEN MenuTitle matches siirto ELSE TRUE) AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
                USE-INDEX MenuTitle
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
         ufk = 0 ufk[1]= 35 ufk[2]= 717 ufk[6] = 983 ufk[5]= 11 ufk[8]= 8
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW MenuTree.MenuId ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) MenuTree.MenuId WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW MenuTree.MenuTitle ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) MenuTree.MenuTitle WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1.
         IF order = 3 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1.
         IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN
         firstline = 0
         memory = rtab[FRAME-LINE].
         FIND MenuTree where recid(MenuTree) = memory.
         DO i = 1 TO FRAME-LINE - 1:

            IF order = 1 THEN FIND prev MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
            USE-INDEX MenuId no-lock no-error.
            ELSE IF order = 2 THEN FIND prev MenuTree where MenuType < 3 AND
            (IF mc THEN MenuTitle matches siirto ELSE TRUE) AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
             USE-INDEX MenuTitle
            no-lock no-error.
            IF AVAILABLE MenuTree THEN
            ASSIGN firstline = i memory = recid(MenuTree).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on the last row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND MenuTree where recid(MenuTree) = rtab[1] no-lock.

            IF order = 1 THEN FIND prev MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
            USE-INDEX MenuId no-lock no-error.
            ELSE IF order = 2 THEN FIND prev MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0 AND
            (IF mc THEN MenuTitle matches siirto ELSE TRUE) USE-INDEX MenuTitle
            no-lock no-error.

            IF NOT AVAILABLE MenuTree THEN DO:
               message "You are on the first row !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               Level = substr(MenuTree.Level,2) + string(MenuTree.Position).
               DISPLAY MenuTree.MenuId MenuTree.MenuTitle Level.
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

            IF order = 1 THEN FIND NEXT MenuTree where MenuType < 3
               AND  LOOKUP(MenuTree.TokenCode,lcTokens) > 0
            USE-INDEX MenuId no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT MenuTree where MenuType < 3 AND
            (IF mc THEN MenuTitle matches siirto ELSE TRUE)             AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
            USE-INDEX MenuTitle no-lock no-error.

            IF NOT AVAILABLE MenuTree THEN DO:
               message "You are on the last row !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               Level = substr(MenuTree.Level,2) + string(MenuTree.Position).
               DISPLAY 
               MenuTree.MenuId MenuTree.MenuTitle Level WITH FRAME sel.
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

         IF order = 1 THEN FIND prev MenuTree where MenuType < 3  AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
         USE-INDEX MenuId no-lock no-error.
         ELSE IF order = 2 THEN FIND prev MenuTree where MenuType < 3 AND
         (IF mc THEN MenuTitle matches siirto ELSE TRUE) AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
         USE-INDEX MenuTitle
         no-lock no-error.

         IF AVAILABLE MenuTree THEN DO:
            memory = recid(MenuTree).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):

               IF order = 1 THEN FIND prev MenuTree where MenuType < 3
               AND    LOOKUP(MenuTree.TokenCode,lcTokens) > 0
               USE-INDEX MenuId no-lock no-error.
               ELSE IF order = 2 THEN FIND prev MenuTree where MenuType < 3 AND
               (IF mc THEN MenuTitle matches siirto ELSE TRUE)           AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
               USE-INDEX MenuTitle no-lock no-error.

               IF AVAILABLE MenuTree THEN memory = recid(MenuTree).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "You are on the first page !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "You are on the last page !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND MenuTree where recid(MenuTree) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku tunnuksella */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku tunnuksella */
        cfc = "puyr". RUN ufcolor.
        haku = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        if haku <> "" THEN DO:
           FIND FIRST MenuTree where MenuTree.MenuId = INPUT haku AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
           no-lock no-error.
           IF NOT AVAILABLE MenuTree THEN DO:
              FIND FIRST MenuTree where MenuTree.MenuId ge INPUT haku AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
              no-lock no-error.
              IF NOT AVAILABLE MenuTree THEN DO:
                 FIND FIRST MenuTree where MenuTree.MenuId le INPUT haku AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
                 no-lock no-error.
                 IF NOT AVAILABLE MenuTree THEN DO:
                    BELL.
                    message "None found !".
                    PAUSE 1 no-message.
                    NEXT BROWSE.
                 END.
              END.
           END.
           /*  was found */
           ASSIGN order = 1 memory = recid(MenuTree) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku tunnuksella */

     /* Haku nimella */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku nimella */
        cfc = "puyr". RUN ufcolor.
        haku2 = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST MenuTree where MenuTree.MenuTitle = INPUT haku2 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
           no-lock no-error.
           IF NOT AVAILABLE MenuTree THEN DO:
              FIND FIRST MenuTree where MenuTree.MenuTitle ge INPUT haku2 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
              no-lock no-error.
              IF NOT AVAILABLE MenuTree THEN DO:
                 FIND FIRST MenuTree where MenuTree.MenuTitle le INPUT haku2
                 AND     LOOKUP(MenuTree.TokenCode,lcTokens) > 0
                 no-lock no-error.
                 IF NOT AVAILABLE MenuTree THEN DO:
                    BELL.
                    message "None found !".
                    PAUSE 1 no-message.
                    NEXT BROWSE.
                 END.
              END.
           END.
           /*  was found */
           ASSIGN order = 2 memory = recid(MenuTree) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku nimella */


     else if lookup(nap,"F5,5,enter,return") > 0 THEN DO:
        FIND MenuTree where recid(MenuTree) = rtab[frame-line(sel)] no-lock.
        ASSIGN siirto = MenuId.
        LEAVE LOOP.
     END.

     else if lookup(nap,"F6,6,enter,return") > 0 THEN DO:
        FIND MenuTree where recid(MenuTree) = rtab[frame-line(sel)] no-lock.
        RUN nninfo(MenuTree.MenuId).
        NEXT LOOP.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
        USE-INDEX MenuId no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST MenuTree where MenuType < 3 AND
        (IF mc THEN MenuTitle matches siirto ELSE TRUE) AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
         USE-INDEX MenuTitle
        no-lock no-error.
        ASSIGN memory = recid(MenuTree) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST MenuTree where MenuType < 3 AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
        USE-INDEX MenuId no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST MenuTree where MenuType < 3 AND
        (IF mc THEN MenuTitle matches siirto ELSE TRUE) AND
                            LOOKUP(MenuTree.TokenCode,lcTokens) > 0
        USE-INDEX MenuTitle
        no-lock no-error.
        ASSIGN memory = recid(MenuTree) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        siirto = "".
        LEAVE LOOP.
     END.
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

