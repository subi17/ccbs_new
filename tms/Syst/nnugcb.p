/* -------------------------------------------------------------------------
  MODULE .......: nnugcb.p
  FUNCTION .....: Browse users AND pick them up AS members into UserGroup
  APPLICATION ..: NN
  CREATED ......: 21-01-96 PT
  CHANGED ......: 31.10.02 jr Eventlog
  Version ......: M15
  ----------------------------------------------------------------------- */

{commali.i}
{eventval.i} 

DEF INPUT PARAMETER UserGroup LIKE UserGrp.UserGroup NO-UNDO.

DEF VAR UserCode   LIKE TMSUser.UserCode  NO-UNDO.
DEF VAR UserName  LIKE TMSUser.UserName NO-UNDO.
DEF VAR UserNum  LIKE TMSUser.UserNum NO-UNDO.

DEF VAR firstline    AS INT NO-UNDO.
DEF VAR order        AS INT NO-UNDO.
DEF VAR ex-order     AS INT NO-UNDO.
DEF VAR memory       AS RECID              NO-UNDO.
def var line         as int format "99"    NO-UNDO.
DEF VAR delline      AS INT                NO-UNDO.
DEF VAR must-print   AS LOG            NO-UNDO.
DEF VAR must-add     AS LOG            NO-UNDO.
DEF VAR ufkey        AS LOG            NO-UNDO.
DEF VAR fr-header    AS CHAR.
def var ok           as log format "Yes/No" NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24      NO-UNDO.
DEF VAR i            AS INT NO-UNDO.
DEF VAR xrecid       AS RECID.
def var mess         as c   format "x(34)"  NO-UNDO EXTENT 5.
def var memb         as lo format "*/" NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhUgMember AS HANDLE NO-UNDO.
   lhUgMember = BUFFER UgMember:HANDLE.
   RUN StarEventInitialize(lhUgMember).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhUgMember).
   END.
END.


FIND FIRST Company no-lock.

form
    memb                             column-label "Member"
    TMSUser.UserCode                     column-label "UserId"
    help "User's ID"
    TMSUser.UserNum                     column-label "UserNo"
    TMSUser.UserName  format "x(16)"    column-label "Name"
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    color value(cfc) title color value(ctc) " CHOOSE MEMBERS INTO GROUP " +
    UserGroup + " " FRAME sel.



form /* FIND User BY number */
    UserCode help "Enter User Id"
    with row 4 col 2 title color value(ctc) " FIND USERID "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* FIND User BY name */
    UserName help "Enter User's name"
    with row 4 col 2 title color value(ctc) " FIND name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* FIND User BY abbreviation */
    UserNum help "Enter User No."
    with row 4 col 2 title color value(ctc) " FIND USER NO."
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc. view FRAME sel.


FIND FIRST TMSUser USE-INDEX UserCode no-lock no-error.
IF AVAIL TMSUser THEN
   ASSIGN memory = recid(TMSUser) must-print = TRUE must-add    = FALSE.
ELSE DO:
   bell. message "User PaymFile is empty - press ENTER !".
   PAUSE no-message.
   RETURN.
END.
ASSIGN xrecid = ? delline  = 0 ufkey  = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:
    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 18 col 30 "  Order by UserID  ".
       if order = 2 then put screen row 18 col 30 "  Order by UserNo. ".
       if order = 3 then put screen row 18 col 30 "  Order by Name    ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND TMSUser where recid(TMSUser) = memory no-lock no-error.
         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */
         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.
         repeat WITH FRAME sel:
            IF AVAILABLE TMSUser THEN DO:
               memb = can-find(UGMember where UGMember.UserCode = TMSUser.UserCode
               AND UGMember.UserGroup = UserGroup).

               DISPLAY memb TMSUser.UserCode TMSUser.UserName TMSUser.UserNum.

               rtab[FRAME-LINE] = recid(TMSUser).
               IF order = 1 THEN FIND NEXT TMSUser
               USE-INDEX UserCode no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT TMSUser
               USE-INDEX UserNum no-lock no-error.
               ELSE IF order = 3 THEN FIND NEXT TMSUser
               USE-INDEX UserName no-lock no-error.
            END.
            ELSE DO:  CLEAR no-pause.  rtab[FRAME-LINE] = ?. END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE. DOWN.
         END.
         up FRAME-LINE - 1.  DOWN firstline.
         ASSIGN firstline = 0 must-print = FALSE. PAUSE 0 no-message.
         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1. ASSIGN delline = 0.
BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 542 ufk[2]= 543 ufk[3]= 30  ufk[4]= 0
         ufk[5]= 515 ufk[6]= 0   ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.  RUN ufkey.
      END.
      HIDE MESSAGE no-pause. IF order = 1 THEN
         CHOOSE ROW TMSUser.UserCode ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 2 THEN
         CHOOSE ROW TMSUser.UserNum ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 3 THEN
         CHOOSE ROW TMSUser.UserName ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc)
      TMSUser.UserCode TMSUser.UserNum TMSUser.UserName WITH FRAME sel.
      IF rtab[FRAME-LINE] = ? THEN NEXT.
      nap = keylabel(LASTKEY).
      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 4 THEN order = 1. END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 3. END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND TMSUser where recid(TMSUser) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev TMSUser
            USE-INDEX UserCode no-lock no-error.
            ELSE IF order = 2 THEN FIND prev TMSUser
            USE-INDEX UserNum no-lock no-error.
            ELSE IF order = 3 THEN FIND prev TMSUser
            USE-INDEX UserName no-lock no-error.
            IF AVAILABLE TMSUser THEN
               ASSIGN firstline = i memory = recid(TMSUser).
            ELSE LEAVE.
         END.
         must-print = TRUE. NEXT LOOP.
      END.
      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         bell. message "You are on an empty row, move upwards !".
         PAUSE 1 no-message. NEXT.
      END.
      ASSIGN nap = keylabel(LASTKEY).


      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND TMSUser where recid(TMSUser) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev TMSUser
            USE-INDEX UserCode no-lock no-error.
            ELSE IF order = 2 THEN FIND prev TMSUser
            USE-INDEX UserNum no-lock no-error.
            ELSE IF order = 3 THEN FIND prev TMSUser
            USE-INDEX UserName no-lock no-error.
            IF NOT AVAILABLE TMSUser THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message. NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               memb = can-find(UGMember where UGMember.UserCode = TMSUser.UserCode
               AND UGMember.UserGroup = UserGroup).
               DISPLAY memb TMSUser.UserCode TMSUser.UserName TMSUser.UserNum.
               DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
               ASSIGN rtab[1] = recid(TMSUser) memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND TMSUser where recid(TMSUser) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT TMSUser
            USE-INDEX UserCode no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT TMSUser
            USE-INDEX UserNum no-lock no-error.
            ELSE IF order = 3 THEN FIND NEXT TMSUser
            USE-INDEX UserName no-lock no-error.
            IF NOT AVAILABLE TMSUser THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.  PAUSE 1 no-message. NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               memb = can-find(UGMember where UGMember.UserCode = TMSUser.UserCode
               AND UGMember.UserGroup = UserGroup).
               DISPLAY memb TMSUser.UserCode TMSUser.UserName TMSUser.UserNum.
               DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
               rtab[FRAME-DOWN] = recid(TMSUser).
               /* finally LAST line's KeyValue is saved */
               ASSIGN memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND TMSUser where recid(TMSUser) = memory no-lock no-error.
         IF order = 1 THEN FIND prev TMSUser
         USE-INDEX UserCode no-lock no-error.
         ELSE IF order = 2 THEN FIND prev TMSUser
         USE-INDEX UserNum no-lock no-error.
         ELSE IF order = 3 THEN FIND prev TMSUser
         USE-INDEX UserName  no-lock no-error.
         IF AVAILABLE TMSUser THEN DO:
            memory = recid(TMSUser).
            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev TMSUser
               USE-INDEX UserCode no-lock no-error.
               ELSE IF order = 2 THEN FIND prev TMSUser
               USE-INDEX UserNum no-lock no-error.
               ELSE IF order = 3 THEN FIND prev TMSUser
               USE-INDEX UserName no-lock no-error.
               IF AVAILABLE TMSUser THEN memory = recid(TMSUser).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE. NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".  BELL. PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE". BELL. PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND TMSUser where recid(TMSUser) = memory no-lock.
            must-print = TRUE. NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN ufcolor.
        UserCode = "". ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE UserCode WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        if UserCode <> "" THEN DO:
           FIND FIRST TMSUser where TMSUser.UserCode >= UserCode
           USE-INDEX UserCode no-lock no-error.
           IF NOT AVAILABLE TMSUser THEN DO:
              bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
           END.
           /*  TMSUser  was found */
           ASSIGN order = 1 memory = recid(TMSUser) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN ufcolor. UserNum = 0.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE UserNum WITH FRAME hayr3.
        HIDE FRAME hayr3 no-pause.
        IF UserNum <> 0 THEN DO:
           FIND FIRST TMSUser where TMSUser.UserNum >= UserNum
           no-lock no-error.
           IF NOT AVAILABLE TMSUser THEN DO:
              bell.  message "CAN'T FIND".  PAUSE 1 no-message. NEXT BROWSE.
           END.
           /*  TMSUser  was found */
           ASSIGN order = 2 memory = recid(TMSUser) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     /* Haku sarakk. 3 */
     if lookup(nap,"3,f3") > 0 THEN DO:  /* haku sar. 3 */
        cfc = "puyr". run ufcolor. UserName = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE UserName WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if UserName <> "" THEN DO:
           FIND FIRST TMSUser where TMSUser.UserName >= UserName
           no-lock no-error.
           IF NOT AVAILABLE TMSUser THEN DO:
              bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
           END.
           /*  TMSUser  was found */
           ASSIGN order = 3 memory = recid(TMSUser) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */

     else if lookup(nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:

        /* ADD OR REMOVE */
        FIND TMSUser where recid(TMSUser) = rtab[frame-line(sel)] no-lock.

        FIND UGMember where UGMember.UserGroup = UserGroup AND
                            UGMember.UserCode  = TMSUser.UserCode
        exclusive-lock no-error.

        IF AVAIL UGMember THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUgMember).
           DELETE UGMember.
           DISP FALSE @ memb WITH FRAME sel.
        END.
        ELSE DO:
           CREATE UGMember.
           ASSIGN
           UGMember.UserGroup = UserGroup
           UGMember.UserCode   = TMSUser.UserCode
           UGMember.UserName = TMSUser.UserName.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUgMember).
           DISP TRUE @ memb WITH FRAME sel.
        END.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST TMSUser
        USE-INDEX UserCode no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST TMSUser
        USE-INDEX UserNum no-lock no-error.
        ELSE IF order = 3 THEN FIND FIRST TMSUser
        USE-INDEX UserName no-lock no-error.
        ASSIGN memory = recid(TMSUser) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST TMSUser
        USE-INDEX UserCode no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST TMSUser
        USE-INDEX UserNum no-lock no-error.
        ELSE IF order = 3 THEN FIND LAST TMSUser
        USE-INDEX UserName no-lock no-error.
        ASSIGN memory = recid(TMSUser) must-print = TRUE.
        NEXT LOOP.
     END.
     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel no-pause.
si-recid = xrecid.

