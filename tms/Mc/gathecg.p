/* -------------------------------------------------------------------------
  MODULE .......: gathecg.p
  FUNCTION .....: Browse customers AND pic them AS members
  APPLICATION ..: NN
  CREATED ......: 21-01-96 PT
  CHANGED ......: 08.11.02 JR Eventlog
                  23.05.03/aam can-find corrected
                  15.09.03/aam brand
  Version ......: M15
  ----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

DEF TEMP-TABLE TMarked
FIELD CustGroup LIKE CustGroup.CustGroup
INDEX CustGroup CustGroup.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR TMarked.

DEF VAR CustGroup   LIKE CustGroup.CustGroup  NO-UNDO.
DEF VAR CustName  LIKE CustGroup.CGName NO-UNDO.

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
def var smname       as char format "x(12)" NO-UNDO.
DEF VAR xrecid       AS RECID.
def var mess         as c   format "x(34)"  NO-UNDO EXTENT 5.
def var memb         as lo format "*/" NO-UNDO.


IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCGMember).
   END.
END.

form
    memb                   column-label "Member"
    CustGroup.CustGroup      /* COLUMN-LABEL FORMAT */
    CustGroup.CGName      /* COLUMN-LABEL FORMAT */
    CustGroup.CreDate

WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    color value(cfc) title color value(ctc) " CHOOSE MEMBERS INTO GROUP " 
    FRAME sel.

form /* FIND CustGroup BY number */
    CustGroup help "Enter Customer No."
    with row 4 col 2 title color value(ctc) " FIND CUST. No. "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* FIND CustGroup BY Name */
    CustName help "Enter Customer's name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.


form
   mess[1]  NO-LABEL SKIP
   mess[2]  NO-LABEL SKIP
   mess[3]  NO-LABEL SKIP
   mess[4]  NO-LABEL skip(1)
   mess[5]  NO-LABEL SKIP
WITH OVERLAY centered ROW 5 FRAME frm.

mess[1] = "This customer has:".
mess[4] = "where starting Amount is allowed.".
mess[5] = "This overrides all those settings.".

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc. view FRAME sel.


FIND FIRST CustGroup
   USE-INDEX CustGroup  WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
IF AVAIL CustGroup  THEN
   ASSIGN memory = recid(CustGroup) must-print = TRUE must-add    = FALSE.
ELSE DO:
   bell. message "Customer File is empty - press ENTER !".
   PAUSE no-message.
   RETURN.
END.
ASSIGN xrecid = ? delline  = 0 ufkey  = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:
    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 18 col 30 "   Order by Group Code ".
       if order = 2 then put screen row 18 col 30 "     Order by Name     ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
    up FRAME-LINE - 1.
    FIND CustGroup where recid(CustGroup) = memory NO-LOCK no-error.
    /* print 1 page data on the screen
    beginning from the record whose KeyValue = memory
    beginning from line 'delline' */
    /* IF a line has just been deleted, THEN ... */
    IF delline > 0 THEN DOWN delline - 1.
    repeat WITH FRAME sel:
       IF AVAILABLE CustGroup  THEN DO:
          memb = 
          can-find(FIRST TMarked where 
                         TMarked.CustGroup = CustGroup.CustGroup).

          DISPLAY memb CustGroup.CustGroup CustGroup.CGName CustGroup.credate
             CustGroup.CreUser .

          rtab[FRAME-LINE] = recid(CustGroup).
          IF order = 1 THEN FIND NEXT CustGroup
          USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 3 THEN FIND NEXT CustGroup
           WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 2 THEN FIND NEXT CustGroup
          USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 4 THEN FIND NEXT CustGroup
           WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
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
    ufk[1]= 1709 ufk[2]= 717 ufk[3]= 0 ufk[4]= 0
    ufk[5]= 515 ufk[6]= 0   ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
    ehto = 3 ufkey = FALSE.  RUN Syst/ufkey.
      END.
      HIDE MESSAGE no-pause. IF order = 1 THEN
    CHOOSE ROW CustGroup.CustGroup ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 2 THEN
    CHOOSE ROW CustGroup.CGName ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc)
      CustGroup.CustGroup CustGroup.CreUser  CustGroup.CGName
      WITH FRAME sel.
      IF rtab[FRAME-LINE] = ? THEN NEXT.
      nap = keylabel(LASTKEY).
      if lookup(nap,"cursor-right") > 0 THEN DO:
    order = order + 1. IF order = 5 THEN order = 1. END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
    order = order - 1. IF order = 0 THEN order = 4. END.

      IF order <> ex-order THEN DO:
    ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
    FIND CustGroup where recid(CustGroup) = memory.
    DO i = 1 TO FRAME-LINE - 1:
       IF order = 1 THEN FIND prev CustGroup
       USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND prev CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND prev CustGroup
       USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 4 THEN FIND prev CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       IF AVAILABLE CustGroup  THEN
          ASSIGN firstline = i memory = recid(CustGroup).
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
       FIND CustGroup where recid(CustGroup) = rtab[1] NO-LOCK.
       IF order = 1 THEN FIND prev CustGroup
       USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND prev CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND prev CustGroup
       USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 4 THEN FIND prev CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       IF NOT AVAILABLE CustGroup  THEN DO:
          message "YOU ARE ON THE FIRST ROW !".
          BELL. PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* a previous one was found */
          scroll DOWN.
          memb = 
            can-find(FIRST TMarked where 
                           TMarked.CustGroup = CustGroup.CustGroup).
          DISPLAY memb CustGroup.CustGroup CustGroup.CGName CustGroup.credate
             CustGroup.CreUser .
          DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
          ASSIGN rtab[1] = recid(CustGroup) memory = rtab[1].
       END.
    END.
    ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
    IF FRAME-LINE = FRAME-DOWN THEN DO:
       FIND CustGroup where recid(CustGroup) = rtab[FRAME-DOWN] NO-LOCK .
       IF order = 1 THEN FIND NEXT CustGroup
       USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND NEXT CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND NEXT CustGroup
       USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 4 THEN FIND NEXT CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
       IF NOT AVAILABLE CustGroup  THEN DO:
          message "YOU ARE ON THE LAST ROW !".
          BELL.  PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* yet another record was found */
          scroll up.
          memb = 
            can-find(FIRST TMarked where 
                           TMarked.CustGroup = CustGroup.CustGroup).
          DISPLAY memb CustGroup.CustGroup CustGroup.CGName CustGroup.credate
             CustGroup.CreUser .
          DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
          rtab[FRAME-DOWN] = recid(CustGroup).
          /* finally LAST line's KeyValue is saved */
          ASSIGN memory = rtab[1].
       END.
    END.
    ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
    memory = rtab[1].
    FIND CustGroup where recid(CustGroup) = memory NO-LOCK no-error.
    IF order = 1 THEN FIND prev CustGroup
    USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
    ELSE IF order = 3 THEN FIND prev CustGroup
     WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
    ELSE IF order = 2 THEN FIND prev CustGroup
    USE-INDEX CGName  WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
    ELSE IF order = 4 THEN FIND prev CustGroup
     WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
    IF AVAILABLE CustGroup  THEN DO:
       memory = recid(CustGroup).
       /* go back one page */
       DO line = 1 TO (FRAME-DOWN - 1):
          IF order = 1 THEN FIND prev CustGroup
          USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 3 THEN FIND prev CustGroup
           WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 2 THEN FIND prev CustGroup
          USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 4 THEN FIND prev CustGroup
           WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
          IF AVAILABLE CustGroup  THEN memory = recid(CustGroup).
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
       FIND CustGroup where recid(CustGroup) = memory NO-LOCK.
       must-print = TRUE. NEXT LOOP.
   END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
   cfc = "puyr". RUN Syst/ufcolor.
   CustGroup = "". ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
   UPDATE CustGroup WITH FRAME hayr.
   HIDE FRAME hayr no-pause.
   IF CustGroup <> "" THEN DO:
      FIND FIRST CustGroup  USE-INDEX CustGroup WHERE 
                 CustGroup.Brand = gcBrand AND
                 CustGroup.CustGroup >= CustGroup
                 NO-LOCK no-error.
      IF NOT AVAILABLE CustGroup  THEN DO:
         bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Customer  was found */
      ASSIGN order = 1 memory = recid(CustGroup) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
   cfc = "puyr". RUN Syst/ufcolor. CustName = "".
   ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
   UPDATE CustName WITH FRAME hayr3.
   HIDE FRAME hayr2 no-pause.
   if CustName <> "" THEN DO:
      FIND FIRST CustGroup  where 
                 CustGroup.Brand = gcBrand  AND
                 CustGroup.CGname >= CustName
      NO-LOCK no-error.
      IF NOT AVAILABLE CustGroup  THEN DO:
         bell.  message "CAN'T FIND".  PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Customer  was found */
      ASSIGN order = 2 memory = recid(CustGroup) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 2 */


     else if lookup(nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:

        /* ADD OR REMOVE */
        FIND CustGroup where recid(CustGroup) = rtab[frame-line(sel)] NO-LOCK.

        FIND TMarked Where
             TMarked.CustGroup = CustGroup.CustGroup  NO-ERROR.
        IF AVAIL TMarked THEN DO:
           DELETE TMarked.
           DISP FALSE @ memb  WITH FRAME sel.
        END.
        ELSE DO:
           CREATE Tmarked.
           ASSIGN
           Tmarked.CustGroup = CustGroup.CustGroup.
           DISP TRUE @ memb WITH FRAME sel.
        END.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST CustGroup
        USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND FIRST CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND FIRST CustGroup
        USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND FIRST CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(CustGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST CustGroup
        USE-INDEX CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND LAST CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND LAST CustGroup
        USE-INDEX CGName WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND LAST CustGroup
        WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(CustGroup) must-print = TRUE.
        NEXT LOOP.
     END.
     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel no-pause.
si-recid = xrecid.

