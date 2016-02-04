/* -------------------------------------------------------------------------
  MODULE .......: gathecg.p
  FUNCTION .....: Browse customers AND pic them AS members
  APPLICATION ..: NN
  CREATED ......: 21-01-96 PT
  CHANGED ......: 08.11.02 JR Eventlog
                  23.05.03/aam can-find corrected
                  15.09.03/jp Brand 
  Version ......: M15
  ----------------------------------------------------------------------- */

{Syst/testpaa.i}
{Syst/eventval.i} 

DEF TEMP-TABLE xxSubserParam LIKE SubserPara.

DEFINE INPUT  PARAMETER msseq AS INT NO-UNDO.
DEFINE INPUT  PARAMETER opChange AS C NO-UNDO.

DEF VAR servcom   LIKE SubserPara.servcom  NO-UNDO.

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

FIND FIRST company where company.Brand = gcBrand no-lock.

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
    memb                   column-label "Name Of User"
    SubserPara.msseq  /* COLUMN-LABEL FORMAT */
    SubserPara.ParaValue      /* COLUMN-LABEL FORMAT */
    SubserPara.ParaName

WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    color value(cfc) title color value(ctc) " CHOOSE MEMBERS INTO GROUP " FRAME sel.
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc. view FRAME sel.


FIND FIRST SubserPara WHERE 
           subserpara.msseq = msseq    no-lock no-error .
IF AVAIL SubserPara  THEN
   ASSIGN memory = recid(SubserPara) must-print = TRUE must-add    = FALSE.
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
    FIND SubserPara where recid(SubserPara) = memory no-lock no-error.
    /* print 1 page data on the screen
    beginning from the record whose KeyValue = memory
    beginning from line 'delline' */
    /* IF a line has just been deleted, THEN ... */
    IF delline > 0 THEN DOWN delline - 1.
    repeat WITH FRAME sel:
       IF AVAILABLE SubserPara  THEN DO:


        DISPLAY memb SubserPara.msseq SubserPara.ParaValue SubserPara.ParaName.

          rtab[FRAME-LINE] = recid(SubserPara).
          IF order = 1 THEN FIND NEXT SubserPara WHERE subserpara.msseq = msseq
          USE-INDEX msseq no-lock no-error.
          ELSE IF order = 3 THEN FIND NEXT SubserPara WHERE 
                                           subserpara.msseq = msseq
           no-lock no-error.
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
    CHOOSE ROW SubserPara.msseq ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 2 THEN
    CHOOSE ROW SubserPara.ParaValue ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc)
      SubserPara.msseq   SubserPara.ParaValue
      WITH FRAME sel.
      IF rtab[FRAME-LINE] = ? THEN NEXT.
      nap = keylabel(LASTKEY).
      if lookup(nap,"cursor-right") > 0 THEN DO:
    order = order + 1. IF order = 5 THEN order = 1. END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
    order = order - 1. IF order = 0 THEN order = 4. END.

      IF order <> ex-order THEN DO:
    ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
    FIND SubserPara where recid(SubserPara) = memory.
    DO i = 1 TO FRAME-LINE - 1:
       IF order = 1 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
       USE-INDEX msseq no-lock no-error.
       ELSE IF order = 3 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
        no-lock no-error.
       IF AVAILABLE SubserPara  THEN
          ASSIGN firstline = i memory = recid(SubserPara).
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
       FIND SubserPara where recid(SubserPara) = rtab[1] no-lock.
       IF order = 1 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
       USE-INDEX msseq no-lock no-error.
       ELSE IF order = 3 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
        no-lock no-error.
       IF NOT AVAILABLE SubserPara  THEN DO:
          message "YOU ARE ON THE FIRST ROW !".
          BELL. PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* a previous one was found */
          scroll DOWN.
         DISPLAY memb SubserPara.msseq SubserPara.ParaValue 
                 SubserPara.ParaName.
          DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
          ASSIGN rtab[1] = recid(SubserPara) memory = rtab[1].
       END.
    END.
    ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
    IF FRAME-LINE = FRAME-DOWN THEN DO:
       FIND SubserPara where recid(SubserPara) = rtab[FRAME-DOWN] no-lock .
       IF order = 1 THEN FIND NEXT SubserPara WHERE subserpara.msseq = msseq
       USE-INDEX msseq no-lock no-error.
       IF NOT AVAILABLE SubserPara  THEN DO:
          message "YOU ARE ON THE LAST ROW !".
          BELL.  PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* yet another record was found */
          scroll up.
         DISPLAY memb SubserPara.msseq SubserPara.ParaValue 
                 SubserPara.ParaName.
          DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
          rtab[FRAME-DOWN] = recid(SubserPara).
          /* finally LAST line's KeyValue is saved */
          ASSIGN memory = rtab[1].
       END.
    END.
    ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
    memory = rtab[1].
    FIND SubserPara where recid(SubserPara) = memory no-lock no-error.
    IF order = 1 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
    USE-INDEX msseq no-lock no-error.

    IF AVAILABLE SubserPara  THEN DO:
       memory = recid(SubserPara).
       /* go back one page */
       DO line = 1 TO (FRAME-DOWN - 1):
          IF order = 1 THEN FIND prev SubserPara WHERE subserpara.msseq = msseq
          USE-INDEX msseq no-lock no-error.
          IF AVAILABLE SubserPara  THEN memory = recid(SubserPara).
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
       FIND SubserPara where recid(SubserPara) = memory no-lock.
       must-print = TRUE. NEXT LOOP.
   END.
     END. /* NEXT page */

     else if lookup(nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:

        /* ADD OR REMOVE */
        FIND SubserPara where recid(SubserPara) = rtab[frame-line(sel)] no-lock.

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST SubserPara WHERE subserpara.msseq = msseq
        USE-INDEX msseq no-lock no-error.
        ASSIGN memory = recid(SubserPara) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST SubserPara WHERE subserpara.msseq = msseq
        USE-INDEX msseq no-lock no-error.
        ASSIGN memory = recid(SubserPara) must-print = TRUE.
        NEXT LOOP.
     END.
     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel no-pause.
si-recid = xrecid.

