/* -------------------------------------------------------------------------
  MODULE .......: nnsgsb.p
  FUNCTION .....: Browse salesmen AND pick them up AS members
  APPLICATION ..: NN
  CREATED ......: 28.12.98 PT
  CHANGED ......: 11.11.02 jr Eventlog
                  17.09.03/aam brand
  Version ......: M15
  ----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER icSMGroup LIKE SMGroup.SmGroup NO-UNDO.

DEF VAR SmName  LIKE Salesman.SmName NO-UNDO.
DEF VAR Salesoffice  LIKE Salesman.SalesOffice NO-UNDO.
DEF VAR Salesman  LIKE Salesman.Salesman NO-UNDO.

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
def var soname       as char format "x(24)" NO-UNDO.
DEF VAR xrecid       AS RECID.
def var mess         as c   format "x(34)"  NO-UNDO EXTENT 5.
def var memb         as lo format "*/" NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSMGMember AS HANDLE NO-UNDO.
   lhSMGMember = BUFFER SMGMember:HANDLE.
   RUN StarEventInitialize(lhSMGMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSMGMember).
   END.
END.

form
    memb             column-label "Member"
    Salesman.Salesman column-label "SmCode"
    Salesman.SmName column-label "Name of Salesman" format "x(16)"
    Salesman.SalesOffice column-label "SOffice"
    soname           column-label "Name of Sales Office"
WITH
    centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(Syst.Var:cfc) TITLE COLOR value(Syst.Var:ctc)
    " CHOOSE MEMBERS INTO Salesman GROUP '" +
    icSMGroup + "' (" + Syst.Var:gcBrand + ") " FRAME sel.


form /* FIND Salesman BY code */
    Salesman help "Enter Salesman's Code"
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND SALESMAN "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr.

form /* FIND Salesman BY name */
    SmName help "Enter salesman's name"
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND NAME "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* FIND Salesoffice */
    Salesoffice help "Enter code of Salesoffice "
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND OFFICE "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr3.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc. view FRAME sel.

FIND SMGroup WHERE
     SMGroup.Brand   = Syst.Var:gcBrand AND
     SMGroup.SMGroup = icSMGroup NO-LOCK NO-ERROR.
IF NOT AVAILABLE SMGroup THEN RETURN.

FIND FIRST Salesman 
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman 
   no-lock no-error.
IF AVAIL Salesman THEN
   ASSIGN memory = recid(Salesman) must-print = TRUE must-add    = FALSE.
ELSE DO:
   bell. message "Salesman PaymFile is empty - press ENTER !".
   PAUSE no-message.
   RETURN.
END.
ASSIGN xrecid = ? delline  = 0 ufkey  = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:
    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 18 col 32 "  By SmanCode ".
       if order = 3 then put screen row 18 col 32 "  By SmanName ".
       if order = 2 then put screen row 18 col 32 "  By SOffice  ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
    up FRAME-LINE - 1.
    FIND Salesman where recid(Salesman) = memory no-lock no-error.
    /* print 1 page data on the screen
    beginning from the record whose KeyValue = memory
    beginning from line 'delline' */
    /* IF a line has just been deleted, THEN ... */
    IF delline > 0 THEN DOWN delline - 1.
    repeat WITH FRAME sel:
       IF AVAILABLE Salesman THEN DO:

          RUN local-disp-row.

          rtab[FRAME-LINE] = recid(Salesman).
          IF order = 1 THEN FIND NEXT Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
          ELSE IF order = 3 THEN FIND NEXT Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
          ELSE IF order = 2 THEN FIND NEXT Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
          ELSE IF order = 4 THEN FIND NEXT Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
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
    Syst.Var:ufk[1]= 885 Syst.Var:ufk[2]= 30  Syst.Var:ufk[3]= 523 Syst.Var:ufk[4]= 0
    Syst.Var:ufk[5]= 515 Syst.Var:ufk[6]= 0   Syst.Var:ufk[7]= 0   Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
    Syst.Var:ehto = 3 ufkey = FALSE.  RUN Syst/ufkey.p.
      END.
      HIDE MESSAGE no-pause. IF order = 1 THEN
    CHOOSE ROW Salesman.Salesman {Syst/uchoose.i} no-error WITH FRAME sel.
      ELSE IF order = 3 THEN
    CHOOSE ROW Salesman.SalesOffice {Syst/uchoose.i} no-error WITH FRAME sel.
      ELSE IF order = 2 THEN
    CHOOSE ROW Salesman.SmName {Syst/uchoose.i} no-error WITH FRAME sel.
      ELSE IF order = 4 THEN
    CHOOSE ROW Salesman.Salesman {Syst/uchoose.i} no-error WITH FRAME sel.
      COLOR DISPLAY value(Syst.Var:ccc)
      Salesman.Salesman Salesman.Salesman Salesman.SalesOffice Salesman.SmName
      WITH FRAME sel.
      IF rtab[FRAME-LINE] = ? THEN NEXT.
      Syst.Var:nap = keylabel(LASTKEY).
      if lookup(Syst.Var:nap,"cursor-right") > 0 THEN DO:
    order = order + 1. IF order = 4 THEN order = 1. END.
      if lookup(Syst.Var:nap,"cursor-left") > 0 THEN DO:
    order = order - 1. IF order = 0 THEN order = 2. END.

      IF order <> ex-order THEN DO:
    ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
    FIND Salesman where recid(Salesman) = memory NO-LOCK.
    DO i = 1 TO FRAME-LINE - 1:
       IF order = 1 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       ELSE IF order = 3 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
       ELSE IF order = 2 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
       ELSE IF order = 4 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       IF AVAILABLE Salesman THEN
          ASSIGN firstline = i memory = recid(Salesman).
       ELSE LEAVE.
    END.
    must-print = TRUE. NEXT LOOP.
      END.
      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
    bell. message "You are on an empty row, move upwards !".
    PAUSE 1 no-message. NEXT.
      END.
      ASSIGN Syst.Var:nap = keylabel(LASTKEY).


      /* previous line */
      if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
    IF FRAME-LINE = 1 THEN DO:
       FIND Salesman where recid(Salesman) = rtab[1] no-lock.
       IF order = 1 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       ELSE IF order = 3 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
       ELSE IF order = 2 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
       ELSE IF order = 4 THEN FIND prev Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       IF NOT AVAILABLE Salesman THEN DO:
          message "YOU ARE ON THE FIRST ROW !".
          BELL. PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* a previous one was found */
          scroll DOWN.
          RUN local-disp-row.

          DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
          ASSIGN rtab[1] = recid(Salesman) memory = rtab[1].
       END.
    END.
    ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
    IF FRAME-LINE = FRAME-DOWN THEN DO:
       FIND Salesman where recid(Salesman) = rtab[FRAME-DOWN] no-lock .
       IF order = 1 THEN FIND NEXT Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       ELSE IF order = 3 THEN FIND NEXT Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT Salesman
       WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
       IF NOT AVAILABLE Salesman THEN DO:
          message "YOU ARE ON THE LAST ROW !".
          BELL.  PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* yet another record was found */
          scroll up.
          RUN local-disp-row.

          DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
          rtab[FRAME-DOWN] = recid(Salesman).
          /* finally LAST line's KeyValue is saved */
          ASSIGN memory = rtab[1].
       END.
    END.
    ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
    memory = rtab[1].
    FIND Salesman where recid(Salesman) = memory no-lock no-error.
    IF order = 1 THEN FIND prev Salesman
    WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
    ELSE IF order = 3 THEN FIND prev Salesman
    WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
    ELSE IF order = 2 THEN FIND prev Salesman
    WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName  no-lock no-error.
    ELSE IF order = 4 THEN FIND prev Salesman
    WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
    IF AVAILABLE Salesman THEN DO:
       memory = recid(Salesman).
       /* go back one page */
       DO line = 1 TO (FRAME-DOWN - 1):
          IF order = 1 THEN FIND prev Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
          ELSE IF order = 3 THEN FIND prev Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
          ELSE IF order = 2 THEN FIND prev Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
          ELSE IF order = 4 THEN FIND prev Salesman
          WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
          IF AVAILABLE Salesman THEN memory = recid(Salesman).
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
     else if lookup(Syst.Var:nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
   /* cursor TO the downmost line */
   IF rtab[FRAME-DOWN] = ? THEN DO:
       message "YOU ARE ON THE LAST PAGE". BELL. PAUSE 1 no-message.
   END.
   ELSE DO: /* the downmost line wasn't empty */
       memory = rtab[FRAME-DOWN].
       FIND Salesman where recid(Salesman) = memory no-lock.
       must-print = TRUE. NEXT LOOP.
   END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
   Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
   Salesman = "". Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
   UPDATE Salesman WITH FRAME hayr.
   HIDE FRAME hayr no-pause.
   if Salesman <> "" THEN DO:
      FIND FIRST Salesman where 
                 Salesman.Brand = Syst.Var:gcBrand AND
                 Salesman.Salesman >= Salesman
      USE-INDEX Salesman no-lock no-error.
      IF NOT AVAILABLE Salesman THEN DO:
         bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Salesman  was found */
      ASSIGN order = 1 memory = recid(Salesman) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 3 */
     if lookup(Syst.Var:nap,"3,f3") > 0 THEN DO:  /* haku sar. 3 */
   Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p. Salesoffice = "".
   Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
   UPDATE Salesoffice WITH FRAME hayr3.
   HIDE FRAME hayr3 no-pause.
   if Salesoffice <> "" THEN DO:
      FIND FIRST Salesman where 
                 Salesman.Brand = Syst.Var:gcBrand AND
                 Salesman.SalesOffice >= Salesoffice
      no-lock no-error.
      IF NOT AVAILABLE Salesman THEN DO:
         bell.  message "CAN'T FIND".  PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  salesofficewas found */
      ASSIGN order = 3 memory = recid(Salesman) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 3 */

     /* Haku sarakk. 2 */
     if lookup(Syst.Var:nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
   Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p. SmName = "".
   Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
   UPDATE SmName WITH FRAME hayr2.
   HIDE FRAME hayr2 no-pause.
   if SmName <> "" THEN DO:
      FIND FIRST Salesman where 
                 Salesman.Brand = Syst.Var:gcBrand AND
                 Salesman.SmName >= SmName
      no-lock no-error.
      IF NOT AVAILABLE Salesman THEN DO:
         bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Salesman  was found */
      ASSIGN order = 2 memory = recid(Salesman) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 2 */

     else if lookup(Syst.Var:nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:

   /* ADD OR REMOVE */
   FIND Salesman where recid(Salesman) = rtab[frame-line(sel)] no-lock.

   FIND SMGMember OF SMGroup where
        SMGMember.Salesman = Salesman.Salesman
   exclusive-lock no-error.

   IF AVAIL SMGMember THEN DO:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSMGMember).
      DELETE SMGMember.
      DISP FALSE @ memb WITH FRAME sel.
   END.
   ELSE DO:
      CREATE SMGMember.
      ASSIGN
      SMGMember.Brand    = SMGroup.Brand 
      SMGMember.SmGroup  = SMGroup.SMGroup
      SMGMember.Salesman = Salesman.Salesman
      SMGMember.SmName   = Salesman.SmName.
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGMember).
      DISP TRUE @ memb WITH FRAME sel.
   END.
     END.

     else if lookup(Syst.Var:nap,"home,h") > 0 THEN DO:
   IF order = 1 THEN FIND FIRST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
   ELSE IF order = 3 THEN FIND FIRST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
   ELSE IF order = 2 THEN FIND FIRST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
   ELSE IF order = 4 THEN FIND FIRST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
   ASSIGN memory = recid(Salesman) must-print = TRUE.
   NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"end,e") > 0 THEN DO : /* LAST record */
   IF order = 1 THEN FIND LAST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
   ELSE IF order = 3 THEN FIND LAST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesoffice no-lock no-error.
   ELSE IF order = 2 THEN FIND LAST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX SmName no-lock no-error.
   ELSE IF order = 4 THEN FIND LAST Salesman
   WHERE Salesman.Brand = Syst.Var:gcBrand USE-INDEX Salesman no-lock no-error.
   ASSIGN memory = recid(Salesman) must-print = TRUE.
   NEXT LOOP.
     END.
     else if lookup(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel no-pause.
Syst.Var:si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND Salesoffice where 
        SalesOffice.Brand       = Salesman.Brand AND
        Salesoffice.SalesOffice = Salesman.SalesOffice
   no-lock no-error.
   if avail Salesoffice then soname = SOName. else soname = "".

   memb = can-find(SMGMember OF SMGroup where
                   SMGMember.Salesman = Salesman.Salesman).

   DISPLAY memb Salesman.Salesman Salesman.SmName
           Salesman.SalesOffice soname
           WITH FRAME sel.

END PROCEDURE.

