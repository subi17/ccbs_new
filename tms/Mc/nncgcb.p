/* -------------------------------------------------------------------------
  MODULE .......: nncgcb.p
  FUNCTION .....: Browse customers AND pic them AS members
  APPLICATION ..: NN
  CREATED ......: 21-01-96 PT
  CHANGED ......: 08.11.02 JR  Eventlog
                  10.01.03 JP  CustomerNo format
                  19.03.03 aam run tasks when changes occur (fecgtask)
                  16.09.03/aam brand
                  01.10.03/aam salesman removed
  Version ......: M15
  ----------------------------------------------------------------------- */

{commali.i}
{eventval.i} 
{fecgtask.i}

DEF INPUT PARAMETER CustGroup LIKE CustGroup.CustGroup NO-UNDO.

DEF VAR CustNum   LIKE Customer.CustNum  NO-UNDO.
DEF VAR CustName  LIKE Customer.CustName NO-UNDO.
DEF VAR SearchName  LIKE Customer.SearchName NO-UNDO.

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
DEF VAR lcTask       AS CHAR           NO-UNDO. 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhCGMember).
   END.
END.

form
    memb                              column-label "Member"
    Customer.CustNum   format "zzzzzzz9"  column-label "Custnr"
    Customer.SearchName                   column-label "Searchgr"
    Customer.CustName  format "x(16)"   column-label "Name"
    Customer.ZipCode   format "x(12)"   column-label "District"
    Customer.RepCodes format "x(6)"    column-label "Rep."
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    color value(cfc) title color value(ctc) " CHOOSE MEMBERS INTO GROUP " +
    CustGroup + " (" + gcBrand + ") " FRAME sel.

form /* FIND Customer BY number */
    CustNum help "Enter Customer No."
    with row 4 col 2 title color value(ctc) " FIND CUST. No. "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* FIND Customer BY Name */
    CustName help "Enter Customer's name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* FIND Customer BY abbreviation */
    SearchName help "Enter abbreviation of name"
    with row 4 col 2 title color value(ctc) " FIND ABBREVIATION "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.

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

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc. view FRAME sel.


FIND FIRST Customer  USE-INDEX CustNum  WHERE Customer.Brand = gcBrand 
NO-LOCK no-error.
IF AVAIL Customer  THEN
   ASSIGN memory = recid(Customer) must-print = TRUE must-add    = FALSE.
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
       if order = 1 then put screen row 18 col 30 "   Order by number    ".
       if order = 2 then put screen row 18 col 30 "  Order by abbreviat. ".
       if order = 3 then put screen row 18 col 30 "    Order by Name     ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
    up FRAME-LINE - 1.
    FIND Customer where recid(Customer) = memory no-lock no-error.
    /* print 1 page data on the screen
    beginning from the record whose KeyValue = memory
    beginning from line 'delline' */
    /* IF a line has just been deleted, THEN ... */
    IF delline > 0 THEN DOWN delline - 1.
    repeat WITH FRAME sel:
       IF AVAILABLE Customer  THEN DO:

          RUN local-disp-row.

          rtab[FRAME-LINE] = recid(Customer).
          IF order = 1 THEN FIND NEXT Customer
          USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 2 THEN FIND NEXT Customer
          USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 3 THEN FIND NEXT Customer
          USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
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
    ufk[1]= 707 ufk[2]= 708 ufk[3]= 30 ufk[4]= 0
    ufk[5]= 515 ufk[6]= 0   ufk[7]= 726 ufk[8]= 8 ufk[9]= 1
    ehto = 3 ufkey = FALSE.  RUN ufkey.
      END.

      HIDE MESSAGE no-pause. IF order = 1 THEN
    CHOOSE ROW Customer.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 2 THEN
    CHOOSE ROW Customer.SearchName ;(uchoose.i;) no-error WITH FRAME sel.
      ELSE IF order = 3 THEN
    CHOOSE ROW Customer.CustName ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc)
      Customer.CustNum Customer.SearchName Customer.CustName
      WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? THEN NEXT.
      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
    order = order + 1. IF order = 4 THEN order = 1. END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
    order = order - 1. IF order = 0 THEN order = 3. END.

      IF order <> ex-order THEN DO:
    ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
    FIND Customer where recid(Customer) = memory.
    DO i = 1 TO FRAME-LINE - 1:
       IF order = 1 THEN FIND prev Customer
       USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND prev Customer
       USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND prev Customer
       USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       IF AVAILABLE Customer  THEN
          ASSIGN firstline = i memory = recid(Customer).
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
       FIND Customer where recid(Customer) = rtab[1] no-lock.
       IF order = 1 THEN FIND prev Customer
       USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND prev Customer
       USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND prev Customer
       USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       IF NOT AVAILABLE Customer  THEN DO:
          message "YOU ARE ON THE FIRST ROW !".
          BELL. PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* a previous one was found */
          scroll DOWN.
          RUN local-disp-row.
          DO i = FRAME-DOWN TO 2 BY -1:  rtab[i] = rtab[i - 1]. END.
          ASSIGN rtab[1] = recid(Customer) memory = rtab[1].
       END.
    END.
    ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
    IF FRAME-LINE = FRAME-DOWN THEN DO:
       FIND Customer where recid(Customer) = rtab[FRAME-DOWN] no-lock .
       IF order = 1 THEN FIND NEXT Customer
       USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 2 THEN FIND NEXT Customer
       USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       ELSE IF order = 3 THEN FIND NEXT Customer
       USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
       IF NOT AVAILABLE Customer  THEN DO:
          message "YOU ARE ON THE LAST ROW !".
          BELL.  PAUSE 1 no-message. NEXT BROWSE.
       END.
       ELSE DO:
          /* yet another record was found */
          scroll up.
          RUN local-disp-row.
          DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1].  END.
          rtab[FRAME-DOWN] = recid(Customer).
          /* finally LAST line's KeyValue is saved */
          ASSIGN memory = rtab[1].
       END.
    END.
    ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
    memory = rtab[1].
    FIND Customer where recid(Customer) = memory no-lock no-error.
    IF order = 1 THEN FIND prev Customer
    USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
    ELSE IF order = 2 THEN FIND prev Customer
    USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
    ELSE IF order = 3 THEN FIND prev Customer
    USE-INDEX CustName  WHERE Customer.Brand = gcBrand NO-LOCK no-error.
    IF AVAILABLE Customer  THEN DO:
       memory = recid(Customer).
       /* go back one page */
       DO line = 1 TO (FRAME-DOWN - 1):
          IF order = 1 THEN FIND prev Customer
          USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 2 THEN FIND prev Customer
          USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
          ELSE IF order = 3 THEN FIND prev Customer
          USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
          IF AVAILABLE Customer  THEN memory = recid(Customer).
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
       FIND Customer where recid(Customer) = memory no-lock.
       must-print = TRUE. NEXT LOOP.
   END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
   cfc = "puyr". RUN ufcolor.
   CustNum = 0. ehto = 9. RUN ufkey. ufkey = TRUE.
   UPDATE CustNum WITH FRAME hayr.
   HIDE FRAME hayr no-pause.
   IF CustNum <> 0 THEN DO:
      FIND FIRST Customer where 
                 Customer.Brand    = gcBrand AND
                 Customer.CustNum >= CustNum
      USE-INDEX CustNum no-lock no-error.
      IF NOT AVAILABLE Customer  THEN DO:
         bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Customer  was found */
      ASSIGN order = 1 memory = recid(Customer) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
   cfc = "puyr". run ufcolor. SearchName = "".
   ehto = 9. RUN ufkey. ufkey = TRUE.
   UPDATE SearchName WITH FRAME hayr3.
   HIDE FRAME hayr3 no-pause.
   if SearchName <> "" THEN DO:
      FIND FIRST Customer  where 
                 Customer.Brand       = gcBrand AND
                 Customer.SearchName >= SearchName
      no-lock no-error.
      IF NOT AVAILABLE Customer  THEN DO:
         bell.  message "CAN'T FIND".  PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Customer  was found */
      ASSIGN order = 2 memory = recid(Customer) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 2 */

     /* Haku sarakk. 3 */
     if lookup(nap,"3,f3") > 0 THEN DO:  /* haku sar. 3 */
   cfc = "puyr". run ufcolor. CustName = "".
   ehto = 9. RUN ufkey. ufkey = TRUE.
   UPDATE CustName WITH FRAME hayr2.
   HIDE FRAME hayr2 no-pause.
   if CustName <> "" THEN DO:
      FIND FIRST Customer  where 
                 Customer.Brand     = gcBrand AND
                 Customer.CustName >= CustName
      no-lock no-error.
      IF NOT AVAILABLE Customer  THEN DO:
         bell.  message "CAN'T FIND". PAUSE 1 no-message. NEXT BROWSE.
      END.
      /*  Customer  was found */
      ASSIGN order = 3 memory = recid(Customer) must-print = TRUE.
      NEXT LOOP.
   END.
     END. /* Haku sar. 3 */

     else if lookup(nap,"7,f7") > 0 THEN DO WITH FRAME sel: 
        FIND Customer where recid(Customer) = rtab[FRAME-LINE] no-lock.

        ASSIGN
           ufkey = TRUE
           ufk   = 0
           ehto  = 1
           ufk[1] = 1883 ufk[2] = 1888 
           ufk[4] = 0 ufk[5]= 0 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8.
        run ufkey.   

        IF toimi = 8 THEN NEXT BROWSE.

        IF toimi = 1 THEN RUN commontt(Customer.CustNum).

        IF toimi = 2 THEN RUN mobilett(Customer.CustNum).

     END.

     else if lookup(nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:

        /* ADD OR REMOVE */
        FIND Customer where recid(Customer) = rtab[frame-line(sel)] no-lock.

        FIND CGMember where CGMember.CustGroup = CustGroup AND
             CGMember.CustNum  = Customer.CustNum
        exclusive-lock no-error.

        IF AVAIL CGMember THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCGMember).
           lcTask = fECGLeaveTask(TRUE).
           DELETE CGMember.
           DISP FALSE @ memb WITH FRAME sel.
        END.
        ELSE DO:
           CREATE CGMember.
           ASSIGN
           CGMember.Brand     = Customer.Brand
           CGMember.CustGroup = CustGroup
           CGMember.CustNum  = Customer.CustNum
           CGMember.CustName = Customer.CustName.
           lcTask = fECGEnterTask(TRUE). 

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCGMember).
           DISP TRUE @ memb WITH FRAME sel.
        END.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
   IF order = 1 THEN FIND FIRST Customer
   USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ELSE IF order = 2 THEN FIND FIRST Customer
   USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ELSE IF order = 3 THEN FIND FIRST Customer
   USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ASSIGN memory = recid(Customer) must-print = TRUE.
   NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
   IF order = 1 THEN FIND LAST Customer
   USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ELSE IF order = 2 THEN FIND LAST Customer
   USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ELSE IF order = 3 THEN FIND LAST Customer
   USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
   ASSIGN memory = recid(Customer) must-print = TRUE.
   NEXT LOOP.
     END.
     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   memb = can-find(CGMember where 
                   CGMember.CustNum = Customer.CustNum AND 
                   CGMember.CustGroup = CustGroup).

   DISPLAY memb Customer.CustNum Customer.CustName Customer.RepCodes
           Customer.ZipCode Customer.SearchName 
           WITH FRAME sel.

END PROCEDURE.
