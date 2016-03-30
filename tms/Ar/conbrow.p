/* ----------------------------------------------------------------------
  MODULE .......: conbrow.p
  TASK .........: brose old contact lists
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 07.06.04
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'contact'}

DEF TEMP-TABLE ttContact NO-UNDO
   FIELD UserCode  AS CHAR
   FIELD ConDate   AS DATE
   FIELD Unhandled AS INT
   FIELD Handled   AS INT
   INDEX UserCode UserCode ConDate DESC .
                           
DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcBrandName  AS CHAR                   NO-UNDO.
DEF VAR liOldVoucher AS INT                    NO-UNDO. 

form
    ttContact.UserCode  FORMAT "X(8)"     COLUMN-LABEL "User"
    ttContact.ConDate   FORMAT "99-99-99" COLUMN-LABEL "Date"
    ttContact.Unhandled FORMAT ">>>>>9"   COLUMN-LABEL "Unhandled"
    ttContact.Handled   FORMAT ">>>>>9"   COLUMN-LABEL "Handled"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Contact lists "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FUNCTION fCalcQty RETURNS LOGICAL.

   ASSIGN ttContact.Unhandled = 0
          ttContact.Handled   = 0.
                 
   FOR EACH Contact NO-LOCK WHERE
            Contact.Brand    = gcBrand AND
            Contact.UserCode = ttContact.UserCode AND
            Contact.ConDate  = ttContact.ConDate:
            
      IF Contact.ConState = 0 
      THEN ttContact.Unhandled = ttContact.Unhandled + 1.
      ELSE ttContact.Handled   = ttContact.Handled + 1.
   END.         

END FUNCTION.

IF icUserCode > "" THEN DO:
   FIND TmsUser WHERE TmsUser.UserCode = icUserCode NO-LOCK NO-ERROR.
   IF NOT AVAIL TMSUSER THEN DO:
      MESSAGE 
      "Unknown UserCode " icUserCode SKIP
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
END.

FOR EACH Contact NO-LOCK WHERE 
         Contact.Brand    = gcBrand AND
         Contact.UserCode = icUserCode:
   FIND FIRST ttContact WHERE
              ttContact.UserCode = Contact.UserCode AND
              ttContact.ConDate     = Contact.ConDate NO-ERROR.
   IF NOT AVAILABLE ttContact THEN DO:
      CREATE ttContact.
      ASSIGN ttContact.UserCode = Contact.UserCode
             ttContact.ConDate     = Contact.ConDate.
   END.
   IF Contact.ConState = 0 
   THEN ttContact.Unhandled = ttContact.Unhandled + 1.
   ELSE ttContact.Handled   = ttContact.Handled + 1.
   
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Date , By 3, By 4".


FIND FIRST ttContact
  NO-LOCK NO-ERROR.
IF AVAILABLE ttContact THEN ASSIGN
   Memory       = recid(ttContact)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No contact lists available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
   END.


   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttContact WHERE recid(ttContact) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttContact THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttContact).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 11
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttContact.ConDate {Syst/uchoose.i} NO-ERROR
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttContact.ConDate WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttContact WHERE recid(ttContact) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttContact THEN
              ASSIGN FIRSTrow = i Memory = recid(ttContact).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttContact THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttContact)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttContact THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ttContact).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttContact WHERE recid(ttContact) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttContact THEN DO:
           Memory = recid(ttContact).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttContact THEN Memory = recid(ttContact).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttContact WHERE recid(ttContact) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return,5,F5") > 0 THEN DO:

       /* change */
       RUN local-find-this(FALSE).
       
       IF AVAILABLE ttContact THEN DO:
          ufkey = TRUE.

          /* browse list */
          RUN Ar/conlist (ttContact.UserCode,
                       ttContact.ConDate,
                       0).

          /* update quantities, first today (mark as handled uses today) */
          FIND FIRST ttContact WHERE
                     ttContact.UserCode = icUserCode AND
                     ttContact.ConDate  = TODAY NO-ERROR.
          IF NOT AVAILABLE ttContact THEN DO:
             CREATE ttContact.
             ASSIGN ttContact.UserCode = icUserCode 
                    ttContact.ConDate  = TODAY
                    must-print         = TRUE
                    memory             = RECID(ttContact).
          END.
          fCalcQty().
          
          /* then the chosen one */  
          RUN local-find-this(FALSE).
          fCalcQty().
          
       END.
           
       RUN local-disp-row.
       xrecid = recid(ttContact).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttContact) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttContact) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttContact WHERE recid(ttContact) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttContact WHERE recid(ttContact) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttContact
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttContact
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttContact
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttContact
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttContact.UserCode
       ttContact.ConDate
       ttContact.Unhandled
       ttContact.Handled
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          ttContact.UserCode
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

