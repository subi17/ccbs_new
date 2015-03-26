/*----------------------------------------------------------------------
  MODULE .......: CTCTServAttr.p
  TASK .........: Attributes of CLI type's service components 
  APPLICATION ..: TMS
  AUTHOR .......: aam (from CTServAttr.p)
  CREATED ......: 07.12.04
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'CTServAttr'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCTServAttr AS HANDLE NO-UNDO.
   lhCTServAttr = BUFFER CTServAttr:HANDLE.
   RUN StarEventInitialize(lhCTServAttr).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhCTServAttr).
   END.
END.


DEF INPUT PARAMETER iiCTServEl AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcServAttr  LIKE CTServAttr.ServAttr  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 9.
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
def var Memo-exists  AS log format "M/"        NO-UNDO.
DEF VAR llfind       AS LOG                    NO-UNDO.

form
    CTServAttr.ServAttr    format "x(14)" 
    CTServAttr.FromDate 
    CTServAttr.DefValue    
    ServAttr.SAName        column-label "Service Name" format "x(35)"
    CTServAttr.ChgAllowed  column-label "ChgA" format "Y/N"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Attributes of Service Component '" + CTServEl.ServCom  + "'"
    FRAME sel.

form
    CTServEl.ServPac    COLON 20 
       ServPac.SPName AT 41 FORMAT "X(35)" NO-LABEL SKIP
    CTServEl.ServCom    COLON 20  SKIP   
       ServCom.SCName AT 22 NO-LABEL FORMAT "X(45)" SKIP
    CTServAttr.ServAttr COLON 20 FORMAT "X(14)" SKIP
       ServAttr.SAName AT 22 NO-LABEL FORMAT "X(55)" SKIP(1)
    CTServAttr.FromDate   COLON 20     SKIP
    CTServAttr.DefValue   COLON 20 FORMAT "X(30)"   SKIP      
    CTServAttr.ChgAllowed COLON 20     SKIP
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek CTServAttr  BY  CTServAttr */
    lcServAttr
    HELP "Enter Code of Service Attribute"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FIND CTServEl WHERE CTServEl.CTServEl = iiCTServEl NO-LOCK NO-ERROR.
IF NOT AVAILABLE CTServEl THEN RETURN. 

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".

RUN local-find-first.
IF AVAILABLE CTServAttr THEN ASSIGN
   Memory       = recid(CTServAttr)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CTServAttr  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           disp CTServEl.ServPac 
                CTServEl.ServCom
                WITH FRAME lis.
           
           PROMPT-FOR 
           CTServAttr.ServAttr
              VALIDATE(INPUT CTServAttr.ServAttr = "" OR
                       CAN-FIND(ServAttr WHERE
                                ServAttr.Brand   = gcBrand AND
                                ServAttr.ServCom = CTServEl.ServCom AND
                                ServAttr.ServAttr = INPUT CTServAttr.ServAttr),
                       "Unknown service attribute")
           CTServAttr.FromDate
              VALIDATE (INPUT CTServAttr.ServAttr = "" OR
                        INPUT CTServAttr.FromDate NE ?,
                        "Date is mandatory").
   
           IF INPUT FRAME lis CTServAttr.ServAttr = "" THEN 
           LEAVE add-row.

           IF CAN-FIND(CTServAttr WHERE
                       CTServAttr.CTServEl = iiCTServEl AND 
                       CTServAttr.ServAttr = INPUT CTServAttr.ServAttr AND
                       CTServAttr.FromDate = INPUT CTServAttr.FromDate)
           THEN DO:
              MESSAGE "Attribute" INPUT CTServAttr.ServAttr
                      "already exists with given date"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END. 

           CREATE CTServAttr.
           ASSIGN
           CTServAttr.CTServEl = iiCTServEl  
           CTServAttr.ServAttr = INPUT FRAME lis CTServAttr.ServAttr
           CTServAttr.FromDate = INPUT FRAME lis CTServAttr.FromDate.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCTServAttr).

           ASSIGN
           Memory = recid(CTServAttr)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.
      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE CTServAttr THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CTServAttr WHERE recid(CTServAttr) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel: 
           IF AVAILABLE CTServAttr THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CTServAttr).
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
        ufk[1]= 35  
        ufk[2]= 0 
        ufk[3]= 0
        ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {uright1.i '"4,5,6"'}
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CTServAttr.ServAttr ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CTServAttr.ServAttr WITH FRAME sel.
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
        FIND CTServAttr WHERE recid(CTServAttr) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CTServAttr THEN
              ASSIGN FIRSTrow = i Memory = recid(CTServAttr).
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
           IF NOT AVAILABLE CTServAttr THEN DO:
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
                rtab[1] = recid(CTServAttr)
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
           IF NOT AVAILABLE CTServAttr THEN DO:
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
              rtab[FRAME-DOWN] = recid(CTServAttr).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CTServAttr WHERE recid(CTServAttr) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CTServAttr THEN DO:
           Memory = recid(CTServAttr).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CTServAttr THEN Memory = recid(CTServAttr).
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
           FIND CTServAttr WHERE recid(CTServAttr) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcServAttr WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcServAttr > "" THEN DO:
          FIND FIRST CTServAttr WHERE 
                     CTServAttr.CTServEl  = iiCTServEl AND 
                     CTServAttr.ServAttr >= lcServAttr 
                     NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CTServAttr THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CTServAttr/CTServAttr was found */
          ASSIGN order = 1 Memory = recid(CTServAttr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CTServAttr.ServAttr CTServAttr.FromDate .

       RUN local-find-NEXT.
       IF AVAILABLE CTServAttr THEN Memory = recid(CTServAttr).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CTServAttr THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CTServAttr).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CTServAttr.ServAttr CTServAttr.FromDate.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCTServAttr).

           DELETE CTServAttr.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CTServAttr
           WHERE CTServAttr.CTServEl = iiCTServEl) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCTServAttr).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CTServAttr.ServAttr.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCTServAttr).

       RUN local-disp-row.
       xrecid = recid(CTServAttr).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CTServAttr) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CTServAttr) must-print = TRUE.
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
      FIND CTServAttr WHERE recid(CTServAttr) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CTServAttr WHERE recid(CTServAttr) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CTServAttr
       WHERE CTServAttr.CTServEl = iiCTServEl NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CTServAttr
       WHERE CTServAttr.CTServEl = iiCTServEl NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CTServAttr
       WHERE CTServAttr.CTServEl = iiCTServEl NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CTServAttr
       WHERE CTServAttr.CTServEl = iiCTServEl NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CTServAttr.ServAttr
       ServAttr.SAName  WHEN AVAILABLE ServAttr
       CTServAttr.FromDate
       CTServAttr.DefValue
       CTServAttr.ChgAllowed
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND ServAttr WHERE
        ServAttr.Brand    = gcBrand          AND
        ServAttr.ServCom  = CTServEl.ServCom AND
        ServAttr.ServAttr = CTServAttr.ServAttr NO-LOCK NO-ERROR.
        
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      FIND ServPac WHERE
           ServPac.Brand   = gcBrand AND
           ServPac.ServPac = CTServEl.ServPac NO-LOCK NO-ERROR.

      FIND ServCom WHERE
           ServCom.Brand   = gcBrand AND
           ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.
           
      DISP 
      CtServEl.ServPac
      ServPac.SPName   WHEN AVAILABLE ServPac
      CTServEl.ServCom
      ServCom.ScName   WHEN AVAILABLE ServCom
      CTServAttr.ServAttr
      ServAttr.SAName  WHEN AVAILABLE ServAttr
      CTServAttr.DefValue
      CTServAttr.FromDate
      CTServAttr.ChgAllowed
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9.
         RUN ufkey.
      
         UPDATE
           CTServAttr.DefValue
           CTServAttr.ChgAllowed
         WITH FRAME lis
         EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.

