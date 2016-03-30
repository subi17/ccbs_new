/*----------------------------------------------------------------------
  MODULE .......: mnpsub.P
  TASK .........: Browse and add mnpsubs
  APPLICATION ..: TMS
  AUTHOR .......: petria
  CREATED ......: 16.07.07
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MNPProcess'}
{Func/timestamp.i}
{Func/xmlfunction.i}
{Func/ftaxdata.i}
{Func/timestamp.i}

{Syst/eventval.i}

DEFINE INPUT PARAMETER piMNPSeq AS INTEGER NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhmnpsub AS HANDLE NO-UNDO.
   lhmnpsub = BUFFER mnpsub:HANDLE.
   RUN StarEventInitialize(lhmnpsub).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhmnpsub).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.

FORM
   mnpsub.cli LABEL "MSISDN" format "x(12)"
   mnpsub.msseq LABEL "Subscr.ID" FORMAT ">>>>>>>9"
   mnpsub.icc  LABEL "ICC" format "x(20)"
   mnpsub.portingtime LABEL "Porting Time"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " MNP Subs "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

/*
FORM
    "CountryName..:" mnpsub.CountryName SKIP
    "Prefix.......:" mnpsub.Prefix      SKIP
    "Zone.........:" mnpsub.RateZone    FORMAT "9" SKIP
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.
*/

PAUSE 0.
VIEW FRAME sel.

orders = " By Prefix ,  By Zone  ".

RUN local-find-first.
IF AVAILABLE mnpsub THEN ASSIGN
   Memory       = recid(mnpsub)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No mnpsub available!"
   VIEW-AS ALERT-BOX.
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
        FIND mnpsub WHERE recid(mnpsub) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE mnpsub THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(mnpsub).
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
        ufk = 0
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0
     /*   ufk[5]= 5  WHEN lcRight = "RW" */
        ufk[6]= 0  /* WHEN lcRight = "RW" */
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW mnpsub.cli {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
/*        COLOR DISPLAY VALUE(ccc) mnpsub.cli WITH FRAME sel.*/
      END.
/*      IF order = 2 THEN DO:
        CHOOSE ROW mnpsub.Ratezone {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) mnpsub.Ratezone WITH FRAME sel.
      END. */
      
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
     FIND mnpsub WHERE recid(mnpsub) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE mnpsub THEN
              ASSIGN FIRSTrow = i Memory = recid(mnpsub).
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
           IF NOT AVAILABLE mnpsub THEN DO:
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
                rtab[1] = recid(mnpsub)
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
           IF NOT AVAILABLE mnpsub THEN DO:
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
              rtab[FRAME-DOWN] = recid(mnpsub).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND mnpsub WHERE recid(mnpsub) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE mnpsub THEN DO:
           Memory = recid(mnpsub).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE mnpsub THEN Memory = recid(mnpsub).
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
           FIND mnpsub WHERE recid(mnpsub) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
     
        must-add = TRUE.
        
        NEXT LOOP.
        
     END. /* ADD NEW */
    /* 
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(TRUE).
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhmnpsub). 

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       RUN local-UPDATE-record.
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhmnpsub). 

       RUN local-disp-row.
       xrecid = recid(mnpsub).
     
       LEAVE.

     END.
*/
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(mnpsub) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(mnpsub) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

/* --------------- PROCEDURES ------------------- */


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND mnpsub WHERE recid(mnpsub) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND mnpsub WHERE recid(mnpsub) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST mnpsub where mnpsub.mnpseq = pimnpseq NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST mnpsub where mnpsub.mnpseq = pimnpseq NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT mnpsub WHERE mnpsub.mnpseq = pimnpseq NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV mnpsub where mnpsub.mnpseq = pimnpseq NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.

   DISPLAY 
      mnpsub.cli
      mnpsub.icc
      mnpsub.msseq
      fTS2HMS(mnpsub.portingtime) format "x(19)" @ mnpsub.portingtime
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.
/*
PROCEDURE local-UPDATE-record:

   DEFINE BUFFER   xxmnpsub   FOR mnpsub.

   UPDATE
      mnpsub.CountryName
      mnpsub.Prefix
      mnpsub.RateZone
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).
      
      IF LOOKUP(nap,poisnap) > 0 THEN DO:

         IF mnpsub.Prefix ENTERED THEN DO:
                        
            FIND FIRST xxmnpsub WHERE 
               xxmnpsub.Prefix = INPUT mnpsub.Prefix
               AND RECID(xxmnpsub) NE RECID(mnpsub) NO-LOCK NO-ERROR.
            IF AVAIL xxmnpsub THEN DO:
               MESSAGE "Country with same prefix already exists!:"
                  INPUT mnpsub.Prefix
                  VIEW-AS ALERT-BOX.
               NEXT-PROMPT mnpsub.Prefix.
               NEXT.
            END.
         END.
/*
         IF mnpsub.RateZone ENTERED THEN DO:
            IF  INPUT mnpsub.RateZone < 1 OR INPUT mnpsub.RateZone > 5 THEN DO:

               MESSAGE "Rate zone must be between 1 and 5!:" INPUT mnpsub.RateZone
                  VIEW-AS ALERT-BOX.
               NEXT-PROMPT mnpsub.RateZone.
               NEXT.
            END.
         END.
*/
      END.

      APPLY LASTKEY.

   END.

END PROCEDURE.
*/

