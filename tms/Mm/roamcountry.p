/*----------------------------------------------------------------------
  MODULE .......: ROAMCOUNTRY.P
  TASK .........: Browse and add RoamCountrys
  APPLICATION ..: TMS
  AUTHOR .......: petria
  CREATED ......: 16.07.07
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'RoamCountry'}
{timestamp.i}
{xmlfunction.i}
{ftaxdata.i}
{timestamp.i}

{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRoamCountry AS HANDLE NO-UNDO.
   lhRoamCountry = BUFFER RoamCountry:HANDLE.
   RUN StarEventInitialize(lhRoamCountry).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRoamCountry).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 2.
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
   RoamCountry.CountryName LABEL "Country"
   RoamCountry.Prefix      LABEL "Prefix"
   RoamCountry.RateZone    LABEL "Zone"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " ROAMING COUNTRIES "
    + string(pvm,"99-99-99") + " "
    FRAME sel.


FORM
    "CountryName..:" RoamCountry.CountryName SKIP
    "Prefix.......:" RoamCountry.Prefix      SKIP
    "Zone.........:" RoamCountry.RateZone    FORMAT "9" SKIP
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

VIEW FRAME sel.


orders = " By Prefix ,  By Zone  ".

RUN local-find-first.
IF AVAILABLE RoamCountry THEN ASSIGN
   Memory       = recid(RoamCountry)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No RoamCountry available!" SKIP
      "Do You want to add one?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
   IF ok THEN must-add = TRUE.
   ELSE RETURN.
END.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a RoamCountry  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor. 
ADD-ROW:

      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           CREATE RoamCountry.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRoamCountry). 


           ASSIGN
              Memory = recid(RoamCountry)
              xrecid = Memory.
        
           LEAVE ADD-ROW.
      
        END.
      
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RoamCountry THEN LEAVE LOOP.
      NEXT LOOP.
      
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND RoamCountry WHERE recid(RoamCountry) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RoamCountry THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RoamCountry).
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
        ufk[5]= 5  WHEN lcRight = "RW"
        ufk[6]= 0  
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RoamCountry.Prefix ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RoamCountry.Prefix WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
        CHOOSE ROW RoamCountry.Ratezone ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RoamCountry.Ratezone WITH FRAME sel.
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
     FIND RoamCountry WHERE recid(RoamCountry) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RoamCountry THEN
              ASSIGN FIRSTrow = i Memory = recid(RoamCountry).
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
           IF NOT AVAILABLE RoamCountry THEN DO:
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
                rtab[1] = recid(RoamCountry)
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
           IF NOT AVAILABLE RoamCountry THEN DO:
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
              rtab[FRAME-DOWN] = recid(RoamCountry).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RoamCountry WHERE recid(RoamCountry) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RoamCountry THEN DO:
           Memory = recid(RoamCountry).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RoamCountry THEN Memory = recid(RoamCountry).
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
           FIND RoamCountry WHERE recid(RoamCountry) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
     
        must-add = TRUE.
        
        NEXT LOOP.
        
     END. /* ADD NEW */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(TRUE).
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRoamCountry). 

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       RUN local-UPDATE-record.
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRoamCountry). 

       RUN local-disp-row.
       xrecid = recid(RoamCountry).
     
       LEAVE.

     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RoamCountry) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RoamCountry) must-print = TRUE.
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
      FIND RoamCountry WHERE recid(RoamCountry) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RoamCountry WHERE recid(RoamCountry) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST RoamCountry NO-LOCK NO-ERROR.
   IF order = 2 THEN 
      FIND FIRST RoamCountry USE-INDEX RateZone NO-LOCK NO-ERROR .

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST RoamCountry NO-LOCK NO-ERROR.
   IF order = 2 THEN 
      FIND LAST RoamCountry USE-INDEX RateZone NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT RoamCountry NO-LOCK NO-ERROR.
   IF order = 2 THEN 
      FIND NEXT RoamCountry USE-INDEX RateZone NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV RoamCountry NO-LOCK NO-ERROR.
   IF order = 2 THEN 
      FIND PREV RoamCountry USE-INDEX RateZone NO-LOCK NO-ERROR. 

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.

   DISPLAY 
      RoamCountry.CountryName
      RoamCountry.Prefix
      RoamCountry.RateZone
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEFINE BUFFER   xxRoamCountry   FOR RoamCountry.

   UPDATE
      RoamCountry.CountryName
      RoamCountry.Prefix
      RoamCountry.RateZone
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).
      
      IF LOOKUP(nap,poisnap) > 0 THEN DO:

         IF RoamCountry.Prefix ENTERED THEN DO:
                        
            FIND FIRST xxRoamCountry WHERE 
               xxRoamCountry.Prefix = INPUT RoamCountry.Prefix
               AND RECID(xxRoamCountry) NE RECID(RoamCountry) NO-LOCK NO-ERROR.
            IF AVAIL xxRoamCountry THEN DO:
               MESSAGE "Country with same prefix already exists!:"
                  INPUT RoamCountry.Prefix
                  VIEW-AS ALERT-BOX.
               NEXT-PROMPT RoamCountry.Prefix.
               NEXT.
            END.
         END.
      END.

      APPLY LASTKEY.

   END.

END PROCEDURE.


