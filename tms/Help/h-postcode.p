/* -----------------------------------------------
  MODULE .......: h-PostCode
  FUNCTION .....: PostCode help
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.11.06
  CHANGED ......: 
  Version ......: yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}

DEF shared VAR siirto AS CHAR.

DEF VAR lcPostOffice AS CHAR NO-UNDO.
DEF VAR lcZipCode    AS CHAR NO-UNDO.
DEF VAR lcRegion     AS CHAR NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR lcDefCountry AS CHAR                   NO-UNDO.

form
    PostCode.ZipCode   
    PostCode.PostOffice   
    PostCode.Region
WITH ROW FrmRow CENTERED OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) "  PostCodes  " 
    FRAME sel.

form /* seek  PostCode */
    "Zip Code:" lcZipCode
    HELP "Enter zip code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Zip Code "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  */
    "Post Office:" lcPostOffice
    FORMAT "X(30)" 
    HELP "Enter postoffice (city)"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Post Office "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  */
    "Region:" lcRegion
    HELP "Enter region"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Region "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


/* default country */
lcDefCountry = fCParamC("CountryCodeDef").

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE PostCode THEN ASSIGN
   Memory       = recid(PostCode)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PostCode WHERE recid(PostCode) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PostCode THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PostCode).
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
           ufk    = 0
           ufk[1] = 1097
           ufk[2] = 1098
           ufk[3] = 1099 
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.

        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW PostCode.ZipCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) PostCode.ZipCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW PostCode.PostOffice ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) PostCode.PostOffice WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW PostCode.Region ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) PostCode.Region WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND PostCode WHERE recid(PostCode) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PostCode THEN
              ASSIGN FIRSTrow = i Memory = recid(PostCode).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE PostCode THEN DO:
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
                rtab[1] = recid(PostCode)
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
           IF NOT AVAILABLE PostCode THEN DO:
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
              rtab[FRAME-DOWN] = recid(PostCode).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PostCode WHERE recid(PostCode) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PostCode THEN DO:
           Memory = recid(PostCode).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PostCode THEN Memory = recid(PostCode).
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
           FIND PostCode WHERE recid(PostCode) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        lcZipCode = "".
        UPDATE lcZipCode WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF lcZipCode > "" THEN DO:
           FIND FIRST PostCode WHERE 
                      PostCode.Country  = lcDefCountry AND
                      PostCode.ZipCode >= lcZipCode
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE PostCode THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(PostCode)
              must-print = TRUE
              order      = 1.
              
           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f2.
        lcPostOffice = "".
        UPDATE lcPostOffice WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF lcPostOffice > "" THEN DO:
           FIND FIRST PostCode WHERE 
                      PostCode.Country     = lcDefCountry AND
                      PostCode.PostOffice >= lcPostOffice
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE PostCode THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(PostCode)
              must-print = TRUE
              order      = 2.
              
           NEXT LOOP.
        END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f3.
        lcPostOffice = "".
        UPDATE lcRegion WITH FRAME f3.
        HIDE FRAME f3 NO-PAUSE.

        IF lcRegion > "" THEN DO:
           FIND FIRST PostCode WHERE 
                      PostCode.Country  = lcDefCountry AND
                      PostCode.Region  >= lcRegion
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE PostCode THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(PostCode)
              must-print = TRUE
              order      = 3.
              
           NEXT LOOP.
        END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).

        IF AVAILABLE PostCode THEN DO:
           ASSIGN siirto   = PostCode.ZipCode
                  si-recid = RECID(PostCode).
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PostCode) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PostCode) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        ASSIGN siirto = ""
               si-recid = ?.
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PostCode WHERE recid(PostCode) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PostCode WHERE recid(PostCode) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST PostCode USE-INDEX ZipCode NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST PostCode USE-INDEX PostOffice NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND FIRST PostCode USE-INDEX Region NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST PostCode USE-INDEX ZipCode NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST PostCode USE-INDEX PostOffice NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND LAST PostCode USE-INDEX Region NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT PostCode USE-INDEX ZipCode NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT PostCode USE-INDEX PostOffice NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND NEXT PostCode USE-INDEX Region NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV PostCode USE-INDEX ZipCode NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV PostCode USE-INDEX PostOffice NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND PREV PostCode USE-INDEX Region NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       PostCode.ZipCode    
       PostCode.PostOffice
       PostCode.Region
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.


