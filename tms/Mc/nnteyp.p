/* -----------------------------------------------
  MODULE .......: NNTEYP.P
  FUNCTION .....: OTSIKKOTEKSTIEN YLL[PITO
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-04-96
  MODIFIED .....: 25.06.98 kl vast => ok
                  18.05.99 jp uright1 & uright2 added  
                  26.04.02/tk Eventlogging added
                  17.09.02/jp Language validation
                  05.03.03/tk tokens
                  15.09.03/aam brand
                  28.06.04/aam gcHelpParam
  Version ......: M15
  ------------------------------------------------------ */
&GLOBAL-DEFINE BrTable HdrText

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'hdrtext'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhHdrText AS HANDLE NO-UNDO.
   lhHdrText = BUFFER HdrText:HANDLE.
   RUN StarEventInitialize(lhHdrText).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhHdrText).
   END.

END.


DEF shared VAR siirto AS CHAR.

def var ok          as lo format "Yes/No"   NO-UNDO.
DEF VAR haku        LIKE HdrText.te-nro      NO-UNDO.
DEF VAR hakukie     LIKE HdrText.te-kie      NO-UNDO.
DEF VAR hakutext    LIKE HdrText.te-text     NO-UNDO.
DEF VAR firstline   AS INT                  NO-UNDO.
DEF VAR order       AS INT                  NO-UNDO.
DEF VAR ex-order    AS INT                  NO-UNDO.
DEF VAR Memory      AS RECID                NO-UNDO.
def var line        as int format "99"      NO-UNDO.
DEF VAR delline     AS INT                  NO-UNDO.
DEF VAR must-print  AS LOG                  NO-UNDO.
DEF VAR must-add    AS LOG                  NO-UNDO.
DEF VAR ufkey       AS LOG                  NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24      NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR xrecid      AS RECID.

form
    HdrText.Brand   FORMAT "X(4)" COLUMN-LABEL "Brand"
    HdrText.te-nro     /* column-label "Nr"       */
    HdrText.te-kie     /* column-label "Language" */
    HdrText.te-text    /* column-label "Text"     */
        format "x(62)"
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi + " Texts at different Languages "
    + string(pvm,"99-99-99") + " " FRAME sel.

form
    HdrText.te-nro   label "Number"    AT 2
    HdrText.te-kie   label "Language"  AT 2          
    HdrText.te-text  label "Text"      AT 2 
       VIEW-AS EDITOR SIZE 70 BY 12

    WITH  OVERLAY ROW 3 CENTERED COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 
    FRAME lis.

{Func/brand.i}

form /* HdrTextn nimi :n tunnuksella hakua varten */
    "Brand:" lcBrand skip
    "Code :" haku
    help " Give text's code or beginning of it "
    with row 4 col 2 title color value(ctc) " TEXTCODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* HdrTextn kielikoodilla hakua varten */
    "Brand ..:" lcBrand skip
    "Language:" hakukie
    help "Give Language code (1 - 9)"
    with row 4 col 2 title color value(ctc) " LANGUAGE CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hakie.

form /* HdrTextn nimi :n nimella hakua varten */
    "Brand:" lcBrand skip
    "Text :" hakutext
    help " Give text or beginning of it "
    with row 4 col 2 title color value(ctc) " TEXT "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST HdrText WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE HdrText THEN 
   ASSIGN 
      Memory     = recid(HdrText) 
      must-print = TRUE 
      must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Header texts available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN 
      Memory     = ?  
      must-print = FALSE 
      must-add   = TRUE.
END. 

ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 33 "    By Code     ".
       if order = 2 then put screen row 19 col 33 "   By Language  ".
       if order = 3 then put screen row 19 col 33 "    By Name     ".
    END.

   IF must-add THEN DO:  /* HdrText -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN ufkey.
         DO TRANSACTION:
            PROMPT-FOR HdrText.te-nro HdrText.te-kie EDITING:
               READKEY.
               nap = keylabel(LASTKEY).
               IF lookup(nap,poisnap) > 0 THEN DO:
                  HIDE MESSAGE.
                  if frame-field = "te-nro" THEN DO:
                     IF INPUT FRAME lis te-nro = 0 THEN LEAVE add-new.
                  END.

                  else if frame-field = "te-kie" THEN DO:

                     IF INPUT FRAME lis te-kie = 0 OR 
                     NOT CAN-FIND(language WHERE 
                                  language.language = INPUT FRAME lis te-kie)
                     THEN DO:
                        BELL.
                        MESSAGE
                        "Unknown language Code".
                        NEXT-PROMPT te-nro.
                        NEXT.
                     END.
                     IF can-find(HdrText where
                     HdrText.Brand  = lcBrand AND
                     HdrText.te-nro = INPUT FRAME lis te-nro AND
                     HdrText.te-kie = INPUT FRAME lis te-kie) THEN DO:
                        BELL.
                        message "Texts already exists !".
                        NEXT.
                     END.
                  END.
               END.
               APPLY LASTKEY.
            END.


            CREATE HdrText.
            ASSIGN
            HdrText.Brand  = lcBrand 
            HdrText.te-nro = INPUT FRAME lis HdrText.te-nro.
            HdrText.te-kie = INPUT FRAME lis HdrText.te-kie.
            UPDATE HdrText.te-text.

            ASSIGN
            Memory = recid(HdrText)
            xrecid = Memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhHdrText).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST HdrText WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE HdrText THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND HdrText where recid(HdrText) = Memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = Memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE HdrText THEN DO:
               DISPLAY HdrText.Brand HdrText.te-nro HdrText.te-text
                       te-kie /* qq */.
               rtab[FRAME-LINE] = recid(HdrText).
               IF order = 1 THEN FIND NEXT HdrText 
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND NEXT HdrText USE-INDEX te-KIe
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 3 THEN FIND NEXT HdrText USE-INDEX te-text
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 36 ufk[2]= 749 ufk[3]= 134 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
 
         /* used as help */
         IF gcHelpParam > "" THEN ASSIGN
            ufk[5] = 11
            ufk[6] = 0.
 
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW HdrText.te-nro ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) HdrText.te-nro WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW HdrText.te-kie  ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) HdrText.te-kie  WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW HdrText.te-text ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) HdrText.te-text WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 4 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 3.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 Memory = rtab[FRAME-LINE].
         FIND HdrText where recid(HdrText) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev HdrText 
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND prev HdrText USE-INDEX te-KIe
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND prev HdrText USE-INDEX te-text
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            IF AVAILABLE HdrText THEN
               ASSIGN firstline = i Memory = recid(HdrText).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "YOU ARE ON THE FIRST ROW !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND HdrText where recid(HdrText) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev HdrText 
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND prev HdrText USE-INDEX te-KIe
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND prev HdrText USE-INDEX te-text
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            IF NOT AVAILABLE HdrText THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY  HdrText.Brand HdrText.te-nro HdrText.te-text
                       te-kie /* qq */.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(HdrText)
               Memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND HdrText where recid(HdrText) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT HdrText 
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND NEXT HdrText USE-INDEX te-KIe
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND NEXT HdrText USE-INDEX te-text
               WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
            IF NOT AVAILABLE HdrText THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY HdrText.Brand HdrText.te-nro HdrText.te-text
                       te-kie /* qq */.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(HdrText).
               /* finally LAST line's KeyValue is saved */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND HdrText where recid(HdrText) = Memory NO-LOCK NO-ERROR.
         IF order = 1 THEN FIND prev HdrText 
            WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 2 THEN FIND prev HdrText USE-INDEX te-KIe
            WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 3 THEN FIND prev HdrText USE-INDEX te-text
            WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
         IF AVAILABLE HdrText THEN DO:
            Memory = recid(HdrText).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev HdrText 
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND prev HdrText USE-INDEX te-KIe
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 3 THEN FIND prev HdrText USE-INDEX te-text
                  WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
               IF AVAILABLE HdrText THEN Memory = recid(HdrText).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            Memory = rtab[FRAME-DOWN].
            FIND HdrText where recid(HdrText) = Memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN ufcolor.
        haku = 0.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr.
        UPDATE lcBrand WHEN gcAllBrand
               haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.

        IF haku <> 0 THEN DO:
           FIND FIRST HdrText where 
              HdrText.Brand = lcBrand AND
              HdrText.te-nro = INPUT haku
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE HdrText THEN 
              FIND FIRST HdrText where 
                 HdrText.Brand = lcBrand AND
                 HdrText.te-nro ge INPUT haku
                 NO-LOCK NO-ERROR.

           IF NOT AVAILABLE HdrText THEN
              FIND FIRST HdrText where 
                 HdrText.Brand = lcBrand AND
                 HdrText.te-nro le INPUT haku
              NO-LOCK NO-ERROR.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sarakk. 2 */
        cfc = "puyr". RUN ufcolor.
        haku = 0.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hakie.
        UPDATE lcBrand WHEN gcAllBrand
               hakukie WITH FRAME hakie.
        HIDE FRAME hakie no-pause.

        IF hakukie <> 0 THEN DO:

           FIND FIRST HdrText where 
              HdrText.Brand = lcBrand AND
              HdrText.te-kie >= INPUT hakukie
           NO-LOCK NO-ERROR.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     /* Haku sarakk. 3 */
     if lookup(nap,"3,f3") > 0 THEN DO:  /* haku sar. 3 */
        cfc = "puyr". RUN ufcolor.
        hakutext = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN gcAllBrand
               hakutext WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.

        if hakutext <> "" THEN DO:

           FIND FIRST HdrText where 
              HdrText.Brand = lcBrand AND
              HdrText.te-text = INPUT hakutext
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE HdrText THEN 
           FIND FIRST HdrText where 
              HdrText.Brand = lcBrand AND
              HdrText.te-text ge INPUT hakutext
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE HdrText THEN 
           FIND FIRST HdrText where 
              HdrText.Brand = lcBrand AND
              HdrText.te-text le INPUT hakutext
           NO-LOCK NO-ERROR.

           IF NOT fRecFound(3) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */

     if (lookup(nap,"5,f5") > 0 AND ufk[5] > 0) OR
        (lookup(nap,"enter,return") > 0 AND gcHelpParam > "")
     THEN DO:  /* lisays */

        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
          must-add = TRUE.
          NEXT LOOP.
        END. 
     END.

     else if lookup(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* removal */

        delline = FRAME-LINE.
        FIND HdrText where recid(HdrText) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) HdrText.te-nro HdrText.te-text
        te-kie /* qq */.

        IF order = 1 THEN FIND NEXT HdrText 
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND NEXT HdrText USE-INDEX te-KIe
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND NEXT HdrText USE-INDEX te-text
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        IF AVAILABLE HdrText THEN Memory = recid(HdrText).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND HdrText where recid(HdrText) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev HdrText 
              WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND prev HdrText USE-INDEX te-KIe
              WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 3 THEN FIND prev HdrText USE-INDEX te-text
              WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
           IF AVAILABLE HdrText THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              Memory = recid(HdrText).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND HdrText where recid(HdrText) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY value(ccc) HdrText.te-nro HdrText.te-text
        te-kie /* qq */.
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhHdrText).

            DELETE HdrText.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST HdrText WHERE HdrText.Brand = lcBrand) 
            THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSACTION:
        /* change */

        FIND HdrText where recid(HdrText) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN ufkey.
        cfc = "lis". RUN ufcolor.
        DISPLAY HdrText.te-nro HdrText.te-kie HdrText.te-text.

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhHdrText).

        IF gcHelpParam = "" THEN 
        UPDATE HdrText.te-text
          /*   te-kie  */.
        ELSE PAUSE MESSAGE "Press ENTER to continue".
          
        HIDE FRAME lis no-pause.
        DISPLAY HdrText.te-text
                te-kie /* qq */
        WITH FRAME sel.
        xrecid = recid(HdrText).

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhHdrText).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST HdrText 
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND FIRST HdrText USE-INDEX te-KIe
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND FIRST HdrText USE-INDEX te-text
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ASSIGN Memory = recid(HdrText) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST HdrText 
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND LAST HdrText USE-INDEX te-KIe
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND LAST HdrText USE-INDEX te-text
           WHERE HdrText.Brand = lcBrand NO-LOCK NO-ERROR.
        ASSIGN Memory = recid(HdrText) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        xRecid = 0.
        LEAVE LOOP.
     END. 

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid    = xrecid.

