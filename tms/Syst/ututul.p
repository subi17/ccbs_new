/* -----------------------------------------------
  MODULE .......: ututul.p
  FUNCTION .....: Tulosteiden yllApito
  APPLICATION ..: TS
  AUTHOR .......: TT
  CREATED ......: 27.06.91
  changePVM ....: 03.04.92 /tt 02.11.93 pt : vain superit
                  10.05.94 /tt LisAtty tulosteiden MAX.mAArAn kysyminen
                  25.09.96 /tt --> Ruotsinnettu
                  25.06.98 /kl vast => ok
                  09.02.99 /pt in English
                  25.05.99 /jp uright1 & uright2 added
                  25.04.02 lp added EMail
                  30.09.02/aam PrintQty removed
                  06.11.02/jr  Eventlog
                  06.03.03/tk  tokens
                  23.09.04/aam don't check email
  Version ......: M15
  ------------------------------------------------------ */

{Func/chkmail.i}
{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSReport'}

DEF NEW shared VAR si-tul LIKE TMSReport.RepName NO-UNDO.

DEF VAR ret          AS i                      NO-UNDO.
def var ok           as lo format "Yes/No" NO-UNDO.
DEF VAR memory       AS RECID              NO-UNDO.
def var line         as int format "99"    NO-UNDO.
DEF VAR delline      AS INT                NO-UNDO.
DEF VAR must-print   AS LOG                NO-UNDO.
DEF VAR must-add     AS LOG                NO-UNDO.
DEF VAR ufkey        AS LOG                NO-UNDO.
DEF VAR fr-header    AS CHAR.
DEF VAR rtab         AS RECID EXTENT 24    NO-UNDO.
DEF VAR i            AS INT                NO-UNDO.
DEF VAR xrecid       AS RECID.
DEF VAR moremail     AS CHAR   init ""     NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSReport AS HANDLE NO-UNDO.
   lhTMSReport = BUFFER TMSReport:HANDLE.
   RUN StarEventInitialize(lhTMSReport).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhTMSReport).
   END.
END.

form
    TMSReport.RepName  /* column-label "Printout Code" */
    TMSReport.Memo  /* column-label "Printout Name" 
    help "Name of printout"                         */
    TMSReport.PageWidth   /* column-label "Width"         */
    TMSReport.UpdPerm      column-label "Verify" format "Yes/No"
    TMSReport.ChEMail   format "Y/N" Column-label "C"
    TMSReport.EMail  format "x(20)"
WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc) TITLE COLOR value(ctc)
    ynimi + " PRINTOUTS "
    + string(pvm,"99-99-99")
    FRAME sel.

form
    TMSReport.RepName label "Printout Code ...."
    help "Code of a printout"   SKIP
    TMSReport.Memo label "Name of a printout"
    help "Name for a printout" SKIP
    TMSReport.PageWidth  label "Width (characters)"
    help "Width of a printout" SKIP
    TMSReport.UpdPerm  label "Verify (Y/N) ....." format "Yes/No"
    help "Shall a verification question to be made before start (Y/N) ?" SKIP
    TMSReport.ChEMail LABEL "E-mail changeable "
    HELP "Is this E-mail address changeable ? (Y/N)"
    TMSReport.EMail  LABEL "E-mail address ..."   format "x(50)"
    HELP "E-mail address" SKIP
    moremail       NO-LABEL format "x(70)"
    HELP "More E-mail addresses" SKIP

    WITH  OVERLAY ROW 8 centered COLOR value(cfc)
    TITLE COLOR value(ctc) fr-header WITH side-labels
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
PAUSE 0 no-message.
view FRAME sel.
FIND FIRST TMSReport no-lock no-error.
IF AVAILABLE TMSReport THEN DO:
   ASSIGN
   memory = recid(TMSReport)
   must-print = TRUE
   must-add    = FALSE.
END.
ELSE DO:
   ASSIGN
   memory = ?
   must-print = FALSE
   must-add    = TRUE.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

   IF must-add THEN DO:  /* TMSReport -ADD  */
      ASSIGN
      cfc = "lis"
      ufkey = TRUE
      fr-header = " ADD ".
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis:
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         PROMPT-FOR TMSReport.RepName
         VALIDATE
            (RepName = "" OR
            NOT can-find(TMSReport using  RepName),
            "REPORT " + string(input RepName) + " already exists !").
         if input RepName = "" THEN LEAVE add-new.
         CREATE TMSReport.
         ASSIGN
         RepName = INPUT FRAME lis RepName.
         UPDATE 
            TMSReport.Memo 
            TMSReport.PageWidth 
            TMSReport.UpdPerm 
            TMSReport.ChEMail 
            TMSReport.EMail 
            moremail.
    addEMail: REPEAT WITH FRAME lis:
          IF moremail NE "" THEN
             ASSIGN
             TMSReport.EMail = SUBSTRING(TMSReport.EMail,1,50) + moremail
             moremail      = "".
          LEAVE addemail.
    END. /* addemail */

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSReport).

         ASSIGN
         memory = recid(TMSReport)
         xrecid = memory.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST TMSReport no-lock no-error.
      IF NOT AVAILABLE TMSReport THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND TMSReport where recid(TMSReport) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE TMSReport THEN DO:
               DISPLAY TMSReport.RepName TMSReport.Memo
               TMSReport.PageWidth TMSReport.UpdPerm 
               TMSReport.ChEMail TMSReport.EMail. 
               rtab[FRAME-LINE] = recid(TMSReport).
               FIND NEXT TMSReport no-lock no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         up FRAME-LINE - 1.
         ASSIGN must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 164 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW TMSReport.Memo ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc) TMSReport.Memo WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "YOU ARE ON AN EMPTY ROW, MOVE UPWARDS !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND TMSReport where recid(TMSReport) = rtab[1] no-lock.
            FIND prev TMSReport no-lock no-error.
            IF NOT AVAILABLE TMSReport THEN DO:
               message "THIS IS THE 1ST ROW !".        
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY TMSReport.RepName TMSReport.Memo
               TMSReport.PageWidth TMSReport.UpdPerm 
               TMSReport.ChEMail TMSReport.EMail.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(TMSReport)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND TMSReport where recid(TMSReport) = rtab[FRAME-DOWN] no-lock .
            FIND NEXT TMSReport no-lock no-error.
            IF NOT AVAILABLE TMSReport THEN DO:
               message "THIS IS THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* was found vielA seuraava tietue */
               scroll up.
               DISPLAY TMSReport.RepName TMSReport.Memo
               TMSReport.PageWidth TMSReport.UpdPerm 
               TMSReport.ChEMail TMSReport.EMail.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(TMSReport).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND TMSReport where recid(TMSReport) = memory no-lock no-error.
         FIND prev TMSReport no-lock no-error.

         IF AVAILABLE TMSReport THEN DO:
            memory = recid(TMSReport).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev TMSReport no-lock no-error.
               IF AVAILABLE TMSReport THEN memory = recid(TMSReport).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "THIS IS THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:

        /* cursor TO the downmost line */

        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND TMSReport where recid(TMSReport) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     if lookup(nap,"1,f1") > 0 THEN DO:  /* lisAys */
        FIND TMSReport where recid(TMSReport) = rtab[FRAME-LINE] no-lock.
        ASSIGN si-tul = TMSReport.RepName.
        RUN Syst/ututum.p.
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisAys */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO:  /* removal */
        delline = FRAME-LINE.
        FIND TMSReport where recid(TMSReport) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) TMSReport.RepName TMSReport.Memo
        TMSReport.PageWidth TMSReport.UpdPerm .

        FIND NEXT TMSReport no-lock no-error.
        IF AVAILABLE TMSReport THEN memory = recid(TMSReport).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND TMSReport where recid(TMSReport) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           FIND prev TMSReport no-lock no-error.
           IF AVAILABLE TMSReport THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(TMSReport).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND TMSReport where recid(TMSReport) = rtab[FRAME-LINE] exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) TMSReport.RepName TMSReport.Memo
        TMSReport.PageWidth TMSReport.UpdPerm  TMSReport.ChEMail 
        TMSReport.EMail.
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSReport).
            DELETE TMSReport.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST TMSReport) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis:
        /* change */
        {Syst/uright2.i}
        FIND TMSReport where recid(TMSReport) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        cfc = "lis". RUN Syst/ufcolor.
        moremail = SUBSTRING(TMSReport.EMail,51,50).
        DISPLAY 
            TMSReport.RepName
            TMSReport.Memo 
            TMSReport.PageWidth 
            TMSReport.UpdPerm 
            TMSReport.ChEMail 
            TMSReport.EMail.

        IF lcRight = "RW" THEN DO :

           RUN Syst/ufkey.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSReport).

           UPDATE 
               TMSReport.Memo 
               TMSReport.PageWidth 
               TMSReport.UpdPerm 
               TMSReport.ChEMail 
               TMSReport.EMail 
               moremail.

    edEMail: REPEAT WITH FRAME lis:
          IF moremail NE "" THEN
             ASSIGN
             TMSReport.EMail = SUBSTRING(TMSReport.EMail,1,50) + moremail
             moremail      = "".
          LEAVE edemail.
    END. /* edemail */

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSReport).

        END.
        ELSE PAUSE.


        HIDE FRAME lis no-pause.
        DISPLAY TMSReport.Memo TMSReport.PageWidth TMSReport.UpdPerm 
        TMSReport.ChEMail TMSReport.EMail WITH FRAME sel .     
        xrecid = recid(TMSReport).
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* LAST record */
        FIND LAST TMSReport no-lock.
        ASSIGN
        memory = recid(TMSReport)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
        FIND FIRST TMSReport no-lock.
        ASSIGN
        memory = recid(TMSReport)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

