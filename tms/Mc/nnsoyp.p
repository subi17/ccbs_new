/* -----------------------------------------------
  MODULE .......: NNSOYP.P
  FUNCTION .....: Maintain sales offices
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 21-01-98
  MODIFIED .....: 24.03.98 pt NEW FIELD CostCentre
                  18.05.99 jp uright1 & uright2  
                  26.04.02/tk eventlogging added
                  25.02.03 tk tokens
                  11.09.03 tk brand

  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable salesoffice

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'salesoffice'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSalesoffice AS HANDLE NO-UNDO.
   lhSalesoffice = BUFFER Salesoffice:HANDLE.
   RUN StarEventInitialize(lhSalesoffice).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSalesoffice).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR haku-so-code    LIKE Salesoffice.SalesOffice  NO-UNDO.
DEF VAR haku-so-name    LIKE Salesoffice.SOName  NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.

form
    Salesoffice.Brand
    Salesoffice.SalesOffice
    Salesoffice.SOName
    Salesoffice.CostCentre /* column-label "Cct" help "Cost center" */
WITH width 80 OVERLAY scroll 1 15 DOWN
    color value(cfc) title color value(ctc) " " + ynimi +
    " Maintain sales offices " + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    Salesoffice.SalesOffice
    Salesoffice.SOName
    VALIDATE(Salesoffice.soname ne "","Missing salesoffice name!")
    Salesoffice.CostCentre 
    VALIDATE(SalesOffice.CostCentre ne "000","Missing Cost Centre!")
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /*  search WITH FIELD Salesoffice */
    "Brand:" lcBrand skip
    "Code :" haku-so-code
    help "Give code or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /*  search WITH FIELD SOName */
    "Brand:" lcBrand skip
    "Name :" haku-so-name
    help "Give Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.


run pFindFirst.
IF AVAILABLE Salesoffice THEN ASSIGN
   memory     = recid(Salesoffice)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Salesoffices available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = TRUE.
END.
LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 33 " Order by code ".
       if order = 2 then put screen row 19 col 33 " Order by Name ".
    END.

   IF must-add THEN DO:  /* Salesoffice -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR Salesoffice.SalesOffice
           VALIDATE
              (Salesoffice.SalesOffice = "" OR
              NOT can-find(Salesoffice using  Salesoffice.SalesOffice where
                           Salesoffice.Brand = lcBrand),
              " " + string(INPUT Salesoffice.SalesOffice) +
              " already exists !").
           if input Salesoffice.SalesOffice = "" THEN LEAVE add-new.
           CREATE Salesoffice.
           ASSIGN
           Salesoffice.Brand = lcBrand
           Salesoffice.SalesOffice = INPUT FRAME lis Salesoffice.SalesOffice.
           UPDATE 
              Salesoffice.SOName
              Salesoffice.CostCentre.
           ASSIGN
             memory = recid(Salesoffice)
             xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSalesoffice).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      run pFindFirst.
      IF NOT AVAILABLE Salesoffice THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Salesoffice where recid(Salesoffice) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE Salesoffice THEN DO:
              DISPLAY 
                 Salesoffice.Brand
                 Salesoffice.SalesOffice 
                 Salesoffice.SOName 
                 Salesoffice.CostCentre   .
              rtab[FRAME-LINE] = recid(Salesoffice).
              run pFindNext.
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW Salesoffice.SalesOffice ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Salesoffice.SalesOffice WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Salesoffice.SOName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Salesoffice.SOName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND Salesoffice where recid(Salesoffice) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           run pFindPrev.
           IF AVAILABLE Salesoffice THEN
              ASSIGN firstline = i memory = recid(Salesoffice).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND Salesoffice where recid(Salesoffice) = rtab[1] no-lock.
           run pFindPrev.
           IF NOT AVAILABLE Salesoffice THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 Salesoffice.Brand
                 Salesoffice.SalesOffice 
                 Salesoffice.SOName 
                 Salesoffice.CostCentre.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(Salesoffice)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Salesoffice where recid(Salesoffice) = rtab[FRAME-DOWN] no-lock.
           run pFindNext.
           IF NOT AVAILABLE Salesoffice THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 Salesoffice.Brand
                 Salesoffice.SalesOffice 
                 Salesoffice.SOName 
                 Salesoffice.CostCentre   .
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Salesoffice).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Salesoffice where recid(Salesoffice) = memory no-lock no-error.
        run pFindPrev.
        IF AVAILABLE Salesoffice THEN DO:
           memory = recid(Salesoffice).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              run pFindPrev.
              IF AVAILABLE Salesoffice THEN memory = recid(Salesoffice).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND Salesoffice where recid(Salesoffice) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       haku-so-code = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISP lcBrand with frame haku-f1.
       UPDATE 
         lcBrand WHEN gcAllBrand
         haku-so-code WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if haku-so-code <> "" THEN DO:
          FIND FIRST Salesoffice where 
                     Salesoffice.Brand = lcBrand AND
                     Salesoffice.SalesOffice >= haku-so-code
          /* search condition */ no-lock no-error.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       haku-so-name = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISP lcBrand WITH frame haku-f2.
       UPDATE 
         lcBrand WHEN gcAllBrand
         haku-so-name WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       if haku-so-name <> "" THEN DO:
          FIND FIRST Salesoffice where 
                     Salesoffice.Brand = lcBrand AND
                     Salesoffice.SOName >= haku-so-name
          USE-INDEX SOName /* search condition */ no-lock no-error.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */

       delline = FRAME-LINE.
       FIND Salesoffice where recid(Salesoffice) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          Salesoffice.Brand   
          Salesoffice.SalesOffice 
          Salesoffice.SOName  
          Salesoffice.CostCentre.

       run pFindNext.
       IF AVAILABLE Salesoffice THEN memory = recid(Salesoffice).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND Salesoffice where recid(Salesoffice) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          run pFindPrev.
          IF AVAILABLE Salesoffice THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(Salesoffice).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND Salesoffice where recid(Salesoffice) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          Salesoffice.Brand  
          Salesoffice.SalesOffice 
          Salesoffice.SOName 
          Salesoffice.CostCentre.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSalesoffice).

           DELETE Salesoffice.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST Salesoffice
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND Salesoffice where recid(Salesoffice) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY 
          Salesoffice.SalesOffice  
          Salesoffice.SOName 
          Salesoffice.CostCentre.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSalesoffice).

          UPDATE 
             Salesoffice.SOName 
             Salesoffice.CostCentre.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSalesoffice).
       END.
       ELSE PAUSE.
       HIDE FRAME lis no-pause.
       DISPLAY Salesoffice.SOName  Salesoffice.CostCentre

       WITH FRAME sel.
       xrecid = recid(Salesoffice).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       run pFindFirst.
       ASSIGN memory = recid(Salesoffice) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       run pFindLast.
       ASSIGN memory = recid(Salesoffice) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

procedure pFindNext:
   case order:
      when 1 THEN 
         FIND NEXT Salesoffice WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND NEXT Salesoffice USE-INDEX SOName WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
   end.
end procedure.

procedure pFindPrev:
   case order:
      when 1 THEN 
         FIND PREV Salesoffice WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND PREV Salesoffice USE-INDEX SOName WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
   end.
end procedure.

procedure pFindFirst:
   case order:
      when 1 THEN 
         FIND FIRST Salesoffice WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND FIRST Salesoffice USE-INDEX SOName WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
   end.
end procedure.

procedure pFindLast:
   case order:
      when 1 THEN 
         FIND LAST Salesoffice WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND LAST Salesoffice USE-INDEX SOName WHERE
                   Salesoffice.Brand = lcBrand
         no-lock no-error.
   end.
end procedure.

