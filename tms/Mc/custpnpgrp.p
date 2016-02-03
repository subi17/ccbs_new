/* -----------------------------------------------
  MODULE .......: CUSTPNPGRP.P
  FUNCTION .....: Maintain Customer PNP Group
  APPLICATION ..: Master
  AUTHOR .......: JR
  CREATED ......: 18-11-02
  MODIFIED .....: 27.02.03 tk tokens
                  17.09.03 jp Brand 
  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable CustPnpGroup


{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'custpnpgroup'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustPNPGroup AS HANDLE NO-UNDO.
   lhCustPNPGroup = BUFFER CustPNPGroup:HANDLE.
   RUN StarEventInitialize(lhCustPNPGroup).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhCustPNPGroup).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustNum  LIKE CustPNPGroup.CustNum  NO-UNDO.
DEF VAR PnpGroup LIKE CustPNPGroup.PnpGroup NO-UNDO.
DEF VAR PnPPrior LIKE CustPNPGroup.PnPPrior NO-UNDO.

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
   CustPnpGroup.Brand 
   CustPNPGroup.CustNum 
   CustPNPGroup.PnpGroup 
   CustPNPGroup.PnPPrior
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " maintain Customer PNP Groups "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

{Func/brand.i}

form
   CustPNPGroup.CustNum validate(can-find(Customer where
                     Customer.CustNum = CustNum AND 
                     customer.Brand = lcBrand ),"Unknown Customer")
   CustPNPGroup.PnpGroup
   CustPNPGroup.PnPPrior
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH side-labels 1 columns
   FRAME lis.

form /*  search WITH FIELD CustNum */
    "Brand Code:" lcBrand  HELP "Enter Brand"
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "CustomerNo:" CustNum
    help "Give Customer number"
    with row 4 col 2 title color value(ctc) " FIND Customer "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WICTH FIELD PnpGroup */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Pnp Group :" PnpGroup help "Give PNP Group"
    with row 4 col 2 title color value(ctc) " FIND PNP "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form /*  search WITH FIELD PnPPriorform */


    PnPPrior
    help "Give Priority"
    with row 4 col 2 title color value(ctc) " FIND Priority "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CustPNPGroup
WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.
IF AVAILABLE CustPNPGroup THEN ASSIGN
   memory     = recid(CustPNPGroup)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No PNP groups available !" VIEW-AS ALERT-BOX.
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
       if order = 1 then put screen row 19 col 30 " Order by ".
       if order = 2 then put screen row 19 col 30 " Order by ".
       if order = 3 then put screen row 19 col 30 " Order by ".
    END.

   IF must-add THEN DO:  /* CustPNPGroup -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           CREATE CustPNPGroup.
           UPDATE 
              CustPNPGroup.CustNum 
              CustPNPGroup.PnpGroup 
              CustPNPGroup.PnPPrior
           WITH FRAME lis EDITING.
              READKEY. nap = keylabel(LASTKEY).
              if keylabel(lastkey) = "F4" THEN UNDO add-new, LEAVE add-new.
              IF lookup(nap,poisnap) > 0 THEN DO:
                 if frame-field = "CustNum" THEN DO:
                    if input frame lis CustPNPGroup.CustNum  = "" THEN
                       UNDO add-new, LEAVE add-new.
                 END.
              END.
              APPLY LASTKEY.
           END.
           ASSIGN
              memory = recid(CustPNPGroup)
              xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustPNPGroup).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST CustPNPGroup
      WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.
      IF NOT AVAILABLE CustPNPGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND CustPNPGroup where recid(CustPNPGroup) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CustPNPGroup THEN DO:
              DISPLAY 
                 CustPNPGroup.CustNum 
                 CustPNPGroup.PnpGroup 
                 CustPNPGroup.PnPPrior.
              rtab[FRAME-LINE] = recid(CustPNPGroup).
              IF order = 1 THEN FIND NEXT CustPNPGroup
              WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 1761
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW CustPNPGroup.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CustPNPGroup.CustNum WITH FRAME sel.
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
        FIND CustPNPGroup where recid(CustPNPGroup) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev CustPNPGroup
           WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

           IF AVAILABLE CustPNPGroup THEN
              ASSIGN firstline = i memory = recid(CustPNPGroup).
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
           FIND CustPNPGroup where recid(CustPNPGroup) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev CustPNPGroup
           WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

           IF NOT AVAILABLE CustPNPGroup THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 CustPNPGroup.CustNum 
                 CustPNPGroup.PnpGroup 
                 CustPNPGroup.PnPPrior.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(CustPNPGroup)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND CustPNPGroup where recid(CustPNPGroup) = rtab[FRAME-DOWN] 
           no-lock .
           IF order = 1 THEN FIND NEXT CustPNPGroup
           WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

           IF NOT AVAILABLE CustPNPGroup THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 CustPNPGroup.CustNum 
                 CustPNPGroup.PnpGroup 
                 CustPNPGroup.PnPPrior.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(CustPNPGroup).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CustPNPGroup where recid(CustPNPGroup) = memory no-lock no-error.
        IF order = 1 THEN FIND prev CustPNPGroup
        WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

        IF AVAILABLE CustPNPGroup THEN DO:
           memory = recid(CustPNPGroup).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev CustPNPGroup
              WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

              IF AVAILABLE CustPNPGroup THEN memory = recid(CustPNPGroup).
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
           FIND CustPNPGroup where recid(CustPNPGroup) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       CustNum = 0.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       Disp lcBrand With FRAME f1.
       UPDATE lcBrand WHEN gcAllBrand = TRUE 
              CustNum WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if CustNum <> 0 THEN DO:
          FIND FIRST CustPNPGroup where 
                     CustPNPGroup.CustNum >= CustNum AND 
                     CustPNPGroup.Brand    = gcBrand 
          no-lock no-error.

           IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
        FIND CustPNPGroup where recid(CustPNPGroup) = rtab[frame-line(sel)]
             NO-lock.
        FIND FIRST pnpgroup where pnpgroup.pnpgroup = custpnpgroup.pnpgroup
        no-lock no-error.

        if not avail pnpgroup then do:
           MESSAGE
           "PNP group" custpnpgroup.pnpgroup " does not exist!"
           view-as alert-box.
           next.
        end.

       RUN pnplist.p(pnpgroup.pnpSeq). 
       ufkey = true.
       run ufkey.
       PAUSE 0.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND CustPNPGroup where recid(CustPNPGroup) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          CustPNPGroup.CustNum 
          CustPNPGroup.PnpGroup 
          CustPNPGroup.PnPPrior.

       IF order = 1 THEN FIND NEXT CustPNPGroup
       WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

       IF AVAILABLE CustPNPGroup THEN memory = recid(CustPNPGroup).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND CustPNPGroup where recid(CustPNPGroup) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev CustPNPGroup
          WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

          IF AVAILABLE CustPNPGroup THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(CustPNPGroup).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND CustPNPGroup where recid(CustPNPGroup) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          CustPNPGroup.CustNum 
          CustPNPGroup.PnpGroup 
          CustPNPGroup.PnPPrior.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustPNPGroup).

           DELETE CustPNPGroup.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST CustPNPGroup
           WHERE CustPNPGroup.Brand = gcBrand) THEN DO:
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

     DO WITH FRAME lis transaction:
       /* change */
       FIND CustPNPGroup where recid(CustPNPGroup) = rtab[frame-line(sel)]
       exclusive-lock.

       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       DISPLAY 
          CustPNPGroup.CustNum
          CustPNPGroup.PnpGroup 
          CustPNPGroup.PnPPrior.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustPNPGroup).

          UPDATE   
             CustPNPGroup.CustNum 
             CustPNPGroup.PnpGroup 
             CustPNPGroup.PnPPrior.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustPNPGroup).
       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.
       DISPLAY 
          CustPNPGroup.CustNum 
          CustPNPGroup.PnpGroup 
          CustPNPGroup.PnPPrior
       WITH FRAME sel.
       xrecid = recid(Currency).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST CustPNPGroup
       WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

       ASSIGN memory = recid(CustPNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST CustPNPGroup
       WHERE CustPNPGroup.Brand = gcBrand no-lock no-error.

       ASSIGN memory = recid(CustPNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

