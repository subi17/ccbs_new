/* -----------------------------------------------
  MODULE .......: NNIGYP.P
  FUNCTION .....: Updating Invoicing Groups
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED  .....: 05.10.97
  MODIFIED .....: 02.12.97 kl InvNum handling added
                  03.08.98 kl FUNCTION.i / fOLRefresh
                  14.10.98 kl mininv + upmth FOR invoice groups
                  05.11.98 kl DO NOT allow similar 3 FIRST digits in InvGroup
                  16.12.98 kl bug in creating a NEW InvGroup fixed
                  18.01.99 kl if InvGroup = "" when adding, LEAVE
                  13.04.99 pt change FORMAT of invoice numbers 9 > 8 digits
                  18.05.99 jp uright1 & uright2 added  
                  20.10.99 kl extra fee IF NOT autogiro
                  05.10.01 kl two column labels hard-coded
                  13.02.02 ht CollCustNum for "collection for Intrum"
                  26.04.02 tk eventlogging added
                  20.05.02/tk RUN Mc/memo
                  20.05.02/tk invoice texts
                  31.05.02 aam UnbilledLimit includes days, NOT months 
                  09.09.02/jp some validation
                  13.11.02/jr New Memo
                  27.02.03/tk tokens
                  04.03.03/aam invrunlog
                  11.09.03/tk  brand
                  06.02.04 jp  CustNum for memos
                  24.01.05/aam CollCust removed
                  20.09.05/aam use InvFrom for redirecting invoice nbr usage,
                               first 5 character check removed for InvGroup-id
                  13.11.06/aam TaxZone             
                  16.11.06/aam invoice nbr sequences, IgInvNum
                  05.01.07/aam voucher nbr sequences, IgVoucher
  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable InvGroup

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invgroup'}
{Func/function.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvGroup AS HANDLE NO-UNDO.
   lhInvGroup = BUFFER InvGroup:HANDLE.
   RUN StarEventInitialize(lhInvGroup).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhInvGroup).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.
DEF BUFFER bgroup FOR InvGroup.

DEF VAR haku-ig-code LIKE InvGroup.InvGroup  NO-UNDO.
DEF VAR haku-ig-name LIKE InvGroup.IGName NO-UNDO.
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
DEF VAR lcZoneName AS CHAR                   NO-UNDO.

form
    InvGroup.Brand 
    InvGroup.InvGroup     /* COLUMN-LABEL FORMAT */
    InvGroup.IGName     /* column-label */ format "x(25)"
    InvGroup.InvForm        column-label "Seq. Grp" format "x(8)"
    InvGroup.BillPerm        column-label "I" format "Y/N" space(0)
    InvGroup.UpdCustBal          column-label "A" format "Y/N" space(0)
    InvGroup.Banned         column-label "B" format "Y/N"
    InvGroup.MinInvAmt      column-label "MInv" format "zz9.99"
    InvGroup.TaxZone        FORMAT "X(5)" COLUMN-LABEL "TaxZ."
    WITH width 80 OVERLAY scroll 1 15 DOWN ROW 1
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Invoice groups "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    InvGroup.InvGroup    /* LABEL FORMAT */ COLON 22
    InvGroup.IGName    /* LABEL FORMAT */   COLON 22
    VALIDATE(INPUT invgroup.igname ne "","Missing Invoice group name!")
    InvGroup.InvForm     COLON 22
       LABEL "Sequence Group"
       HELP "Use invoice number sequence from this group"
       VALIDATE(INPUT InvGroup.InvForm = INPUT InvGroup.InvGroup OR
                CAN-FIND(InvGroup WHERE
                         InvGroup.Brand    = gcBrand AND
                         InvGroup.InvGroup = INPUT InvGroup.InvForm),
                "Unknown group") 
    InvGroup.CompName   /* LABEL FORMAT */ COLON 22
    InvGroup.Contact   /* LABEL FORMAT */  COLON 22
    InvGroup.BillPerm    label "Invoiced"  COLON 22
    InvGroup.UpdCustBal  label "Accounts"  COLON 22
    InvGroup.MinInvAmt                     COLON 22
    
    InvGroup.TaxZone COLON 22
       VALIDATE(CAN-FIND(TaxZone WHERE TaxZone.TaxZone = INPUT InvGroup.TaxZone)
                OR INPUT InvGroup.TaxZone = "",
                "Unknown tax zone")
    lcZoneName  
       FORMAT "X(30)" 
       NO-LABEL 
    
    /* InvGroup.InvFee */
    InvGroup.UnbilledLimit COLON 22
        format ">>9"
        label "Ignore min.amt"
        help "Days from last to bill to ignore minimum invoicing amount"
    InvGroup.Banned COLON 22
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header 
    side-labels FRAME lis.

form /* Invoicing Group search WITH FIELD InvGroup */
    "Brand:" lcBrand skip       
    "Code :" haku-ig-code
    help "Give a code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /* Invoicing Group search WITH FIELD IGName */
    "Brand:" lcBrand skip
    "Name :" haku-ig-name
    help "Give a name"
    with row 4 col 2 title color value(ctc) " FIND NAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

FUNCTION fZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   lcZoneName = "".
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN lcZoneName = TaxZone.TZName. 
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

run pFindFirst.
IF AVAILABLE InvGroup THEN ASSIGN
   memory       = recid(InvGroup)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No invoicing groups available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 34 "   By Code   ".
       if order = 2 then put screen row 19 col 34 "   By Name   ".
    END.

   IF must-add THEN DO:  /* InvGroup -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
      
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
       
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.

         DO TRANSAction:
         
         CREATE InvGroup.
         InvGroup.Brand = lcBrand.
         
         UPDATE 
            InvGroup.InvGroup   InvGroup.IGName
            InvGroup.InvForm
            InvGroup.CompName  InvGroup.Contact
            InvGroup.BillPerm   InvGroup.UpdCustBal
            InvGroup.MinInvAmt 
            InvGroup.TaxZone
            InvGroup.UnbilledLimit
         EDITING:
            READKEY. nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               if frame-field = "InvGroup" THEN DO:
                  if input frame lis InvGroup.InvGroup = "" THEN 
                     UNDO, LEAVE add-new.
                  /* FIND matching code */
                  FIND FIRST bgroup where
                             bgroup.InvGroup = INPUT FRAME lis InvGroup.InvGroup
                  no-lock no-error.
                  IF AVAIL bgroup THEN DO:
                     BELL.
                     message "Code " bgroup.InvGroup " already exists !".
                     message " - Press ENTER - ".
                     PAUSE no-message.
                     NEXT-PROMPT InvGroup.InvGroup.
                     NEXT.
                  END.
                  ELSE DO:
                     ASSIGN InvGroup.InvGroup.
                     DISPLAY InvGroup.InvGroup @ InvGroup.InvForm
                     WITH FRAME lis.
                  END. 
               END.
               
               ELSE IF FRAME-FIELD = "InvForm" THEN DO:
               END.

               ELSE IF FRAME-FIELD = "TaxZone" THEN DO:
                  fZoneName(INPUT INPUT FRAME lis InvGroup.TaxZone).
                  DISPLAY lcZoneName WITH FRAME lis.
               END.

            
            END.

            APPLY LASTKEY.

         END.

         END.   


         /* UPDATE upd -field FOR nncdr -module checking */
         IF NEW InvGroup THEN DO:
            fOLRefresh(TRUE).
            /* create invoice run logs for new group */
            RUN Mc/invrunlog(YEAR(TODAY) * 100 + MONTH(TODAY),
                          YEAR(TODAY) * 100 + 12).
         END.

         ASSIGN
            memory = recid(InvGroup)
            xrecid = memory.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhInvGroup).

      END.  /* add-new */

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      run pFindFirst.
      IF NOT AVAILABLE InvGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND InvGroup where recid(InvGroup) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE InvGroup THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(InvGroup).
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
         ufk[1]= 35  ufk[2]= 30 ufk[3]= 927 ufk[4]= 1120
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 1760 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW InvGroup.InvGroup {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) InvGroup.InvGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW InvGroup.IGName {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) InvGroup.IGName WITH FRAME sel.
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
         FIND InvGroup where recid(InvGroup) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            run pFindPrev.
            IF AVAILABLE InvGroup THEN
               ASSIGN firstline = i memory = recid(InvGroup).
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
            FIND InvGroup where recid(InvGroup) = rtab[1] no-lock.
            run pFindPrev.
            IF NOT AVAILABLE InvGroup THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(InvGroup)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND InvGroup where recid(InvGroup) = rtab[FRAME-DOWN] no-lock .
            run pFindNext.
            IF NOT AVAILABLE InvGroup THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               
               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(InvGroup).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND InvGroup where recid(InvGroup) = memory no-lock no-error.
         run pFindPrev.
         IF AVAILABLE InvGroup THEN DO:
            memory = recid(InvGroup).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               run pFindPrev.
               IF AVAILABLE InvGroup THEN memory = recid(InvGroup).
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
            FIND InvGroup where recid(InvGroup) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.
        haku-ig-code = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISP lcBrand with frame haku-f1.
        UPDATE 
           lcBrand when gcAllBrand
           haku-ig-code WITH FRAME haku-f1.
        HIDE FRAME haku-f1 no-pause.
        if haku-ig-code <> "" THEN DO:
           FIND FIRST InvGroup where 
                      InvGroup.Brand = lcBrand AND
                      InvGroup.InvGroup >= haku-ig-code
           /* search condition */ no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        haku-ig-name = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISP lcBrand WITH FRAME haku-f2.
        UPDATE 
           lcBrand when gcAllBrand
           haku-ig-name WITH FRAME haku-f2.
        HIDE FRAME haku-f2 no-pause.
        if haku-ig-name <> "" THEN DO:
           FIND FIRST InvGroup where 
                      InvGroup.Brand = lcBrand AND
                      InvGroup.IGName >= haku-ig-name
           USE-INDEX IGName /* search condition */ no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0   /* memo */
     THEN DO TRANS:
        FIND InvGroup WHERE RECID(InvGroup) = rtab[FRAME-LINE(sel)]
        NO-LOCK NO-ERROR.
        RUN Mc/memo(INPUT 0,
                 INPUT "INVGROUP",
                 INPUT STRING(InvGroup.InvGroup),
                 INPUT "Invoice group code").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* number sequences */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:
     
        FIND InvGroup WHERE RECID(InvGroup) = rtab[FRAME-LINE(sel)]
           NO-LOCK NO-ERROR.
           
        REPEAT WITH FRAME sel:
           ASSIGN ufk    = 0
                  ufk[1] = 1100
                  ufk[2] = 1119
                  ufk[8] = 8
                  ehto   = 0.
           RUN Syst/ufkey.
            
           /* invoice number sequences */
           IF toimi = 1 THEN DO:

              IF AVAILABLE InvGroup THEN DO:
                 IF InvGroup.InvForm NE InvGroup.InvGroup THEN DO: 
                    MESSAGE "Invoice number sequences are taken from"
                            "group" InvGroup.InvForm
                    VIEW-AS ALERT-BOX INFORMATION.
                 END.

                 ELSE RUN Mc/iginvnum(InvGroup.InvGroup).
              END.
           END.

           /* voucher number sequences */
           ELSE IF toimi = 2 THEN DO:

              IF AVAILABLE InvGroup THEN DO:
                 RUN Mc/igvoucher(InvGroup.InvGroup).
              END.
           END.
            
           ELSE IF toimi = 8 THEN LEAVE.
        END.

        ufkey = TRUE.
        NEXT LOOP.        
     END.          
 
     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */

        delline = FRAME-LINE.
        FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) 
           InvGroup.Brand 
           InvGroup.InvGroup 
           InvGroup.IGName 
           InvGroup.BillPerm 
           InvGroup.UpdCustBal 
           InvGroup.Banned 
           InvGroup.MinInvAmt.

        run pFindNext.
        IF AVAILABLE InvGroup THEN memory = recid(InvGroup).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           run pFindPrev.
           IF AVAILABLE InvGroup THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(InvGroup).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message "DO YOU REALLY WANT TO ERASE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY value(ccc) 
           InvGroup.InvGroup 
           InvGroup.IGName 
           InvGroup.BillPerm 
           InvGroup.UpdCustBal 
           InvGroup.Banned 
           InvGroup.MinInvAmt.

        IF ok THEN DO:

            FIND FIRST Customer WHERE 
                       Customer.Brand = gcBrand AND
                       Customer.InvGroup = InvGroup.InvGroup
            no-lock no-error.
            IF AVAIL Customer THEN DO:
               BELL. MESSAGE
               "There are customers belonging to this invoice group " + 
               "- cannot be deleted !".
               PAUSE 3 no-message.
               NEXT.
            END.

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInvGroup).

            DELETE InvGroup.
            /* CREATE OLRefresh -file FOR nncdr -module */
            fOLRefresh(TRUE).

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST InvGroup
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

     ELSE IF lookup(nap,"7,f7") > 0 THEN DO:
        FIND InvGroup WHERE recid(InvGroup) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invotxt.p("InvGroup",InvGroup.InvGroup).
        must-print=true.
        ufkey=true.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
        /* change */

        FIND InvGroup where recid(InvGroup) = rtab[frame-line(sel)]
        exclusive-lock.
        ASSIGN ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.
        assign fr-header = " CHANGE ".

        fZoneName(InvGroup.TaxZone).
        
        DISPLAY 
           InvGroup.InvGroup
           InvGroup.IGName 
           InvGroup.InvForm
           InvGroup.CompName 
           InvGroup.BillPerm 
           InvGroup.UpdCustBal 
           InvGroup.MinInvAmt 
           InvGroup.TaxZone
           lcZoneName
           InvGroup.UnbilledLimit 
           InvGroup.Banned.

        IF lcRight = "RW" THEN DO:

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvGroup).

           UPDATE InvGroup.IGName
                  InvGroup.InvForm
                  InvGroup.CompName 
                  InvGroup.Contact 
                  InvGroup.BillPerm 
                  InvGroup.UpdCustBal 
                  InvGroup.MinInvAmt 
                  InvGroup.TaxZone
                  InvGroup.UnbilledLimit 
                  InvGroup.Banned
           WITH FRAME lis EDITING:
           
              READKEY. nap = keylabel(LASTKEY).
              IF lookup(nap,poisnap) > 0 THEN DO:
            
                 IF FRAME-FIELD = "InvForm" THEN DO:
                 END.

                 ELSE IF FRAME-FIELD = "TaxZone" THEN DO:
                    fZoneName(INPUT INPUT FRAME lis InvGroup.TaxZone).
                    DISPLAY lcZoneName WITH FRAME lis.
                 END.
              
              END.
              
              APPLY LASTKEY.
           END.

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvGroup).
        END.
        ELSE PAUSE.

        HIDE FRAME lis no-pause.


        /* UPDATE upd -field FOR nncdr -module checking */
        IF BillPerm entered OR UpdCustBal entered THEN fOLRefresh(TRUE).

        RUN local-disp-row.

        xrecid = recid(InvGroup).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        run pFindFirst.
        ASSIGN memory = recid(InvGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        run pFindLast.
        ASSIGN memory = recid(InvGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   DISPLAY InvGroup.Brand
           InvGroup.InvGroup 
           InvGroup.IGName 
           InvGroup.BillPerm 
           InvGroup.UpdCustBal
           InvGroup.Banned
           InvGroup.MinInvAmt
           InvGroup.TaxZone
           InvGroup.InvForm
   WITH FRAME sel.

END PROCEDURE.


procedure pFindFirst:
   case order:
      when 1 THEN 
         FIND FIRST InvGroup WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND FIRST InvGroup USE-INDEX IGName WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
   end.     
end procedure.

procedure pFindLast:
   case order:
      when 1 THEN 
         FIND LAST InvGroup WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND LAST InvGroup USE-INDEX IGName WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
   end.     
end procedure.

procedure pFindNext:
   case order:
      when 1 THEN 
         FIND NEXT InvGroup WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND NEXT InvGroup USE-INDEX IGName WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
   end.     
end procedure.

procedure pFindPrev:
   case order:
      when 1 THEN 
         FIND PREV InvGroup WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
      when 2 THEN 
         FIND PREV InvGroup USE-INDEX IGName WHERE
                    InvGroup.Brand = lcBrand
         no-lock no-error.
   end.     
end procedure.

