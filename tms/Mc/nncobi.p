/* ---------------------------------------------------------------------------
  MODULE .......: NNCOBI.P
  FUNCTION .....: Browse Billable items of an contract
  APPLICATION ..: NN
  AUTHOR .......: kal
  CREATED ......: 01-12-98
  MODIFIED .....: 04.12.98 kl DISPLAY Concerns
                  08.12.98 pt BillMethod & Interval into TITLE bar on FRAME sel.
                  24.08.00 pt show BillCode on FRAME sel
                  25.10.02 aam billed items cannot be deleted
                  08.11.02 jr Eventlog
                  15.11.02 lp use invdet.i for invoice detailes
                  03.03.03 tk tokens
                  16.09.03/aam brand
                  24.01.05/aam allow update of last item's concerns[2]
                  30.12.05 /vk allow update of bill period and period dates
                  24.01.06 jt DYNAMIC-FUNCTION("fDispCustName"
                  06.03.06 /vk asking password also when user pressed ENTER
                  14.11.07 jt  Layout changes in frame sel
                  14.11.07 jt  display M if ffitem has memo
  Version ......: M15
  --------------------------------------------------------------------------- */

&GLOBAL-DEFINE TMSCodeDef NO

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ffitem'}
{Syst/eventval.i} 
{Ar/invdet.i}
{Func/finvbal.i}
{Func/cparam2.i}

DEF INPUT PARAMETER  FFNum  AS i  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR BillPeriod      LIKE FFItem.BillPeriod  NO-UNDO.
DEF VAR prod-name      LIKE BillItem.BIName     NO-UNDO.
DEF VAR Qty        LIKE FFItem.Amt    NO-UNDO.
DEF VAR xrecid         AS RECID                        init ?.
DEF VAR firstline      AS INT                 NO-UNDO  init 0.
DEF VAR order          AS INT                 NO-UNDO  init 1.
DEF VAR ordercount     AS INT                 NO-UNDO  init 1.
DEF VAR ufkey          AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline        AS INT                 NO-UNDO  init 0.
DEF VAR ex-order       AS INT                 NO-UNDO.
DEF VAR memory         AS RECID               NO-UNDO.
def var line           as int format "99"     NO-UNDO.
DEF VAR must-print     AS LOG                 NO-UNDO.
DEF VAR must-add       AS LOG                 NO-UNDO.
DEF VAR fr-header      AS CHAR                NO-UNDO.
DEF VAR rtab           AS RECID EXTENT 24     NO-UNDO.
DEF VAR i              AS INT                 NO-UNDO.
DEF VAR rc             AS INT                 NO-UNDO.
def var ok             as log format "Yes/No" NO-UNDO.
DEF VAR llLast         AS LOG                 NO-UNDO.
DEF VAR lcPassword     AS CHAR                NO-UNDO.
DEF VAR llUpdate       AS LOG                 NO-UNDO. 
DEF VAR lcAskPassWd    AS CHAR                NO-UNDO.
DEF VAR lcCustName     AS CHAR                NO-UNDO.  
DEF VAR lcExtInvNum    AS CHAR                NO-UNDO.
DEF VAR lcMemoAvail    AS CHAR                NO-UNDO. 

DEF BUFFER bFFItem FOR FFItem.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFFItem AS HANDLE NO-UNDO.
   lhFFItem = BUFFER FFItem:HANDLE.
   RUN StarEventInitialize(lhFFItem).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhFFItem).
   END.
END.

form
    FFItem.BillPeriod 
    FFItem.Concerns[1] column-label "From" format "99999999"
    FFItem.Concerns[2] column-label "To" format "99999999"
    FFItem.BillCode    column-label "Billing Item"     format "x(12)"
    lcExtInvNum        column-label "Invoice"          FORMAT "x(12)"
    FFItem.Amt         column-label "Amount"
    lcMemoAvail        column-label "M" FORMAT "x(1)"
WITH
    centered ROW 2 OVERLAY scroll 1 13 DOWN
    color value(cfc) title color value(ctc) " " +
    substr(lcCustName,1,16) + " / " + substr(prod-name,1,16) +
    ": Billable ITEMS, Type = " + string(FixedFee.BillMethod) + ", intvl = " +
    string(FixedFee.Interval) + " "  FRAME sel.

form
    "Billing Period .:" FFItem.BillPeriod                    SKIP
    "From Period ....:" FFItem.Concerns[1] FORMAT "99999999" SKIP
    "To Period.......:" FFItem.Concerns[2] FORMAT "99999999" SKIP
    "Billable Payment:" FFItem.Amt                           SKIP
    "Our Own Cost ...:" FFItem.OwnCost                       SKIP
 WITH  OVERLAY ROW 7 centered
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header WITH NO-LABEL
    FRAME lis.

form /*  search WITH FIELD CustNum */
    BillPeriod
    help "Give Period YyyyMm"
    with row 4 col 2 title color value(ctc) " FIND Period "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* memo */
WITH
    OVERLAY ROW 7 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update memo "
    FRAME memo.

FIND FixedFee where FixedFee.FFNum = FFNum no-lock.

FIND BillItem where 
     BillItem.Brand    = FixedFee.Brand AND
     BillItem.BillCode = FixedFee.BillCode no-lock no-error.
IF AVAIL BillItem THEN prod-name = BIName.
else prod-name = "!! UNKNOWN !!!".

FIND Customer where Customer.CustNum = FixedFee.CustNum no-lock.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
                                    
FIND FIRST FFItem
where FFItem.FFNum = FFNum no-lock no-error.
IF AVAILABLE FFItem THEN ASSIGN
   memory     = recid(FFItem)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   BELL. MESSAGE
   "ERROR: NO Billable items at all  - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

ASSIGN lcRight = "".
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.
    
   print-line:
   DO:
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND FFItem where recid(FFItem) = memory no-lock no-error.
        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE FFItem THEN DO:
              FIND FIRST BillItem   where 
                         BillItem.Brand    = FixedFee.Brand AND
                         BillItem.BillCode = FFItem.BillCode 
              no-lock no-error.
              IF AVAIL BillItem THEN prod-name = BIName.
              else prod-name = "!! UNKNOWN !!!".
              RUN local-find-others.
              DISPLAY 
                 FFItem.BillCode
                 lcExtInvNum
                 FFItem.Amt 
                 FFItem.BillPeriod 
                 FFItem.Concerns[1]
                 FFItem.Concerns[2]
                 lcMemoAvail.
              rtab[FRAME-LINE] = recid(FFItem).
              IF order = 1 THEN FIND NEXT FFItem
              where FFItem.FFNum = FFNum no-lock no-error.
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
        ufk[1]= 771 ufk[2]= 0  ufk[3]= 927 ufk[4]= 0
        ufk[5]= 1491   
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto  = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW FFItem.BillPeriod ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) FFItem.BillPeriod WITH FRAME sel.
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
        FIND FFItem where recid(FFItem) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev FFItem
           where FFItem.FFNum = FFNum no-lock no-error.
           IF AVAILABLE FFItem THEN
              ASSIGN firstline = i memory = recid(FFItem).
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
           FIND FFItem where recid(FFItem) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev FFItem
           where FFItem.FFNum = FFNum no-lock no-error.
           IF NOT AVAILABLE FFItem THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND FIRST BillItem   where 
                         BillItem.Brand    = FixedFee.Brand AND
                         BillItem.BillCode = FFItem.BillCode 
              no-lock no-error.
              IF AVAIL BillItem THEN prod-name = BIName.
              else prod-name = "!! UNKNOWN !!!".
              RUN local-find-others.
              DISPLAY 
                 FFItem.BillCode
                 lcExtInvNum
                 FFItem.Amt 
                 FFItem.BillPeriod 
                 FFItem.Concerns[1]
                 FFItem.Concerns[2]
                 lcMemoAvail.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(FFItem)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND FFItem where recid(FFItem) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT FFItem
           where FFItem.FFNum = FFNum no-lock no-error.
           IF NOT AVAILABLE FFItem THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND FIRST BillItem   where 
                         BillItem.Brand    = FixedFee.Brand AND
                         BillItem.BillCode  = FFItem.BillCode 
              no-lock no-error.
              IF AVAIL BillItem THEN prod-name = BIName.
              else prod-name = "!! UNKNOWN !!!".
              
              RUN local-find-others.
              DISPLAY 
                 FFItem.BillCode
                 lcExtInvNum
                 FFItem.Amt 
                 FFItem.BillPeriod 
                 FFItem.Concerns[1]
                 FFItem.Concerns[2]
                 lcMemoAvail.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(FFItem).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FFItem where recid(FFItem) = memory no-lock no-error.
        IF order = 1 THEN FIND prev FFItem
        where FFItem.FFNum = FFNum no-lock no-error.
        IF AVAILABLE FFItem THEN DO:
           memory = recid(FFItem).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev FFItem
              where FFItem.FFNum = FFNum no-lock no-error.
              IF AVAILABLE FFItem THEN memory = recid(FFItem).
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
           FIND FFItem where recid(FFItem) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       BillPeriod = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE BillPeriod WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF BillPeriod <> ? THEN DO:
          FIND FIRST FFItem where
                     FFItem.BillPeriod >= BillPeriod AND
                     FFItem.FFNum = FFNum no-lock no-error.
          IF NOT AVAILABLE FFItem THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  FFItem was found */
          ASSIGN order = 1 memory = recid(FFItem) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */


     if lookup(nap,"3,f3") > 0 THEN     /* memo */
     DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:
       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey. RUN Syst/ufcolor.
       FIND FFItem where recid(FFItem) = rtab[frame-line(sel)]
       exclusive-lock.

       DISP FFItem.Memo [1 FOR 5] WITH FRAME memo 1 col.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFFItem).
          UPDATE text(FFItem.Memo [1 FOR 5]) WITH FRAME memo 1 col.

          /* UNDO WITH F4 -key */
          if keylabel(lastkey) = "F4" THEN DO:
             HIDE FRAME memo no-pause.
             UNDO LOOP.
          END.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFFItem).
       END.
       ELSE PAUSE.

       HIDE FRAME memo no-pause.
       PAUSE 0.
     END.
   
     else if lookup(nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP: 
     /* edit */
         /* The program checks, whether this user has an inner password  */
         /* and the variable lcPassword will get it's value.             */
         /* If the password has already been given (lcRight = "RW"), the */
         /* program will not ask it again.                               */
        IF (lcRight EQ "RW") THEN ASSIGN llUpdate = TRUE.
        ELSE DO:   
          lcPassword = fCParamC("MSAddressChg").
          IF lcPassword = ? THEN lcPassword = "". 
          llUpdate = FALSE.
          
          IF lcPassword > "" THEN DO:
             lcAskPassWd = "".
             PAUSE 0.
             UPDATE lcAskPassWd 
               BLANK
               FORMAT "X(20)" 
               LABEL "Password"
               HELP "Password for changing period data"
             WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE PERIOD DATA "
                SIDE-LABELS FRAME fPassword.
             
             llUpdate = (lcAskPassWd = lcPassword). 
             IF (llUpdate EQ TRUE) THEN ASSIGN lcRight = "RW".            
          END.
          ELSE DO:
            ASSIGN llUpdate = TRUE
                   lcRight  = "RW".
          END.
        END.  
        IF llUpdate THEN DO:
           FIND FFItem where recid(FFItem) = rtab[FRAME-LINE] EXCLUSIVE-LOCK.               ehto = 9. 
           RUN Syst/ufkey. 
           ufkey = TRUE.
           UPDATE
               FFItem.BillPeriod
               FFItem.Concerns[1]
               FFItem.Concerns[2]
                 validate(INPUT FRAME sel FFItem.Concerns[1] <= 
                          INPUT Frame sel FFItem.Concerns[2], 
                          "Unlogical dates !")
           WITH FRAME sel. 
           xrecid = recid(FFItem).
           RELEASE FFItem.                  
        END. /* EDITING */
     END.     
     
     else if lookup(nap,"5,f5") > 0 THEN DO:  /* view  */
        FIND FFItem where recid(FFItem) = rtab[FRAME-LINE] no-lock.
        IF FFItem.InvNum > 0 THEN DO:
           ehto = 5.
           RUN Syst/ufkey.p.
           RUN pInvoiceDetails(FFItem.InvNum,
                               TRUE).
           ufkey = true.
        END.
        ELSE DO:
           MESSAGE "NOT INVOICED - press ENTER !"
           VIEW-AS ALERT-BOX
           WARNING.
           NEXT.
        END.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND FFItem where recid(FFItem) = rtab[FRAME-LINE] no-lock.

       IF FFItem.Billed THEN DO:
          MESSAGE "Item is billed, it cannot be deleted."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT.
       END.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          FFItem.BillPeriod
          FFItem.Concerns[1]
          FFItem.Concerns[2]
          FFItem.BillCode   
          lcExtInvNum
          FFItem.Amt       
          lcMemoAvail.

       IF order = 1 THEN FIND NEXT FFItem
       where FFItem.FFNum = FFNum no-lock no-error.
       IF AVAILABLE FFItem THEN memory = recid(FFItem).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND FFItem where recid(FFItem) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev FFItem
          where FFItem.FFNum = FFNum no-lock no-error.
          IF AVAILABLE FFItem THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(FFItem).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND FFItem where recid(FFItem) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          FFItem.BillPeriod
          FFItem.Concerns[1]
          FFItem.Concerns[2]
          FFItem.BillCode   
          lcExtInvNum
          FFItem.Amt       
          lcMemoAvail.


       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFFItem).
           DELETE FFItem.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST FFItem
           where FFItem.FFNum = FFNum) THEN DO:
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
     DO WITH FRAME lis TRANSACTION ON ENDKEY UNDO, NEXT LOOP:
       /* change */
       
       assign fr-header = " CHANGE " cfc = "lis".  RUN Syst/ufcolor.

       run local-find-this(FALSE).

       IF FFItem.Billed THEN DO:
          BELL.
          message "You are not allowed to change a billed item !"
          VIEW-AS ALERT-BOX.
          NEXT.
       END.

       DISPLAY 
         FFItem.BillPeriod
         FFItem.Concerns[1]
         FFItem.Concerns[2]
         FFItem.Amt
         FFItem.OwnCost.
       
       /* The program checks, whether the user has already given the */
       /* correct password, so that it would not ask it again.       */
       IF (lcRight EQ "RW") THEN ASSIGN llUpdate = TRUE. 
       ELSE DO:
         lcPassword = fCParamC("MSAddressChg").
         IF lcPassword = ? THEN lcPassword = "". 
         llUpdate = FALSE.
          
         IF lcPassword > "" THEN DO:
            lcAskPassWd = "".
            PAUSE 0.
            UPDATE lcAskPassWd 
               BLANK
               FORMAT "X(20)" 
               LABEL "Password"
               HELP "Password for changing period data"
               WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE PERIOD DATA "
                    SIDE-LABELS FRAME fPassword.
             
            llUpdate = (lcAskPassWd = lcPassword).
            IF (llUpdate EQ TRUE) THEN ASSIGN lcRight = "RW".
         END.
         ELSE DO:
           ASSIGN llUpdate = TRUE
                  lcRight  = "RW".
         END.
       END.
            
       IF llUpdate THEN DO WITH FRAME lis ON ENDKEY UNDO, NEXT LOOP:
          run local-find-this(TRUE).
          ehto = 9. 
          RUN Syst/ufkey. 
          ufkey = TRUE.
          UPDATE
             FFItem.BillPeriod
             FFItem.Concerns[1]
             FFItem.Concerns[2]
               validate(INPUT FRAME sel FFItem.Concerns[1] <= 
                        INPUT Frame sel FFItem.Concerns[2], 
                        "Unlogical dates !")
             FFItem.Amt
             FFItem.OwnCost
          WITH FRAME lis. 
          xrecid = recid(FFItem).
          RELEASE FFItem.        
          HIDE FRAME lis NO-PAUSE.
       END. /* EDITING */
       ELSE DO:
         ehto = 9. 
         RUN Syst/ufkey. 
         ufkey = TRUE.
         /* if item is the last one then allow update of end period */  
         llLast = NOT CAN-FIND(FIRST bFFItem WHERE
                                     bFFItem.FFNum = FFItem.FFNum AND
                                     bFFItem.BillPer > FFItem.BillPer).
         /* end period of fee must already be set */
         IF llLast THEN 
         llLast = (FixedFee.EndPeriod <= FFItem.BillPer). 
          
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFFItem).
         UPDATE
            FFItem.Concerns[2] WHEN llLast
            VALIDATE(INPUT FFItem.Concerns[2] >= FFItem.Concerns[1],
                     "End period must be atleast equal to begin period")
         WITH FRAME lis.            
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFFItem).
         xrecid = recid(FFItem).
         HIDE FRAME lis NO-PAUSE.
       END.
     END.
     
     
     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST FFItem
       where FFItem.FFNum = FFNum no-lock no-error.
       ASSIGN memory = recid(FFItem) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST FFItem
       where FFItem.FFNum = FFNum no-lock no-error.
       ASSIGN memory = recid(FFItem) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FFItem WHERE recid(FFItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FFItem WHERE recid(FFItem) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-others:
   
   DEF VAR liLoop AS INTEGER NO-UNDO.
   
   lcMemoAvail = "".
   
   DO liLoop = 1 TO 5:
      IF FFItem.Memo[liLoop] > "" THEN lcMemoAvail = "M".
   END.
 
   FIND Invoice NO-LOCK WHERE
        Invoice.Brand  = gcBrand AND
        Invoice.InvNum = FFItem.InvNum
   NO-ERROR.
   IF AVAIL Invoice THEN lcExtInvNum = Invoice.ExtInvId.
   ELSE lcExtInvNum = "".

END PROCEDURE.


