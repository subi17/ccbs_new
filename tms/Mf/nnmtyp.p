/* ------------------------------------------------
  MODULE .......: NNMTYP.P
  FUNCTION .....: Maintain monthly call counters
  APPLICATION ..: nn
  AUTHOR .......: kal
  CREATED ......: 28-11-97
  MODIFIED .....: 11.06.98 kl
                  04.11.02 jr Eventlog
                  07.03.03 tk tokens
                              action for f7 disabled, there was no keylabel
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MthCall'}

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF STREAM whitelist.

DEF VAR seek-cust-nr LIKE MthCall.CustNum   NO-UNDO.
DEF VAR seek-mth     LIKE MthCall.Month     NO-UNDO.
DEF VAR CustNum      LIKE MthCall.CustNum   NO-UNDO.
DEF VAR cust-name    LIKE Customer.CustName NO-UNDO.
DEF VAR Month        LIKE MthCall.Month     NO-UNDO.
DEF VAR Limit        LIKE MthCall.Limit     NO-UNDO.
DEF VAR CloseDate    LIKE MthCall.CloseDate NO-UNDO.
DEF VAR Called       LIKE MthCall.Called    NO-UNDO.
DEF VAR xrecid       AS RECID                        init ?.
DEF VAR firstline    AS INT                 NO-UNDO  init 0.
DEF VAR order        AS INT                 NO-UNDO  init 1.
DEF VAR ordercount   AS INT                 NO-UNDO  init 3.
DEF VAR ufkey        AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline      AS INT                 NO-UNDO  init 0.
DEF VAR ex-order     AS INT                 NO-UNDO.
DEF VAR memory       AS RECID               NO-UNDO.
def var line         as int format "99"     NO-UNDO.
DEF VAR must-print   AS LOG                 NO-UNDO.
DEF VAR must-add     AS LOG                 NO-UNDO.
DEF VAR fr-header    AS CHAR                NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24     NO-UNDO.
DEF VAR i            AS INT                 NO-UNDO.
def var ok           as log format "Yes/No" NO-UNDO.
def var whitelist    as c   format "x(40)"  NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhClosedCust AS HANDLE NO-UNDO.
   lhClosedCust = BUFFER ClosedCust:HANDLE.
   RUN StarEventInitialize(lhClosedCust).

   DEFINE VARIABLE lhMthCall AS HANDLE NO-UNDO.
   lhMthCall = BUFFER MthCall:HANDLE.
   RUN StarEventInitialize(lhMthCall).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhClosedCust).
   END.
END.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   whitelist = TMSUser.RepDir + "/".
END.

form
   MthCall.CustNum      /*  label "Cust.nr"       */
   cust-name            /*  column-label "Name"   */     format "x(21)"
   MthCall.Month        /*  column-label "Month"  */     format "999999"
   MthCall.Called       /*  column-label "Called" */     format ">,>>>,>>>.99"
   MthCall.Limit        /*  column-label "Limit"  */
   MthCall.CloseDate    /*  column-label "Closed" */     format "99-99-99"
   MthCall.Printed      /*  column-label "Pr."    */     format "*/"
   MthCall.CloseType    /*  column-label "V"      */
WITH
   width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain monthly call counters "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
    "Customer number ..:" MthCall.CustNum
       help "Existing customer number"  cust-name  format "x(25)" SKIP
    "Month ............:" MthCall.Month
       help "Month of limitation (YYYYMM)"                        SKIP
    "Called so far ....:" MthCall.Called       
       help "Value of Calls so far"                               SKIP
    "Mothly Limit .....:" MthCall.Limit
       help "Limit of calls"                                      SKIP
    "Closing Date .....:" MthCall.CloseDate
       help "Date when subscription was closed"                   SKIP
    "Closing value ....:" MthCall.CloseType
    "(1 = Limit 2 = Invoice 3 = Both)"
    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH NO-LABEL
    FRAME lis.

form /*  search WITH FIELD CustNum */
    seek-cust-nr
    help "Give customer number"
    with row 4 col 2 title color value(ctc) " FIND CUSTOMER number "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD Month */
    seek-mth
    help "Give month (YYMM)"
    with row 4 col 2 title color value(ctc) " FIND MONTH "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form /* UPDATE whitelist FileName */
    skip(1)
    "   Date:" CloseDate 
       help "Customers closed on this Date (empty for all)" 
    SKIP
    "   File:" whitelist
       help "Filename of the whitelist file"
    skip(1)
    WITH ROW 8 centered width 55
    title color value(cfc) " ADD CLOSED NUMBERS TO A File "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME wl.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MthCall
/* search condition */ no-lock no-error.
IF AVAILABLE MthCall THEN ASSIGN
   memory       = recid(MthCall)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No call counters available !" VIEW-AS ALERT-BOX.
   RETURN.

END.

/*
ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = TRUE.
*/

ASSIGN
   Month = year(pvm) * 100 + month(pvm).

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by customer number ".
       if order = 2 then put screen row 19 col 30 " Order by month           ".
       if order = 3 then put screen row 19 col 30 " Order by closing Date    ".
    END.

/* ADDING DISABLED !!
   IF must-add THEN DO:  /* MthCall -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new:
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        ASSIGN CustNum = 0 Month = 0 Limit = 0 Called = 0 CloseDate = ?.
        UPDATE CustNum Month Limit Called CloseDate WITH FRAME lis EDITING:
           READKEY.
           nap = keylabel(LASTKEY).
           IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
              if frame-field = "cust-nr" THEN DO:
                 ASSIGN CustNum.
                 IF CustNum = 0 THEN LEAVE add-new.
                 FIND Customer where Customer.CustNum = CustNum no-lock no-error.
                 IF NOT AVAIL Customer THEN DO:
                    message "Customer does not exist !".
                    NEXT-PROMPT CustNum.
                    NEXT.
                 END.
                 DISP as-limi[2] @ Limit.
                 cust-name = Customer.CustName.
                 DISP cust-name WITH FRAME lis.
              END. /* CustNum */
              if frame-field = "mth" THEN DO:
                 ASSIGN Month.
                 IF Month = 0 THEN DO:
                    NEXT-PROMPT CustNum.
                    NEXT.
                 END.
                 FIND MthCall where
                    MthCall.CustNum = CustNum AND
                    MthCall.Month     = Month     no-lock no-error.
                 IF AVAIL MthCall THEN DO:
                    message "Customer - month exists already !".
                    NEXT-PROMPT Month.
                    NEXT.
                 END.
              END. /* Month */
              if frame-field = "limit" THEN DO:
                 ASSIGN Limit.
                 IF Limit = 0 THEN DO:
                    NEXT-PROMPT Month.
                    NEXT.
                 END.
              END. /* Limit */
           END.
           APPLY LASTKEY.
        END.
        ASSIGN
           memory = recid(MthCall)
           xrecid = memory.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST MthCall
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE MthCall THEN LEAVE LOOP.
      NEXT LOOP.
   END.
*/
print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MthCall where recid(MthCall) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MthCall THEN DO:
              FIND Customer where
                   Customer.CustNum = MthCall.CustNum no-lock no-error.
              IF AVAIL Customer THEN cust-name = CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY MthCall.CustNum cust-name MthCall.Month
                 MthCall.Called MthCall.Limit MthCall.CloseDate
                 MthCall.Printed MthCall.CloseType.
              rtab[FRAME-LINE] = recid(MthCall).
              IF order = 1 THEN FIND NEXT MthCall
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
              /* search condition */ no-lock no-error.
              ELSE IF order = 3 THEN FIND NEXT MthCall USE-INDEX closed
              /* search condition */ no-lock no-error.
         /*   ELSE IF order = 4 THEN FIND NEXT MthCall USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
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
        ufk[1]= 35  ufk[2]= 910 
        ufk[3]= (IF lcRight = "RW" THEN 913 ELSE 0) 
        ufk[4]= (IF lcRight = "RW" THEN 909 ELSE 0)
        ufk[5]= (IF lcRight = "RW" THEN 903 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4   ELSE 0)
        ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MthCall.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MthCall.Month ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.Month WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW MthCall.CloseDate ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.CloseDate WITH FRAME sel.
      END.
/*    ELSE IF order = 4 THEN DO:
        CHOOSE ROW MthCall.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.? WITH FRAME sel.
      END.
*/
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
        FIND MthCall where recid(MthCall) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MthCall
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND prev MthCall USE-INDEX closed
           /* search condition */ no-lock no-error.
       /*  ELSE IF order = 4 THEN FIND prev MthCall USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MthCall THEN
              ASSIGN firstline = i memory = recid(MthCall).
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
           FIND MthCall where recid(MthCall) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MthCall
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND prev MthCall USE-INDEX closed
           /* search condition */ no-lock no-error.
       /*  ELSE IF order = 4 THEN FIND prev MthCall USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MthCall THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND Customer where
                   Customer.CustNum = MthCall.CustNum no-lock no-error.
              IF AVAIL Customer THEN cust-name = CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY MthCall.CustNum cust-name MthCall.Month
                      MthCall.Called MthCall.Limit MthCall.CloseDate
                      MthCall.Printed MthCall.CloseType.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MthCall)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MthCall where recid(MthCall) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MthCall
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND NEXT MthCall USE-INDEX closed
           /* search condition */ no-lock no-error.
       /*  ELSE IF order = 4 THEN FIND NEXT MthCall USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MthCall THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND Customer where
                   Customer.CustNum = MthCall.CustNum no-lock no-error.
              IF AVAIL Customer THEN cust-name = CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY MthCall.CustNum cust-name MthCall.Month
                      MthCall.Called MthCall.Limit MthCall.CloseDate
                      MthCall.Printed MthCall.CloseType.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MthCall).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MthCall where recid(MthCall) = memory no-lock no-error.
        IF order = 1 THEN FIND prev MthCall
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
        /* search condition */ no-lock no-error.
        ELSE IF order = 3 THEN FIND prev MthCall USE-INDEX closed
        /* search condition */ no-lock no-error.
   /*   ELSE IF order = 4 THEN FIND prev MthCall USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MthCall THEN DO:
           memory = recid(MthCall).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MthCall
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
              /* search condition */ no-lock no-error.
              ELSE IF order = 3 THEN FIND prev MthCall USE-INDEX closed
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 4 THEN FIND prev MthCall USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MthCall THEN memory = recid(MthCall).
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
           FIND MthCall where recid(MthCall) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       seek-cust-nr = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE seek-cust-nr WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF seek-cust-nr <> ? THEN DO:
          FIND FIRST MthCall where MthCall.CustNum >= seek-cust-nr
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MthCall THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  mthcall/cust-nr was found */
          ASSIGN order = 1 memory = recid(MthCall) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       seek-mth = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE seek-mth WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       IF seek-mth <> ? THEN DO:
          FIND FIRST MthCall where MthCall.Month >= seek-mth
          USE-INDEX Month /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MthCall THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  mthcall/mth was found */
          ASSIGN order = 2 memory = recid(MthCall) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"3,f3") > 0 AND lcRight = "RW" THEN DO TRANSAction:
        FIND MthCall where recid(MthCall) = rtab[FRAME-LINE]
           exclusive-lock.
        /* can't change IF already Printed */
        IF MthCall.Printed THEN DO:
           BELL.
           message "Customer is already Printed !".
           PAUSE 2 no-message.
           NEXT LOOP.
        END.
        ok = FALSE.
        message "ARE YOU SURE YOU WANT TO CHANGE SELECTED ROW ?" UPDATE ok.
        IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMthCall).
           IF MthCall.CloseDate = ? THEN
              ASSIGN MthCall.CloseDate  = TODAY
                     MthCall.CloseType = MthCall.CloseType + 1.
           ELSE IF MthCall.Printed = FALSE THEN
              ASSIGN MthCall.CloseDate  = ?
                     MthCall.CloseType = MthCall.CloseType - 1.
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMthCall).          
           DISPLAY MthCall.CloseDate MthCall.CloseType.
        END.
        NEXT LOOP.
     END.

     /* change ALL FOR active MONTH */
     if lookup(nap,"4,f4") > 0 AND lcRight = "RW" THEN DO:
        FIND MthCall where recid(MthCall) = rtab[FRAME-LINE] no-lock.
        RUN Mf/nnmtyp2(MthCall.Month).
        ASSIGN must-print = TRUE ufkey = TRUE.
        NEXT LOOP.
     END.

     /* append numbers TO monthly File where Printed is FALSE */
     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP: 
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        whitelist = whitelist + 
                    "wlc-" + substr(string(Month,"999999"),3) + ".dat".

        message "Finding customers with closing Date defined ...".

        FIND LAST MthCall where 
                  MthCall.CloseDate  NE ? AND
                  MthCall.Printed = FALSE
        USE-INDEX closed no-lock no-error.
        IF AVAIL MthCall THEN CloseDate = MthCall.CloseDate.
        ELSE CloseDate = ?.

        PAUSE 0.

        UPDATE CloseDate whitelist WITH FRAME wl.
        if whitelist = "" THEN DO:
           HIDE FRAME wl no-pause.
           NEXT LOOP.
        END.
        OUTPUT STREAM whitelist TO value(whitelist) append.

        FOR EACH MthCall        where
                 MthCall.Printed = FALSE AND
                (IF CloseDate NE ? THEN 
                      MthCall.CloseDate = CloseDate
                 ELSE MthCall.CloseDate NE ?)
            USE-INDEX closed,

            EACH CLI no-lock where
                 CLI.CustNum  = MthCall.CustNum

        BREAK
           BY MthCall.CustNum:

              PUT STREAM whitelist UNFORMATTED
                 substring(CLI.CLI,2) SKIP.

              IF last-of(MthCall.CustNum) THEN DO:
                 ASSIGN MthCall.Printed = TRUE.
                 /* customer is added TO ClosedCust -file only
                    IF it's NOT there already AS open */
                 FIND FIRST ClosedCust  where
                            ClosedCust.CustNum = MthCall.CustNum AND
                           (ClosedCust.State   = TRUE            OR
                            ClosedCust.Printed = FALSE) no-lock no-error.

                 IF NOT AVAIL ClosedCust THEN DO:
                    CREATE ClosedCust.
                    ASSIGN
                       ClosedCust.CustNum = MthCall.CustNum
                       ClosedCust.Date  = MthCall.CloseDate
                       ClosedCust.Called = MthCall.CloseType.
                    IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMthCall).                   END.
              END. /* last-of */

        END. /* FOR EACH */

        OUTPUT STREAM whitelist CLOSE.
        ASSIGN must-print = TRUE.
        HIDE FRAME wl no-pause.
        NEXT LOOP.

     END. /* f5 */

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND MthCall where recid(MthCall) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          MthCall.CustNum MthCall.Month MthCall.Called MthCall.Limit
          MthCall.CloseDate MthCall.CloseType.

       IF order = 1 THEN FIND NEXT MthCall
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
       /* search condition */ no-lock no-error.
       ELSE IF order = 3 THEN FIND NEXT MthCall USE-INDEX closed
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 4 THEN FIND NEXT MthCall USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE MthCall THEN memory = recid(MthCall).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MthCall where recid(MthCall) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MthCall
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
          /* search condition */ no-lock no-error.
          ELSE IF order = 3 THEN FIND prev MthCall USE-INDEX closed
          /* search condition */ no-lock no-error.
     /*   ELSE IF order = 4 THEN FIND prev MthCall USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE MthCall THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(MthCall).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MthCall where recid(MthCall) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          MthCall.CustNum MthCall.Month MthCall.Called MthCall.Limit
          MthCall.CloseDate MthCall.CloseType.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMthCall).
           DELETE MthCall.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MthCall
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

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       HIDE FRAME lis.
       FIND MthCall where recid(MthCall) = rtab[frame-line(sel)]
       exclusive-lock.
       IF MthCall.Printed = FALSE THEN DO:
          assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
          RUN Syst/ufkey.
          cfc = "lis". RUN Syst/ufcolor.
          FIND Customer where
               Customer.CustNum = MthCall.CustNum no-lock no-error.
          IF AVAIL Customer THEN cust-name = CustName.
          else cust-name = "!! UNKNOWN !!".
          DISPLAY MthCall.CustNum cust-name MthCall.Month
                  MthCall.Called MthCall.CloseDate MthCall.CloseType.
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMthCall).
          UPDATE MthCall.Limit.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMthCall).
          HIDE FRAME lis no-pause.
          DISPLAY MthCall.Limit MthCall.CloseDate MthCall.CloseType
          WITH FRAME sel.
          xrecid = recid(MthCall).
       END.
       /* prevent CloseDate modifying afterwards, because it's in ClosedCust -file */
       ELSE DO:
          BELL.
          message "You can't change Printed rows !".
          PAUSE 2 no-message.
       END.
     END.
/*
     /* change ALL FOR active MONTH */
     if lookup(nap,"7,f7") > 0 THEN DO:
        RUN Mc/nnigcl(Month).
        ASSIGN must-print = TRUE ufkey = TRUE.
        NEXT LOOP.
     END.
*/
     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MthCall
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MthCall USE-INDEX Month
       /* search condition */ no-lock no-error.
       ELSE IF order = 3 THEN FIND FIRST MthCall USE-INDEX closed
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 4 THEN FIND FIRST MthCall USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MthCall) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MthCall
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MthCall USE-INDEX Month
       /* search condition */ no-lock no-error.
       ELSE IF order = 3 THEN FIND LAST MthCall USE-INDEX closed
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 4 THEN FIND LAST MthCall USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MthCall) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

