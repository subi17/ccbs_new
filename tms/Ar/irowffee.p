/* ---------------------------------------------------------------------------
  MODULE .......: IROWFFEE.P
  FUNCTION .....: Browse Billable items of an contract
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 12-02-02
  MODIFIED .....: 11-11-02 jr Eventlog
                  11-11-02 jr Removed unused find's
                  14.02.03/aam don't change memo
                  05.03.03 aam customer is needed for title 
                  09.06.03 aam calcobj removed
                  15.09.03 aam brand
                  28.10.03 jp  disp with frame sel??
                  19.03.04 aam input CLI, use temp-table,
                               renamed nnlrcoit -> irowffee
                  30.03.04 kl  fixes after index changes
                  16.08.05 aam CLIType
  Version ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER  iiInvNum    AS I    NO-UNDO.
DEF INPUT PARAMETER  icBillCode  AS c    NO-UNDO.
DEF INPUT PARAMETER  icCLI       AS CHAR NO-UNDO. 

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
DEF VAR lcCLIType      AS CHAR                NO-UNDO. 


DEF TEMP-TABLE ttRow NO-UNDO
   FIELD FFItem  AS INT
   FIELD BillPer AS INT
   FIELD CLIType AS CHAR
   INDEX BillPer BillPer.


IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

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
    FFItem.Concerns[1] column-label "Fr.Per." format "99999999"
    FFItem.Concerns[2] column-label "To Per." format "99999999"
    ttRow.CLIType      COLUMN-LABEL "CLIType" FORMAT "X(8)"
    FFItem.Amt         column-label "Amount"
    FFItem.Memo[1]     column-label "Memo"    format "x(30)"
WITH
    centered ROW 2 OVERLAY scroll 1 13 DOWN
    color value(cfc) title color value(ctc) " " +
    substr(Customer.CustName,1,18) + " / " + icBillCode  +
    ": Invoice = " + string(iiInvNum) + ", CustNo = " +
    string(Customer.CustNum) + " "  FRAME sel.

form
    "CustomerNo ......:" FFItem.CustNum                          SKIP
    "ProdCode ........:" FFItem.BillCode                        SKIP 
    "Period ..........:" FFItem.BillPeriod                        SKIP
    "Billable payment.:" FFItem.Amt                          SKIP
    "Our own cost ....:" FFItem.OwnCost                         SKIP
    "Invoiced ........:"  FFItem.InvNum                            SKIP
    "------------------"                                         SKIP
    FFItem.Memo[1]                                            SKIP
    FFItem.Memo[2]                                            SKIP

 WITH  OVERLAY ROW 5 centered
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
    color value(cfc) title color value(cfc) " Memo for invoice "
    FRAME memo.


FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.   
FIND Customer OF Invoice NO-LOCK no-error.

FOR EACH FFItem NO-LOCK WHERE
         FFItem.InvNum   = iiInvNum  AND
         FFItem.BillCode = icBillCode:
         
   ASSIGN ok        = FALSE       
          lcCLIType = "".
      
   FOR FIRST FixedFee OF FFItem NO-LOCK WHERE
             FixedFee.HostTable = "MobSub":
             
      FOR EACH MSOwner NO-LOCK WHERE
               MsOwner.MSSeq   = INTEGER(FixedFee.KeyValue) AND
               MsOwner.TsBeg  <= FFItem.Concerns[2]         AND
               MsOwner.TsEnd  >= FFItem.Concerns[1]
      BY MsOwner.TsEnd DESC:         
         
         IF icCli = "" OR MsOwner.CLI = icCLI 
         THEN DO:
            ASSIGN ok        = TRUE
                   lcCLIType = MsOwner.CLIType.
            LEAVE.
         END.
      END.
   END.
      
   IF (icCLI > "" AND NOT ok) OR
      (icCLI = "" AND ok)
   THEN NEXT. 
 
   CREATE ttRow.
   ASSIGN ttRow.FFItem  = RECID(FFItem)
          ttRow.BillPer = FFItem.BillPeriod
          ttRow.CLIType = lcCLIType.
END.

FIND FIRST ttRow NO-ERROR. 

IF AVAILABLE ttRow THEN ASSIGN
   memory     = recid(ttRow)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   BELL. MESSAGE
   "ERROR: NO Billable items at all  - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
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
        FIND ttRow where recid(ttRow) = memory    no-lock no-error.
        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE ttRow THEN DO:
              RUN local-disp-row.

              rtab[FRAME-LINE] = recid(ttRow).
              IF order = 1 THEN FIND NEXT ttRow no-error.
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
        ufk[5]= 0   ufk[6]= 0  ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto  = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW FFItem.BillPeriod {Syst/uchoose.i} no-error WITH FRAME sel.
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
        FIND ttRow where recid(ttRow) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev ttRow no-error.
           IF AVAILABLE ttRow THEN
              ASSIGN firstline = i memory = recid(ttRow).
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
           FIND ttRow where recid(ttRow) = rtab[1].
           IF order = 1 THEN FIND prev ttRow no-error.
           
           IF NOT AVAILABLE ttRow THEN DO:
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
              rtab[1] = recid(ttRow)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND ttRow where recid(ttRow) = rtab[FRAME-DOWN] .
           IF order = 1 THEN FIND NEXT ttRow no-error.
           IF NOT AVAILABLE ttRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttRow).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttRow where recid(ttRow) = memory no-lock no-error.
        IF order = 1 THEN FIND prev ttRow no-error.

        IF AVAILABLE ttRow THEN DO:
           memory = recid(ttRow).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev ttRow no-error.
              IF AVAILABLE ttRow THEN memory = recid(ttRow).
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
           FIND ttRow where recid(ttRow) = memory.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       BillPeriod = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE BillPeriod WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF BillPeriod <> ? THEN DO:
          FIND FIRST ttRow WHERE 
                     ttRow.BillPer >= BillPeriod NO-ERROR. 
          IF NOT AVAILABLE ttRow THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  ttRow was found */
          ASSIGN order = 1 memory = recid(ttRow) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */


     if lookup(nap,"3,f3") > 0 THEN     /* memo */
     DO  WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:
       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey.p. RUN Syst/ufcolor.p.
       FIND ttRow where recid(ttRow) = rtab[frame-line(sel)].
       FIND FFItem WHERE RECID(FFItem) = ttRow.FFItem NO-LOCK.

       DISPLAY FFItem.Memo [1 FOR 5] WITH FRAME memo 1 col.

       PAUSE MESSAGE "Press ENTER to continue". 

       HIDE MESSAGE NO-PAUSE.
       HIDE FRAME memo no-pause.
     END.

     else if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION ON ENDKEY UNDO, NEXT LOOP:
       /* change */

       assign fr-header = " VIEW " cfc = "lis".  RUN Syst/ufcolor.p.

       FIND ttRow where recid(ttRow) = rtab[frame-line(sel)].
       FIND FFItem WHERE RECID(FFItem) = ttRow.FFItem NO-LOCK.

       DISPLAY 
       FFItem.BillCode
       FFItem.CustNum
       FFItem.BillPeriod
       FFItem.Amt
       FFItem.OwnCost
       FFItem.InvNum
       FFItem.Amt  
       FFItem.Memo[1] 
       FFItem.Memo[2]
       WITH FRAME lis.
       xrecid = recid(ttRow).

       PAUSE 0.
       MESSAGE "PRESS ENTER TO CONTINUE !".
       PAUSE NO-MESSAGE.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST ttRow NO-ERROR.
       ASSIGN memory = recid(ttRow) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST ttRow NO-ERROR. 
       ASSIGN memory = recid(ttRow) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND FFItem WHERE RECID(FFItem) = ttRow.FFItem NO-LOCK.

   FIND BillItem where 
        BillItem.Brand = gcBrand AND
        BillItem.BillCode = FFItem.BillCode no-lock no-error.
   IF AVAIL BillItem THEN prod-name = BIName.
   else prod-name = "!! UNKNOWN !!!".

   FIND FIRST FixedFee where FixedFee.FFNum = FFItem.FFNum no-lock.

   DISPLAY FFItem.Memo[1]
           FFItem.Amt 
           FFItem.BillPeriod 
           ttRow.CLIType
           FFItem.Concerns[1] FORMAT "99999999"
           FFItem.Concerns[2] FORMAT "99999999"
           FFItem.Memo[1] WITH FRAME sel.

END PROCEDURE.

