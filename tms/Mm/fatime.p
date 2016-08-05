/* ----------------------------------------------------------------------
  MODULE .......: FATime.P
  TASK .........: UPDATE Billable Items
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 02-09-99
  CHANGED ......: 30.11.1999 jpo: DISPLAY fields (local-UPDATE-records)
                  31.10.2002  jpo: fatclass
                  05.11.2002 jpo: interval, NOT READY YET
                  05.11.2002 jr  Eventlog
                  22.01.2003 jp  ffitemnum
                  17.06.2003 aam FATType,
                                 input parameters icFatGrp, iiCustNum & icCLI
                                 (FATime1.p, custFATime.p & mobFATime.p
                                  removed) 
                  16.03.2004 aam "Def"-CLI                
                  01.06.2004 aam check msowner if mobsub not found
                  06.06.2005 aam memo to f4, 
                                 invoice text to f3
                  15.12.2005 aam username removed, format changes
                  02.01.2006 aam make an adv.payment when FATimes of certain
                                 groups are removed
                  27.04.2006 aam FatPerc, VatIncl,
                                 defaults for new FATime from FATGroup         
                  16.05.2006 aam show eventlog (F7)
                                 note: shows all events on custnum/cli/type 
                                 -level (invnum cannot be used)
                  19.04.2007 aam show ExtInvID               
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable FATime

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Fatime'}
{Syst/tmsconst.i}

DEF INPUT PARAMETER  icFatGrp    AS CHAR NO-UNDO.
DEF INPUT PARAMETER  iiCustNum   AS INT  NO-UNDO.
DEF INPUT PARAMETER  icCLI       AS CHAR NO-UNDO. 
DEF INPUT PARAMETER  iiMsSeq     AS INT  NO-UNDO. 

DEF new shared VAR siirto AS CHAR.
DEF VAR CustName   as C FORMAT "x(35)" NO-UNDO.

DEF VAR MsSeq      LIKE FATime.MsSeq  NO-UNDO.
    DEF VAR Period LIKE FATime.Period NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR rc           AS INT                    NO-UNDO.
DEF VAR payername    AS C                      NO-UNDO.
DEF VAR unitname     AS C                      NO-UNDO. 
DEF VAR unitnames    AS C                      NO-UNDO. 
DEF VAR CLI          like   mobsub.cli           NO-UNDO.

DEF VAR custnum       LIKE mobsub.CustNum       NO-UNDO.
DEF VAR loop          AS I                      No-UNDO.
DEF VAR qty           AS i                      NO-undo.
DEF VAR invoiced      AS LOG                    No-UNDO.
DEF VAR lcFatType     AS CHAR                   NO-UNDO.
DEF VAR liVoucher     AS INT                    NO-UNDO. 
DEF VAR liCrePeriod   AS INT                    NO-UNDO.
DEF VAR liNextPer     AS INT                    NO-UNDO.
DEF VAR lcQtyUnit     AS CHAR                   NO-UNDO.
DEF VAR lcExtInvID    AS CHAR                   NO-UNDO.
DEF VAR lcIntInvNum   AS CHAR                   NO-UNDO. 
DEF VAR lcEventKey    AS CHAR                   NO-UNDO. 
DEF VAR liTransPeriod AS INT                    NO-UNDO.
 
DEF BUFFER xxFATime FOR FATime.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFATime AS HANDLE NO-UNDO.
   lhFATime = BUFFER FATime:HANDLE.
   RUN StarEventInitialize(lhFATime).

   DEFINE VARIABLE lhxxFATime AS HANDLE NO-UNDO.
   lhxxFATime = BUFFER xxFATime:HANDLE.
   RUN StarEventInitialize(lhxxFATime).


   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhFATime).
   END.
END.

unitnames     = "Not Invoicing, Unit price, Total Price".

form
    FATime.CustNum
    CustName            FORMAT "x(10)"   COLUMN-LABEL "Name"
    FATime.CLI          FORMAT "X(12)"   COLUMN-LABEL "MSISDN"
    liTransPeriod       FORMAT "999999"  COLUMN-LABEL "Period"
    FATime.FTGrp        COLUMN-LABEL "FATGroup"
    FATime.Amt             FORMAT "->>>9.99" COLUMN-LABEL "Amount"
    FATime.FatPerc         FORMAT ">>9.99"   COLUMN-LABEL "Perc."
    FATime.Used            FORMAT "->>>9.99"
    FATime.Transfer        format "T/" column-label "T" SPACE(0)
    FATime.QtyUnit         format "x(1)" column-label "U"  SPACE(0)
    Invoiced               format "I/" column-label "I"  SPACE(0)

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " FATime "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    FATime.CLI 
       LABEL "MSISDN ........"
       HELP  "CLI number    "      SKIP
    FATime.Custnum FORMAT ">>>>>>>9"
       LABEL "Customer ......"
       CustName NO-LABEL  AT 36 SKIP
    FATime.FTGrp FORMAT "X(16)" 
       LABEL "FATime Group .."
       Validate(input FATime.ftgrp ne "","Input FATime Group")    
       FATGroup.FtgName NO-LABEL  AT 36   SKIP
    FATime.Period   
       LABEL "Period ........" SKIP
    FATime.FATType 
       LABEL "FATime Type ..."
       lcFATType AT 27 NO-LABEL FORMAT "X(20)"  SKIP
    FATime.QtyUnit        
       LABEL "Quantity Unit ."
       lcQtyUnit AT 27 NO-LABEL FORMAT "X(20)" SKIP
    FATime.VatIncl
       LABEL "VAT ..........." SKIP
    FATime.Amt 
       LABEL "Amount ........" SKIP
    FATime.FATPerc      
       LABEL "Percentage ...." SKIP
    FATime.used                                     
       LABEL "Used  ........." SKIP
    FATime.Transfer   
       LABEL "Transferrable ." SKIP
    FATime.TransQty 
       LABEL "Transferred ..." SKIP
    FATime.TransPeriod 
       LABEL "Transf.Period.." SKIP
    FATime.LastPeriod
       LABEL "Last Period ..." 
       FORMAT ">>>>>>" SKIP
    FATime.Memo[1]    
       LABEL "Text To Invoice" SKIP
    FATime.Memo[2]  AT 18 NO-LABEL SKIP
    lcExtInvID 
       LABEL "Billed on Inv. "
       FORMAT "X(12)"
    lcIntInvNum NO-LABEL FORMAT "X(12)"
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek Billable item  BY  CLI */
    Custnum
    HELP "Enter Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER. NO "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* seek Billable item  BY  CLI */
    CLI
    HELP "Enter CLI No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI. NO "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Billable item  BY Period */
    period
    HELP "Enter Billing Period"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Period "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


form
    FATime.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Invoice Text " WITH NO-LABELS 1 columns
    FRAME f4.

FUNCTION fDispFATType RETURNS LOGICAL.

   lcFATType = "".
   IF FATime.FATType <= 3 THEN 
   lcFATType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "FATGroup",
                                "FATType",
                                STRING(FATime.FATType)).

   DISPLAY FATime.FATType lcFATType WITH FRAME lis.

END FUNCTION.

FUNCTION fDispQtyUnit RETURNS LOGICAL.

   lcQtyUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "FatGroup",
                                 "QtyUnit",
                                 FATime.QtyUnit).

   DISPLAY lcQtyUnit WITH FRAME lis.

END FUNCTION.


FUNCTION fDefaults RETURNS LOGICAL:

   ASSIGN FATime.FatType  = FatGroup.FatType
          FATime.Transfer = FatGroup.Transfer
          FATime.Amt      = FatGroup.Amount
          FATime.FatPerc  = FatGroup.FatPerc
          FATime.QtyUnit  = FatGroup.QtyUnit
          FATime.Interval = FatGroup.Interval.
                     
   DO i = 1 TO 5:
      FATime.Memo[i] = FatGroup.InvMemo[i].
   END.   

   DISPLAY FATime.FatType
           FATime.Transfer
           FATime.Amt
           FATime.FatPerc
           FATime.QtyUnit
           FATime.Memo[1 for 2] 
   WITH FRAME lis.
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By CustNo  ,   By MSISDN ,  By period  , By 4".

IF icFatGrp > "" THEN DO:
   FIND FIRST FATime WHERE 
              FATime.BRand = gcBrand AND 
              FATime.FtGrp = icFatGrp
   NO-LOCK NO-ERROR.
   MaxOrder = 1.
END.
ELSE IF iiCustNum > 0 THEN DO:
   FIND FIRST FATime WHERE 
             FATime.Brand   = gcBrand AND 
             FATime.CustNum = iiCustNum 
   NO-LOCK NO-ERROR.
   MaxOrder = 1.
END.
ELSE IF icCLI > "" THEN DO:
   FIND FIRST FATime WHERE 
              FATime.Brand = gcBrand AND 
              FATime.CLI = icCLi AND
              FATime.MsSeq = iiMsSeq
   NO-LOCK NO-ERROR.
   MaxOrder = 1.
END.

ELSE FIND FIRST FATime  use-index InvNum
WHERE FATime.Brand = gcBrand NO-LOCK NO-ERROR.


IF AVAILABLE FATime THEN ASSIGN
   Memory       = recid(FATime)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO TRANS:  /* Add a FATime  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
           CLEAR FRAME lis NO-PAUSE.

           CREATE FATime.
           ASSIGN
           FATime.FatNum  = next-value(ftseq)
           FATime.fatid   = NEXT-value(ft-seq)
           FATime.period  = year(today) * 100 + month(today)
           FATime.QtyUnit = "Amt"
           FATime.FtGrp   = icFatGrp
           FATime.CustNum = iiCustNum
           FATime.Brand   = gcBrand 
           FATime.CLI     = icCLi
           FATime.MsSeq   = iiMsSeq
           FATime.FatType = 0.

           /* defaults from group */ 
           IF FATime.FtGrp > "" THEN DO:
              FIND FatGroup WHERE 
                   FatGroup.Brand = gcBrand AND
                   FatGroup.FtGrp = FATime.FtGrp NO-LOCK.
              fDefaults().
           END.
           
           IF icCLI > "" AND icCLI NE "DEF" THEN DO:
              FIND Mobsub WHERE MobSub.Cli = icCLI NO-LOCK NO-ERROR.
              IF AVAILABLE Mobsub THEN FATime.CustNum = Mobsub.CustNum.
           END.

           IF FATime.CustNum > 0 THEN DO:
              FIND Customer WHERE Customer.CustNum = FATime.CustNum NO-LOCK.
              FATime.VatIncl = Customer.VatIncl.
           END. 
           
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF FATime.CLI > "" AND FATime.MsSeq = 0 THEN 
           FOR FIRST MsOwner NO-LOCK WHERE
                     MsOwner.CLI     = FATime.CLI AND
                     MsOwner.CustNum = FATime.CustNum:
              FATime.MsSeq = MsOwner.MsSeq.
           END.
 
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFATime).
           
           ASSIGN
           Memory = recid(FATime)
           xrecid = Memory.
           LEAVE.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST FATime
      WHERE FATime.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FATime THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FATime WHERE recid(FATime) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FATime THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FATime).
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
        ufk[1]= 714 ufk[2]= 209 ufk[3]= 2150 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7] = 1752 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF iiCustNum > 0  THEN ASSIGN ufk[1] = 0
                                      ufk[5] = 0.
        IF icCLI     > "" THEN ASSIGN ufk[1] = 0
                                      ufk[2] = 0.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FATime.custnum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATime.custnum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FATime.cli {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATime.cli WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW liTransPeriod {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) liTransPeriod WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).
      IF rtab[FRAME-LINE] = ? AND LOOKUP(nap,"5,F5,8,F8") = 0
      THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND FATime WHERE recid(FATime) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FATime THEN
              ASSIGN FIRSTrow = i Memory = recid(FATime).
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
           IF NOT AVAILABLE FATime THEN DO:
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
                rtab[1] = recid(FATime)
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
           IF NOT AVAILABLE FATime THEN DO:
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
              rtab[FRAME-DOWN] = recid(FATime).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FATime WHERE recid(FATime) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FATime THEN DO:
           Memory = recid(FATime).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FATime THEN Memory = recid(FATime).
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
           FIND FATime WHERE recid(FATime) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR  FRAME f1.
       SET custnum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Custnum ENTERED THEN DO:

          IF icFatGrp > "" THEN 
          FIND FIRST FATime WHERE 
                     FATime.BRand = gcBrand AND
                     FATime.FtGrp    = icFatGrp AND 
                     FATime.CustNum >= CustNum
               NO-LOCK NO-ERROR.
          ELSE FIND FIRST FATime WHERE 
                          FATime.BRand = gcBrand AND
                          FATime.CustNum >= CustNum
               NO-LOCK NO-ERROR.

          IF NOT AVAILABLE FATime THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FATime/CLI was found */
          ASSIGN order = 1 Memory = recid(FATime) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR  FRAME f2.
       SET CLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CLI ENTERED THEN DO:

          IF icFatGrp > "" THEN 
          FIND FIRST FATime WHERE 
                     FATime.BRand = gcBrand AND
                     FATime.FtGrp = icFatGrp AND 
                     FATime.CLI  >= CLI
               NO-LOCK NO-ERROR.
          ELSE IF iiCustNum > 0 THEN 
          FIND FIRST FATime WHERE
                     FATime.BRand = gcBrand AND
                     FATime.CustNum = iiCustNum AND
                     FATime.CLI    >= CLI
               NO-LOCK NO-ERROR. 
          ELSE FIND FIRST FATime WHERE 
                          FATime.BRand = gcBrand AND
                          FATime.CLI >= CLI
               NO-LOCK NO-ERROR.

          IF NOT AVAILABLE FATime THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FATime/CLI was found */
          ASSIGN order = 1 Memory = recid(FATime) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        {Syst/uright2.i}.
        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. 
        RUN Syst/ufkey. ufkey = TRUE.
        RUN local-find-this(TRUE).
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATime).
        UPDATE FATime.Memo WITH FRAME f4.
        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATime).
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN 
     DO: /* MEMO */
        RUN local-find-this(false).
        RUN Mc/memo(INPUT FATime.Custnum,
                 INPUT "FATime",
                 INPUT STRING(FATime.FatNum),
                 INPUT "FATime").
        ufkey = TRUE. ehto = 9.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0
     THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
        liTransPeriod FATime.cli 
        FATime.FTGrp  FATime.Amt  FATime.Used FATime.Transfer   
        FATime.QtyUnit   CustName  .

       Memory = ?.

       RUN local-find-NEXT.
       IF AVAILABLE FATime THEN Memory = recid(FATime).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FATime THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FATime).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       IF FATime.InvNum NE 0 THEN DO:
          MESSAGE
          "This FATime is BILLED, removing is not allowed"
          VIEW-AS ALERT-BOX TITLE "NOT ALLOWED".
          NEXT LOOP.
       END.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       liTransPeriod .
       IF ok THEN DO:

           FIND Customer OF FATime NO-LOCK. 
            
           /* if account for billing item is adv.payment related
              -> make an adv.payment out of unused amount */
           RUN Mc/fat2advp (FATime.FatNum,
                         OUTPUT liVoucher).
              
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFATime).
           DELETE FATime.

           IF liVoucher > 0 THEN
              MESSAGE "Removed amount was transferred as advance payment to"
                      "customer" Customer.InvCust "with voucher" 
                      liVoucher
              VIEW-AS ALERT-BOX INFORMATION.
              
           /* was LAST record DELETEd ? */
           IF memory = ? THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* show eventlog */
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:

        RUN local-find-this(FALSE).

        IF AVAILABLE FATime THEN DO:
           IF FATime.Period >= 200904 THEN 
              lcEventKey = getStarEventKey(lhFATime).
           ELSE 
              lcEventKey = "#BEGIN" + CHR(255) + 
                           STRING(FATime.CustNum) + CHR(255) +
                           FATime.CLI + CHR(255) + 
                           STRING(FATime.FATType) + CHR(255).
   
           RUN Mc/eventsel ("FATime",lcEventKey).
        END.
        
        ufkey = TRUE.
        NEXT LOOP.
     END.

     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATime).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATime).
       RUN local-disp-row.
       xrecid = recid(FATime).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FATime) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FATime) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FATime WHERE recid(FATime) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FATime WHERE recid(FATime) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icFatGrp > "" THEN DO:
      FIND FIRST FATime WHERE 
                 FATime.BRand = gcBrand AND
                 FATime.FtGrp = icFatGrp NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCustNum > 0 THEN DO:
      FIND FIRST FATime WHERE 
                 FATime.BRand = gcBrand AND
                 FATime.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.

   ELSE IF icCLI > "" THEN DO:
      FIND FIRST FATime WHERE 
                 FATime.BRand = gcBrand AND
                 FATime.CLI = icCLI AND
                 FATime.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.

   ELSE DO:
       IF order = 1 THEN 
       FIND FIRST FATime USE-INDEX InvNum WHERE 
                  FATime.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
       FIND FIRST FATime USE-INDEX CLI WHERE
                  FATime.Brand = gcBrand NO-LOCK NO-ERROR.
   END. 

END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icFatGrp > "" THEN DO:
      FIND LAST FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.FtGrp = icFatGrp NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCustNum > 0 THEN DO:
      FIND LAST FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.

   ELSE IF icCLI > "" THEN DO:
      FIND LAST FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CLI = icCLI AND
                FATime.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.

   ELSE DO:
       IF order = 1 THEN 
       FIND LAST FATime USE-INDEX InvNum WHERE
                 FATime.BRand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
       FIND LAST FATime USE-INDEX CLI WHERE
                FATime.BRand = gcBrand
       NO-LOCK NO-ERROR.
   END. 

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF icFatGrp > "" THEN DO:
      FIND NEXT FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.FtGrp = icFatGrp NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCustNum > 0 THEN DO:
      FIND NEXT FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.

   ELSE IF icCLI > "" THEN DO:
      FIND NEXT FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CLI = icCLI AND
                FATime.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.

   ELSE DO:
       IF order = 1 THEN 
       FIND NEXT FATime USE-INDEX InvNum WHERE
                 FATime.BRand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
       FIND NEXT FATime USE-INDEX CLI WHERE 
                 FATime.BRand = gcBrand  NO-LOCK NO-ERROR.
   END. 

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF icFatGrp > "" THEN DO:
      FIND PREV FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.FtGrp = icFatGrp NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCustNum > 0 THEN DO:
      FIND PREV FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.

   ELSE IF icCLI > "" THEN DO:
      FIND PREV FATime WHERE 
                FATime.BRand = gcBrand AND
                FATime.CLI = icCLI AND
                FATime.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.

   ELSE DO:
       IF order = 1 THEN 
       FIND PREV FATime USE-INDEX InvNum WHERE
                 FATime.BRand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
       FIND PREV FATime USE-INDEX CLI WHERE
                 FATime.BRand = gcBrand NO-LOCK NO-ERROR.
   END. 

END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       FATime.CLI
       liTransPeriod
       FATime.qtyunit FORMAT "x(1)"
       FATime.Amt
       FATime.FatPerc
       FATime.Transfer
       FATime.Used
       FATime.FTGrp
       CustName
       FATime.Custnum
       invoiced
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

       IF FATime.Payer NE 0 THEN DO:
          FIND FIRST Customer  WHERE 
                     Customer.CustNum = FATime.Payer NO-LOCK NO-ERROR.
          IF AVAIL Customer  THEN 
             payername = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                          BUFFER Customer).
          ELSE payername = "Unknown".
       END.
       ELSE payername = "FreeOfCharge".

       FIND FIRST FATGroup WHERE
                  FATGroup.Brand = gcBrand AND 
                  FATGroup.FTGrp = FATime.FTGrp NO-LOCK NO-ERROR.

       CustName = "".
       IF FATime.CustNum > 0 THEN DO:
          FIND Customer  WHERE Customer.CustNum = FATime.custnum 
          no-lock no-error.

          IF AVAIL Customer THEN 
             CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                         BUFFER Customer).
       END.
       ELSE IF FATime.CLI = "DEF" THEN CustName = "Default".
       
       ASSIGN
       invoiced = FATime.Invnum ne 0.

       IF FATime.TransPeriod = 0 THEN
          liTransPeriod = FATime.Period.
       ELSE liTransPeriod = FATime.TransPeriod.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      fDispFATType().
      fDispQtyUnit().

      ASSIGN lcExtInvID  = ""
             lcIntInvNum = "".

      IF FATime.InvNum > 0 THEN DO:
         lcIntInvNum = "(" + STRING(FATime.InvNum) + ")".
         
          FIND Invoice WHERE Invoice.InvNum = FATIme.InvNum NO-LOCK NO-ERROR.
         IF AVAILABLE Invoice THEN lcExtInvID = Invoice.ExtInvID.
      END.
      
      DISP
         FATime.CLI
         FATime.CustNum
         custname
         FATime.Period
         FATime.FTGrp       
         FATime.qtyunit
         FATime.VatIncl
         FATime.Amt 
         FATime.FatPerc
         FATime.used
         FATime.Transfer
         FATime.TransQty
         FATime.TransPeriod
         FATime.LastPeriod
         FATGroup.FtgName  WHEN AVAIL FATGroup
         lcExtInvID
         lcIntInvNum
         FATime.Memo[1 FOR 2]
      WITH FRAME lis.

      IF NEW Fatime THEN toimi = 1.
      ELSE DO: 
         ASSIGN 
            ehto   = 0
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND gcHelpParam = ""
            ufk[8] = 8.
         RUN Syst/ufkey.
      END.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

       /*  FIND CURRENT Fatime NO-LOCK. */
            
         ehto = 9.
         RUN Syst/ufkey.
 
         PROMPT
            FATime.cli        WHEN NEW FATime AND icCLI = ""
            FATime.Custnum    WHEN NEW FATime AND iiCustNum = 0 AND icCLI = ""
            FATime.FTGrp      WHEN NEW FATime AND icFatGrp = "" 
            FATime.Period     WHEN NEW FATime
            FATime.VatIncl    WHEN NEW FATime
            FATime.Amt        WHEN NEW FATime
            FATime.FatPerc    WHEN NEW FATime
            FATime.Memo[1 FOR 2]
         WITH FRAME lis EDITING:
             READKEY.

             IF FRAME-FIELD = "Qtyunit" AND keylabel(lastkey) = "F9" THEN DO:
                RUN Help/h-tmscodes.p(INPUT "FATime",  /* TableName*/
                                     "qtyunit", /* FieldName */
                                     "QtyUnit", /* GroupCode */
                               OUTPUT siirto).
                ASSIGN
                FATime.qtyunit = INPUT FATime.Qtyunit.
                disp FATime.qtyunit with frame lis.
                ehto = 9. 
                RUN Syst/ufkey.p.
             END.

             ELSE IF FRAME-FIELD = "FATType" AND keylabel(lastkey) = "F9" 
             THEN DO:

                RUN Help/h-tmscodes.p(INPUT "FATime",  /* TableName*/
                                     "FATType", /* FieldName */
                                     "Billing", /* GroupCode */
                               OUTPUT siirto).
                IF siirto NE "" AND siirto NE ? THEN DO:
                   FATime.FATType = INTEGER(siirto).
                   fDispFATType().
                END.

                ehto = 9. 
                RUN Syst/ufkey.p.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CLI" THEN DO:
                
                   IF INPUT FRAME lis FATime.CLI = "Def" THEN DO:
                      DISPLAY "Default" @ CustName WITH FRAME lis.  
                   END.
                   
                   ELSE IF INPUT FRAME lis FATime.CLI = "" THEN DO:
                       FATime.FATType = 1.
                       fDispFATType().
                   END.

                   ELSE DO:
                      FIND MobSub WHERE 
                           Mobsub.Brand = gcBrand AND 
                           MobSub.Cli =
                         INPUT FRAME lis FATime.CLI  NO-LOCK NO-ERROR.
                      IF NOT AVAIL Mobsub THEN DO:
                         FIND FIRST MsOwner WHERE
                                    MsOwner.CLI = INPUT FRAME lis FATime.CLI 
                         NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE MsOwner THEN DO:
                            MESSAGE "UNKNOWN Mobile subscription!".
                            NEXT.
                         END.
                      END.
                      IF AVAILABLE MobSub THEN DO:
                         ASSIGN FATime.msseq = mobsub.msseq
                                FATime.FATType = 0. 

                         DISP 
                         Mobsub.CustNum @ FATime.custnum with frame lis.
                      END.
                      ELSE IF AVAILABLE MsOwner THEN DO:
                         ASSIGN FATime.msseq = MsOwner.msseq
                                FATime.FATType = 0. 

                         DISP 
                         MsOwner.CustNum @ FATime.custnum with frame lis.
                      
                      END.
                      
                      fDispFATType().

                   END.   
                END.

                IF FRAME-FIELD = "Custnum" THEN DO:
                    
                   IF NOT (INPUT FATime.CLI = "DEF" AND
                           INPUT FATime.CustNum = 0)
                   THEN DO: 
                      FIND Customer WHERE
                           Customer.Brand   = gcBrand AND 
                           Customer.CustNum = INPUT FRAME lis FATime.CustNum 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL Customer  THEN DO:
                         BELL.
                         MESSAGE
                         "Unknown Customer!".
                         NEXT.
                      END.
                      DISP Customer.CustName   @ custname with frame lis.
                   END.   
                END.

                ELSE IF FRAME-FIELD = "Period" THEN DO:
                   RUN Syst/uperch(INPUT FRAME lis FATime.Period,OUTPUT rc).
                   IF rc NE 0 THEN NEXT.
                END.

                ELSE IF FRAME-FIELD = "Ftgrp" THEN DO:
                   FIND FIRST fatgroup WHERE
                              FAtGroup.Brand = gcBRand AND 
                              fatgroup.ftgrp =  INPUT FRAME lis FATime.FTgrp 
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL Fatgroup THEN DO:
                      BELL.
                      MESSAGE "Unknown FATime Group!".
                      NEXT.
                   END.
                   DISP FATGroup.FtgName WITH FRAME lis.
                   
                   fDefaults().
                END.

                ELSE IF FRAME-FIELD = "FatPerc" OR FRAME-FIELD = "Amt"
                THEN DO:
                   IF INPUT FRAME lis FATime.Amt > 0 AND
                      INPUT FRAME lis FATime.FatPerc > 0
                   THEN DO:
                      MESSAGE "You can't define both amount and percentage."
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.
 
             END.

             APPLY LASTKEY.

          END. /* EDITING */

          
         LEAVE.
      END.
      


      LEAVE.
   END.
   
      FIND CURRENT FATime NO-LOCK.

      IF CURRENT-CHANGED FATime THEN DO:

         MESSAGE
            ({&MSG_RECORD_CHANGED})
            VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

         RETURN.
      END.
      ELSE DO:
      
         FIND CURRENT FATime EXCLUSIVE-LOCK.
         
         ASSIGN
            FATime.cli
            FATime.Custnum
            FATime.FTGrp
            FATime.Period
            FATime.VatIncl
            FATime.Amt
            FATime.FatPerc
            FATime.Memo[1 FOR 2].
          
          /* create FATime items */

          if new FATime then DO:

             IF FATime.CLI > "" AND FATime.MsSeq = 0 THEN 
             FOR FIRST MsOwner NO-LOCK WHERE
                       MsOwner.CLI     = INPUT FATime.CLI AND
                       MsOwner.CustNum = INPUT FATime.CustNum:
                FATime.MsSeq = MsOwner.MsSeq.
             END.
           
             liCrePeriod = FATime.Period.
                
             FIND FatGroup NO-LOCK WHERE
                  FatGroup.Brand = gcBrand AND
                  FatGroup.FtGrp = INPUT FATime.FtGrp.
                  
             IF FatGroup.PeriodQty > 1 THEN 
             DO loop = 2 TO FatGroup.PeriodQty :

                /* next period */
                DO liNextPer = 1 TO FatGroup.Interval:
                   IF liCrePeriod MOD 100 < 12 
                   THEN liCrePeriod = liCrePeriod + 1.
                   ELSE liCrePeriod = (TRUNCATE(liCrePeriod / 100,0) + 1) 
                                      * 100 + 1.
                END.   

                CREATE xxFATime.
                BUFFER-COPY FATime EXCEPT fatnum TO xxFATime.
                ASSIGN
                xxFATime.FatNum = next-value(ftseq)
                xxFATime.Period = liCrePeriod
                qty = qty + 1.
     
                IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhxxFATime).
             END.

             IF QTY > 0 THEN
             MESSAGE
             "Totally" qty "additional FATime items created"
             VIEW-AS ALERT-BOX TITLE "FATIMES CREATED".
          END.

      END.
      FIND CURRENT FATime NO-LOCK.

END PROCEDURE.


