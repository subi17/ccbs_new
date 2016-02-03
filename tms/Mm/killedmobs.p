/* ----------------------------------------------------------------------
  MODULE .......: killedmobsub
  TASK .........: BROWSE table msowner (KILLED MOBSUB)
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......:
                  05.08.05 aam same detailed frame as in msowner.p,
                               change address with password
                  17.11.05 aam change also clitype with password             
                  25.01.06 aam BtName removed, username from customer
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}
{Func/cparam2.i}

def new shared var siirto AS char.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMSOwner AS HANDLE NO-UNDO.
   lhMSOwner = BUFFER MSOwner:HANDLE.
   RUN StarEventInitialize(lhMSOwner).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhMSOwner).
   END.
END.


DEF VAR CustNum      like msowner.CustNum        NO-UNDO.
DEF VAR CLI      like msowner.CLI        NO-UNDO.
DEF VAR imno       like msowner.Imsi        NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 3.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR alive        AS LOG format "MSISDN No in Use/MSISDN No Free" NO-UNDO.
DEF VAR lcPassword   AS CHAR                   NO-UNDO. 
DEF VAR lcUserName   AS CHAR                   NO-UNDO. 

form
    msowner.CustNum     /* column-label format */
    msowner.CLI     /* column-label format */
    msowner.Imsi
    msowner.TSBegin
    msowner.TSEnd
    alive COLUMN-LABEL "U" FORMAT "*/"

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  KILLED MOBSUB  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "MSISDN Number ...:" MSOwner.CLI  skip
    "Customer ........:" MSOwner.CustNum TO 30 SKIP
    "Invoicing Target :" MSOwner.BillTarget   SKIP(1)
    "Mobile user .....:" MSOwner.MsSeq TO 30   
       lcUserName  AT 35 FORMAT "X(30)" 
           SKIP
       Customer.Address    AT 35    SKIP
       Customer.ZipCode AT 35 
          Customer.PostOffice SKIP
       Customer.Country    AT 35 SKIP(1)
    "Starting Date ...:" MSOwner.TsBegin                               SKIP
    "Ending Date .....:" MSOwner.TsEnd                                 SKIP
    "IMSI Number .....:" MSOwner.IMSI                                  SKIP
    "Mobile Type .....:" MSOwner.Clitype CLIType.CliName               SKIP
    "Contract ........:" MSOwner.Contract 
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.

form /* seek  CustNum */
    CustNum
    HELP "Enter Customer number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTNO "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  CLI */
    CLI
    HELP "Enter MSISDN Number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN"
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

form /* seek  CLI */
    imno
    HELP "Enter IMSI number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND IMSI "
    COLOR VALUE(cfc) NO-labels overlay FRAME f3.



cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By CustNo  ,  By Msisdn  ,  By Imsi  , By 4".

lcPassword = fCParamC("MSAddressChg").
IF lcPassword = ? THEN lcPassword = "".


FIND FIRST msowner use-index custnum  WHERE 
           msowner.Brand = gcBrand  AND 
           msowner.TSEnd < 99999999 
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE msowner THEN ASSIGN
   memory       = recid(msowner)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a msowner  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR msowner.CustNum
           validate
              (msowner.CustNum NOT ENTERED or
              NOT CAN-FIND(msowner using  msowner.CustNum WHERE 
                                          msowner.Brand = gcBrand ),
              "History Data " + string(INPUT msowner.CustNum) +
              " already exists !").
           IF INPUT FRAME lis msowner.CustNum = "" THEN 
           LEAVE add-row.
           create msowner.
           ASSIGN
           msowner.Brand   = gcBrand 
           msowner.CustNum = INPUT FRAME lis msowner.CustNum.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(msowner)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST msowner use-index custnum WHERE 
                 msowner.Brand = gcBrand AND 
                 msowner.TSEnd < 99999999 

      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE msowner THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND msowner WHERE recid(msowner) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE msowner THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(msowner).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 707  ufk[2]= 209 ufk[3]= 211  ufk[4]= 0
        ufk[5]= 716  
        ufk[6]= 1342 
        ufk[7]= 2354 
        ufk[8]= 8 
        ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row msowner.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) msowner.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row msowner.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) msowner.CLI WITH FRAME sel.
      END.

      IF order = 3 THEN DO:
        choose row msowner.Imsi ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) msowner.Imsi WITH FRAME sel.
      END.
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND msowner WHERE recid(msowner) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE msowner THEN
              ASSIGN FIRSTrow = i memory = recid(msowner).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE msowner THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(msowner)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE msowner THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(msowner).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND msowner WHERE recid(msowner) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE msowner THEN DO:
           memory = recid(msowner).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE msowner THEN memory = recid(msowner).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND msowner WHERE recid(msowner) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET CustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST msowner WHERE 
                     msowner.CustNum >= CustNum AND 
                     msowner.Brand = gcBrand 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE msowner THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some msowner/CustNum was found */
          ASSIGN order = 1 memory = recid(msowner) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET CLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          FIND FIRST msowner WHERE 
                     msowner.CLI >= CLI AND 
                     msowner.Brand = gcBrand 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE msowner THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some msowner/CLI was found */
          ASSIGN order = 2 memory = recid(msowner) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

    /* Search by col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME F3.
       SET imno WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          FIND FIRST msowner WHERE 
                     msowner.Imsi >= imno AND 
                     msowner.Brand = gcBrand
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE msowner THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some msowner/CLI was found */
          ASSIGN order = 2 memory = recid(msowner) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* call value / detail */
       RUN local-find-this(false).
       run mobcallm(msowner.CLI).
       ufkey = TRUE.
       run ufkey.p.
       PAUSE 0.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:  /* call value / detail */
       RUN local-find-this(false).
       run msisdniv(msowner.msseq).
       ufkey = true.
       run ufkey.p.
       PAUSE 0.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:  /* revive Mobsub */ 
       RUN local-find-this(false).
       run revivems.p(msowner.msseq).
       ufkey = true.
       run ufkey.p.
       PAUSE 0.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " VIEW " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY msowner.CustNum.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOwner).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOwner).

       RUN local-disp-row.
       xrecid = recid(msowner).
       RELEASE msowner.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(msowner) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(msowner) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find msowner WHERE recid(msowner) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find msowner WHERE recid(msowner) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST msowner USE-INDEX CustNum 
       WHERE msowner.TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST msowner USE-INDEX CLI 
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST msowner USE-INDEX imsi 
       WHERE TSEnd < 99999999 
      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST msowner USE-INDEX CustNum 
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST msowner USE-INDEX CLI
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST msowner USE-INDEX imsi
       WHERE TSEnd < 99999999 
      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT msowner USE-INDEX CustNum 
        WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT msowner USE-INDEX CLI
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT msowner USE-INDEX imsi
       WHERE TSEnd < 99999999 
      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV msowner USE-INDEX CustNum 
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV msowner USE-INDEX CLI
       WHERE TSEnd < 99999999 

      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV msowner USE-INDEX imsi
       WHERE TSEnd < 99999999 
      AND msowner.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others. 
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       msowner.CustNum 
       msowner.CLI
       msowner.Imsi
       msowner.TSBegin
       msowner.TSEnd
       alive
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
FIND FIRST mobsub WHERE
           mobsub.CLI = msowner.CLI NO-LOCK NO-ERROR.
IF AVAIL mobsub THEN alive = TRUE. ELSE alive = FALSE.           
END PROCEDURE.

PROCEDURE local-update-record:

   DEF VAR lcAskPassWd AS CHAR NO-UNDO.
   DEF VAR llUpdate    AS LOG  NO-UNDO. 
    
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.
      
      ehto = 5.
      RUN ufkey.
      
      FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer 
      THEN lcUserName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                         BUFFER Customer).
      ELSE lcUserName = "".
   
      FIND BillTarg WHERE 
           BillTarg.CustNum    = MSOwner.CustNum AND
           BillTarg.BillTarget = MSOwner.BillTarget
      NO-LOCK NO-ERROR.

      
      FIND CLIType where 
        CliType.Brand   = gcBrand  AND 
        CLIType.Clitype = MSOwner.Clitype NO-LOCK NO-ERROR.  
   
      DISP 
      MSOwner.CLI
      MSOwner.CustNum 
      MSOwner.MsSeq
      lcUserName
      Customer.Address
      Customer.ZipCode Customer.PostOffice 
      Customer.Country
      MSOwner.BillTarget
      MSOwner.TsBegin
      MSOwner.TsEnd
      MSOwner.IMSI
      MSOwner.Clitype
      CLIType.CliName WHEN AVAIL CLIType
      MSOwner.Contract

      WITH FRAME lis.
      message "Press ENTER !".
      PAUSE no-message.

      /* don't show even menu to user */
      IF LOOKUP(KEYLABEL(LASTKEY),"1,F1") > 0 THEN DO:
      
          llUpdate = FALSE.
          
          IF lcPassword > "" THEN DO:
             lcAskPassWd = "".
             PAUSE 0.
             UPDATE lcAskPassWd 
                BLANK
                FORMAT "X(20)" 
                LABEL "Password"
                HELP "Password for changing address data"
                WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE ADDRESS "
                     SIDE-LABELS FRAME fPassword.
             
             llUpdate = (lcAskPassWd = lcPassword).
             
          END.
          ELSE llUpdate = TRUE.
          
          IF llUpdate THEN DO:
           
             FIND CURRENT msowner EXCLUSIVE-LOCK.
             
             UPDATE
             MsOwner.CLIType
             WITH FRAME lis EDITING:
                READKEY.
                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
                DO WITH FRAME lis:
                
                   PAUSE 0.
                   IF FRAME-FIELD = "CLIType" THEN DO:
                      FIND CLIType WHERE CLIType.CLIType =
                      INPUT FRAME lis MSOwner.CLIType NO-LOCK NO-ERROR.
                      IF NOT AVAIL CLIType THEN DO:
                         BELL.
                         MESSAGE "Unknown CLIType !".
                         NEXT.
                      END.
                      DISP CLIType.CLIName WITH FRAME lis.
                   END.
                END.
                APPLY LASTKEY.
             END. /* EDITING */
          END.
      END.
          
      LEAVE.
   END.

END PROCEDURE.

