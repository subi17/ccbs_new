/* ----------------------------------------------------------------------
  MODULE .......: nnsvyp.p
  TASK .........: UPDATE of Direct Debit Authorizations
  APPLICATION ..: nn
  AUTHOR .......: ht
  CREATED ......: 12-11-99
  CHANGED ......: 02.11.01 ht 
                  03.03.03/aam don't go into insert mode if none exists,
                               eventlog
                  17.09.03/aam brand
                  11.11.03/aam longer format form CustNum 
                  09.03.04/aam format for "billable"
                  10.08.04/aam AuthID, Memo
                  02.09.04/aam ilFailed
                  29.09.04/aam use of ktun corrected
  VERSION ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable DDAuth

{Syst/commali.i}  
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDDAuth AS HANDLE NO-UNDO.
   lhDDAuth = BUFFER DDAuth:HANDLE.
   RUN StarEventInitialize(lhDDAuth).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDDAuth).
   END.

END.

DEF INPUT PARAMETER ilFailed AS LOG NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustNum    LIKE DDAuth.CustNum     NO-UNDO.
DEF VAR Archive    LIKE DDAuth.Archive     NO-UNDO.
DEF VAR CustName   LIKE DDAuth.CustName    NO-UNDO.
DEF VAR Identif    LIKE DDAuth.Identif     NO-UNDO.
DEF VAR ddProcess  LIKE DDAuth.ddProcess   NO-UNDO.  
DEF VAR BankAcc    LIKE DDAuth.BankAcc     NO-UNDO.  
DEF VAR AuthDate   LIKE DDAuth.AuthDate    NO-UNDO.  
DEF VAR OldBankAcc LIKE DDAuth.OldBankAcc  NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR ktun         AS C                      NO-UNDO.
DEF VAR lcHeader     AS CHAR                   NO-UNDO. 

ktun = "NEW,CHANGE,TERMINATION,MAINTENANCE,,,,,,".
/* tmscodes:sta... */


form
    DDAuth.Brand         FORMAT "x(4)"  COLUMN-LABEL "Bran"
    DDAuth.CustNum       FORMAT ">>>>>>>9"
    Customer.CustName    FORMAT "x(20)" COLUMN-LABEL "Customer's Name"
    DDAuth.ddProcess     COLUMN-LABEL "P"
    DDAuth.Archive   
    DDAuth.AuthDate      FORMAT "99-99-99" COLUMN-LABEL "Date"
    DDAuth.CustName      FORMAT "x(11)" COLUMN-LABEL "Authr.Name"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi + lcHeader 
    + string(pvm,"99-99-9999") + " "
    FRAME sel.

form
    DDAuth.CustNum     colon 22 FORMAT ">>>>>>>>>>>9" Customer.CustName NO-LABEL  
    DDAuth.Archive     colon 22                         
    DDAuth.CustName    colon 22 FORMAT "X(35)"
       LABEL "Name In Authorization"                       
    DDAuth.Identif     colon 22 FORMAT "X(30)"         
    DDAuth.ddProcess   colon 22 
    VALIDATE(INPUT DDAuth.ddProcess > 0 AND
             INPUT DDAuth.ddProcess < 5,"Must be 1 ... 4 !")
    ktun no-label                 FORMAT "x(20)"          
    DDAuth.BankAcc     colon 22 FORMAT "99999999999999"     
    DDAuth.AuthDate    colon 22 FORMAT "99-99-9999"                        
    DDAuth.OldBankAcc  colon 22 FORMAT "99999999999999"    

WITH  OVERLAY ROW 7 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form /* seek ddAuth  BY  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" CustNum FORMAT ">>>>>>>9"
    HELP "Give Customer Nbr."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ddAuth  BY Archive */
    "Brand .:" lcBrand skip
    "Archive:" Archive
    HELP "Give Archive Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ARCHIVE CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

IF ilFailed 
THEN lcHeader = " FAILED DD AUTHORIZATIONS ".
ELSE lcHeader = " DIRECT DEBIT AUTHORIZATIONS ".

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "Order by customer nbr,Order by archive code,By 3, By 4".

RUN local-find-first.

IF AVAILABLE DDAuth THEN ASSIGN
   memory       = recid(DDAuth)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

    IF must-add THEN DO:  /* Add a DDAuth  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           PROMPT-FOR 
           DDAuth.CustNum
           WITH FRAME lis EDITING:
              READKEY.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "CustNum" THEN DO:
                    IF INPUT DDAuth.CustNum = 0
                    THEN UNDO add-row, LEAVE add-row.

                    /* onko tunnettu Customer ? */
                    FIND FIRST Customer WHERE 
                               Customer.Brand = lcBrand AND
                               Customer.CustNum = 
                    INPUT FRAME lis DDAuth.CustNum NO-LOCK NO-ERROR.
                    IF NOT AVAIL Customer THEN DO:
                       BELL.
                       MESSAGE "UNKNOWN CUSTOMER !".
                       NEXT.
                    END.
                    /* Customer loytyi */
                    DISP Customer.CustName.
                 END.
              END.

              APPLY LASTKEY.
           END.

           CREATE DDAuth.
           ASSIGN
           DDAuth.Brand   = lcBrand
           DDAuth.CustNum = INPUT FRAME lis DDAuth.CustNum
           DDAuth.AuthID  = NEXT-VALUE(dpseq).

           RUN local-update-record. 

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDDAuth).

           ASSIGN
           memory = recid(DDAuth)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST DDAuth
      WHERE DDAuth.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DDAuth THEN LEAVE LOOP.
      NEXT LOOP.

   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND DDAuth WHERE recid(DDAuth) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DDAuth THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DDAuth).
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
        ufk[1]= 702  ufk[2]= 0 /* 35 */ ufk[3]= 0 ufk[4]= 927
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        
        IF ilFailed THEN ASSIGN 
           ufk[1] = 0
           ufk[5] = 0.
        
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DDAuth.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DDAuth.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW DDAuth.Archive {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DDAuth.Archive WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND DDAuth WHERE recid(DDAuth) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DDAuth THEN
              ASSIGN FIRSTrow = i memory = recid(DDAuth).
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
           IF NOT AVAILABLE DDAuth THEN DO:
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
                rtab[1] = recid(DDAuth)
                memory  = rtab[1].
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
           IF NOT AVAILABLE DDAuth THEN DO:
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
              rtab[FRAME-DOWN] = recid(DDAuth).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DDAuth WHERE recid(DDAuth) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DDAuth THEN DO:
           memory = recid(DDAuth).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DDAuth THEN memory = recid(DDAuth).
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
           memory = rtab[FRAME-DOWN].
           FIND DDAuth WHERE recid(DDAuth) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */                          
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              CustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF CustNum > 0 THEN DO:
          FIND FIRST DDAuth WHERE 
                     DDAuth.Brand = lcBrand AND
                     DDAuth.CustNum >= CustNum
          USE-INDEX CustNum NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              Archive WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF Archive > "" THEN DO:
          FIND FIRST DDAuth WHERE 
                     DDAuth.Brand = lcBrand AND
                     DDAuth.Archive >= Archive
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     else IF lookup(nap,"4,f4") > 0 THEN do:

        RUN local-find-this (FALSE).

        RUN Mc/memo.p(INPUT DDAuth.CustNum,
                 INPUT "DDAuth",
                 INPUT STRING(DDAuth.AuthID),
                 INPUT "DD Authorization").

        ufkey = TRUE.
        NEXT.
     end.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DDAuth.CustNum DDAuth.Archive .

       RUN local-find-NEXT.
       IF AVAILABLE DDAuth THEN memory = recid(DDAuth).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DDAuth THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(DDAuth).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       DDAuth.CustNum DDAuth.Archive .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDDAuth).

           DELETE DDAuth.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST DDAuth
           WHERE DDAuth.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " MUUTA " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY DDAuth.CustNum.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDDAuth).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDDAuth).

       RUN local-disp-row.
       xrecid = recid(DDAuth).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(DDAuth) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(DDAuth) must-print = TRUE.
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
       FIND DDAuth WHERE recid(DDAuth) = rtab[frame-line(sel)] 
       EXCLUSIVE-LOCK.
    ELSE
       FIND DDAuth WHERE recid(DDAuth) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN DO:
          IF ilFailed THEN
          FIND FIRST DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum = 0
          NO-LOCK NO-ERROR.
          ELSE 
          FIND FIRST DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum > 0
          NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND FIRST DDAuth 
       WHERE DDAuth.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN DO:
          IF ilFailed THEN
          FIND LAST DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum = 0
          NO-LOCK NO-ERROR.
          ELSE 
          FIND LAST DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum > 0
          NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND LAST DDAuth 
       WHERE DDAuth.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN DO:
          IF ilFailed THEN
          FIND NEXT DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum = 0
          NO-LOCK NO-ERROR.
          ELSE 
          FIND NEXT DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum > 0
          NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND NEXT DDAuth 
       WHERE DDAuth.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN DO:
          IF ilFailed THEN
          FIND PREV DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum = 0
          NO-LOCK NO-ERROR.
          ELSE 
          FIND PREV DDAuth      USE-INDEX CustNum 
          WHERE DDAuth.Brand   = lcBrand AND
                DDAuth.CustNum > 0
          NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND PREV DDAuth 
       WHERE DDAuth.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       /* FIND additional information from other tables FOR DISPLAY */
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       DDAuth.Brand
       DDAuth.CustNum
       DDAuth.ddProcess
       Customer.CustName WHEN     AVAIL Customer
       "!! UNKNOWN !!" WHEN NOT AVAIL Customer @ Customer.CustName
       DDAuth.Archive
       DDAuth.AuthDate
       DDAuth.CustName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Customer WHERE Customer.CustNum = DDAuth.CustNum NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      IF DDAuth.DDProcess > 0 AND
         DDAuth.DDProcess < 5 
      THEN DISP ENTRY(DDAuth.ddProcess,ktun) @ ktun WITH FRAME lis.
      ELSE DISP "" @ ktun WITH FRAME lis.
      
      DISP DDAuth.CustNum         
           DDAuth.Archive       
           DDAuth.CustName        
           DDAuth.Identif  
           DDAuth.ddProcess      
           DDAuth.BankAcc       
           DDAuth.AuthDate       
           DDAuth.OldBankAcc    
      WITH FRAME lis.

      UPDATE
          DDAuth.CustNum       WHEN NOT NEW DDAuth
          DDAuth.Archive       WHEN NEW DDAuth
          DDAuth.CustName      WHEN NEW DDAuth
          DDAuth.Identif       WHEN NEW DDAuth
          DDAuth.ddProces      WHEN NEW DDAuth
          DDAuth.BankAcc       WHEN NEW DDAuth
          DDAuth.AuthDate      WHEN NEW DDAuth
          DDAuth.OldBankAcc    WHEN NEW DDAuth
       WITH FRAME lis  EDITING:
         READKEY.
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.

            IF FRAME-FIELD = "CustNum" THEN DO:
               IF INPUT FRAME lis DDAuth.CustNum = 0 THEN DO:
                  DISP "" @ Customer.CustName.
               END.
               ELSE DO:
                  FIND FIRST Customer WHERE   
                             Customer.Brand   = lcBrand AND
                             Customer.CustNum =
                  INPUT FRAME lis DDAuth.CustNum NO-LOCK NO-ERROR.
                  IF NOT AVAIL Customer THEN DO:
                     BELL.
                     MESSAGE "UNKNOWN CUSTOMER !".
                     NEXT.
                  END.
                  DISP Customer.CustName.
               END.
            END.
            ELSE IF FRAME-FIELD = "ddProcess" THEN DO:
               IF INPUT DDAuth.ddProcess = 0 OR
                  INPUT DDAuth.ddProcess > 4 THEN DO:
                  BELL.
                  MESSAGE "Allowed 1 ... 4 !".
                  NEXT.
               END.
               DISP ENTRY(INPUT DDAuth.ddProcess,ktun) @ ktun.
            END.      
         END.
         APPLY LASTKEY.
      END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.

