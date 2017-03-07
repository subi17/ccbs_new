/* ----------------------------------------------------------------------
  MODULE .......: precust.p
  TASK .........: BROWSE OF CUSTOMER'S PRESELECT TRANSACT.
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 10-03-00
  CHANGED ......: 27.06.00 pt minor changes TO English idioms 
                  03.03.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}  /*qupd = TRUE.*/
{Func/timestamp.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'presel'}

DEF INPUT PARAMETER CustNum LIKE Presel.CustNum.
/*
CustNum = 1001.
*/
DEF NEW shared VAR siirto AS CHAR.

DEF VAR CLI LIKE Presel.CLI NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
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
DEF VAR ft           AS LOG                    NO-UNDO.
DEF VAR pstypes      AS CHAR                   NO-UNDO.
DEF VAR pstypet      AS CHAR                   NO-UNDO.
DEF VAR pecode       AS CHAR                   NO-UNDO.
DEF VAR asnimi       AS char format "x(30)"    NO-UNDO.
DEF VAR prevsubno    LIKE Presel.CLI.
DEF VAR dpstype      LIKE Presel.PsType.
DEF VAR subnr        LIKE Presel.CLI.
DEF VAR Orderer      LIKE Presel.Orderer.
DEF VAR AuthNo       LIKE Presel.AuthNo.
DEF VAR AuthDate     LIKE Presel.AuthDate.
DEF VAR delsubno     LIKE Presel.CLI.


form
    Presel.CLI    COLUMN-LABEL "A-Sub. no (CLI)"
    Presel.PsType  
    Presel.Orderer  FORMAT "x(24)"
    Presel.SentDate
    Presel.ConfDate
    pecode          COLUMN-LABEL "Rc" format "x(2)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Preselect CLIs of Customer " + STRING(CustNum) + " "
    FRAME sel.


form
SKIP
"   CustNo ...:"  Presel.CustNum                  skip
"   CustName .:"  /*Customer.CustName*/ 
                  asnimi                         skip(1)
"   CLI ....:"  Presel.CLI                   skip
"   RepType .....:"  Presel.PsType HELP "Type of Presel. 1)Nat 2)Intn'l 3)Both"
                  pstypet                        SKIP

"   Orderer ..:"  Presel.Orderer                 skip
/*"   AuthNo ...:"  Presel.AuthNo                  skip*/
"   AuthDate .:"  Presel.AuthDate                skip(1)
/*
"   SentDate .:"  Presel.SentDate                skip
"   ConfDate .:"  Presel.ConfDate                skip(1)
"   ReturnC ..:"  Presel.ReturnCode                 skip
"              "  Presel.ErrText                  skip(1)
"   OutFileS .:"  Presel.FileSeq1                skip
"   InFileS ..:"  Presel.FileSeq2                skip(1)
*/
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.



form /* seek PRESELECT  BY CLI */
    CLI
    HELP "Enter Subscriber number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

pstypes = "NATIONAL,INTERNATIONAL,NAT & INT".

orders = "   By CLI   ,By 2 ,By 3, By 4".


FIND FIRST Presel
where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
IF AVAILABLE Presel THEN ASSIGN
   memory       = recid(Presel)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
   "  There are NO A-sub nos- selected for " SKIP
   "  Carrier Preselect " SKIP
   "  for this customer ( " + string(CustNum) + " ) !" SKIP
   VIEW-AS ALERT-BOX ERROR.
   LEAVE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 2 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 32 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a Presel  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           CREATE Presel.
           ASSIGN
           Presel.CustNum = CustNum
           Presel.PsType  = 3.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR  
              KEYLABEL(LASTKEY) = "f4" THEN 
           UNDO add-row, LEAVE add-row.


           ASSIGN
           memory = recid(Presel)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Presel
      where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Presel THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Presel WHERE recid(Presel) = memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Presel THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Presel).
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
        ufk[1]= 36  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Presel.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Presel.CLI WITH FRAME sel.
      END.
/*    ELSE IF order = 2 THEN DO:
        CHOOSE ROW Presel.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Presel.CLI WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW Presel.?? {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Presel.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW Presel.??  {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Presel.? WITH FRAME sel.
      END.
*/
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND Presel WHERE recid(Presel) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Presel THEN
              ASSIGN FIRSTrow = i memory = recid(Presel).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Presel THEN DO:
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
                rtab[1] = recid(Presel)
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
           IF NOT AVAILABLE Presel THEN DO:
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
              rtab[FRAME-DOWN] = recid(Presel).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Presel WHERE recid(Presel) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Presel THEN DO:
           memory = recid(Presel).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Presel THEN memory = recid(Presel).
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
           FIND Presel WHERE recid(Presel) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY col 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F1.
       SET CLI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          FIND FIRST Presel WHERE Presel.CLI >= CLI AND
                                  Presel.CustNum = CustNum 


          USE-INDEX CLI NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Presel THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some presel/SubNo was found */
          ASSIGN order = 1 memory = recid(Presel) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Presel.CLI Presel.PsType Presel.Orderer Presel.ConfDate
       pecode.

       RUN local-find-NEXT.
       IF AVAILABLE Presel THEN memory = recid(Presel).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Presel THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(Presel).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Presel.CLI Presel.PsType Presel.Orderer Presel.ConfDate pecode.
       delsubno = Presel.CLI.
       IF ok THEN DO:
           DELETE Presel.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Presel
           where Presel.CustNum = CustNum) THEN DO:
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
       {Syst/uright2.i}
       /* change */
       RUN local-find-this(TRUE).
       FIND Customer WHERE Customer.CustNum = Presel.CustNum NO-LOCK NO-ERROR.

       FIND CLI where CLI.CLI = Presel.CLI NO-LOCK NO-ERROR.
       RUN Mf/viewpres.p(CLI.CLI).
       ufkey = TRUE.


       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(Presel).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(Presel) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(Presel) must-print = TRUE.
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
      FIND FIRST Presel WHERE recid(Presel) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FIRST Presel WHERE recid(Presel) = rtab[frame-line(sel)] 
       NO-LOCK.


END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Presel 
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
    /* ELSE IF order = 2 THEN FIND FIRST Presel USE-INDEX CLI
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST Presel USE-INDEX index-3
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST Presel USE-INDEX index-4
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Presel 
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
    /* ELSE IF order = 2 THEN FIND LAST Presel USE-INDEX CLI
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST Presel USE-INDEX index-3
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST Presel USE-INDEX index-4
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Presel 
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
    /* ELSE IF order = 2 THEN FIND NEXT Presel USE-INDEX CLI
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT Presel USE-INDEX index-3
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT Presel USE-INDEX index-4
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV Presel 
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
    /* ELSE IF order = 2 THEN FIND PREV Presel USE-INDEX CLI
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV Presel USE-INDEX index-3
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV Presel USE-INDEX index-4
       where Presel.CustNum = CustNum NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-disp-row:
       /* FIND additional information from other tables FOR DISPLAY */
       RUN local-find-others.
       DISPLAY 
         Presel.CLI 
         Presel.PsType
         Presel.Orderer
         Presel.ConfDate
         Presel.SentDate
         pecode
       WITH FRAME sel.


END PROCEDURE.

PROCEDURE local-find-others.
    FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK NO-ERROR.
    IF Presel.FileSeq2 =  0 THEN pecode = "".
    ELSE pecode = STRING(Presel.ReturnCode).

END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      asnimi = Customer.CustName.
      pstypet = ENTRY(Presel.PsType,pstypes).

      DISP
         Presel.CustNum
         pstypet
         asnimi
         Presel.CLI
         Presel.PsType 
         Presel.Orderer
         Presel.AuthDate

      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:

         UPDATE
             asnimi
             Presel.CLI
             Presel.PsType 
             Presel.Orderer
             /*Presel.AuthNo*/
             Presel.AuthDate

         WITH FRAME lis                                          
         EDITING:


            READKEY.

            IF FRAME-FIELD = "CLI" AND KEYLABEL(LASTKEY) = "F9" THEN DO:

               RUN Help/h-psubno.p(CustNum).
               IF siirto NE ? THEN DO:
                  ASSIGN Presel.CLI = siirto.
                  DISP Presel.CLI WITH FRAME lis.
                  NEXT.
               END.
            END.


            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
               IF FRAME-FIELD = "PsType" THEN DO:

                  IF INPUT FRAME lis Presel.PsType > 3 OR
                     INPUT FRAME lis Presel.PsType < 1  THEN DO:
                       disp "" @ pstypet WITH FRAME lis.
                       MESSAGE "You have to choose a RepType between 1-3 !".
                     NEXT.
                  END.
                  pstypet = ENTRY(INPUT PsType,pstypes).
                  DISP pstypet WITH FRAME lis.

               END.


               IF FRAME-FIELD = "CLI" THEN DO:
                  FIND CLI WHERE CLI.CLI =
                  INPUT FRAME lis Presel.CLI      AND 
                  CLI.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
                  IF NOT AVAIL CLI THEN DO:
                     BELL.
                     MESSAGE "Unknown Subscriper's Nro !".
                     NEXT.
                  END.
                  /*DISP <table>.<field2>.*/
               END.
            END.
           APPLY LASTKEY.
         END. /* EDITING */
         /*
         ASSIGN
         Presel.CustName = asnimi.
         */
         /* Timestamps */
         Presel.CrStamp = fMakeTS().
         Presel.ChStamp = fMakeTS().
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

