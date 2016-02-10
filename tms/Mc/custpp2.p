/* ----------------------------------------------------------------------
  MODULE .......: CUSTPP2.P
  TASK .........: Show Product packages of a customer
  APPLICATION ..:   
  AUTHOR .......: tk
  CREATED ......: 13-05-02
  CHANGED ......: 13-05-02 tk  eventlogging added
                  20.05.02 tk  RUN Mc/memo
                  21.05.02 aam call PPItem from f3 AND CustPP from f2
                  11.10.02 jr  Fixed validations 
                  26.02.03 tk  tokens
                  06.02.04 jp  custnum for memo
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'custpp'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustPP AS HANDLE NO-UNDO.
   lhCustPP = BUFFER CustPP:HANDLE.
   RUN StarEventInitialize(lhCustPP).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustPP).
   END.
END.

DEF /* VAR */ INPUT PARAMETER  CustNum LIKE CustPP.CustNum. /*  init 1. */

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ProdPack         LIKE CustPP.ProdPack          NO-UNDO. 
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
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

form
    CustPP.ProdPack      /* COLUMN-LABEL FORMAT */
    ProdPack.PPName
    CustPP.ValidFrom
    CustPP.Salesman
    CustPP.Reseller

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " BillCode Packs of customer " + STRING(CustNum) + ": " + 
    Customer.CustName + " "
    FRAME sel.

form
    CustPP.ProdPack  LABEL "Prodpack "  ProdPack.PPName  AT 25 NO-LABEL SKIP
    CustPP.CustPP    LABEL "Contr.No."                                  SKIP
    CustPP.ValidFrom LABEL "ValidFrom"                                  SKIP
    CustPP.ValidTo   LABEL "Expired.."                                  SKIP
    CustPP.Salesman  LABEL "Salesman."  Salesman.SmName  AT 25 NO-LABEL SKIP
    CustPP.Reseller  LABEL "Reseller."  Reseller.RsName  AT 25 NO-LABEL SKIP


WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek ProdPack  BY  ProdPack */
    ProdPack
    HELP "Enter Code of BillCode Package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FIND FIRST Customer WHERE Customer.CustNum = CustNum USE-INDEX CustNum NO-LOCK.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By 2,By 3, By 4".


FIND FIRST CustPP WHERE
CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
IF AVAILABLE CustPP THEN ASSIGN
   memory       = recid(CustPP)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE 
         "No prod packs available" SKIP
         "for this customer !"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CustPP  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR CustPP.ProdPack WITH FRAME lis  EDITING:
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 if frame-field = "ProdPack" THEN DO:
                    if input CustPP.ProdPack = "" THEN LEAVE add-row.
                     FIND ProdPack where 
                          ProdPack.ProdPack = INPUT CustPP.ProdPack
                    no-lock no-error.
                    IF NOT AVAIL ProdPack THEN DO:
                       bell. message "Unknown BillCode Package !".
                       NEXT.
                    END.
                    DISP ProdPack.PPName.                
                    FIND CustPP where
                         CustPP.ProdPack = INPUT CustPP.ProdPack AND
                         CustPP.CustNum = CustNum
                    NO-LOCK NO-ERROR.
                    IF AVAIL CustPP THEN DO:     
                       BELL. MESSAGE
                       "Customer " + STRING(CustNum) SKIP 
                       " already has already (at least one) ProdPack '"
                        + input frame lis CustPP.ProdPack + "' !"
                       VIEW-AS ALERT-BOX WARNING.
                    END.
                 END.
              END.         
              APPLY LASTKEY.
           END.      

           CREATE CustPP.
           ASSIGN
           CustPP.CustPP   = NEXT-VALUE(cpp-seq)
           CustPP.CustNum  = CustNum
           CustPP.ProdPack = INPUT FRAME lis CustPP.ProdPack.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustPP).

           ASSIGN
           memory = recid(CustPP)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CustPP WHERE
      CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CustPP THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CustPP WHERE recid(CustPP) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CustPP THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CustPP).
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
        ufk[1]= 35 ufk[2]= 1759 ufk[3]= 250 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)   
        ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CustPP.ProdPack ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CustPP.ProdPack WITH FRAME sel.
      END.
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
        FIND CustPP WHERE recid(CustPP) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CustPP THEN
              ASSIGN FIRSTrow = i memory = recid(CustPP).
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
           IF NOT AVAILABLE CustPP THEN DO:
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
                rtab[1] = recid(CustPP)
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
           IF NOT AVAILABLE CustPP THEN DO:
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
              rtab[FRAME-DOWN] = recid(CustPP).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CustPP WHERE recid(CustPP) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CustPP THEN DO:
           memory = recid(CustPP).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CustPP THEN memory = recid(CustPP).
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
           FIND CustPP WHERE recid(CustPP) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ProdPack WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF ProdPack ENTERED THEN DO:
          FIND FIRST CustPP WHERE 
                     CustPP.ProdPack >= ProdPack AND 
                     CustPP.CustNum = CustNum 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CustPP THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some custpp/PpId was found */
          ASSIGN order = 1 memory = recid(CustPP) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO TRANS:  /* other customers  */
       RUN local-find-this(FALSE).                                        
       RUN Mc/custpp(CustPP.ProdPack).

       ufkey = TRUE.
       NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* Package Contains */
       RUN local-find-this(FALSE).                                        

       RUN Mc/ppcomp(CustPP.ProdPack). 

       ufkey = TRUE.
       NEXT LOOP.
     END.


     IF LOOKUP(nap,"4,F4") > 0 THEN DO TRANS: /* memo */
       FIND CustPP WHERE RECID(CustPP) = rtab[FRAME-LINE(sel)]
       NO-LOCK NO-ERROR.
       RUN Mc/memo(INPUT CustPP.CustNum,
                INPUT "CUSTPP",
                INPUT STRING(CustPP.CustPP),
                INPUT "Customer Prodpack").
       ufkey = TRUE.
       NEXT.
     END.



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
       CustPP.ProdPack. 

       RUN local-find-NEXT.
       IF AVAILABLE CustPP THEN memory = recid(CustPP).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CustPP THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(CustPP).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CustPP.ProdPack.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustPP).

           DELETE CustPP.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CustPP
           WHERE CustPP.ProdPack = ProdPack) THEN DO:
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
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CustPP.ProdPack.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustPP).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustPP).

       RUN local-disp-row.
       xrecid = recid(CustPP).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(CustPP) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(CustPP) must-print = TRUE.
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
      FIND CustPP WHERE recid(CustPP) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CustPP WHERE recid(CustPP) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CustPP
       WHERE CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CustPP
       WHERE CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CustPP
       WHERE CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CustPP
       WHERE CustPP.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CustPP.ProdPack
       ProdPack.PPName
       CustPP.ValidFrom
       CustPP.Salesman
       CustPP.Reseller


       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND ProdPack where
            ProdPack.ProdPack    = CustPP.ProdPack    NO-LOCK NO-ERROR.
       FIND Salesman WHERE 
            Salesman.Salesman = CustPP.Salesman NO-LOCK NO-ERROR.
       FIND Reseller   WHERE 
            Reseller.Reseller   = CustPP.Reseller NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
        CustPP.CustPP
        ProdPack.PPName WHEN      AVAIL ProdPack
        Salesman.SmName WHEN     AVAIL Salesman
        "!! UNKNOWN !!"  WHEN NOT AVAIL Salesman    @ Salesman.SmName
        Reseller.RsName   WHEN     AVAIL Reseller
        "!! UNKNOWN !!"  WHEN NOT AVAIL Reseller AND 
                              CustPP.Reseller NE ""  @ Reseller.RsName
        CustPP.ValidFrom
        CustPP.ValidTo
        CustPP.Salesman
        CustPP.Reseller
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:

         UPDATE

         CustPP.ValidFrom
         CustPP.ValidTo
         CustPP.Salesman
         CustPP.Reseller
         WITH FRAME lis EDITING:
            READKEY.
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                   PAUSE 0.

               IF FRAME-FIELD = "ContDate" THEN DO:
                  IF INPUT CustPP.ValidFrom = ? THEN DO:
                     BELL.
                     MESSAGE "Contract Date is mandatory !".
                     NEXT.
                  END.
               END.

               IF FRAME-FIELD = "ExpDate" THEN DO:
                  IF INPUT CustPP.ValidFrom NE ? THEN DO:
                     IF INPUT CustPP.ValidTo < INPUT CustPP.ValidFrom THEN DO:
                        BELL.
                        MESSAGE 
                        "ExCommPaymDate must not be earlier than ValidFrom !".
                        NEXT.
                     END.   
                  END.
               END.


               ELSE IF FRAME-FIELD = "salesman" THEN DO:
                  /* Salesman code is mandatory */
                  FIND Salesman WHERE Salesman.Salesman = INPUT CustPP.Salesman
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL Salesman THEN DO:
                     BELL.
                     MESSAGE "Unknown Salesman !".
                     NEXT.
                  END.
                  DISP Salesman.SmName.
               END.

               ELSE IF FRAME-FIELD = "reseller" THEN DO:
                  /* Salesman code is NOT mandatory */

                  IF INPUT CustPP.Reseller = "" THEN DO:
                     DISP "" @ Reseller.RsName.
                  END.
                  ELSE DO:
                     /* check validity of given Reseller */

                     FIND Reseller WHERE Reseller.Reseller = 
                                     INPUT CustPP.Reseller
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL Reseller THEN DO:
                        BELL.
                        MESSAGE "Unknown reseller !".
                        NEXT.
                     END.
                     DISP Reseller.RsName.    
                  END. /* Reseller was entered */   
               END.
            END.
            APPLY LASTKEY.
         END. /* EDITING */
         LEAVE.
      END.
      ELSE PAUSE.  
   END.
END PROCEDURE.

