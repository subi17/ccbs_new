/* ----------------------------------------------------------------------
  MODULE .......: IMSI.P
  TASK .........: UPDATE IMSI data
  APPLICATION ..: nn
  AUTHOR .......: nn
  CREATED ......: 31-05-99
  CHANGED ......: 16.05.99 pt pin2, puk2
                  03.08.99 pt mu-Lname, mu-Fname
                  07.10.99 jp urights added  
                  02.12.99 pt IMSI.CustNum MUST NOT be changed
                  11.10.02 jr Removed BillLevel
                  04.11.02 jr Eventlog
                  03.03.03 tk tokens
                  09.09.03 jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'imsi'}
{Syst/eventval.i} 
{Func/func.p}

DEF INPUT PARAMETER ICC LIKE SIM.ICC NO-UNDO.

DEF /* NEW */ SHARED VAR siirto AS CHAR.

DEF VAR IMSI  LIKE IMSI.IMSI  NO-UNDO.
DEF VAR CustNum LIKE IMSI.CustNum NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR pr-custno    AS i                      NO-UNDO.

DEF VAR new_imsi     AS LOG                    NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhIMSI AS HANDLE NO-UNDO.
   lhIMSI = BUFFER IMSI:HANDLE.
   RUN StarEventInitialize(lhIMSI).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhIMSI).
   END.
END.

form  /* Note ! FRAME SEL has exceptionally 2 rows / record */
    IMSI.IMSI      /* COLUMN-LABEL FORMAT */
    IMSI.CustNum     /* COLUMN-LABEL FORMAT */
    Customer.CustName  
    IMSI.PIN1
  SKIP
    IMSI.PIN2        AT 70
WITH centered OVERLAY 3 DOWN ROW 2
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc)  " IMSI numbers  on SIM(ICC) " + ICC + " "
    FRAME sel.

form
    IMSI.IMSI     /* LABEL FORMAT */
    IMSI.CustNum    /* LABEL FORMAT */
    Customer.CustName
    BillTarg.BillTarget
    IMSI.PIN1
    IMSI.PIN2
    IMSI.PUK1
    IMSI.PUK2
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek IMSI number  BY  IMSI */
    IMSI
    help "Enter IMSI No."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND IMSI No. "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek IMSI number  BY CustNum */
    CustNum
    help "Enter CustoNo"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CUST No "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By IMSI No,By Cust No.,By 3, By 4".

FIND SIM where SIM.ICC = ICC  no-lock.
FIND Customer of SIM WHERE customer.Brand = gcBrand no-lock no-error.

FIND FIRST IMSI where IMSI.ICC = ICC 
NO-LOCK NO-ERROR.
IF AVAILABLE IMSI THEN ASSIGN
   Memory       = recid(IMSI)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No IMSI records available !" VIEW-AS ALERT-BOX.
      HIDE FRAME lis NO-PAUSE.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       IF maxOrder NE 1 THEN
       PUT SCREEN ROW 12 col 30 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a IMSI  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           MESSAGE 
           "Double SIM card not possible yet"
           VIEW-AS ALERT-BOX TITLE "INFORMATION".
           LEAVE.
/*
           RUN local-UPDATE-record.

           ASSIGN
           Memory = recid(IMSI)
           xrecid = Memory. */
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST IMSI where IMSI.ICC = ICC 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE IMSI THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND IMSI WHERE recid(IMSI) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE IMSI THEN DO:
              RUN local-find-others.
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(IMSI).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 35  ufk[2]= 702 ufk[3]= 238 ufk[4]= 201
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW IMSI.IMSI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) IMSI.IMSI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW IMSI.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) IMSI.CustNum WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND IMSI WHERE recid(IMSI) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE IMSI THEN
              ASSIGN FIRSTrow = i Memory = recid(IMSI).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE IMSI THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(IMSI)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND IMSI WHERE recid(IMSI) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE IMSI THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(IMSI).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND IMSI WHERE recid(IMSI) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE IMSI THEN DO:
           Memory = recid(IMSI).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE IMSI THEN Memory = recid(IMSI).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND IMSI WHERE recid(IMSI) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       IMSI = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE IMSI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF IMSI <> "" THEN DO:
          FIND FIRST IMSI WHERE IMSI.IMSI >= IMSI  AND
          IMSI.ICC =  SIM.ICC NO-LOCK NO-ERROR.
          IF NOT AVAILABLE IMSI THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some IMSI/IMSI was found */
          ASSIGN order = 1 Memory = recid(IMSI) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       CustNum = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum <> 0 THEN DO:
          FIND FIRST IMSI WHERE IMSI.CustNum >= CustNum  AND
          IMSI.ICC = icc
          USE-INDEX CustNum NO-LOCK NO-ERROR.
          IF NOT AVAILABLE IMSI THEN DO:
             bell. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some IMSI/CustNum was found */
          ASSIGN order = 2 Memory = recid(IMSI) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:  /* SUBSCRIBER */
       FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE] NO-LOCK.
       ufkey = TRUE.

       IF IMSI.UserSeq = 0 THEN DO:
          MESSAGE
          "There is no Subscriber Associated" SKIP
          "With This IMSI No."
          VIEW-AS ALERT-BOX error.
       END.   
       ELSE RUN Mm/shmobu(IMSI.UserSeq).
       ufkey = TRUE.
       NEXT LOOP.
     END.  

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* MSISDN */
       FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE] NO-LOCK.
       ufkey = TRUE.
       /* show ALL MSISDN no.s associated TO this IMSI no. */
       RUN Mm/msisdni(IMSI.IMSI).
       NEXT LOOP.
     END.  

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE] NO-LOCK.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       IMSI.IMSI IMSI.CustNum /* sd */.

       RUN local-find-NEXT.
       IF AVAILABLE IMSI THEN Memory = recid(IMSI).
       ELSE DO:
          /* read back the record that is TO be  removed */
          FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE] NO-LOCK.
          /* THEN previous record */

          IF AVAILABLE IMSI THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(IMSI).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND IMSI WHERE recid(IMSI) = rtab[FRAME-LINE]
       EXCLUSIVE-LOCK.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       IMSI.IMSI IMSI.CustNum /* sd */.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhIMSI).
           DELETE IMSI.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST IMSI  where IMSI.ICC = ICC)
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND IMSI WHERE recid(IMSI) = rtab[FRAME-line(sel)]
       EXCLUSIVE-LOCK.
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY IMSI.IMSI.


       ASSIGN 
        pr-custno = IMSI.CustNum
        new_imsi  = FALSE.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIMSI).
       RUN local-UPDATE-record.
       HIDE FRAME lis NO-PAUSE.

       IF LOOKUP(KEYFUNCTION(LASTKEY),"END-ERROR,ENDKEY") > 0 OR
       KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.

       /* IF customer number OR billing Level were changed ... */
       IF IMSI.CustNum NE pr-custno THEN DO:

          MESSAGE 
          "New Customer No. and/or Billing Level" SKIP
          "were UPDATEd onto MSISDN"     SKIP
          "records also."
          VIEW-AS ALERT-BOX TITLE " CUSTOMER DATA CHANGED ".
       END.

       RUN local-disp-row.
       xrecid = recid(IMSI).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(IMSI) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(IMSI) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST IMSI
       where IMSI.ICC = ICC NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST IMSI where IMSI.ICC = ICC        USE-INDEX CustNum
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST IMSI        
       where IMSI.ICC = ICC NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST IMSI where IMSI.ICC = ICC        USE-INDEX CustNum
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT IMSI
       where IMSI.ICC = ICC NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT IMSI where IMSI.ICC = ICC 
       USE-INDEX CustNum
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev IMSI
       where IMSI.ICC = ICC NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev IMSI where IMSI.ICC = icc
       USE-INDEX CustNum
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       FIND  Customer of IMSI no-lock no-error.

       DISPLAY
       IMSI.IMSI
       IMSI.CustNum
       DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                        BUFFER Customer) when  AVAIL Customer
       "" when NOT AVAIL Customer @ Customer.CustName
       IMSI.PIN1
       IMSI.PIN2
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others:
       FIND Customer WHERE  Customer.CustNum =  IMSI.CustNum NO-LOCK NO-ERROR.


       FIND FIRST MobSub WHERE 
                  MobSub.Brand = gcBrand AND 
                  MobSub.IMSI = IMSI.IMSI  NO-LOCK NO-ERROR.

       FIND BillTarg OF Customer WHERE
            BillTarg.BillTarget = Mobsub.BillTarget
       NO-LOCK NO-ERROR.     

END PROCEDURE.


PROCEDURE local-UPDATE-record:
    REPEAT WITH FRAME LIS:
       RUN local-find-others.
       DISP
          IMSI.CustNum
          DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                           BUFFER Customer) WHEN  AVAIL Customer
          BillTarg.BillTarget   WHEN  AVAIL BillTarg
          IMSI.PIN1
          IMSI.PIN2
          IMSI.PUK1
          IMSI.PUK2

       WITH FRAME lis.

       IF lcRight = "RW" THEN DO:

          UPDATE
             IMSI.CustNum    WHEN NEW IMSI
             IMSI.PIN1
             IMSI.PIN2
             IMSI.PUK1
             IMSI.PUK2
          WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME LIS:
                PAUSE 0.
                IF FRAME-FIELD = "CustNum" THEN DO:
                   IF INPUT FRAME lis IMSI.CustNum NE 0 THEN DO:
                      FIND Customer WHERE 
                           Customer.CustNum = INPUT FRAME lis IMSI.CustNum 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL Customer THEN DO:
                         BELL.
                         MESSAGE "Unknown Customer !".
                         NEXT.
                      END.
                      DISP Customer.CustName WITH FRAME lis.
                   END.  /* custNo was entered */
                   ELSE DISP "" @  Customer.CustName WITH  FRAME LIS.
                END.

             END.

             APPLY LASTKEY.
          END. /* EDITING */
          IF new_imsi AND
             llDoEvent THEN RUN StarEventMakeCreateEvent(lhIMSI).

          IF NOT new_imsi AND
             llDoEvent THEN RUN StarEventMakeModifyEvent(lhIMSI).

          new_imsi = FALSE.
       LEAVE.
       END.
       ELSE PAUSE.
    END. /* repeat */
END PROCEDURE.

