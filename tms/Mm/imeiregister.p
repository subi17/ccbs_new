/* ----------------------------------------------------------------------
  MODULE .......: IMEIRegister
  TASK .........: UPDATEs table IMEIRegister
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.03.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable IMEIRegister

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'IMEIRegister'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhIMEIRegister AS HANDLE NO-UNDO.
   lhIMEIRegister = BUFFER IMEIRegister:HANDLE.
   RUN StarEventInitialize(lhIMEIRegister).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhIMEIRegister).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcIMEI       AS CHAR                   NO-UNDO.
DEF VAR lcBillCode   AS CHAR                   NO-UNDO.
DEF VAR lcBIName     AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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


FORM
    IMEIRegister.IMEI   
    IMEIRegister.BillCode 
    lcBIName  FORMAT "X(20)" COLUMN-LABEL "Name"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  IMEI " + "  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    IMEIRegister.Brand            COLON 18
    IMEIRegister.IMEI             COLON 18
    IMEIRegister.BillCode         COLON 18
       lcBIName NO-LABEL FORMAT "X(30)" 
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "IMEI:"  lcIMEI FORMAT "X(17)" 
    HELP "Enter IMEI"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND IMEI "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "Brand ......:" lcBrand skip
    "Billing Item:" lcBillCode FORMAT "X(16)" 
    HELP "Enter billing item code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Billing Item "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE IMEIRegister THEN ASSIGN
   Memory       = recid(IMEIRegister)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No IMEI Register available" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a IMEIRegister  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ IMEIRegister.Brand.

           PROMPT-FOR IMEIRegister.IMEI WITH FRAME lis.
           IF INPUT IMEIRegister.IMEI = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST IMEIRegister WHERE 
                             IMEIRegister.Brand     = lcBrand AND
                             IMEIRegister.IMEI = INPUT IMEIRegister.IMEI)
           THEN DO:
              MESSAGE "IMEI already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           CREATE IMEIRegister.
           ASSIGN 
              IMEIRegister.Brand = lcBrand
              IMEIRegister.IMEI  = INPUT FRAME lis IMEIRegister.IMEI.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhIMEIRegister).

           ASSIGN
           Memory = recid(IMEIRegister)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE IMEIRegister THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND IMEIRegister WHERE recid(IMEIRegister) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE IMEIRegister THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(IMEIRegister).
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
        ufk   = 0
        ufk[1]= 135
        ufk[2]= 703
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW IMEIRegister.IMEI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) IMEIRegister.IMEI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW IMEIRegister.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) IMEIRegister.BillCode WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND IMEIRegister WHERE recid(IMEIRegister) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE IMEIRegister THEN
              ASSIGN FIRSTrow = i Memory = recid(IMEIRegister).
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
           IF NOT AVAILABLE IMEIRegister THEN DO:
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
                rtab[1] = recid(IMEIRegister)
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
           IF NOT AVAILABLE IMEIRegister THEN DO:
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
              rtab[FRAME-DOWN] = recid(IMEIRegister).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND IMEIRegister WHERE recid(IMEIRegister) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE IMEIRegister THEN DO:
           Memory = recid(IMEIRegister).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE IMEIRegister THEN Memory = recid(IMEIRegister).
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
           FIND IMEIRegister WHERE recid(IMEIRegister) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcIMEI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcIMEI > "" THEN DO:
          FIND FIRST IMEIRegister USE-INDEX IMEI WHERE 
                     IMEIRegister.Brand = lcBrand AND
                     IMEIRegister.IMEI >= lcIMEI
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       SET lcBrand WHEN gcAllBrand 
           lcBillCode WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       IF lcBillCode > "" THEN DO:
          FIND FIRST IMEIRegister USE-INDEX BillCode WHERE 
                     IMEIRegister.Brand = lcBrand AND
                     IMEIRegister.BillCode >= lcBillCode
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       IMEIRegister.IMEI IMEIRegister.BillCode.
       
       RUN local-find-NEXT.
       IF AVAILABLE IMEIRegister THEN Memory = recid(IMEIRegister).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE IMEIRegister THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(IMEIRegister).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       IMEIRegister.IMEI IMEIRegister.BillCode.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhIMEIRegister).

           DELETE IMEIRegister.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE IMEIRegister THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIMEIRegister).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY IMEIRegister.IMEI.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhIMEIRegister).

       RUN local-disp-row.
       xrecid = recid(IMEIRegister).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(IMEIRegister) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(IMEIRegister) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND IMEIRegister WHERE recid(IMEIRegister) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND IMEIRegister WHERE recid(IMEIRegister) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST IMEIRegister USE-INDEX IMEI WHERE 
                 IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST IMEIRegister USE-INDEX BillCode WHERE 
                 IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST IMEIRegister USE-INDEX IMEI WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST IMEIRegister USE-INDEX BillCode WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT IMEIRegister USE-INDEX IMEI WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT IMEIRegister USE-INDEX BillCode WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV IMEIRegister USE-INDEX IMEI WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV IMEIRegister USE-INDEX BillCode WHERE 
                IMEIRegister.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       IMEIRegister.IMEI 
       IMEIRegister.BillCode
       lcBIName
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcBIName = "".
   FIND FIRST BillItem WHERE
              BillItem.Brand = gcBrand AND
              BillItem.BillCode = IMEIRegister.BillCode NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llDispMenu   AS LOG  NO-UNDO.
   DEF VAR lcNewValue   AS CHAR NO-UNDO.
   DEF VAR ldtNewDate   AS DATE NO-UNDO.

   llDispMenu = NOT NEW IMEIRegister.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         IMEIRegister.Brand          
         IMEIRegister.IMEI 
         IMEIRegister.BillCode           
         lcBIName
      WITH FRAME lis.

      IF llDispMenu THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.
      END.
      ELSE ASSIGN toimi      = 1
                  llDispMenu = TRUE.
                  
      IF toimi = 1 THEN DO TRANS:
         RUN pUpdate.
      END.
            
      ELSE IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

PROCEDURE pUpdate:

   FIND CURRENT IMEIRegister EXCLUSIVE-LOCK.
      
   ehto = 9.
   RUN Syst/ufkey.p.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      UPDATE
         IMEIRegister.BillCode  
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.
            
            IF FRAME-FIELD = "BillCode" THEN DO:
               FIND FIRST BillItem WHERE 
                          BillItem.Brand = gcBrand AND
                          BillItem.BillCode = INPUT IMEIRegister.BillCode
                  NO-LOCK NO-ERROR.
               IF NOT AVAILABLE BillItem THEN DO:
                  MESSAGE "Unknown billing item"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
            
         END.
            
         APPLY LASTKEY.
      END.
   
      LEAVE.
   END.

END PROCEDURE.

