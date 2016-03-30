/*----------------------------------------------------------------------
  MODULE .......: mnpoperator.p
  TASK .........: Browse and add MNPOperator
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 07/2010 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MNPOperator'} 
{Func/timestamp.i}
{Func/ftaxdata.i}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMNPOperator AS HANDLE NO-UNDO.
   lhMNPOperator = BUFFER MNPOperator:HANDLE.
   RUN StarEventInitialize(lhMNPOperator).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhMNPOperator).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 2.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.

DEFINE BUFFER xxMNPOperator  FOR MNPOperator.
FORM
    MNPOperator.OperName format "x(20)"
    MNPOperator.OperCode format "x(4)" column-label "RRC"
    MNPOperator.Active
    MNPOperator.ICCPrefix format "x(10)"
/* MNPOperator.CancelAgreement FORMAT "Yes/No"   */
    MNPOperator.NRN
    MNPOperator.OperBrand
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " MNP OPERATORS "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    MNPOperator.OperCode label "Code (RRC)" 
    MNPOperator.OperName label "Name" format "x(40)"
    MNPOperator.OperBrand LABEL "Operator Brand"
/*    MNPOperator.CancelAgreement label "Cancel Agreement" */
    MNPOperator.ICCPrefix LABEL "ICC Prefix" format "x(10)"
    MNPOperator.NRN LABEL "NRN"
    MNPOperator.Active LABEL "Active"
WITH  OVERLAY ROW 4 centered 1 columns
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr with side-labels
    NO-LABELS 
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By OperName ,  By OperCode".

RUN local-find-first.
IF AVAILABLE MNPOperator THEN ASSIGN
   Memory       = recid(MNPOperator)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No MNPOperator available!" SKIP
      "Do You want to add one?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
   IF ok THEN must-add = TRUE.
   ELSE RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MNPOperator  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:

      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           CREATE MNPOperator.
           MNPOperator.Brand = gcBrand.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMNPOperator).

           ASSIGN
              Memory = recid(MNPOperator)
              xrecid = Memory.
        
           LEAVE ADD-ROW.
      
        END.
      
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE MNPOperator THEN LEAVE LOOP.
      NEXT LOOP.
      
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPOperator THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPOperator).
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
        ufk = 0
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0
        ufk[5]=  (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]=  (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MNPOperator.Opername {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPOperator.Opername WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
        CHOOSE ROW MNPOperator.OperCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPOperator.OperCode WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPOperator THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPOperator).
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
           IF NOT AVAILABLE MNPOperator THEN DO:
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
                rtab[1] = recid(MNPOperator)
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
           IF NOT AVAILABLE MNPOperator THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPOperator).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE MNPOperator THEN DO:
            Memory = recid(MNPOperator).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE MNPOperator THEN Memory = recid(MNPOperator).
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
            FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
     
      ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
     
         must-add = TRUE.
        
         NEXT LOOP.
        
      END. /* ADD NEW */
     
           
      ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO TRANS:

         RUN local-find-this(FALSE).
        
         ok = FALSE.
         COLOR DISPLAY value(ctc)
            MNPOperator.Opername
            MNPOperator.OperCode.
        
         RUN local-find-next.
         IF AVAIL MNPOperator THEN
            memory = RECID(MNPOperator).
         ELSE DO:
            RUN local-find-this(FALSE).
            RUN local-find-prev.
            IF AVAILABLE MNPOperator THEN ASSIGN
               memory = RECID(MNPOperator)
               delrow = delrow - 1.
         END.
        
         RUN local-find-this(TRUE).

         MESSAGE
            "ARE YOU SURE YOU WANT TO DELETE THIS ROW?" 
         UPDATE ok.

         COLOR DISPLAY value(ccc)
            MNPOperator.Opername
            MNPOperator.OperCode.
         
         IF OK THEN DO:
            
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMNPOperator).
            DELETE MNPOperator.

            RUN local-find-first.

         END.
         ELSE DO:
           
            delrow = 0.
           
            RUN local-find-this(false).
        
            memory = RECID(MNPOperator).
         
         END.

         IF NOT AVAIL MNPOperator THEN
            LEAVE LOOP.
        
         must-print = TRUE.
        
         NEXT LOOP.
        
      END. /* DELETE */
           
      ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
      REPEAT WITH FRAME lis TRANSACTION
      ON ENDKEY UNDO, LEAVE:

         /* change */
         RUN local-find-this(TRUE).

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMNPOperator).

         ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
         cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
      
         RUN local-UPDATE-record.
         HIDE FRAME lis NO-PAUSE.

         /* IF  User Wanted TO Cancel this Change TRANSACTION */
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMNPOperator).

         RUN local-disp-row.
         xrecid = recid(MNPOperator).
     
         LEAVE.
   
      END.
      
      ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(MNPOperator) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(MNPOperator) must-print = TRUE.
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
      FIND MNPOperator WHERE recid(MNPOperator) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
   ELSE
      FIND MNPOperator WHERE recid(MNPOperator) = rtab[frame-line(sel)] 
      NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST MNPOperator WHERE
                 MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperName NO-ERROR.
   ELSE IF order = 2 THEN
      FIND FIRST MNPOperator WHERE
                 MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperCode NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand 
      NO-LOCK USE-INDEX OperName NO-ERROR.
   ELSE IF order = 2 THEN
      FIND LAST MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperCode NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperName NO-ERROR.
   ELSE IF order = 2 THEN
      FIND NEXT MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperCode NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperName NO-ERROR.
   ELSE IF order = 2 THEN
      FIND PREV MNPOperator WHERE
                MNPOperator.Brand EQ gcBrand
      NO-LOCK USE-INDEX OperCode NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      MNPOperator.OperCode
      MNPOperator.NRN
      MNPOperator.OperName
      MNPOperator.ICCPrefix
      MNPOperator.Active
      MNPOperator.OperBrand
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bMNPOperator FOR MNPOperator.

   UPDATE
      MNPOperator.OperCode
      MNPOperator.OperName
      MNPOperator.OperBrand
      MNPOperator.ICCPrefix
      MNPOperator.NRN
      MNPOperator.Active
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).

      APPLY LASTKEY.

   END.

   IF MNPOperator.Active AND
      MNPOperator.OperBrand EQ "" THEN DO:

      FIND FIRST bMNPOperator NO-LOCK WHERE
                 bMNPOperator.Brand = MNPOperator.Brand AND
                 bMNPOperator.OperCode = MNPOperator.OperCode AND
                 bMNPOperator.Active = TRUE AND
           ROWID(bMNPOperator) NE ROWID(MNPOperator) NO-ERROR.

      IF AVAIL bMNPOperator THEN DO: 
         MESSAGE "The same RRC is already used with operator"
            bMNPOperator.OperName CHR(10)
            "Operator Brand should be defined"
         VIEW-AS ALERT-BOX TITLE "WARNING".
      END.
   END.

END PROCEDURE.
