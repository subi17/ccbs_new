/* ----------------------------------------------------------------------
  MODULE .......: premium_numbers_info.p
  TASK .........: Handle Premium Numbers Information
  APPLICATION ..: TMS CUI
  AUTHOR .......: Vikas
  CREATED ......: 15.11.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable PremiumNumber

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PremiumNumber'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPremiumNumber AS HANDLE NO-UNDO.
   lhPremiumNumber = BUFFER PremiumNumber:HANDLE.
   RUN StarEventInitialize(lhPremiumNumber).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhPremiumNumber).
   END.

END.

DEF shared VAR siirto AS CHAR.

DEF VAR xrecid        AS RECID                  NO-UNDO  INIT ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  INIT 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  INIT 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  INIT 15.
DEF VAR order         AS INT                    NO-UNDO  INIT 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  INIT 1.
DEF VAR ufkey         AS LOG                    NO-UNDO  INIT TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  INIT 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcField       AS CHAR                   NO-UNDO. 
DEF VAR lcCode        AS CHAR                   NO-UNDO.
DEF VAR lcBNumberPreFix AS CHAR                 NO-UNDO.
DEF VAR ldValidFrom   AS DATE                   NO-UNDO FORMAT "99-99-99".

DEF BUFFER bPremiumNumber FOR PremiumNumber.

FORM
    PremiumNumber.BNumberPreFix FORMAT "X(9)"
    PremiumNumber.OperatorName  FORMAT "X(40)"
    PremiumNumber.ValidFrom
    PremiumNumber.ValidTo
WITH ROW FrmRow width 80 centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " +
       " Premium Numbers Information "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    PremiumNumber.Brand          COLON 20
    PremiumNumber.BNumberPreFix  COLON 20 FORMAT "X(9)"
    PremiumNumber.OperatorName   COLON 20 FORMAT "X(256)" VIEW-AS EDITOR SIZE 40 BY 3
    PremiumNumber.ValidFrom      COLON 20
    PremiumNumber.ValidTo        COLON 20
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand        : " lcBrand skip
    "BNumberPreFix: " lcBNumberPreFix FORMAT "X(9)" 
    HELP "Enter BNumberPreFix"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BNumberPreFix "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

IF gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN local-find-first.
       
IF AVAILABLE PremiumNumber THEN ASSIGN
   Memory       = recid(PremiumNumber)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Premium Numbers Information!"
         VIEW-AS ALERT-BOX.
      RETURN.
   END. /* IF lcRight NE "RW" THEN DO: */

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

IF must-add THEN DO:  /* Add a PremiumNumber */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ PremiumNumber.Brand.

           PROMPT-FOR PremiumNumber.BNumberPreFix
                      PremiumNumber.ValidFrom WITH FRAME lis.
           IF INPUT PremiumNumber.BNumberPreFix = "" OR
              INPUT PremiumNumber.ValidFrom = "" THEN UNDO, LEAVE ADD-ROW.

           IF LENGTH(INPUT PremiumNumber.BNumberPreFix) < 5 THEN DO:
              MESSAGE "BNumberPreFix should be minimum 5 digits"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END. /* IF LENGTH(INPUT PremiumNumber.BNumberPreFix) < 5 THEN DO: */

           ldValidFrom = DATE(INPUT PremiumNumber.ValidFrom).

           IF ldValidFrom < TODAY THEN DO:
              MESSAGE "ValidFrom date could not be past date"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END. /* IF ldValidFrom < TODAY THEN DO: */

           IF CAN-FIND(FIRST PremiumNumber USE-INDEX BNumberPrefix WHERE
                             PremiumNumber.Brand = lcBrand AND
                             PremiumNumber.BNumberPreFix = INPUT PremiumNumber.BNumberPreFix)
           THEN DO:
              MESSAGE "Premium number information already exists for " +
                      "BNumberPreFix: " + INPUT PremiumNumber.BNumberPreFix SKIP
                      "All existing active BNumberPreFix " +
                      INPUT PremiumNumber.BNumberPreFix + " will be closed." SKIP
                      "ARE YOU SURE YOU WANT TO CONTINUE (Y/N) ? "
              VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

              IF NOT ok THEN NEXT.

              FOR EACH bPremiumNumber EXCLUSIVE-LOCK USE-INDEX BNumberPrefix WHERE
                       bPremiumNumber.Brand = lcBrand AND
                       bPremiumNumber.BNumberPreFix = INPUT PremiumNumber.BNumberPreFix:
                 IF bPremiumNumber.ValidTo = (ldValidFrom - 1) THEN NEXT.
                 bPremiumNumber.ValidTo = (ldValidFrom - 1).
              END. /* FOR EACH bPremiumNumber EXCLUSIVE-LOCK */
           END. /* IF CAN-FIND(FIRST PremiumNumber USE-INDEX BNumberPrefix */
            
           CREATE PremiumNumber.
           ASSIGN
              PremiumNumber.Brand         = lcBrand
              PremiumNumber.BNumberPreFix = INPUT FRAME lis PremiumNumber.BNumberPreFix
              PremiumNumber.ValidFrom     = ldValidFrom.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPremiumNumber).

           ASSIGN
           Memory = recid(PremiumNumber)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PremiumNumber THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PremiumNumber WHERE recid(PremiumNumber) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PremiumNumber THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PremiumNumber).
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
        ufk    = 0
        ufk[1] = 816
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7] = 0  
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.

        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW PremiumNumber.BNumberPreFix ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) PremiumNumber.BNumberPreFix WITH FRAME sel.
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
        FIND PremiumNumber WHERE recid(PremiumNumber) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PremiumNumber THEN
              ASSIGN FIRSTrow = i Memory = recid(PremiumNumber).
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
           IF NOT AVAILABLE PremiumNumber THEN DO:
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
                rtab[1] = recid(PremiumNumber)
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
           IF NOT AVAILABLE PremiumNumber THEN DO:
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
              rtab[FRAME-DOWN] = recid(PremiumNumber).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PremiumNumber WHERE recid(PremiumNumber) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PremiumNumber THEN DO:
           Memory = recid(PremiumNumber).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PremiumNumber THEN Memory = recid(PremiumNumber).
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
           FIND PremiumNumber WHERE recid(PremiumNumber) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcBNumberPreFix WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcBNumberPreFix > "" THEN DO:
          FIND FIRST PremiumNumber WHERE 
                     PremiumNumber.Brand = lcBrand AND
                     PremiumNumber.BNumberPreFix >= lcBNumberPreFix
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

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
       PremiumNumber.BNumberPreFix PremiumNumber.OperatorName.
        
       RUN local-find-NEXT.
       IF AVAILABLE PremiumNumber THEN Memory = recid(PremiumNumber).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PremiumNumber THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETED */
             Memory = recid(PremiumNumber).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO DELETE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PremiumNumber.BNumberPreFix PremiumNumber.OperatorName.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPremiumNumber).

           DELETE PremiumNumber.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE PremiumNumber THEN DO:
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
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPremiumNumber).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PremiumNumber.BNumberPreFix.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPremiumNumber).

       RUN local-disp-row.
       xrecid = recid(PremiumNumber).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PremiumNumber) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PremiumNumber) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

IF gcHelpParam > "" THEN DO:
   FIND FIRST PremiumNumber WHERE recid(PremiumNumber) = xRecid NO-LOCK.
   siirto = PremiumNumber.BNumberPreFix.
END.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PremiumNumber WHERE recid(PremiumNumber) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PremiumNumber WHERE recid(PremiumNumber) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST PremiumNumber USE-INDEX BNumberPrefix WHERE
                 PremiumNumber.Brand = lcBrand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST PremiumNumber USE-INDEX BNumberPrefix WHERE
                PremiumNumber.Brand = lcBrand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT PremiumNumber USE-INDEX BNumberPrefix WHERE
                PremiumNumber.Brand = lcBrand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV PremiumNumber USE-INDEX BNumberPrefix WHERE
                PremiumNumber.Brand = lcBrand
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.

    DISP PremiumNumber.BNumberPreFix
         PremiumNumber.OperatorName
         PremiumNumber.ValidFrom
         PremiumNumber.ValidTo
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

  MaintMenu:
  REPEAT ON ENDKEY UNDO, LEAVE:

     RUN local-find-others.

     DISP PremiumNumber.Brand
          PremiumNumber.BNumberPreFix
          PremiumNumber.OperatorName
          PremiumNumber.ValidFrom
          PremiumNumber.ValidTo WITH FRAME lis.

     IF NEW PremiumNumber THEN toimi = 1.
     ELSE DO:
        ASSIGN 
            ufk    = 0
            ufk[1] = 7   WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
        RUN Syst/ufkey.
     END.

     IF toimi = 1 THEN 
     REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT PremiumNumber EXCLUSIVE-LOCK.
      
         ehto = 9.
         RUN Syst/ufkey.

         IF NEW PremiumNumber THEN
         UPDATE
            PremiumNumber.OperatorName
            PremiumNumber.ValidTo
            WITH FRAME lis EDITING:

            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
            END.
            
            APPLY LASTKEY.
         END.
         ELSE
         UPDATE
            PremiumNumber.OperatorName
            PremiumNumber.ValidFrom
            PremiumNumber.ValidTo
            WITH FRAME lis EDITING:

            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
            END.
            
            APPLY LASTKEY.
         END.

         IF PremiumNumber.ValidTo < PremiumNumber.ValidFrom THEN DO:
            MESSAGE "ValidTo could not be less than ValidFrom"
            VIEW-AS ALERT-BOX ERROR.
            UNDO,RETRY.
         END. /* IF PremiumNumber.ValidTo < PremiumNumber.ValidFrom THEN */

         IF NEW PremiumNumber THEN LEAVE MaintMenu.
         LEAVE.
     END.
     ELSE IF toimi = 8 THEN LEAVE.

  END.

END PROCEDURE.


