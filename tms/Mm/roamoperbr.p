/* ----------------------------------------------------------------------
 MODULE .......: ROAMOPERBR.P
  TASK .........: Browse and add RoamOpers
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 16.02.07
  CHANGED ......: 13.03.07 kl TestFileSeq, Production fields
                              PLMN update for CDRs
                  15.05.07 kl IMSI2 as "X(3)"
                  
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'RoamOper'}
{timestamp.i}
{xmlfunction.i}
{ftaxdata.i}
{timestamp.i}
{tmsconst.i}

{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRoamTariff AS HANDLE NO-UNDO.
   lhRoamTariff = BUFFER RoamTariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff ).
   
   DEFINE VARIABLE lhRoamOper AS HANDLE NO-UNDO.
   lhRoamOper = BUFFER RoamOper:HANDLE.
   RUN StarEventInitialize(lhRoamOper).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRoamOper).
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
DEFINE VARIABLE lcIMSI       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcPLMN       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcProdCode   AS CHARACTER               NO-UNDO INIT "-,V,G,A".
DEFINE VARIABLE lcProdDisp   AS CHARACTER FORMAT "X(1)" NO-UNDO.
DEFINE VARIABLE llOK         AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE lcRoamGroup  LIKE RoamOper.RoamGroup    NO-UNDO.
DEFINE BUFFER   xxRoamOper   FOR RoamOper.


FORM
    RoamOper.IMSI  
    RoamOper.PLMN
    RoamOper.Country FORMAT "X(15)"  
    RoamOper.CommName    FORMAT "X(16)"
    lcRoamGroup
    lcProdDisp  COLUMN-LABEL "P"
    RoamOper.FileSeq 
    RoamOper.TestFileSeq COLUMN-LABEL "TestSeq"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " TOPUP PLMNS  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    "IMSI .....:" RoamOper.IMSI    SKIP
    "PLMN .....:" RoamOper.PLMN     SKIP
    "Country ..:" RoamOper.Country  SKIP
    "Name .....:" RoamOper.Name     FORMAT "X(20)" SKIP
    "Comm. Name:" RoamOper.CommName FORMAT "X(20)" SKIP
    "Production:" lcProdDisp
       HELP "'-'=NONE,'V'=VOICE,'G'=GPRS,'A'=ALL" SKIP
    "RoamGroup.:" lcRoamGroup   
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

FORM /* seek  RoamOper */
    lcIMSI
    HELP "Enter form code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND IMSI "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM /* seek  PLMN */
    lcPLMN
    HELP "Enter porting code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND PLMN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  , By Status , By 4".

RUN local-find-first.
IF AVAILABLE RoamOper THEN ASSIGN
   Memory       = recid(RoamOper)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No TopUp PLMN available!" SKIP
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

   IF must-add THEN DO:  /* Add a RoamOper  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:

      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           CREATE RoamOper.

           RUN local-UPDATE-record(TRUE).

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRoamOper).

           ASSIGN
              Memory = recid(RoamOper)
              xrecid = Memory.
        
           LEAVE ADD-ROW.
      
        END.
      
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RoamOper THEN LEAVE LOOP.
      NEXT LOOP.
      
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND RoamOper WHERE recid(RoamOper) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RoamOper THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RoamOper).
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
        ufk[1]= 1343  ufk[2]= 95 ufk[3]= 0
        ufk[5]= 5  WHEN lcRight = "RW"
        ufk[6]= 4  WHEN lcRight = "RW" 
        ufk[7]= 9006 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RoamOper.IMSI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RoamOper.Country WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW RoamOper.PLMN ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RoamOper.PLMN WITH FRAME sel.
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
        FIND RoamOper WHERE recid(RoamOper) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RoamOper THEN
              ASSIGN FIRSTrow = i Memory = recid(RoamOper).
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
           IF NOT AVAILABLE RoamOper THEN DO:
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
                rtab[1] = recid(RoamOper)
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
           IF NOT AVAILABLE RoamOper THEN DO:
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
              rtab[FRAME-DOWN] = recid(RoamOper).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RoamOper WHERE recid(RoamOper) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RoamOper THEN DO:
           Memory = recid(RoamOper).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RoamOper THEN Memory = recid(RoamOper).
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
           FIND RoamOper WHERE recid(RoamOper) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcIMSI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcIMSI ENTERED THEN DO:
          FIND FIRST RoamOper WHERE 
                     RoamOper.IMSI >= lcIMSI
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE RoamOper THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some RoamOper/RoamOper was found */
          ASSIGN order = 1 Memory = recid(RoamOper) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET lcPLMN WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcPLMN ENTERED THEN DO:
          FIND FIRST RoamOper WHERE 
                     RoamOper.PLMN BEGINS lcPLMN
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE RoamOper THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some RoamOper/PLMN was found */
          ASSIGN order = 2 Memory = recid(RoamOper) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
     
        must-add = TRUE.
        
        NEXT LOOP.
        
     END. /* ADD NEW */
         
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO TRANS:

        RUN local-find-this(FALSE).
        
        ok = FALSE. 
        
        COLOR DISPLAY value(ctc)
           RoamOper.Country
           RoamOper.PLMN
           RoamOper.IMSI 
           WITH FRAME sel.
        
        RUN local-find-next.
        IF AVAIL RoamOper THEN
           memory = RECID(RoamOper).
        ELSE DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF AVAILABLE RoamOper THEN ASSIGN
              memory = RECID(RoamOper)
              delrow = delrow - 1.
        END.
        
        RUN local-find-this(TRUE).
           
        MESSAGE
         "ARE YOU SURE YOU WANT TO DELETE THIS ROW?" 
        UPDATE ok.
        
        IF ok AND RoamOper.PLMN ne "" THEN DO:
           
           FIND FIRST xxRoamOper WHERE
               xxRoamOper.PLMN = RoamOper.PLMN AND
               RECID(xxRoamOper) NE RECID(RoamOper) NO-LOCK NO-ERROR.
           
           IF NOT AVAIL xxRoamOper THEN
              FIND FIRST RoamTariff WHERE 
                 RoamTariff.PriceList = RoamOper.PLMN AND
                 RoamTariff.ValidTo >= TODAY NO-LOCK NO-ERROR.
              
           IF AVAIL RoamTariff THEN DO:
              ok = false.
              MESSAGE 
               "ACTIVE ROAMING TARIFFS FOR THIS PLMN EXISTS!" SKIP
               "ARE YOU SURE YOU WANT TO DELETE THIS ROW AND DEACTIVATE RELATED TARIFFS?" 
              UPDATE ok VIEW-AS ALERT-BOX BUTTONS YES-NO.
           
              IF OK THEN DO:
                 FOR EACH RoamTariff WHERE
                    RoamTariff.PriceList = RoamOper.PLMN AND
                    RoamTariff.ValidTo >= TODAY EXCLUSIVE-LOCK:
                 
                    IF llDoEvent THEN RUN StarEventSetOldBuffer (lhRoamTariff).
                    RoamTariff.ValidTo = TODAY.
                   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRoamTariff).
                 
                 END.
              END.
           END.   
        END.   

        COLOR DISPLAY value(ccc)
           RoamOper.Country
           RoamOper.PLMN
           RoamOper.IMSI
           WITH FRAME sel.
       
        IF OK THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRoamOper).
           DELETE RoamOper.

           RUN local-find-first.

        END.
        ELSE DO:
           
           delrow = 0.
           
           RUN local-find-this(false).
        
           memory = RECID(RoamOper).

        END.

        IF NOT AVAIL RoamOper THEN
           LEAVE LOOP.
        
        must-print = TRUE.
        
        NEXT LOOP.
        
     END. /* DELETE */

     /* Roaming prices for operator */ 
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND lcRight = "RW" THEN DO:
        RUN local-find-this(FALSE).
        RUN roamtarifflist(RoamOper.PLMN).
        ufkey = true. 
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRoamOper).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY RoamOper.Country.

       RUN local-UPDATE-record(FALSE).
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRoamOper).

       RUN local-disp-row.
       xrecid = recid(RoamOper).
     
       LEAVE.

     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RoamOper) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RoamOper) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND RoamOper WHERE recid(RoamOper) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RoamOper WHERE recid(RoamOper) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST RoamOper USE-INDEX IMSI NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST RoamOper USE-INDEX PLMN NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST RoamOper USE-INDEX IMSI NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST RoamOper USE-INDEX PLMN NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT RoamOper USE-INDEX IMSI  NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT RoamOper USE-INDEX PLMN NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV RoamOper USE-INDEX IMSI  NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV RoamOper USE-INDEX PLMN NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   lcRoamGroup = RoamOper.RoamGroup.
   DISPLAY 
      RoamOper.Country
      RoamOper.PLMN
      RoamOper.IMSI
      RoamOper.FileSeq
      RoamOper.TestFileSeq
      RoamOper.CommName
      lcProdDisp
      lcRoamGroup 
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   lcProdDisp = ENTRY(RoamOper.Production + 1,lcProdCode).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEFINE INPUT PARAMETER llNew AS LOGICAL NO-UNDO.

   DEFINE VARIABLE lcOldPLMN AS CHARACTER NO-UNDO.

   DEFINE BUFFER bufCDR FOR RoamCDR.
   DEFINE BUFFER bufGPRSCDR FOR RoamGPRS.
   
   RUN local-find-others.
   
   IF NOT llNEW THEN lcOldPLMN = RoamOper.PLMN.
   lcRoamGroup = RoamOper.RoamGroup. 

   DISP
      RoamOper.IMSI
      RoamOper.PLMN
      RoamOper.Country
      RoamOper.Name
      RoamOper.CommName
      lcProdDisp
      lcRoamGroup 
      WITH FRAME lis.

   PROMPT
      RoamOper.IMSI
      RoamOper.PLMN
      RoamOper.Country
      RoamOper.Name
      RoamOper.CommName
      lcProdDisp
      lcRoamGroup 
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).

      IF LOOKUP(nap,poisnap) > 0 THEN DO:
         IF FRAME-FIELD = "lcProdDisp" THEN DO:
            IF LOOKUP(INPUT lcProdDisp,lcProdCode) = 0 THEN DO:
               MESSAGE "Use one of these codes:" lcProdCode VIEW-AS ALERT-BOX.
               NEXT.
            END.
         END.
         ELSE IF FRAME-FIELD = "lcRoamGroup" THEN DO:
            FIND FIRST RoamGroup WHERE 
                       RoamGroup.RoamGroup = INPUT lcRoamGroup NO-LOCK NO-ERROR.
            IF NOT AVAIL RoamGroup THEN DO:
               MESSAGE "Group was not found!:" INPUT lcRoamGroup
                  VIEW-AS ALERT-BOX.
               NEXT.
            END.
         END.
      END.

      APPLY LASTKEY.

   END.
      
   FIND CURRENT RoamOper NO-LOCK.
   
   IF CURRENT-CHANGED RoamOper THEN DO:
      
      MESSAGE ({&MSG_RECORD_CHANGED}) 
      VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
         
      RETURN.

   END. 
   ELSE DO: 
   
      FIND CURRENT RoamOper EXCLUSIVE-LOCK.
      
      ASSIGN
         RoamOper.IMSI
         RoamOper.PLMN
         RoamOper.Country
         RoamOper.Name
         RoamOper.CommName
         lcProdDisp
         lcRoamGroup
         RoamOper.Production = LOOKUP(lcProdDisp,lcProdCode) - 1.
      
   END.

   FIND CURRENT RoamOper NO-LOCK.

   FOR EACH xxRoamOper WHERE xxRoamOper.PLMN = RoamOper.PLMN EXCLUSIVE-LOCK:
      xxRoamOper.RoamGroup = lcRoamGroup.
   END.
/*
   IF NOT llNew AND RoamOper.PLMN ENTERED THEN DO:

      llOK = FALSE.

      MESSAGE
         "You have changed PLMN code. Do You want to update it to CDRs ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE llOK.
   
      IF llOK THEN DO:
         
         FOR EACH RoamCDR NO-LOCK WHERE
                  RoamCDR.DateRead <= TODAY AND
                  RoamCDR.PLMN      = lcOldPLMN:
            
            FIND FIRST bufCDR WHERE
                 RECID(bufCDR) = RECID(RoamCDR)
            EXCLUSIVE-LOCK NO-ERROR.
              
            bufCDR.PLMN = RoamOper.PLMN.

         END.
         
         FOR EACH RoamGPRS NO-LOCK WHERE
                  RoamGPRS.DateRead <= TODAY AND
                  RoamGPRS.PLMN      = lcOldPLMN:
            
            FIND FIRST bufGPRSCDR WHERE
                 RECID(bufGPRSCDR) = RECID(RoamGPRS)
            EXCLUSIVE-LOCK NO-ERROR.
              
            bufGPRSCDR.PLMN = RoamOper.PLMN.

         END.
      
      END.

   END.
*/
END PROCEDURE.

