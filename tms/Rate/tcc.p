/* ----------------------------------------------------------------------
  MODULE .......: TCC
  TASK .........: UPDATEs table TCC
  APPLICATION ..: TMS
  AUTHOR .......: jpo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TCC'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTCC AS HANDLE NO-UNDO.
   lhTCC = BUFFER TCC:HANDLE.
   RUN StarEventInitialize(lhTCC).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhTCC).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR TCC           LIKE TCC.TCC              NO-UNDO.
DEF VAR TCCName       LIKE TCC.TCCName          NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR orders        AS CHAR                   NO-UNDO.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS LOG  format "Yes/No"    NO-UNDO.
DEF VAR lcDialType    AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcPulses      AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcTCCRule     AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcTariffRule  AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcTrafficType AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcDestType    AS CHAR NO-UNDO FORMAT "X(30)" .
DEF VAR lcTCCPayer    AS CHAR NO-UNDO FORMAT "X(30)" .

form
    TCC.TCC    
    TCC.TCCName FORMAT "X(25)"
    TCC.BCC
    CCN.CCNName FORMAT "X(20)"
    TCC.ErrorCode
    TCC.ValidTo
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  TECHNICAL CALL CASE MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    TCC.TCC      COLON 20
    TCC.TCCName  COLON 20  FORMAT "X(54)"                       
    TCC.Dialtype COLON 20 
       lcDialType  NO-LABEL  SKIP
    TCC.BType    COLON 20
       lcDestType  NO-LABEL SKIP
    TCC.ValidFrom COLON 20 LABEL "Valid" " - "                                          TCC.ValidTo NO-LABEL SKIP
    TCC.DURto COLON 20     
    TCC.Pulses COLON 20 
       lcPulses NO-LABEL SKIP
    TCC.TCCRule COLON 20 
       lcTCCRule NO-LABEL  SKIP
    TCC.TariffRule COLON 20
       lcTariffRule NO-LABEL  SKIP
    TCC.TrafficType COLON 20
       lcTrafficType NO-LABEL SKIP
    TCC.TCCPayer COLON 20 FORMAT "9" 
       lcTCCPayer NO-LABEL SKIP
    TCC.BDest COLON 20
       BDest.BDName NO-LABEL FORMAT "X(25)" SKIP
    TCC.BDestPref COLON 20 FORMAT "X(4)" LABEL "BDest.Prefix"
    TCC.ErrorCode COLON 20 
       MobError.MEName NO-LABEL FORMAT "X(30)"  SKIP
    TCC.Gsmbnr COLON 20
    TCC.BCC COLON 20
       CCN.CCNName NO-LABEL
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc) side-labels FRAME lis.

form /* seek  TCC */
    TCC
    HELP "Enter Code of code "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  TCCName */
    TCCName
    HELP "Enter Name of the Billing Name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST TCC
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE TCC THEN ASSIGN
   Memory       = recid(TCC)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No TCC items available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a TCC  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR TCC.TCC.
           IF INPUT FRAME lis TCC.TCC = "" THEN 
           LEAVE add-row.
           CREATE TCC.
           ASSIGN
              TCC.Brand = gcBrand 
              TCC.TCC  = INPUT FRAME lis TCC.TCC
              TCC.ValidFrom = TODAY
              TCC.ValidTo = DATE(12,31,2049).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTCC).

           ASSIGN
           Memory = recid(TCC)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST TCC
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TCC THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TCC WHERE recid(TCC) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TCC THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TCC).
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
        ufk[1]= 35  
        ufk[2]= 30 
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8]= 8 
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TCC.TCC {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TCC.TCC WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TCC.TCCName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TCC.TCCName WITH FRAME sel.
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
        FIND TCC WHERE recid(TCC) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TCC THEN
              ASSIGN FIRSTrow = i Memory = recid(TCC).
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
           IF NOT AVAILABLE TCC THEN DO:
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
                rtab[1] = recid(TCC)
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
           IF NOT AVAILABLE TCC THEN DO:
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
              rtab[FRAME-DOWN] = recid(TCC).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TCC WHERE recid(TCC) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TCC THEN DO:
           Memory = recid(TCC).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TCC THEN Memory = recid(TCC).
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
           FIND TCC WHERE recid(TCC) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET TCC WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF TCC ENTERED THEN DO:
          FIND FIRST TCC WHERE TCC.TCC >= TCC
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TCC THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TCC/TCC was found */
          ASSIGN order = 1 Memory = recid(TCC) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET TCCName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF TCCName ENTERED THEN DO:
          FIND FIRST TCC WHERE TCC.TCCName >= TCCName
          USE-INDEX TCCName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TCC THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TCC/TCCName was found */
          ASSIGN order = 2 Memory = recid(TCC) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 AND lcRight = "RW" THEN DO:  /* add */

        FIND FIRST TCC WHERE
             recid(TCC) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
                     
        RUN TCCbarr(input TCC.TCC).
        
        ufkey = TRUE.
                                                                                        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       TCC.TCC TCC.TCCName 
       TCC.BCC.

       RUN local-find-NEXT.
       IF AVAILABLE TCC THEN Memory = recid(TCC).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TCC THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TCC).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TCC.TCC TCC.TCCName 
       TCC.BCC.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTCC).

           DELETE TCC.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST TCC
           /* srule */) THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTCC).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TCC.TCC 
               TCC.DURto
               TCC.BCC
                 .

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTCC).

       RUN local-disp-row.
       xrecid = recid(TCC).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TCC) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TCC) must-print = TRUE.
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
      FIND TCC WHERE recid(TCC) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TCC WHERE recid(TCC) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST TCC WHERE TCC.Brand = gcBrand 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST TCC USE-INDEX TCCName 
       WHERE TCC.Brand = gcBrand
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST TCC WHERE TCC.Brand = gcBrand
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST TCC USE-INDEX TCCName  
       WHERE TCC.Brand = gcBrand       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT TCC WHERE TCC.Brand = gcBrand
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT TCC USE-INDEX TCCName
       WHERE TCC.Brand = gcBrand  /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV TCC WHERE TCC.Brand = gcBrand
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV TCC USE-INDEX TCCName
       WHERE TCC.Brand = gcBrand   /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
          TCC.TCC 
          TCC.TCCName
          CCN.CCNName WHEN AVAILABLE CCN 
          TCC.BCC 
          TCC.ErrorCode
          TCC.ValidTo
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND FIRST CCN WHERE 
              CCN.Brand = gcBrand AND 
              CCN.CCN   = TCC.Bcc 
   NO-LOCK NO-ERROR.

   FIND FIRST BDest WHERE
              BDest.Brand = gcBrand AND 
              BDest.BDest = TCC.BDest AND
              BDest.ToDate >= TCC.ValidFrom AND
              BDest.FromDate <= TCC.ValidTo NO-LOCK No-ERROR.
            
   FIND FIRST MobError WHERE 
              MobError.MobError = TCC.ErrorCode No-LOCK NO-ERROR.

   FIND FIRST DialType WHERE
              DialType.DialType = TCC.Dialtype NO-LOCK NO-ERROR.
           
   ASSIGN
      lcDialtype = IF AVAILABLE DialType THEN DialType.DTName
                   ELSE "Unknown".
                                 
   lcPulses      = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TCC",
                                    "Pulses",
                                    STRING(TCC.Pulses)).

   lcTCCRule     = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TCC",
                                    "TCCRule",
                                    STRING(TCC.TCCRule)).

   lcTCCPayer    = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TCC",
                                    "TCCPayer",
                                    STRING(TCC.TCCPayer)).

   IF AVAILABLE BDest THEN 
   lcDestType    = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "BDest",
                                    "BDestType",
                                    STRING(BDest.DestType)).
   ELSE lcDestType = "".                                 

   lcTariffRule  = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TCC",
                                    "TariffRule",
                                    STRING(TCC.TariffRule)).

   lcTrafficType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TCC",
                                    "TrafficType",
                                    STRING(TCC.TrafficType)).

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          TCC.TCCName
          TCC.Dialtype
          TCC.Btype
          TCC.ValidFrom
          TCC.ValidTo
          TCC.DURto
          TCC.BCC
          lcDialtype
          MobError.MEName WHEN AVAIL MobError
          CCN.CCNName     WHEN AVAIL CCN
          BDest.bdName    WHEN AVAIL BDest
          TCC.BDestPref
          lcTCCRule
          lcDestType
          lcPulses
          lcTariffRule
          lcTrafficType
          TCC.TCCPayer 
          lcTCCPayer
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
         UPDATE
            TCC.TCCName
            TCC.Dialtype
            TCC.Btype
            TCC.ValidFrom
            TCC.ValidTo
            TCC.DURto
            TCC.Pulses
            TCC.TCCRule
            TCC.TariffRule
            TCC.TrafficType
            TCC.TCCPayer
            TCC.BDest
            TCC.BDestPref
            TCC.Errorcode
            TCC.Gsmbnr
            TCC.BCC 
      WITH FRAME lis EDITING:
          READKEY.

          IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             PAUSE 0.

             IF FRAME-FIELD = "ValidTo" THEN DO:
                IF INPUT TCC.ValidFrom = ? OR 
                   INPUT TCC.ValidTo = ? OR
                   INPUT TCC.ValidTo < INPUT TCC.ValidFrom THEN DO:
                   MESSAGE "Invalid period"
                   VIEW-AS ALERT-BOX ERROR.
                   NEXT.
                END.
             END.
            
             ELSE IF FRAME-FIELD = "TCCRule" THEN DO:
                IF INPUT TCC.TCCRule = 0 THEN 
                   lcTCCRule = "".
                ELSE DO: 
                   lcTCCRule = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "TCC",
                                       "TCCRule",
                                       STRING(INPUT TCC.TCCRule)).
                   IF lcTCCRule = "" THEN DO:
                      MESSAGE
                         "Unknown rule" INPUT TCC.TCCRule 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.   
                DISP lcTCCRule WITH FRAME lis. 
             END.
             
             ELSE IF FRAME-FIELD = "Pulses" THEN DO:
                IF INPUT TCC.Pulses = 0 THEN 
                   lcPulses = "".
                ELSE DO: 
                   lcPulses = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "TCC",
                                       "Pulses",
                                       STRING(INPUT TCC.Pulses)).
                   IF lcPulses = "" THEN DO:
                      MESSAGE
                         "Unknown 3rd party rule" INPUT TCC.Pulses 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.   
                DISP lcPulses WITH FRAME lis. 
             END.

             ELSE IF FRAME-FIELD = "TariffRule" THEN DO:
                IF INPUT TCC.TariffRule = 0 THEN 
                   lcTariffRule = "".
                ELSE DO: 
                   lcTariffRule = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "TCC",
                                       "TariffRule",
                                       STRING(INPUT TCC.TariffRule)).
                   IF lcTariffRule = "" THEN DO:
                      MESSAGE
                         "Unknown rule" INPUT TCC.TariffRule 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.   
                DISP lcTariffRule WITH FRAME lis. 

             END.
             
             ELSE IF FRAME-FIELD = "TCCPayer" THEN DO:
                IF INPUT TCC.TCCPayer = 0 THEN 
                   lcTCCPayer = "".
                ELSE DO: 
                   lcTCCPayer = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "TCC",
                                       "TCCPayer",
                                       STRING(INPUT TCC.TCCPayer)).
                   IF lcTCCPayer = "" THEN DO:
                      MESSAGE
                         "Unknown payer" INPUT TCC.TCCPayer 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.   
                DISP lcTCCPayer WITH FRAME lis. 
                
             END.
             
             ELSE IF FRAME-FIELD = "TrafficType" THEN DO:

                IF INPUT TCC.TrafficType = 0 THEN 
                   lcTrafficType = "".
                ELSE DO: 
                   lcTrafficType = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "TCC",
                                       "TrafficType",
                                       STRING(INPUT TCC.TrafficType)).
                   IF lcTrafficType = "" THEN DO:
                      MESSAGE
                         "Unknown type" INPUT TCC.TrafficType 
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.   
                DISP lcTrafficType WITH FRAME lis. 
             END.

             ELSE IF FRAME-FIELD = "BDestPref" THEN DO:
                IF INPUT TCC.BDest     NE "" AND 
                   INPUT TCC.BDestPref NE "" THEN DO:
                      MESSAGE
                      "Rating B-dest and Rating prefix cannot be in use"
                      "at the same moment"
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT BDestPref.
                      NEXT.
                END.
             END.

             ELSE IF FRAME-FIELD = "ErrorCode" THEN DO:
                IF INPUT TCC.ErrorCode > 0 AND 
                   NOT CAN-FIND(FIRST MobError WHERE 
                                   MobError.MobError = INPUT TCC.ErrorCode)
                THEN DO:
                   MESSAGE "Unknown error code"
                   VIEW-AS ALERT-BOX ERROR.
                   NEXT.  
                END.
             END.
             
          END.
          APPLY LASTKEY.
       END. /* EDITING */
     END.                        
   
     LEAVE.
   END.

END PROCEDURE.

