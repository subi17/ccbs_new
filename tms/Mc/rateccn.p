/* ----------------------------------------------------------------------
  MODULE .......: RateCCN
  TASK .........: UPDATEs CCN RateCCN
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.09.02
  CHANGED ......: 10.03.03 tk tokens
                  20.03.03/aam one parameter added for tariff.p
                  04.03.03/tk  removed prompt-for
                  04.04.03 kl RUN Mc/tariff,.p new parameter
                  26.06.03 kl RUN Mc/tariff,.p new parameter
                  04.07.03 kl RUN Mc/tariff,.p new parameter
                  08.09.03/aam brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RateCCN

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RateCCN'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRateCCN AS HANDLE NO-UNDO.
   lhRateCCN = BUFFER RateCCN:HANDLE.
   RUN StarEventInitialize(lhRateCCN).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhRateCCN).
   END.

END.

DEF  INPUT PARAMETER iiBDestID  AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liDialType   LIKE RateCCN.DialType     NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
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
DEF VAR oldestdate   AS DATE FORMAT "99.99.99" No-UNDO . 

DEF VAR lcCCNName    AS CHAR NO-UNDO.
DEF VAR lcDTName     AS CHAR NO-UNDO. 

DEF VAR liDefBeg     AS INT NO-UNDO.
DEF VAR liDefEnd     AS INT NO-UNDO.



form
    RateCCN.Brand     
    RateCCN.DialType  COLUMN-LABEL "Dialling Type"
    lcDTName          COLUMN-LABEL "Name"
                      FORMAT "X(20)"
    RateCCN.CCN     /* COLUMN-LABEL FORMAT */ FORMAT ">>>9"
    lcCCNName         COLUMN-LABEL "Name"
                      FORMAT "X(20)"

WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " RATING CCNs "  + string(pvm,"99-99-99") + " "
    FRAME sel.      

form
    RateCCN.Brand     COLON 20
    RateCCN.BDestID   COLON 20 
    RateCCN.BDest     COLON 20
    RateCCN.DestType  COLON 20 
    RateCCN.DialType  COLON 20  /* LABEL FORMAT */
       lcDTName   NO-LABEL 
                  FORMAT "X(30)"
    RateCCN.CCN        COLON 20   /* LABEL FORMAT */ FORMAT ">>>9"
       lcCCNName  NO-LABEL 
                  FORMAT "X(30)"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form 
    "Brand:" lcBrand skip
    "Type :" liDialType
    HELP "Enter Dialling type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Dialling type"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


FIND FIRST BDest WHERE BDest.BDestID = iiBDestID NO-LOCK NO-ERROR.
IF NOT AVAILABLE BDest THEN DO:
   MESSAGE "Unknown b-destination"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* set the reasonable period limit */
ASSIGN liDefBeg = (YEAR(TODAY) - 5) * 100 + 1
       liDefEnd = (YEAR(TODAY) + 5) * 100 + 12.

FIND FIRST RateCCN WHERE
    RateCCN.BDestID = iiBDestID
NO-LOCK NO-ERROR.
IF AVAILABLE RateCCN THEN ASSIGN
   Memory       = recid(RateCCN)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rating CCNs available" VIEW-AS ALERT-BOX.
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a RateCCN  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           CREATE RateCCN.
           ASSIGN
           RateCCN.Brand    = lcBrand
           RateCCN.BDestID  = iiBDestID
           RateCCN.BDest    = BDest.BDest
           RateCCN.DestType = BDest.DestType.

           RUN local-UPDATE-record(TRUE).

           IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
           KEYLABEL(lastkey) = "F4" THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRateCCN).

           ASSIGN
           Memory = recid(RateCCN)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST RateCCN WHERE 
         RateCCN.BDestID = iiBDestID
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE RateCCN THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND RateCCN WHERE recid(RateCCN) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RateCCN THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RateCCN).
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
        ufk[1]= 1631 ufk[2]= 0  ufk[3]= 0  ufk[4]= 878
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)   
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  ufk[8]= 8   ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RateCCN.DialType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RateCCN.DialType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW RateCCN.CCN {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RateCCN.CCN WITH FRAME sel.
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
        FIND RateCCN WHERE recid(RateCCN) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RateCCN THEN
              ASSIGN FIRSTrow = i Memory = recid(RateCCN).
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
           IF NOT AVAILABLE RateCCN THEN DO:
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
                rtab[1] = recid(RateCCN)
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
           IF NOT AVAILABLE RateCCN THEN DO:
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
              rtab[FRAME-DOWN] = recid(RateCCN).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RateCCN WHERE recid(RateCCN) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RateCCN THEN DO:
           Memory = recid(RateCCN).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RateCCN THEN Memory = recid(RateCCN).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the CCN ?  */
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
           FIND RateCCN WHERE recid(RateCCN) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE liDialType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liDialType ENTERED THEN DO:
          FIND FIRST RateCCN WHERE 
             RateCCN.BDestID = iiBDestID AND
             RateCCN.DialType >= liDialType
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* tariffs */
        RUN local-find-this(FALSE).

        IF AVAILABLE RateCCN THEN DO:
           RUN Mc/tariff.p(0,RateCCN.CCN,"",0,RateCCN.BDest,0). 
           UFKEY = TRUE.
        END.
     end. 

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
       RateCCN.DialType RateCCN.CCN .

       RUN local-find-NEXT.
       IF AVAILABLE RateCCN THEN Memory = recid(RateCCN).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RateCCN THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RateCCN).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RateCCN.DialType RateCCN.CCN.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRateCCN).

           DELETE RateCCN.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST RateCCN WHERE 
              RateCCN.BDestID = iiBDestID) 
           THEN DO:
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
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRateCCN).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY RateCCN.DialType.

       RUN local-UPDATE-record(FALSE).
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRateCCN).

       RUN local-disp-row.
       xrecid = recid(RateCCN).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RateCCN) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RateCCN) must-print = TRUE.
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
      FIND RateCCN WHERE recid(RateCCN) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RateCCN WHERE recid(RateCCN) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST RateCCN WHERE 
          RateCCN.BDestID = iiBDestID
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST RateCCN WHERE
          RateCCN.BDestID = iiBDestID
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT RateCCN WHERE
          RateCCN.BDestID = iiBDestID
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV RateCCN WHERE
          RateCCN.BDestID = iiBDestID
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       RateCCN.Brand
       RateCCN.DialType 
       lcDTName
       RateCCN.CCN
       lcCCNName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND DialType OF RateCCN NO-LOCK NO-ERROR.
   lcDTName = IF AVAILABLE DialType 
              THEN DialType.DTName
              ELSE "Unknown".

   FIND CCN OF RateCCN NO-LOCK NO-ERROR.      
   lcCCNName = IF AVAILABLE CCN 
               THEN CCN.CCNName
               ELSE "Unknown".

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   DEF INPUT PARAMETER pNew AS LOG NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP RateCCN.Brand
           RateCCN.BDestID
           RateCCN.BDest
           RateCCN.DestType
           RateCCN.DialType
           lcDTName
           RateCCN.CCN
           lcCCNName
      WITH FRAME lis.

      UPDATE
          RateCCN.DialType WHEN pNew
          RateCCN.CCN
      WITH FRAME lis
      EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "DialType" THEN DO:
                   IF NOT CAN-FIND(DialType WHERE 
                          DialType.DialType = INPUT FRAME lis RateCCN.DialType)
                   THEN DO:
                      MESSAGE "Unknown dialling type"
                      VIEW-AS ALERT-BOX
                      ERROR.
                      NEXT.
                   END.

                   IF CAN-FIND(RateCCN WHERE
                               RateCCN.BDestID = iiBDestID AND
                               RateCCN.DialType = 
                               INPUT FRAME lis RateCCN.DialType) 
                   THEN DO:
                      MESSAGE 
                      "Dialling type" INPUT FRAME lis RateCCN.DialType
                      "already exists for this B-destination!"
                      VIEW-AS ALERT-BOX
                      ERROR.
                      NEXT.
                   END.
                END.
                IF FRAME-FIELD = "CCN" THEN DO:
                   FIND CCN WHERE 
                      CCN.Brand = lcBrand AND
                      CCN.CCN   = INPUT FRAME lis RateCCN.CCN NO-LOCK NO-ERROR.
                   IF NOT AVAIL CCN THEN DO:
                      BELL.
                      MESSAGE "Unknown CCN !".
                      NEXT.
                   END.
                   DISP CCN.CCNName ;& lcCCNName.
                END.
             END.
             APPLY LASTKEY.
      END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.

