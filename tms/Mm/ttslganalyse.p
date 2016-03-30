/* ----------------------------------------------------------------------
  MODULE .......: ttSLG
  TASK .........: UPDATEs table ttSLG
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 27.05.03
  CHANGED ......: 14.01.04 jp servattr
                  01.07.04 tk subser.i moved
                  13.12.04/aam use ttSLG
                  12.12.06/mvi new param to RUN Mm/msrequest (reqstat = ?)
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'} 
{Syst/eventval.i}

DEF  NEW  shared VAR siirto AS CHAR.

DEF TEMP-TABLE ttSLG LIKE SLGAnalyse.

DEF INPUT-OUTPUT PARAMETER TABLE FOR ttSLG.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhttSLG AS HANDLE NO-UNDO.
   lhttSLG = BUFFER ttSLG:HANDLE.
   RUN StarEventInitialize(lhttSLG).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhttSLG).
   END.

END.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 8.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
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
DEF VAR lcText       AS CHAR                   NO-UNDO FORMAT "x(15)".
DEF VAR llChange     AS LOG                    NO-UNDO. 
DEF VAR lcSologStat  AS CHAR                   NO-UNDO. 
DEF VAR llRequest    AS LOG                    NO-UNDO. 
DEF VAR ldPara       AS DEC                    NO-UNDO. 

form
    ttSLG.CliType   COLUMN-LABEL "CliType"   FORMAT "X(7)" 
    ttSLG.BillCode  COLUMN-LABEL "BillCode"  FORMAT "X(8)" 
    BillItem.BiName  FORMAT "X(12)"
    ttSLG.CCN       COLUMN-LABEL "CCN"       FORMAT ">>9"
    CCN.CCNName      FORMAT "X(12)" 
    ttSLG.Bdest     COLUMN-LABEL "Destination"  
    ttSLG.ValidFrom    COLUMN-LABEL "Date" 

WITH ROW FrmRow OVERLAY CENTERED FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    " NEW ATTRIBUTES FOR PERIODICAL/SERVICE COUNTERS " 
    FRAME sel.

form
    ttSLG.CliType    /* LABEL FORMAT */
    ttSLG.BillCode  LABEL "Attribute" FORMAT "X(16)"
    ttSLG.CCN      LABEL "Value of attribute" 
    ttSLG.ValidFrom 
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".

RUN local-find-first.
IF AVAILABLE ttSLG THEN ASSIGN
   Memory       = recid(ttSLG)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No Parameter items available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttSLG WHERE recid(ttSLG) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttSLG THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttSLG).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 4 
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttSLG.CliType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttSLG.CliType WITH FRAME sel.
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
        FIND ttSLG WHERE recid(ttSLG) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttSLG THEN
              ASSIGN FIRSTrow = i Memory = recid(ttSLG).
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
           IF NOT AVAILABLE ttSLG THEN DO:
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
                rtab[1] = recid(ttSLG)
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
           IF NOT AVAILABLE ttSLG THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttSLG).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttSLG WHERE recid(ttSLG) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttSLG THEN DO:
           Memory = recid(ttSLG).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttSLG THEN Memory = recid(ttSLG).
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
           FIND ttSLG WHERE recid(ttSLG) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ttSLG.CliType     ttSLG.BillCode 
       ttSLG.CCN ttSLG.Bdest 
       CCN.CCNName WHEN AVAIL CCN BillItem.BIName.

       RUN local-find-NEXT.
       IF AVAILABLE ttSLG THEN Memory = recid(ttSLG).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttSLG THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttSLG).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ttSLG.CliType ttSLG.BillCode
       ttSLG.CCN ttSLG.Bdest CCN.CCNName WHEN AVAIL CCN BillItem.BIName.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhttSLG).

           DELETE ttSLG.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ttSLG THEN DO:
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
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhttSLG).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhttSLG).

       RUN local-disp-row.
       xrecid = recid(ttSLG).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttSLG) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttSLG) must-print = TRUE.
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
      FIND ttSLG WHERE recid(ttSLG) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttSLG WHERE recid(ttSLG) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttSLG 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttSLG
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttSLG 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttSLG 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       find first ccn where 
                  ccn.brand = gcBrand AND 
                  ccn.ccn   = ttslg.ccn no-lock no-error.

       find first billitem where
                  billitem.Brand    = gcBrand AND 
                  billitem.BillCode = ttslg.BillCode NO-LOCK NO-ERROR.

       DISPLAY 
       ttSLG.CliType
       ttSLG.BillCode 
       CCN.CCNName WHEN AVAIL CCN
       BillItem.BIName
       ttSLG.CCN
       ttSLG.Bdest
       ttSLG.ValidFrom
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.


END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
 
   DISP
       ttSLG.CliType
       ttSLG.BillCode 
       ttSLG.CCN
       ttSLG.Bdest
       ttSLG.ValidFrom
   WITH FRAME lis.   PAUSE 0.

   llChange = (lcRight = "RW").
   
   
   IF llChange THEN
   /* can service be changed from here */
   FOR FIRST CTServEl NO-LOCK WHERE 
             CTServEl.Brand     = gcBrand              AND
             CTServEl.ServCom   = ttSLG.CliType AND
             CTServEl.CLIType   = MobSub.CLIType       AND
             CTServEl.FromDate <= TODAY,
       FIRST CTServAttr OF CTServEl NO-LOCK WHERE
             CTServAttr.ServAttr  = ttSLG.BillCode AND
             CTServAttr.FromDate <= TODAY:
             
      llChange = CTServAttr.ChgAllowed.
   END. 

   IF llChange AND llRequest THEN DO:
      MESSAGE "There is an active change request for service." SKIP
              "Change is not allowed before request is handled."
      VIEW-AS ALERT-BOX
      TITLE " PENDING REQUEST ".
      
      llChange = FALSE.
   END. 


   REPEAT WITH FRAME LIS:

      IF llChange THEN DO:
        
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.   

END PROCEDURE.

