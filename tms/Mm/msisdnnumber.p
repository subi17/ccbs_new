/* ----------------------------------------------------------------------
  MODULE .......: MSISDNNumber
  TASK .........: UPDATEs table MSISDNNumber
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
                  28.02.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}      
{lib/tokenchk.i 'mobsub'}    
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMSISDNNumber AS HANDLE NO-UNDO.
   lhMSISDNNumber = BUFFER MSISDNNumber:HANDLE.
   RUN StarEventInitialize(lhMSISDNNumber).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhMSISDNNumber).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCli        LIKE MSISDNNumber.CLI     NO-UNDO.   
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lcMSISDNType AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcRankName   AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcMenuOptions AS CHARACTER NO-UNDO. 

DEF VAR llIsAdmin    AS LOG NO-UNDO INIT FALSE. 
/* check admin user rights */
IF getTMSRight("SYST") EQ "RW" THEN llIsAdmin = TRUE.

form
    MSISDNNumber.CLI     /* COLUMN-LABEL FORMAT */
    MSISDNNumber.MSISDNType     COLUMN-LABEL "Type" FORMAT "9"
    lcMSISDNType                COLUMN-LABEL "TypeName"
    MSISDNNumber.Rank           COLUMN-LABEL "Class"
    lcRankName                  COLUMN-LABEL "ClassName"
    
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  MSISDN MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "CLI......:" MSISDNNumber.CLI         /* LABEL FORMAT */     SKIP
    "Type ....:" MSISDNNumber.MSISDNType lcMSISDNType            SKIP
    "Rank.....:" MSISDNNumber.Rank        /* LABEL FORMAT */     SKIP

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

form /* seek  MSISDNNumber */
    lcCli
    HELP "Enter Number of CLI "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  BTName */
    BTName
    HELP "Enter Name of the Billing RepType"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST MSISDNNumber
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE MSISDNNumber THEN ASSIGN
   Memory       = recid(MSISDNNumber)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
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

   IF must-add THEN DO:  /* Add a MSISDNNumber  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MSISDNNumber.CLI
           VALIDATE
              (MSISDNNumber.CLI NOT ENTERED OR
              NOT CAN-FIND(MSISDNNumber using  MSISDNNumber.CLI),
              "Billing RepType " + string(INPUT MSISDNNumber.CLI) +
              " already exists !").
           IF INPUT FRAME lis MSISDNNumber.CLI = "" THEN 
           LEAVE add-row.
           CREATE MSISDNNumber.
           ASSIGN
           MSISDNNumber.CLI = INPUT FRAME lis MSISDNNumber.CLI.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSISDNNumber).

           ASSIGN
           Memory = recid(MSISDNNumber)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSISDNNumber
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSISDNNumber THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSISDNNumber WHERE recid(MSISDNNumber) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSISDNNumber THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSISDNNumber).
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
        ufk[1]= 36  ufk[2]= 0 ufk[3]= 2140  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSISDNNumber.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSISDNNumber.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MSISDNNumber.MSISDNType ;(uchoose.i;) 
        NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSISDNNumber.MSISDNType WITH FRAME sel.
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
        FIND MSISDNNumber WHERE recid(MSISDNNumber) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSISDNNumber THEN
              ASSIGN FIRSTrow = i Memory = recid(MSISDNNumber).
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
           IF NOT AVAILABLE MSISDNNumber THEN DO:
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
                rtab[1] = recid(MSISDNNumber)
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
           IF NOT AVAILABLE MSISDNNumber THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSISDNNumber).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSISDNNumber WHERE recid(MSISDNNumber) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSISDNNumber THEN DO:
           Memory = recid(MSISDNNumber).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSISDNNumber THEN Memory = recid(MSISDNNumber).
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
           FIND MSISDNNumber WHERE recid(MSISDNNumber) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lccli WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcCli ENTERED THEN DO:
          FIND FIRST MSISDNNumber WHERE MSISDNNumber.CLI >= lcCli          
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MSISDNNumber THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MSISDNNumber/MSISDNNumber was found */
          ASSIGN order = 1 Memory = recid(MSISDNNumber) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

         RUN local-find-this(FALSE).

         RUN msisdn(INPUT MSISDNNumber.CLI,0,0).
        
         UFKEY = TRUE.

     END.
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDNNumber).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MSISDNNumber.CLI.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDNNumber).

       RUN local-disp-row.
       xrecid = recid(MSISDNNumber).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSISDNNumber) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSISDNNumber) must-print = TRUE.
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
      FIND MSISDNNumber WHERE recid(MSISDNNumber) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MSISDNNumber WHERE recid(MSISDNNumber) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MSISDNNumber
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MSISDNNumber
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MSISDNNumber
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MSISDNNumber
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MSISDNNumber.CLI 
       MSISDNNumber.MSISDNType
       MSISDNNumber.Rank
       lcMSISDNType
       lcRankName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Msisdn" AND
              TMSCodes.FieldName = "msisdntype" AND
              TMSCodes.CodeGroup = "msisdn" AND
              TMSCodes.CodeValue = STRING(msisdnnumber.msisdntype)
   NO-LOCK NO-ERROR.
   
  IF AVAIL tmscodes THEN lcMsisdnType = TMSCodes.CodeName.
  ELSE lcMSISDNTYPE = "".    
  
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "MSISDNNumber" AND
              TMSCodes.FieldName = "Rank" AND
              TMSCodes.CodeGroup = "Rank" AND
              TMSCodes.CodeValue = STRING(msisdnnumber.Rank)
   NO-LOCK NO-ERROR.
   
  IF AVAIL tmscodes THEN lcRankName = TMSCodes.CodeName.
  ELSE lcRankName = "".    

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          MSISDNNumber.MSISDNType
          lcMsisdnType NO-LABEL
          MSISDNNumber.Rank
      WITH FRAME lis.
      IF lcRight = "RW" AND llIsAdmin THEN DO:
         UPDATE
            MSISDNNumber.Rank
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

