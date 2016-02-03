/* -----------------------------------------------
  MODULE .......: barrbrowser.p
  FUNCTION .....: Request queue browser 
  APPLICATION ..: TMS
  AUTHOR .......: JT 
  CREATED ......: 22.08.07
  CHAGES .......: 02.06.08 New UI and barring handling
                  19.05.08 request parameter changes for barrengine
                  21.07.08 Minor changes to UI to avoid stuck transactions
                           (hide messagebox after countdown)
  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/barrfunc.i}
{Func/timestamp.i}

DEFINE INPUT PARAMETER iiMsSeq   AS INTEGER    NO-UNDO.

DEFINE VARIABLE lcMessage AS CHARACTER EXTENT 5 NO-UNDO FORMAT "x(53)".
DEFINE VARIABLE lcTitle   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStatus  AS CHARACTER NO-UNDO.
FORM  SKIP(1)
      lcMessage[1] SKIP(1)
      lcMessage[3]
WITH TITLE lcTitle ROW 7 COL 24 WITH
WIDTH 55 NO-LABELS OVERLAY CENTERED FRAME frMessage.



FIND MobSub NO-LOCK WHERE Mobsub.MsSeq = iiMsSeq NO-ERROR.

IF NOT AVAIL MobSub THEN MESSAGE "MobSub lost!" VIEW-AS ALERT-BOX.

RUN pInitMenu.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR icCoName2    AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS ROWID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR llQueRights  AS LOG                    NO-UNDO. /* User rights OK ?  */

DEF VAR lcBarringMemoText AS CHARACTER         NO-UNDO. 
DEF VAR llMemoOK          AS LOGICAL           NO-UNDO.   
DEF VAR lcPMask           AS CHAR              NO-UNDO init "0100000000001".
DEF VAR lcBCommand        AS CHAR              NO-UNDO.
DEF VAR lcSetStatus       AS CHAR              NO-UNDO.
DEF VAR llOk              AS LOG               NO-UNDO.
DEF VAR lcValStatus       AS CHAR              NO-UNDO.
DEF VAR liReq             AS INT               NO-UNDO.

DEFINE TEMP-TABLE ttBarrings NO-UNDO
 FIELD BarringCode AS CHAR
 FIELD BarringStatus AS CHAR
 FIELD UserCode AS CHAR
 FIELD Mask AS CHAR
 FIELD UIPriority AS INT
 FIELD EventTS AS CHAR
 INDEX BarringCode BarringCode.


form /* Set new commands */
    lcBCommand FORMAT "X(70)"
    HELP "Enter Barring Command"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) 
    "Enter Barring Command (Type #REFRESH in case of re-provisioning)"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


FORM
    ttBarrings.BarringCode FORMAT "x(25)" 
    ttBarrings.Mask FORMAT "x(15)"
    ttBarrings.BarringStatus FORMAT "x(1)" column-label "St"
    ttBarrings.UserCode FORMAT "x(5)"
    ttBarrings.EventTS FORMAT "x(20)"
    WITH OVERLAY ROW 1 12 DOWN COL 1 WIDTH 80
    TITLE COLOR VALUE(ctc) " " + "Barring packages" + " "
    COLOR VALUE(cfc)   

FRAME frTop.

FORM
   lcPMask FORMAT "x(13)" NO-LABEL lcPMask NO-LABEL FORMAT "x(66)" SKIP
   WITH OVERLAY ROW 17 COL 1 WIDTH 80  

   TITLE COLOR VALUE(ctc) " " + "Total Barring Mask" + " "
   COLOR VALUE(cfc)

FRAME frBottom.

FORM
   lcBarringMemoText NO-LABEL VIEW-AS EDITOR SIZE 65 BY 5
   WITH OVERLAY ROW 10 COL 2 WIDTH 70 
   
   TITLE COLOR VALUE(ctc) "" + "Memo for barring" + " "
   COLOR VALUE (cfc)

FRAME frBarrMemo.


VIEW FRAME frMain. PAUSE 0.
VIEW FRAME frTop.
VIEW FRAME frBottom.

RUN local-find-first.
llQueRights = FALSE.
       
IF AVAILABLE ttBarrings THEN ASSIGN
   Memory       = ROWID(ttBarrings)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

def var lite AS INTEGER NO-UNDO.

LOOP:
REPEAT WITH FRAME frTop:
                           
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttBarrings WHERE ROWID(ttBarrings) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME frTop:
           IF AVAILABLE ttBarrings THEN DO:
              RUN local-disp-row.
              
              rtab[FRAME-LINE] = ROWID(ttBarrings).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        
        UP FRAME-LINE - 1.
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
   
   RUN pUpdateBottomFrame.
   
   BROWSE:
   REPEAT WITH FRAME frTop ON ENDKEY UNDO, RETURN:
   
      IF ufkey THEN DO:
         ASSIGN
           ufk    = 0
           ufk[1] = 0 
           ufk[2] = 90
           ufk[5] = 0
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.
      
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttBarrings.BarrIngCode 
         GO-ON (home f1 f4 END CURSOR-DOWN
         CURSOR-UP CURSOR-LEFT CURSOR-RIGHT) NO-ERROR
         WITH FRAME frTop.
         COLOR DISPLAY VALUE(ccc) ttBarrings.BarringCode WITH FRAME rfLLeft.
      END.
      
      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.
      /*Giving command */
      IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
         CLEAR FRAME f2.
         SET lcBCommand WITH FRAME f2.
         HIDE FRAME f2 NO-PAUSE.
         IF lcBCommand ENTERED THEN DO:
            MESSAGE "Entering Command: " + lcBCommand.
            PAUSE 1 NO-MESSAGE.
            MESSAGE "Do You want to apply barring package" SKIP 
                    lcBCommand 
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llOK.
            IF llOk THEN DO:
               lcValStatus = "".
               lcValStatus = fValidateBarrRequest(
                                   MobSub.MsSeq, 
                                   lcBCommand).
               IF lcValStatus NE "" THEN DO:
                  MESSAGE "CMD Validation Error: " + 
                          lcValStatus VIEW-AS ALERT-BOX.
                  ufkey = TRUE.
                  RUN ufkey.
                  NEXT BROWSE.                 
               END.   
               
               IF fIsReasonableSet(lcBCommand, MobSub.MsSeq) EQ FALSE THEN DO:
                  MESSAGE "Barring status already active/inactive"
                  VIEW-AS ALERT-BOX.
                  ufkey = TRUE.
                  RUN ufkey.
                  NEXT BROWSE.                 
               END.

               ehto = 9.
               RUN ufkey.
                
               PAUSE 0.
               VIEW FRAME frBarrMemo.

               lcBarringMemoText = "".
               REPEAT WITH FRAME frBarrMemo ON ENDKEY UNDO, LEAVE:
                  UPDATE lcBarringMemoText WITH
                  FRAME frBarrMemo.   
                  LEAVE.
               END.                 
               HIDE FRAME frBarrMemo NO-PAUSE.  

               RUN barrengine.p(MobSub.MsSeq,
                  lcBCommand,
                  {&REQUEST_SOURCE_MANUAL_TMS},
                  "",
                  fMakeTS(),
                  "",
                  OUTPUT lcSetStatus).
               
               liReq = INT(lcSetStatus) NO-ERROR.
               
               IF liReq > 0 THEN DO:
                  lcSetStatus = "".
                  
                  IF lcBarringMemoText > "" THEN DO:
                     CREATE Memo.
                     ASSIGN
                       Memo.crestamp  = fMakeTS()
                       Memo.Brand     = gcBrand
                       Memo.creuser   = katun
                       Memo.memoseq   = NEXT-VALUE(memoseq)   
                       Memo.hosttable = "MobSub" 
                       Memo.MemoType  = "Service"
                       Memo.KeyValue  = STRING(iiMsSeq)
                       Memo.CustNum   = MobSub.CustNum
                       /*Memo.memotext  = lcBarringMemoText*/
                       Memo.memotitle = "Modified barring" 
                       Memo.MemoText  = REPLACE(
                          REPLACE(lcBCommand,"=0"," released"),
                                                "=1"," applied")
                          + ". " + chr(10)+ lcBarringMemoText. 
                     RELEASE Memo.
                  END.
                  MESSAGE "Command executed" VIEW-AS ALERT-BOX.
               END.
               ELSE
                  MESSAGE "Command " lcBCommand " failed:" SKIP
                     lcSetStatus
                  VIEW-AS ALERT-BOX.
               
               ufkey = TRUE.
               RUN ufkey.
               NEXT BROWSE.
            END. /* llOk*/
         END. /*Command entered*/
         RUN pUpdateBottomFrame.
      END. /* PREVious ROW */
   /* PREVious ROW */
      ELSE IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME frTop:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttBarrings THEN DO:
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
                rtab[1] = ROWID(ttBarrings)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
        RUN pUpdateBottomFrame.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME frTop:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           
           RUN local-find-this(FALSE).
           
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttBarrings THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttBarrings).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.        
        END.
        ELSE DO:
           DOWN 1.
           IF rtab[FRAME-LINE] = ? THEN UP 1.
        END.
        RUN pUpdateBottomFrame.
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttBarrings WHERE ROWID(ttBarrings) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttBarrings THEN DO:
           Memory = ROWID(ttBarrings).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttBarrings THEN Memory = ROWID(ttBarrings).
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME frTop:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttBarrings WHERE ROWID(ttBarrings) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttBarrings) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttBarrings) must-print = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME frTop NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttBarrings WHERE ROWID(ttBarrings) = rtab[frame-line(frTop)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttBarrings WHERE ROWID(ttBarrings) = rtab[frame-line(frTop)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   FIND FIRST ttBarrings NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   FIND LAST ttBarrings NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   FIND NEXT ttBarrings NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   FIND PREV ttBarrings NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

    CLEAR FRAME frTop NO-PAUSE.
    DISPLAY 
       ttBarrings.BarringCode
       ttBarrings.BarringStatus
       ttBarrings.Mask
       ttBarrings.UserCode
       ttBarrings.EventTs
    WITH FRAME frTop.

END PROCEDURE.


PROCEDURE pUpdateBottomFrame.
   RUN local-find-this(FALSE). 
   
   lcPMask = fGetFinalMask(MobSub.MsSeq).
   DISP lcPMask NO-LABEL
   WITH FRAME frBottom NO-ERROR.
   PAUSE 0.

END PROCEDURE.

PROCEDURE pInitMenu.

   DEF VAR lcStatus AS CHAR NO-UNDO. 
   DEF VAR lcEventTS AS CHAR NO-UNDO. 
   DEF VAR ldaEventDate AS DATE NO-UNDO. 
   DEF VAR lcBarrStatus AS CHAR NO-UNDO. 

   EMPTY TEMP-TABLE ttBarrings.

   FOR EACH BarringConf NO-LOCK:
   
      FIND FIRST Barring NO-LOCK WHERE
                 Barring.MsSeq = MobSub.MsSeq AND
                 Barring.BarringCode = BarringConf.BarringCode
      USE-INDEX MsSeq NO-ERROR.

      IF AVAIL Barring THEN
         lcEventTS = fTs2HMS(Barring.EventTS).
      ELSE lcEventTS = "".

      CREATE ttBarrings.
      ASSIGN
         ttBarrings.BarringCode = BarringConf.BarringCode
         ttBarrings.UIPriority = BarringConf.UIPriority
         ttBarrings.Mask = BarringConf.Mask
         ttBarrings.EventTs = lcEventTS
         ttBarrings.BarringStatus = (IF AVAIL Barring
                                     THEN Barring.BarringStatus
                                     ELSE "I")
         ttBarrings.UserCode = Barring.UserCode WHEN AVAIL Barring.
   END.

END PROCEDURE.


