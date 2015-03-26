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

{commali.i}
{barrfunc.i}
{timestamp.i}

DEFINE INPUT PARAMETER iiMsSeq   AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER icCmdList AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lcMessage AS CHARACTER EXTENT 5 NO-UNDO FORMAT "x(53)".
DEFINE VARIABLE lcTitle   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcStatus  AS CHARACTER NO-UNDO.
FORM  SKIP(1)
      lcMessage[1] SKIP(1)
      lcMessage[3]
WITH TITLE lcTitle ROW 7 COL 24 WITH
WIDTH 55 NO-LABELS OVERLAY CENTERED FRAME frMessage.



FIND MobSub NO-LOCK WHERE Mobsub.MsSeq = iiMsSeq NO-ERROR.

IF NOT AVAIL MobSub THEN MESSAGE "MobSub lost!" VIEW-AS ALERT-BOX.

DEFINE TEMP-TABLE ttBarrMenu
 FIELD ServPac AS CHAR
 FIELD BarrDesc    AS CHAR
 INDEX ServPac IS PRIMARY UNIQUE ServPac.

/* Components for barring command */
DEFINE TEMP-TABLE ttCompMenu
 FIELD ServPac    AS CHAR
 FIELD Component   AS CHAR
 FIELD Description AS CHAR.
 
RUN pInitMenu.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 0.
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
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR llQueRights  AS LOG                    NO-UNDO. /* User rights OK ?  */
DEF VAR lcTemp       AS CHAR                   NO-UNDO.
DEF VAR liBarr       AS INT                    NO-UNDO. 
DEF VAR icTitle      AS CHAR                   NO-UNDO.
DEF VAR lcRowText    AS CHAR                   NO-UNDO.
DEF VAR lcSpace      AS CHAR                   NO-UNDO.
DEF VAR llOk         AS LOG                    NO-UNDO.

DEF VAR lcComp       AS CHAR EXTENT 10          NO-UNDO.
DEF VAR lcDesc       AS CHAR EXTENT 10          NO-UNDO.
icTitle = "Subscription Requests".
DEF VAR lcCurrent    AS CHARACTER              NO-UNDO.
DEF VAR lcBarringMemoText AS CHARACTER         NO-UNDO. 
DEF VAR llMemoOK     AS LOGICAL NO-UNDO.   

FORM
    ttBarrMenu.BarrDesc FORMAT "x(78)"
    WITH OVERLAY ROW 1 9 DOWN COL 1 WIDTH 80
    TITLE COLOR VALUE(ctc) " " + "Available Barring packages" + " "
    COLOR VALUE(cfc)   
    NO-LABEL

FRAME frTop.

FORM
   lcComp[1] FORMAT "x(10)" NO-LABEL lcDesc[1] NO-LABEL FORMAT "x(66)" SKIP
   lcComp[2] FORMAT "x(10)" NO-LABEL lcDesc[2] NO-LABEL FORMAT "x(66)" SKIP
   lcComp[3] FORMAT "x(10)" NO-LABEL lcDesc[3] NO-LABEL FORMAT "x(66)" SKIP
   lcComp[4] FORMAT "x(10)" NO-LABEL lcDesc[4] NO-LABEL FORMAT "x(66)" SKIP
   lcComp[5] FORMAT "x(10)" NO-LABEL lcDesc[5] NO-LABEL FORMAT "x(66)" SKIP
   lcComp[6] FORMAT "x(10)" NO-LABEL lcDesc[6] NO-LABEL FORMAT "x(66)" SKIP
   WITH OVERLAY ROW 12 COL 1 WIDTH 80  

   TITLE COLOR VALUE(ctc) " " + "Package components to be applied" + " "
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

/* column-labels for parameters */



DEFINE VARIABLE lcRight AS CHAR INIT "R".

RUN local-find-first.
llQueRights = FALSE.
       
IF AVAILABLE ttBarrMenu THEN ASSIGN
   Memory       = ROWID(ttBarrMenu)
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
        FIND ttBarrMenu WHERE ROWID(ttBarrMenu) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME frTop:
           IF AVAILABLE ttBarrMenu THEN DO:
              RUN local-disp-row.
              
              rtab[FRAME-LINE] = ROWID(ttBarrMenu).
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
           ufk[2] = 0
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.
      
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttBarrMenu.BarrDesc 
         GO-ON (home f1 f2 f3 f4 END CURSOR-DOWN
         CURSOR-UP CURSOR-LEFT CURSOR-RIGHT) NO-ERROR
         WITH FRAME frTop.
         COLOR DISPLAY VALUE(ccc) ttBarrMenu.BarrDesc WITH FRAME rfLLeft.
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

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME frTop:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttBarrMenu THEN DO:
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
                rtab[1] = ROWID(ttBarrMenu)
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
           IF NOT AVAILABLE ttBarrMenu THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttBarrMenu).
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
        FIND ttBarrMenu WHERE ROWID(ttBarrMenu) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttBarrMenu THEN DO:
           Memory = ROWID(ttBarrMenu).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttBarrMenu THEN Memory = ROWID(ttBarrMenu).
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
           FIND ttBarrMenu WHERE ROWID(ttBarrMenu) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).

        IF AVAILABLE ttBarrMenu THEN DO:

           /* YPR-91 */
           IF MobSub.PayType AND
              ttBarrMenu.ServPac = "D_HOTL" THEN DO:
              MESSAGE "D_HOTL barring is not allowed for Prepaid"
              VIEW-AS ALERT-BOX.
              NEXT LOOP.
           END.

           MESSAGE "Do You want to apply barring package" SKIP 
                   ttBarrMenu.BarrDesc "?"
           VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llOK.
           IF llOk THEN DO:
               
              MESSAGE   "Do you want to create a memo for barring?" VIEW-AS
                        ALERT-BOX BUTTONS YES-NO UPDATE llmemoOk.
              IF llmemoOk THEN DO:
                 ehto = 9.
                 RUN ufkey.
                
                 PAUSE 0.
                 VIEW FRAME frBarrMemo.
                  
                 REPEAT WITH FRAME frBarrMemo ON ENDKEY UNDO, LEAVE:
                    UPDATE lcBarringMemoText WITH
                    FRAME frBarrMemo.   
                     
                    IF lcBarringMemoText > "" THEN DO:
                       CREATE Memo.
                       ASSIGN
                          Memo.crestamp  = fMakeTS()
                          Memo.Brand     = gcBrand
                          Memo.creuser   = katun
                          Memo.memoseq   = NEXT-VALUE(memoseq)   
                          Memo.hosttable = "MobSub" 
                          Memo.KeyValue  = STRING(iiMsSeq)
                          Memo.CustNum   = MobSub.CustNum
                          Memo.memotext  = lcBarringMemoText
                          Memo.memotitle = "Barring memo for " + 
                                           ttBarrMenu.BarrDesc.   
                       RELEASE Memo.
                    END.

                    LEAVE.
                 END.
                 
                 HIDE FRAME frBarrMemo NO-PAUSE.  
              END. 

              ASSIGN
                 ufk    = 0
                 ufk[5] = 11
                 ufk[8] = 8
                 ehto   = 3
                 ufkey  = TRUE.
              RUN ufkey.
              
              RUN barrengine(MobSub.MsSeq,
                             ttBarrMenu.ServPac,
                             "4",
                             "",
                             fMakeTS(),
                             "",
                             OUTPUT lcStatus).
               
               liBarr = 0.
               liBarr = INTEGER(lcStatus) NO-ERROR. 
              
              IF liBarr = 0 THEN DO:
                 MESSAGE "Action failed" VIEW-AS ALERT-BOX.
                 HIDE FRAME frTop.  PAUSE 0.
                 HIDE FRAME frBottom. PAUSE 0.
                 LEAVE LOOP.
              END. 

           END. /* END Package creation, TRUE llOk */
           ELSE DO:
              MESSAGE "Action cancelled" VIEW-AS ALERT-BOX.
              HIDE FRAME frTop.  PAUSE 0.
              HIDE FRAME frBottom. PAUSE 0.
              LEAVE LOOP.
           END. /* FALSE llOk */ 
        END.
        
        lcMessage[1] = "Barring package applied, request handling in process!".
        lcMessage[3] = "                   <PRESS ENTER>        ".

        DO liCount = 5 TO 1 BY -1:

           lcTitle = "Message(" + STRING(liCount) + ")".
           HIDE FRAME frMessage.
           DISP lcMessage[1]
                lcMessage[2]
                lcMessage[3]
           WITH FRAME frMessage.
           READKEY PAUSE 1.
           IF KEYLABEL(LASTKEY) > "" THEN LEAVE.
        
        END.
        
        HIDE FRAME frMessage. PAUSE 0.         
        HIDE FRAME frTop.     PAUSE 0.
        HIDE FRAME frBottom.  PAUSE 0.
        LEAVE LOOP.
        
     END. /* End of lookup F5 */

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttBarrMenu) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttBarrMenu) must-print = TRUE.
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
      FIND ttBarrMenu WHERE ROWID(ttBarrMenu) = rtab[frame-line(frTop)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttBarrMenu WHERE ROWID(ttBarrMenu) = rtab[frame-line(frTop)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF lcRight = "R" THEN DO:
      FIND FIRST ttBarrMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF lcRight = "R" THEN DO:
      FIND LAST ttBarrMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF lcRight = "R" THEN DO:
     FIND NEXT ttBarrMenu NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF lcRight = "R" THEN DO:
      FIND PREV ttBarrMenu NO-LOCK NO-ERROR.
   END.                  

END PROCEDURE.

PROCEDURE local-disp-row:

    CLEAR FRAME frTop NO-PAUSE.
    DISPLAY 
       ttBarrMenu.BarrDesc
    WITH FRAME frTop.

END PROCEDURE.


PROCEDURE pUpdateBottomFrame.
   RUN local-find-this(FALSE). 
   
   DEF VAR liCounter AS INTEGER NO-UNDO INIT 0.
   /* Get components from menuitem */
   
   FOR EACH ttCompMenu WHERE
            ttCompMenu.ServPac = ttBarrMenu.ServPac:
      liCounter = liCounter + 1.
      ASSIGN lcComp[liCounter] = ttCompMenu.Component.
             lcDesc[liCounter] = ttCompMenu.Description.

   END.
   
   DO liCounter = 1 TO 6:
      DISP lcComp[liCounter] NO-LABEL
           lcDesc[liCounter] NO-LABEL
      WITH FRAME frBottom NO-ERROR.
      PAUSE 0.
   END.
   
   DO liCounter = 1 TO 6:
      ASSIGN lcComp[liCounter] = ""
             lcDesc[liCounter] = "".
   END.



END PROCEDURE.

PROCEDURE pInitMenu.
   
   DEF VAR liLoop     AS INTEGER   NO-UNDO.
   DEF VAR lcPack     AS CHAR      NO-UNDO.
   DEF VAR lrCLB      AS RECID     NO-UNDO.
   DEF VAR lrOLB      AS RECID     NO-UNDO.
   DEF VAR lcORestore AS CHARACTER NO-UNDO.
   DEF VAR lcCRestore AS CHARACTER NO-UNDO.
   
   ASSIGN 
      lrCLB      = fCheckRestore(iiMsSeq,"CLB")  
      lrOLB      = fCheckRestore(iiMsSeq,"OLB")
      lcORestore = ""
      lcCRestore = "".
      
   /* clb to be restored */  
   IF lrCLB NE ? THEN DO:
      FIND MsRequest WHERE RECID(MsRequest) = lrCLB NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN lcCRestore = MsRequest.ReqCParam1.
   END.      
   /* olb to be restored */
   IF lrOLB NE ? THEN DO:
      FIND MsRequest WHERE RECID(MsRequest) = lrOLB NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN lcORestore = MsRequest.ReqCParam1.
   END.
    
   DO liLoop = 1 TO NUM-ENTRIES(icCmdList,"|"):
      
      ASSIGN
         lcPack    = ENTRY(liLoop,icCmdList,"|")
         lcCurrent = "".
      
      FIND FIRST CtServPac NO-LOCK WHERE
                 CtServPac.Brand    = gcBrand        AND
                 CtServPac.CliType  = MobSub.CliType AND
                 CtServPac.ServPac  = lcPack         AND
                 CtServPac.ToDate   >= TODAY 
      NO-ERROR.
      
      IF NOT AVAIL CtServPac THEN NEXT.

      FIND FIRST ServPac  WHERE
                 ServPac.Brand   = CTServPac.Brand  AND
                 ServPAc.ServPac = CTServPac.ServPac
      NO-LOCK NO-ERROR.
      
      IF NOT AVAIL ServPac THEN NEXT.
      
      CREATE ttBarrMenu.
      IF ServPac.ServPac BEGINS "UN" THEN DO:
         lcCurrent = " (CURRENT)".
         IF SUBSTRING(ServPac.ServPac,3) = lcCRestore THEN 
            lcCRestore = "".
         ELSE IF SUBSTRING(ServPac.ServPac,3) = lcORestore THEN
            lcORestore = "".
      END.
      
      ASSIGN ttBarrMenu.ServPac  = ServPac.ServPac
             ttBarrMenu.BarrDesc = ServPac.SPName + lcCurrent.

      /* Create Service Components for this COMMAND MenuNumber */
      FOR EACH CtServEl NO-LOCK WHERE
               CTServEl.Brand   = gcBrand AND 
               CtServEl.ServPac = ttBarrMenu.ServPac AND
               CtServEl.CliType = MobSub.CliType:
         FIND FIRST ServCom NO-LOCK WHERE
                    ServCom.Brand   = gcBrand AND 
                    ServCom.Servcom = CtServEl.ServCom
         NO-ERROR.
         CREATE ttCompMenu.
         ASSIGN ttCompMenu.ServPac = CtServEl.ServPac
                ttCompMenu.Comp    = CtServEl.ServCom + 
                                   " = "                + 
                                   STRING(CtServEl.DefValue)
                ttCompMenu.DesCription = "  " + ServCom.ScName.
      END.
      
   END.             

   IF lcCRestore > "" THEN DO:
      FIND FIRST ttBarrMenu WHERE ttBarrMenu.ServPac = lcCRestore NO-ERROR.
      IF AVAILABLE ttBarrMenu THEN ASSIGN
         ttBarrMenu.BarrDesc = ttBarrMenu.BarrDesc + " (RESTORATION)"
         /* mark only one package as 'restoration' */
         lcORestore = "".
   END.
   IF lcORestore > "" THEN DO:
      FIND FIRST ttBarrMenu WHERE ttBarrMenu.ServPac = lcORestore NO-ERROR.
      IF AVAILABLE ttBarrMenu THEN
         ttBarrMenu.BarrDesc = ttBarrMenu.BarrDesc + " (RESTORATION)".
   END.

   
END PROCEDURE.

