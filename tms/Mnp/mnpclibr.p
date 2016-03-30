/* ----------------------------------------------------------------------
  MODULE .......: MNPProcess
  TASK .........: UPDATEs table MNPProcess
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
                  28.02.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MNPProcess'}
{Func/fcustdata.i}

DEFINE INPUT PARAMETER piMNPType AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piMsSeq AS INT NO-UNDO.
DEFINE INPUT PARAMETER pcCLI AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttMNPProcess LIKE MNPProcess.

IF pcCLI NE "" THEN DO:
   FOR EACH MNPSub WHERE
      MNPSub.CLI = pcCLI NO-LOCK,
      EACH MNPProcess WHERE
         MNPProcess.MNPSeq = MNPSub.MNPSeq AND
         MNPProcess.MNPType = piMNPType NO-LOCK:
         
         CREATE ttMNPProcess.
         BUFFER-COPY MNPProcess TO ttMNPProcess.
   END.
END.

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhttMNPProcess AS HANDLE NO-UNDO.
   lhttMNPProcess = BUFFER ttMNPProcess:HANDLE.
   RUN StarEventInitialize(lhttMNPProcess).
   
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder). 
   
   DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer). 

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhttMNPProcess).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEFINE VARIABLE lcFormRequest AS CHARACTER FORMAT "x(11)"  NO-UNDO.
DEFINE VARIABLE lcPortRequest AS CHARACTER FORMAT "x(24)"  NO-UNDO.
DEFINE VARIABLE xrecid        AS RECID                     NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow      AS INTEGER                   NO-UNDO  init 0.
DEFINE VARIABLE FrmRow        AS INTEGER                   NO-UNDO  init 1.
DEFINE VARIABLE FrmDown       AS INTEGER                   NO-UNDO  init 15.
DEFINE VARIABLE order         AS INTEGER                   NO-UNDO  init 1.
DEFINE VARIABLE orders        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE maxOrder      AS INTEGER                   NO-UNDO  init 3.
DEFINE VARIABLE ufkey         AS LOGICAL                   NO-UNDO  init TRUE.
DEFINE VARIABLE delrow        AS INTEGER                   NO-UNDO  init 0.
DEFINE VARIABLE pr-order      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE Memory        AS RECID                     NO-UNDO.
DEFINE VARIABLE RowNo         AS INTEGER                   NO-UNDO.
DEFINE VARIABLE must-print    AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE must-add      AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE ac-hdr        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE rtab          AS RECID EXTENT 24           NO-UNDO.
DEFINE VARIABLE i             AS INTEGER                   NO-UNDO.
DEFINE VARIABLE ok            AS LOGICAL format "Yes/No"   NO-UNDO.
DEFINE VARIABLE lcStatus      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE lcMNPType     AS CHARACTER NO-UNDO. 

form
    ttMNPProcess.FormRequest Format "x(12)"
    ttMNPProcess.PortRequest Format "x(24)"
    ttMNPProcess.StatusCode  Format ">>9"     COLUMN-LABEL "StNum"
    lcStatus               FORMAT "x(4)"    COLUMN-LABEL "Status"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  MNP PROCESS LIST  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "FormRequest:" ttMNPProcess.FormRequest FORMAT "X(12)" no-label  skip
    "PortRequest:" ttMNPProcess.PortRequest FORMAT "X(24)" no-label skip
    "StatusCode.:" ttMNPProcess.StatusCode format ">>9" lcStatus no-label  skip
    "Created....:" ttMNPProcess.CreatedTS no-label skip
    "Updated....:" ttMNPProcess.UpdateTS  no-label skip
    "Internal ID:" ttMNPProcess.MNPSeq no-label skip
    "MNP Type...:" ttMNPProcess.MNPType no-label lcMNPType NO-LABEL skip
    "Order ID...:" ttMNPProcess.OrderId no-label skip
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr with no-labels side-labels
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  , By Status , By 4".

maxOrder = 1.

RUN local-find-first.
IF AVAILABLE ttMNPProcess THEN ASSIGN
   Memory       = recid(ttMNPProcess)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No MNP Processes available with this status!" VIEW-AS ALERT-BOX.
   fCleanEventObjects().
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
        FIND ttMNPProcess WHERE recid(ttMNPProcess) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttMNPProcess THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttMNPProcess).
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
           ufk[1] = 0
           ufk[2] = 0
           ufk[3] = 0
           ufk[4] = 0 /* 2821 */
           ufk[5] = 0 /* 2822 */
           ufk[6] = 2806          
           ufk[7] = 2823 
           ufk[8] = 8
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.
      
         RUN Syst/ufkey.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttMNPProcess.FormRequest {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttMNPProcess.FormRequest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttMNPProcess.PortRequest {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttMNPProcess.PortRequest WITH FRAME sel.
      END.

      IF order = 3 THEN DO:
        CHOOSE ROW ttMNPProcess.StatusCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttMNPProcess.StatusCode WITH FRAME sel.
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
        FIND ttMNPProcess WHERE recid(ttMNPProcess) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttMNPProcess THEN
              ASSIGN FIRSTrow = i Memory = recid(ttMNPProcess).
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
           IF NOT AVAILABLE ttMNPProcess THEN DO:
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
                rtab[1] = recid(ttMNPProcess)
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
           IF NOT AVAILABLE ttMNPProcess THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttMNPProcess).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttMNPProcess WHERE recid(ttMNPProcess) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttMNPProcess THEN DO:
           Memory = recid(ttMNPProcess).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttMNPProcess THEN Memory = recid(ttMNPProcess).
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
           FIND ttMNPProcess WHERE recid(ttMNPProcess) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

    /* 
     else if lookup(nap,"4,f4") > 0 then do:
       run local-find-this (false).

       RUN Mnp/mnpfunc.p(mnpprocess.mnpseq).
       ufkey = true.
       next loop.
     
     end. 
     */
     else if lookup(nap,"7,f7") > 0 then do:

       run local-find-this (false).
       memory = recid(ttmnpprocess).
       
       RUN Mnp/mnpsub(ttMNPProcess.mnpseq).
       
       must-print = true.
       ufkey = true.
       next loop.
     
     end. 
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:
       RUN local-find-this (FALSE).

       memory = recid(ttmnpprocess).
       
       /* choose different module for old and new mnp processes */
       IF ttMNPProcess.MNPType EQ 0 THEN RUN Mm/mnpmessages(ttMNPProcess.MNPSeq).
       ELSE RUN Mnp/mnpoperations(ttMNPProcess.MNPSeq).
       
       must-print = true.
       ufkey = true.
       next loop.
     
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(false).

       ASSIGN ac-hdr = " MNP Process " ufkey = TRUE ehto = 5. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttMNPProcess.FormRequest.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttMNPProcess).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttMNPProcess) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttMNPProcess) must-print = TRUE.
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
      FIND ttMNPProcess WHERE recid(ttMNPProcess) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttMNPProcess WHERE recid(ttMNPProcess) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:
   
   FIND FIRST ttMNPProcess NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   FIND LAST ttMNPProcess NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   FIND NEXT ttMNPProcess NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   FIND PREV ttMNPProcess NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
   
   RUN local-find-others.
   
   CLEAR FRAME sel NO-PAUSE.

   DISPLAY 
      ttMNPProcess.FormRequest
      ttMNPProcess.PortRequest
      ttMNPProcess.StatusCode
      lcStatus
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
   
   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "MNPProcess" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "MNP" AND
              TMSCodes.CodeValue = STRING(ttMNPProcess.StatusCode)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcStatus = TMSCodes.CodeName.
   ELSE lcStatus = "".
   
   lcMNPType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "MNPProcess",
                               "MNPType",
                             STRING(ttMNPProcess.MNPType)).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEFINE VARIABLE lcMenu AS CHARACTER NO-UNDO
      EXTENT 3 FORMAT "X(30)".
   
   DEFINE VARIABLE lcOldICC     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOpCode     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustIdType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustId     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCompany    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcFirstName  AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcSurname1   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcSurname2   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcReturn     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrderStatus AS CHARACTER NO-UNDO. 
   
   FORM
      "Firstname ....:" lcFirstName  FORMAT "X(40)" SKIP
      "Surname  .....:" lcSurname1   FORMAT "X(40)" SKIP
      "Surname2 .....:" lcSurname2   FORMAT "X(40)" SKIP
      "Company ......:" lcCompany    FORMAT "X(40)" SKIP
      "ID Type  .....:" lcCustIdType FORMAT "X(20)" SKIP
      "Customer ID ..:" lcCustId     FORMAT "X(20)" SKIP
      "ICC ..........:" lcOldICC     FORMAT "X(20)" SKIP
      "Donor operator:" lcOpCode     FORMAT "X(20)" 
   WITH NO-LABELS TITLE "Update MNP Data" OVERLAY CENTERED ROW 7 
   FRAME fUpdMNPData.
   
   RUN local-find-others.
      
   DISP
      ttMNPProcess.FormRequest
      ttMNPProcess.PortRequest
      ttMNPProcess.StatusCode
      ttMNPProcess.CreatedTS
      ttMNPProcess.UpdateTS
      ttMNPProcess.MNPSeq
      ttMNPProcess.MNPType lcMNPType
      lcStatus
      ttMNPProcess.OrderId
   WITH FRAME lis.
   
   /* OTHERS */
   PAUSE.

   HIDE FRAME lis.

END PROCEDURE.

