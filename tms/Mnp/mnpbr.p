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
{Func/date.i}
{Syst/tmsconst.i}

DEFINE INPUT PARAMETER piOrderId AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piMNPType AS INTEGER NO-UNDO.

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMNPProcess AS HANDLE NO-UNDO.
   lhMNPProcess = BUFFER MNPProcess:HANDLE.
   RUN StarEventInitialize(lhMNPProcess).
   
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder). 
   
   DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer). 

   ON F12 ANYWHERE DO:
      RUN eventview2(lhMNPProcess).
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
DEFINE VARIABLE maxOrder      AS INTEGER                   NO-UNDO  init 1.
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
DEFINE VARIABLE lcState      AS CHARACTER                 NO-UNDO format "x(17)".
DEFINE VARIABLE lcMNPType     AS CHARACTER NO-UNDO. 

form
    MNPProcess.FormRequest Format "x(12)"
    MNPProcess.PortRequest Format "x(24)"
    MNPProcess.StatusCode  Format ">>9"     COLUMN-LABEL "StNum"
    lcStatus               FORMAT "x(4)"    COLUMN-LABEL "Status"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  MNP PROCESS LIST  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "FormRequest:" MNPProcess.FormRequest FORMAT "X(12)" no-label skip
    "PortRequest:" MNPProcess.PortRequest FORMAT "X(24)" no-label skip
    "StatusCode.:" MNPProcess.StatusCode format ">>9" lcStatus no-label lcState no-label skip
    "Created....:" MNPProcess.CreatedTS no-label skip
    "Updated....:" MNPProcess.UpdateTS no-label skip
    "Internal ID:" MNPProcess.MNPSeq format ">>>>>>>9" no-label skip
    "MNP Type...:" MNPProcess.MNPType no-label 
                   lcMNPType FORMAT "x(20)" NO-LABEL skip
    "Order ID...:" MNPProcess.OrderId format ">>>>>>>9" no-label skip

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr with no-labels side-labels
    FRAME lis.

form /* seek  MNPProcess */
    lcFormRequest
    HELP "Enter form code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  PortRequest */
    lcPortRequest
    HELP "Enter porting code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  , By Status , By 4".

IF piOrderId > 0 THEN maxOrder = 1.

RUN local-find-first.
IF AVAILABLE MNPProcess THEN ASSIGN
   Memory       = recid(MNPProcess)
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
        FIND MNPProcess WHERE recid(MNPProcess) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPProcess THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPProcess).
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
           ufk[1] = 35
           ufk[2] = 35
           ufk[3] = 2206 WHEN piOrderId = 0  AND piMNPType < 2
           ufk[4] = 0 /* 2821 */
           ufk[5] = 0 /* 2822 */
           ufk[6] = 2806          
           ufk[7] = 2823 
           ufk[8] = 8
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.
      
         RUN ufkey.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MNPProcess.FormRequest ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPProcess.FormRequest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MNPProcess.PortRequest ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPProcess.PortRequest WITH FRAME sel.
      END.

      IF order = 3 THEN DO:
        CHOOSE ROW MNPProcess.StatusCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPProcess.StatusCode WITH FRAME sel.
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
        FIND MNPProcess WHERE recid(MNPProcess) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPProcess THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPProcess).
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
           IF NOT AVAILABLE MNPProcess THEN DO:
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
                rtab[1] = recid(MNPProcess)
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
           IF NOT AVAILABLE MNPProcess THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPProcess).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MNPProcess WHERE recid(MNPProcess) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MNPProcess THEN DO:
           Memory = recid(MNPProcess).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MNPProcess THEN Memory = recid(MNPProcess).
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
           FIND MNPProcess WHERE recid(MNPProcess) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcFormRequest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcFormRequest ENTERED THEN DO:
          IF piOrderId > 0 THEN
             FIND FIRST MNPProcess USE-INDEX FormRequest WHERE 
                        MNPProcess.FormRequest >= lcFormRequest AND
                        MNPProcess.OrderId = piOrderId
             NO-LOCK NO-ERROR.
          ELSE
             FIND FIRST MNPProcess USE-INDEX FormRequest WHERE 
                        MNPProcess.FormRequest >= lcFormRequest AND
                        MNPProcess.StatusCode = piStatus
             NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MNPProcess THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MNPProcess/MNPProcess was found */
          ASSIGN order = 1 Memory = recid(MNPProcess) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET lcPortRequest WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcPortRequest ENTERED THEN DO:
          IF piOrderId > 0 THEN
             FIND FIRST MNPProcess USE-INDEX PortRequest WHERE 
                        MNPProcess.PortRequest >= lcPortRequest AND
                        MNPProcess.OrderId = piOrderId
             NO-LOCK NO-ERROR.
          ELSE
             FIND FIRST MNPProcess USE-INDEX PortRequest WHERE 
                        MNPProcess.PortRequest >= lcPortRequest AND
                        MNPProcess.StatusCode = piStatus 
             NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MNPProcess THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MNPProcess/PortRequest was found */
          ASSIGN 
            /* order = 2 WHEN piOrderId = 0 */
            Memory = recid(MNPProcess) 
            must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:

       RUN local-find-this (FALSE).

       RUN order.p(0,0,"",MNPProcess.OrderId).
       ufkey = true.
       next loop.

     END.

     else if lookup(nap,"7,f7") > 0 then do:

       run local-find-this (false).
       memory = recid(mnpprocess).
       
       run mnpsub(MNPProcess.mnpseq).
       
       must-print = true.
       ufkey = true.
       next loop.
     
     end. 
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:
       RUN local-find-this (FALSE).

       memory = recid(mnpprocess).
       
       /* choose different module for old and new mnp processes */
       IF MNPProcess.MNPType EQ 0 THEN RUN mnpmessages(MNPProcess.MNPSeq).
       ELSE RUN mnpoperations(MNPProcess.MNPSeq).
       
       must-print = true.
       ufkey = true.
       next loop.
     
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(false).

       ASSIGN ac-hdr = " MNP Process " ufkey = TRUE ehto = 5. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MNPProcess.FormRequest.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(MNPProcess).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MNPProcess) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MNPProcess) must-print = TRUE.
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
      FIND MNPProcess WHERE recid(MNPProcess) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MNPProcess WHERE recid(MNPProcess) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:
   
   IF piOrderId NE 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST MNPProcess  WHERE
                    MNPProcess.OrderId = piOrderId      
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND FIRST MNPProcess USE-INDEX MNPType WHERE
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                    MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN 
         FIND FIRST MNPProcess USE-INDEX PortRequest WHERE
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                    MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         FIND FIRST MNPProcess USE-INDEX StatusCode WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                    MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:
   IF piOrderId NE 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST MNPProcess  WHERE
                   MNPProcess.OrderId = piOrderId      
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND LAST MNPProcess USE-INDEX MNPType WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN 
         FIND LAST MNPProcess USE-INDEX PortRequest WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         FIND LAST MNPProcess USE-INDEX StatusCode WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF piOrderId NE 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT MNPProcess  WHERE
                   MNPProcess.OrderId = piOrderId      
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND NEXT MNPProcess USE-INDEX MNPType WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN 
         FIND NEXT MNPProcess USE-INDEX PortRequest WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         FIND NEXT MNPProcess USE-INDEX StatusCode WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF piOrderId NE 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV MNPProcess  WHERE
                   MNPProcess.OrderId = piOrderId      
         NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN FIND 
         PREV MNPProcess USE-INDEX MNPType WHERE 
              MNPProcess.Brand = gcBrand AND
              MNPProcess.MNPType = piMNPType AND 
              MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN 
         FIND PREV MNPProcess USE-INDEX PortRequest WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         FIND PREV MNPProcess USE-INDEX StatusCode WHERE 
                   MNPProcess.Brand = gcBrand AND
                   MNPProcess.MNPType = piMNPType AND 
                   MNPProcess.StatusCode = piStatus 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
   
   RUN local-find-others.
   
   CLEAR FRAME sel NO-PAUSE.

   DISPLAY 
      MNPProcess.FormRequest
      MNPProcess.PortRequest
      MNPProcess.StatusCode
      lcStatus
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
   
   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcStatus = TMSCodes.CodeName.
   ELSE lcStatus = "".
   
   lcState = "".
   IF MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
      MNPProcess.StatusCode = {&MNP_ST_ASOL} THEN DO:
      
      FIND TMSCodes WHERE 
           TMSCodes.TableName = "MNPProcess" AND
           TMSCodes.FieldName = "StateFlag" AND
           TMSCodes.CodeGroup = "MNP" AND
           TMSCodes.CodeValue = STRING(MNPProcess.StateFlag)
      NO-LOCK NO-ERROR.

      IF AVAIL TMSCodes THEN lcState = TMSCodes.CodeName.
   END.
   
   lcMNPType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "MNPProcess",
                               "MNPType",
                             STRING(MNPProcess.MNPType)).

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
      MNPProcess.FormRequest
      MNPProcess.PortRequest
      MNPProcess.StatusCode
      fTS2HMS(MNPProcess.CreatedTS) FORMAT "x(20)" @ MNPProcess.CreatedTS
      fTS2HMS(MNPProcess.UpdateTS) FORMAT "x(20)" @ MNPProcess.UpdateTS
      MNPProcess.MNPSeq
      MNPProcess.MNPType lcMNPType
      lcStatus
      lcState
      MNPProcess.OrderId
   WITH FRAME lis.
   
   FIND FIRST Order where 
      Order.Brand = gcBrand and
      Order.orderid = mnpprocess.orderid NO-LOCK NO-ERROR. 

   IF AVAIL Order AND MNPProcess.StatusCode = 4 AND Order.StatusCode NE "73" THEN DO:
      
      FIND FIRST TMSCodes WHERE 
                 TMSCodes.TableName = "Order" AND
                 TMSCodes.FieldName = "StatusCode" AND
                 TMSCodes.CodeGroup = "Orders" AND
                 TMSCodes.CodeValue = Order.StatusCode
      NO-LOCK NO-ERROR.
      
      IF AVAIL TMSCodes THEN lcOrderStatus = TMSCodes.CodeName.
      ELSE lcOrderStatus = "".
      
      MESSAGE "Cannot modify MNP process with order status " + 
         Order.StatusCode + " " + lcOrderStatus.
   END. 
   /* AREC */
  IF AVAIL Order AND MNPProcess.StatusCode = 4 AND Order.StatusCode EQ "73" THEN DO:
 
      CHOISES:
      DO WHILE TRUE:

         ASSIGN
            ufk    = 0
            ufk[8] = 8
            ehto   = 3.
      
         RUN ufkey. 
         
         DISPLAY
            " A) Update MNP data         " @ lcMenu[1]  SKIP
            " B) Create a new MNP process" @ lcMenu[2]  SKIP
            " X) Quit                    " @ lcMenu[3]  SKIP
         WITH OVERLAY FRAME choices NO-LABELS.
       
         CHOOSE FIELD lcMenu AUTO-RETURN GO-ON (F8) WITH FRAME choices
            TITLE " Choose MNP modification " CENTERED WITH COL 2 ROW 14.

         HIDE FRAME choices.

         IF LOOKUP(KEYLABEL(LASTKEY),"F1,F2,F4") > 0 THEN NEXT.
         IF LOOKUP(KEYLABEL(LASTKEY),"F8") > 0 THEN LEAVE.

         CASE FRAME-INDEX:
            
            WHEN 1 THEN DO:
            
               FIND FIRST Order WHERE
                          Order.Brand   = gcBrand AND
                          Order.OrderId = MNPProcess.OrderId
               NO-LOCK NO-ERROR.
               FIND FIRST OrderCustomer WHERE
                          OrderCustomer.Brand   = Order.Brand   AND
                          OrderCustomer.OrderId = Order.OrderId AND
                          OrderCustomer.RowType = 1
               NO-LOCK NO-ERROR.
               
               ASSIGN
                  lcCustId     = OrderCustomer.CustId
                  lcCustIdType = OrderCustomer.CustIdType
                  lcCompany    = OrderCustomer.Company
                  lcFirstName  = OrderCustomer.FirstName
                  lcSurname1   = OrderCustomer.Surname1
                  lcSurname2   = OrderCustomer.Surname2
                  lcOldICC     = Order.OldICC
                  lcOpCode     = Order.CurrOper. 
               
               ehto = 9. RUN ufkey.

               MNPDATA:
               REPEAT ON ENDKEY UNDO,LEAVE:

                  UPDATE
                     lcFirstName
                     lcSurname1
                     lcSurname2
                     lcCompany
                     lcCustIdType
                     lcCustId
                     lcOldICC
                     lcOpCode
                  WITH FRAME fUpdMNPData EDITING:

                     READKEY.
                     nap = KEYLABEL(LASTKEY).

                     
                     IF nap = "F4" THEN DO:
                        HIDE FRAME fUpdMNPData.
                        UNDO MNPDATA, LEAVE MNPDATA.
                     END.
                     
                     IF nap = "F9" THEN DO:

                        CASE FRAME-FIELD:

                           WHEN "lcCustIdType" THEN DO:

                              RUN tmscodesbr(input "Customer",
                                             input "CustIdType",
                                             input "N/A",
                                             input "Choose ID Type",
                                             input "",
                                             OUTPUT lcReturn).

                              IF lcReturn NE "" THEN lcCustIdType = lcReturn.

                              DISP lcCustIdType WITH FRAME fUpdMNPData.
                              ehto = 9. RUN ufkey.
                              NEXT.

                           END.

                           WHEN "lcOpCode" THEN DO:

                              RUN h-mnpoperator.
                              
                              IF siirto NE ? THEN DO:
                                 lcOpCode = siirto NO-ERROR.
                              END.

                              DISP lcOpCode WITH FRAME fUpdMNPData.
                              ehto = 9. RUN ufkey.
                              NEXT.

                           END.

                        END CASE.

                     END.

                     IF LOOKUP(nap,poisnap) > 0 THEN DO:

                        IF FRAME-FIELD = "lcCustIdType" THEN DO:

                           ASSIGN lcCustIdType.

                           FIND FIRST TMSCodes WHERE
                                      TMSCodes.TableName = "Customer"   AND
                                      TMSCodes.FieldName = "CustIdType" AND
                                      TMSCodes.CodeValue =  lcCustIdType AND
                                      lcCustIdType NE "N/A"
                           NO-LOCK NO-ERROR.

                           IF NOT AVAILABLE TMSCodes THEN DO:
                              MESSAGE
                                 "Invalid ID Type!"
                              VIEW-AS ALERT-BOX ERROR.
                              NEXT-PROMPT lcCustIdType.
                              NEXT.
                           END.

                        END.

                        ELSE IF FRAME-FIELD = "lcCustId" THEN DO:

                          ASSIGN lcCustId.

                          IF NOT fChkCustID(INPUT lcCustIDType,
                                            INPUT lcCustId) THEN DO:
                             MESSAGE
                                "Invalid customer ID"
                             VIEW-AS ALERT-BOX ERROR.

                             NEXT-PROMPT lcCustId.
                             NEXT.
                          END.

                        END.

                        ELSE IF FRAME-FIELD = "lcOpCode" THEN DO:

                           ASSIGN lcOpCode.

                           FIND FIRST MNPOperator WHERE
                                      MNPOperator.Brand = gcBrand AND
                                      MNPOperator.OperName = lcOpCode AND
                                      MNPOperator.Active EQ True
                           NO-LOCK NO-ERROR.

                           IF NOT AVAILABLE MNPOperator THEN DO:
                              MESSAGE
                                 "Operator does not exist!"
                              VIEW-AS ALERT-BOX ERROR.
                              NEXT-PROMPT lcOpCode.
                              NEXT.
                           END.
                           ELSE DO:
                              lcOpCode = MNPOperator.OperName.
                              NEXT-PROMPT lcCustIdType.
                           END.
                        END.

                     END.
                     
                     IF nap = "F1" THEN DO:
                        HIDE FRAME fUpdMNPData.
                        LEAVE MNPDATA.         
                     END. 

                     APPLY LASTKEY.

                  END.

               END.
               
               IF lcCustId ENTERED OR lcCustIdType ENTERED THEN DO:
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderCustomer).
                  FIND CURRENT OrderCustomer EXCLUSIVE-LOCK.
                  IF lcCustId ENTERED THEN OrderCustomer.CustId = lcCustId.
                  IF lcCustIdType ENTERED THEN 
                     OrderCustomer.CustIdType = lcCustIdType.
                  IF lcCompany ENTERED THEN OrderCustomer.Company = lcCompany.
                  IF lcFirstname ENTERED THEN OrderCustomer.FirstName = lcFirstname.
                  IF lcSurname1 ENTERED THEN OrderCustomer.Surname1 = lcSurname1.
                  IF lcSurname2 ENTERED THEN OrderCustomer.Surname2 = lcSurname2.
                  IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderCustomer).
                  RELEASE OrderCustomer.
               END.

               IF lcOpCode ENTERED OR lcOldICC ENTERED THEN DO:
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).
                  FIND CURRENT Order EXCLUSIVE-LOCK.
                  IF lcOpCode ENTERED THEN Order.CurrOper = lcOpCode.
                  IF lcOldICC ENTERED THEN Order.OldICC = lcOldICC.
                  IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
                  RELEASE Order.
               END.


            END.
            
            WHEN 2 THEN DO:

               ok = FALSE.
               MESSAGE 
                  "ARE YOU SURE YOU WANT TO CREATE A NEW MNP PROCESS (Y/N) ?"
               UPDATE ok.
               IF NOT ok THEN NEXT.

               RUN mnpresend.p(Order.Orderid).

               LEAVE CHOISES.

            END.

            WHEN 3 THEN LEAVE CHOISES.

         END.
         /*
         LEAVE CHOISES.
         */
      END.
      
 
   END.
   /* OTHERS */
   ELSE PAUSE.

   HIDE FRAME lis.
   HIDE FRAME fUpdMNPData.

END PROCEDURE.

