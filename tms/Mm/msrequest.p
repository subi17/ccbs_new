/* ----------------------------------------------------------------------
  MODULE .......: MsRequest
  TASK .........: UPDATEs table MsRequest
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.12.04
  CHANGED ......: 15.12.05/aam new parameter to runscreqim
                  30.12.05/aam new request type 2 
                  11.01.06/aam new request type 3 
                  25.01.06/aam new request types 4-6
                  06.02.06/aam new request types 7-9
                  22.03.06/aam new request types 10-12
                  23.08.06 jp  types 13
                  12.12.06/mvi input parameter iiReqStatus
                  12.12.06/mvi show lcStatus for <= 20 in fReqStatus
                  17.04.07/aam functionality to detailed view 
                               (memo, eventlog, functions)
                  15.05.07/mvi enabled functions for status 19 
                  10.09.07/aam ReqIParam2 change for type 20
                  31.10.07/jp  new input parameter iimsrequest
                  05.11.07/jt  reqstat -> reqtype (when checking request
                               functions)
                  xx.xx.xx/jt  Show more detailed solog/memo info
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable MsRequest

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MsRequest'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMsRequest).
   END.      

END.

DEF INPUT PARAMETER iiReqType   AS INT NO-UNDO.
DEF INPUT PARAMETER iiReqStatus AS INT NO-UNDO.
DEF INPUT PARAMETER iiMSSeq     AS INT NO-UNDO.
DEF INPUT PARAMETER iiCustNum   AS INT NO-UNDO.
DEF INPUT PARAMETER iiMSRequest AS INT NO-UNDO.
DEF INPUT PARAMETER icFilter    AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF BUFFER SubRequest FOR MSREquest .

DEF VAR liRequest    AS INT                    NO-UNDO. 
DEF VAR lcCLI        AS CHAR                   NO-UNDO.
DEF VAR liCustNum    AS INT                    NO-UNDO. 
DEF VAR liStatus     AS INT                    NO-UNDO. 
DEF VAR ldtDate      AS DATE                   NO-UNDO. 
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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
DEF VAR lcStatus     AS CHAR                   NO-UNDO. 
DEF VAR lcCreated    AS CHAR                   NO-UNDO.
DEF VAR lcActivate   AS CHAR                   NO-UNDO. 
DEF VAR lcDone       AS CHAR                   NO-UNDO. 
DEF VAR ldtStampDate AS DATE                   NO-UNDO.
DEF VAR liStampTime  AS INT                    NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR ldtActivate  AS DATE                   NO-UNDO. 
DEF VAR ldtHandled   AS DATE                   NO-UNDO.
DEF VAR ldCurrStamp  AS DEC                    NO-UNDO.
DEF VAR lcInfo       AS CHAR                   NO-UNDO. 
DEF VAR llSendSMS    AS LOG                    NO-UNDO. 
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR llGenBrowse  AS LOG                    NO-UNDO.
DEF VAR lcReqType    AS CHAR                   NO-UNDO.
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR llChoose     AS LOGICAL                NO-UNDO.
DEF VAR lcCancelReason AS CHAR FORMAT "X(60)"  NO-UNDO.
DEF VAR lcIParam1Desc  AS CHAR                 NO-UNDO.
DEF VAR lcIParam2Desc  AS CHAR                 NO-UNDO.
DEF VAR liLoop       AS INTEGER                NO-UNDO.
DEF VAR lcMainRequest  AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEF VAR lcMandatory    AS CHARACTER FORMAT "X(9)"   NO-UNDO.
DEF VAR lcReqStats   AS CHARACTER              NO-UNDO.
DEF VAR lcReqSource  AS CHAR                   NO-UNDO.
DEF VAR liIntCheck   AS INT                    NO-UNDO.
DEF VAR lcUpdated    AS CHAR                   NO-UNDO.
DEF VAR lcReqParamValue AS CHAR                NO-UNDO.

IF icFilter NE "" THEN
   lcReqParamValue = icFilter.

form
    MsRequest.MsRequest   FORMAT ">>>>>>>>9"
    MsRequest.CLI         COLUMN-LABEL "MSISDN"   FORMAT "X(12)"
    MsRequest.CustNum     COLUMN-LABEL "Cust.Nbr" FORMAT ">>>>>>>>9"
    lcCustName            COLUMN-LABEL "Name"     FORMAT "X(15)" 
    ldtActivate           COLUMN-LABEL "Activate" FORMAT "99-99-99"
    ldtHandled            COLUMN-LABEL "Handled"  FORMAT "99-99-99"
    MsRequest.ReqType     COLUMN-LABEL "Type"     FORMAT ">>9"
    MsRequest.ReqStatus   COLUMN-LABEL "S"        FORMAT ">9"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       "  Requests "  + string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    MsRequest.MsRequest  COLON 18  FORMAT ">>>>>>>>9"
    lcMainRequest NO-LABEL
    lcMandatory   NO-LABEL TO 78
    MsRequest.MSSeq      COLON 18   
       LABEL "Subscription ID"
    MsRequest.CLI        COLON 18
    MsRequest.CustNum    COLON 18 FORMAT ">>>>>>>>9"
       lcCustName NO-LABEL FORMAT "X(30)" SKIP(1)

    MsRequest.ReqType     COLON 18 FORMAT ">>9"
       lcReqType NO-LABEL FORMAT "X(30)" SKIP 
    MsRequest.ReqSource   COLON 18 FORMAT "x(2)" LABEL "Request Source" 
       lcReqSource NO-LABEL FORMAT "X(30)" SKIP
    MsRequest.UserCode   COLON 18 FORMAT "X(20)"
       TMSUser.UserName NO-LABEL SKIP
    MsRequest.CreateFees  COLON 18 SKIP
    llSendSMS             COLON 18  
       LABEL "Send SMS" FORMAT "Yes/No" SKIP
    MsRequest.SMSText     COLON 18 FORMAT "X(20)" SKIP   
    MsRequest.ReqStatus   COLON 18 FORMAT ">9"
       lcStatus NO-LABEL FORMAT "X(30)" SKIP(1)
       
    MsRequest.CreStamp   COLON 18 LABEL "Created"
       lcCreated NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.ActStamp   COLON 18 LABEL "Activation"
       lcActivate NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.UpdateStamp COLON 18 LABEL "Latest Update"
       lcUpdated NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.DoneStamp   COLON 18 LABEL "Handled"
       lcDone NO-LABEL FORMAT "X(20)" SKIP
       
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
   MsRequest.Memo VIEW-AS EDITOR SIZE 70 BY 10
   WITH OVERLAY ROW 4 CENTERED TITLE " INFO TEXT " NO-LABELS FRAME fIntMemo.
   
form /* seek  MsRequest */
    "Brand .:" lcBrand skip
    "Request:" liRequest FORMAT ">>>>>>>>>9"
    HELP "Enter request ID"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  MsRequest */
    "Brand:" lcBrand skip
    "CLI .:" lcCLI FORMAT "X(15)"
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND CLI "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum  FORMAT ">>>>>>>9"
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Customer "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  ActStamp */
    "Brand :" lcBrand skip
    "Status:" liStatus FORMAT "9"
    HELP "Enter Status"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Status "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f4.

FUNCTION fReqStatus RETURNS LOGICAL
   (iiStatus AS INT).

   lcStatus = Func.Common:mTMSCodeName("MsRequest",
                               "ReqStatus",
                               STRING(iiStatus)).

   DISPLAY lcStatus WITH FRAME lis.
   
   RETURN (lcStatus > ""). 
   
END FUNCTION.

FUNCTION fDispStamp RETURNS CHARACTER
  (idTimeStamp AS DEC).

  DEF VAR lcStamp AS CHAR NO-UNDO.
  
  IF idTimeStamp > 0 THEN DO:
     Func.Common:mSplitTS(idTimeStamp,
              OUTPUT ldtStampDate,
              OUTPUT liStampTime).
     lcStamp = "(" + STRING(ldtStampDate,"99-99-99") + " " +
                     STRING(liStampTime,"hh:mm:ss") + ")".
  END.
  ELSE lcStamp = "".

  RETURN lcStamp.

END FUNCTION.

FUNCTION fFrameTitle RETURNS LOGIC
  (icTitle AS CHAR).

   FRAME sel:TITLE = " " + Syst.Var:ynimi + "  " +
                     icTitle + "  " +
                     string(TODAY,"99-99-99") + " ".                               
END FUNCTION.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


/***************** CREATE LIST OF Requests that have functions ************/
DEFINE VARIABLE lcTypes AS CHARACTER NO-UNDO.

IF iiReqType >= 0 THEN 
FOR FIRST MsReqStatFunc NO-LOCK WHERE
          MsReqStatFunc.ReqType = iiReqType AND
          LENGTH(MsReqStatFunc.FuncGroup) > 2:
   lcTypes = STRING(MsReqStatFunc.ReqType).
END.

ELSE 
FOR EACH RequestType NO-LOCK WHERE
         RequestType.Brand = Syst.Var:gcBrand,
   FIRST MsReqStatFunc NO-LOCK WHERE
         MsReqStatFunc.ReqType = RequestType.ReqType AND
         LENGTH(MsReqStatFunc.FuncGroup) > 2:
   lcTypes = lcTypes + 
             (IF lcTypes > "" THEN "," ELSE "") + 
             STRING(MsReqStatFunc.ReqType).
END.

   
/***************************************************************************/

IF iiReqType >= 0 THEN DO:
   FIND RequestType WHERE
        RequestType.Brand   = Syst.Var:gcBrand AND
        RequestType.ReqType = iiReqType NO-LOCK NO-ERROR.
           
   IF NOT AVAILABLE RequestType THEN DO:
      MESSAGE "Unknown request type"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   lcTitle = RequestType.ReqName.
END.

ELSE lcTitle = "Requests".

fFrameTitle(lcTitle).

IF iiMSSeq > 0 OR iiCustNum > 0 THEN MaxOrder = 1.
ELSE IF iiReqStatus > 0 THEN MaxOrder = 2.
    
RUN local-find-first.

IF AVAILABLE MsRequest THEN ASSIGN
   Memory       = recid(MsRequest)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a MsRequest  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           IF iiMsSeq = 0 THEN 
              PROMPT-FOR MsRequest.MSSeq.
           ELSE DISPLAY iiMsSeq @ MsRequest.MSSeq WITH FRAME lis.
           

           IF INPUT FRAME lis MsRequest.MSSeq = 0
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST MsRequest WHERE 
                    MsRequest.Brand    = lcBrand AND
                    MsRequest.MSSeq = INPUT FRAME lis MsRequest.MSSeq AND
                    MsRequest.ReqStatus < 2)
           THEN DO:
              MESSAGE 
              "CLI type change for subscription" 
              INPUT FRAME lis MsRequest.MSSeq 
              "is already under way"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.

           FIND MobSub WHERE MobSub.MSSeq = INPUT FRAME lis MsRequest.MSSeq
           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE MobSub THEN DO:
              MESSAGE "Unknown subscription"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
           
           CREATE MsRequest.
           ASSIGN
              MsRequest.Brand    = lcBrand
              MsRequest.MSSeq    = INPUT FRAME lis MsRequest.MSSeq
              MsRequest.CLI      = MobSub.CLI
              MsRequest.CustNum  = MobSub.CustNum
              MsRequest.UserCode = Syst.Var:katun.
              MsRequest.CreStamp = Func.Common:mMakeTS().

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMsRequest).

           ASSIGN
           Memory = recid(MsRequest)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MsRequest WHERE MsRequest.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsRequest THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MsRequest WHERE recid(MsRequest) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MsRequest THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MsRequest).
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
           Syst.Var:ufk   = 0
           Syst.Var:ufk[1]= 135  
           Syst.Var:ufk[2]= 653 
           Syst.Var:ufk[3]= 714  
           Syst.Var:ufk[4]= IF iiReqStatus = ? THEN 559 ELSE 0
           Syst.Var:ufk[8]= 8 
           Syst.Var:ehto = 3 ufkey = FALSE.

        IF iiMSSeq > 0 OR iiCustNum > 0 THEN ASSIGN
           Syst.Var:ufk[1] = 0
           Syst.Var:ufk[2] = 0
           Syst.Var:ufk[3] = 0
           Syst.Var:ufk[4] = 0.
        
        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MsRequest.MsRequest {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MsRequest.MsRequest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MsRequest.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MsRequest.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW MsRequest.ReqStatus {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MsRequest.ReqStatus WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MsRequest WHERE recid(MsRequest) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MsRequest THEN
              ASSIGN FIRSTrow = i Memory = recid(MsRequest).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MsRequest THEN DO:
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
                rtab[1] = recid(MsRequest)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE MsRequest THEN DO:
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
              rtab[FRAME-DOWN] = recid(MsRequest).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MsRequest WHERE recid(MsRequest) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MsRequest THEN DO:
           Memory = recid(MsRequest).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MsRequest THEN Memory = recid(MsRequest).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND MsRequest WHERE recid(MsRequest) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN Syst.Var:gcAllBrand
              liRequest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liRequest > 0 THEN DO:
       
          FIND MsRequest WHERE 
               MsRequest.MsRequest = liRequest
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE MsRequest OR MsRequest.Brand NE Syst.Var:gcBrand OR
             MsRequest.ReqType NE iiReqType OR
             (iiReqStatus NE ? AND MsRequest.ReqStat NE iiReqStatus)
          THEN RELEASE MsRequest.

/* This was too slow */
/*  THEN FIND LAST MsRequest WHERE
                         MsRequest.Brand     = Syst.Var:gcBrand   AND
                         MsRequest.ReqType   = iiReqType AND
                         MsRequest.MsRequest > liRequest AND
                         (IF iiReqStatus NE ? 
                          THEN MsRequest.ReqStat = iiReqStatus
                          ELSE TRUE) 
               USE-INDEX BrandType NO-LOCK NO-ERROR.
*/                    
          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND Syst.Var:ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN Syst.Var:gcAllBrand
              lcCLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcCLI > "" THEN DO:
          FIND FIRST MsRequest WHERE 
                     MsRequest.Brand   = lcBrand   AND
                     MsRequest.ReqType = iiReqType AND
                     MsRequest.CLI    >= lcCLI     AND
                     (IF iiReqStatus NE ? 
                      THEN MsRequest.ReqStat = iiReqStatus
                      ELSE TRUE)                      
          USE-INDEX CLI NO-LOCK NO-ERROR.

          IF NOT fRecFound(order) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */


     /* Search BY col 3 */
     ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND Syst.Var:ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN Syst.Var:gcAllBrand
              liCustNum WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF liCustNum > 0 THEN DO:
          FIND FIRST MsRequest WHERE 
                     MsRequest.Brand    = lcBrand   AND
                     MsRequest.ReqType  = iiReqType AND
                     MsRequest.CustNum >= liCustNum AND
                    (IF iiReqStatus NE ? 
                     THEN MsRequest.ReqStat = iiReqStatus
                     ELSE TRUE)                      
          USE-INDEX CustNum NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* Search BY col 4 */
     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 AND Syst.Var:ufk[4] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F4.
       DISPLAY lcBrand WITH FRAME F4.
       UPDATE lcBrand WHEN Syst.Var:gcAllBrand
              liStatus WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       FIND FIRST MsRequest WHERE 
                  MsRequest.Brand     = lcBrand   AND
                  MsRequest.ReqType   = iiReqType AND
                  MsRequest.ReqStatus >= liStatus AND
                 (IF iiReqStatus NE ? 
                  THEN MsRequest.ReqStat = iiReqStatus
                  ELSE TRUE)   
       USE-INDEX ReqType NO-LOCK NO-ERROR.

       IF NOT fRecFound(4) THEN NEXT BROWSE.

       NEXT LOOP.
     END. /* Search-4 */

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       MESSAGE "Deleting!" VIEW-AS ALERT-BOX.
       RUN local-find-this (FALSE).

       IF MsRequest.ReqStatus >= 2 THEN DO:
          MESSAGE "CLI type change has already been handled."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX ERROR.
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       MsRequest.CLI MsRequest.CustNum.

       RUN local-find-NEXT.
       IF AVAILABLE MsRequest THEN Memory = recid(MsRequest).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MsRequest THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MsRequest).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       MsRequest.CLI MsRequest.CustNum .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMsRequest).

           DELETE MsRequest.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE MsRequest THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis 
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MsRequest.MSSeq.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).

       RUN local-disp-row.
       xrecid = recid(MsRequest).
       
       RELEASE MsRequest.
       
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MsRequest) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MsRequest) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MsRequest WHERE recid(MsRequest) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MsRequest WHERE recid(MsRequest) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiMSRequest > 0 THEN DO: 

      IF iiMSSeq > 0 THEN 
      FIND FIRST MsRequest WHERE 
                 MSRequest.MSrequest = iiMSRequest NO-LOCK NO-ERROR.
      ELSE 
      FIND FIRST MsRequest WHERE
                 MSRequest.OrigRequest = iiMSRequest NO-LOCK NO-ERROR.
   END.
   ELSE 
   IF iiMSSeq > 0 THEN DO:
      IF iiReqType EQ 1 AND lcReqParamValue NE ""  THEN DO:
         FIND FIRST MsRequest
             WHERE MsRequest.MSSeq  = iiMSSeq AND
                   MsRequest.ReqType = iiReqType  AND
                   IF iiReqstatus NE ? THEN
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam1 EQ  lcReqParamValue
         NO-LOCK NO-ERROR.
      END. 
      ELSE IF iiReqType >= 0 THEN 
         FIND FIRST MsRequest 
              WHERE MsRequest.MSSeq   = iiMSSeq AND
                    MsRequest.ReqType = iiReqType AND
                 IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
                     
         NO-LOCK NO-ERROR.
      ELSE IF lcReqParamValue NE "" THEN
         
         FIND FIRST MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   LOOKUP(STRING(MsRequest.ReqType),"8,9") > 0 AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam3 = lcReqParamValue 
         NO-LOCK NO-ERROR.
      
      ELSE FIND FIRST MsRequest 

                WHERE MsRequest.MSSeq = iiMSSeq AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                ELSE TRUE
                USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCustNum > 0 THEN DO:
       FIND FIRST MsRequest USE-INDEX Custnum
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.Custnum = iiCustNum AND
                (iiReqstatus EQ ? OR MsRequest.ReqStatus = iiReqStatus)
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN DO:
          IF iiReqstatus NE ? THEN
          FIND FIRST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.ReqStatus = iiReqStatus
          USE-INDEX ReqType NO-LOCK NO-ERROR.
 
        ELSE 
          FIND FIRST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType 
          USE-INDEX BrandType NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND FIRST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
        
       ELSE IF order = 3 THEN FIND FIRST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX ReqType
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-LAST:


   IF iiMSRequest > 0 THEN DO: 
      
      IF iiMSSeq > 0 THEN 
      FIND LAST  MsRequest WHERE 
                 MSRequest.MSrequest = iiMSRequest NO-LOCK NO-ERROR.
      ELSE 
      FIND LAST  MsRequest WHERE
                 MSRequest.OrigRequest = iiMSRequest NO-LOCK NO-ERROR.
   END.
   ELSE IF iiMSSeq > 0 THEN DO:
      IF iiReqType EQ 1 AND lcReqParamValue NE ""  THEN DO:
         FIND LAST MsRequest
             WHERE MsRequest.MSSeq  = iiMSSeq AND
                   MsRequest.ReqType = iiReqType AND
                   IF iiReqstatus NE ? THEN
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam1 EQ  lcReqParamValue
         NO-LOCK NO-ERROR.
      END. 

      ELSE IF iiReqType >= 0 THEN 
         FIND LAST MsRequest WHERE
                   MsRequest.MSSeq   = iiMSSeq   AND
                   MsRequest.ReqType = iiReqType AND
         IF iiReqstatus NE ? THEN 
                   MsRequest.ReqStatus = iiReqStatus
         ELSE TRUE
                  
         NO-LOCK NO-ERROR.
      ELSE IF lcReqParamValue NE "" THEN 
         FIND LAST MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   LOOKUP(STRING(MsRequest.ReqType),"8,9") > 0 AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam3 = lcReqParamValue
         NO-LOCK NO-ERROR.
      ELSE FIND LAST MsRequest WHERE
                     MsRequest.MSSeq = iiMSSeq AND
               IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
           USE-INDEX MsActStamp NO-LOCK NO-ERROR.
    END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND LAST MsRequest USE-INDEX Custnum WHERE
                 MsRequest.Brand = lcBrand    AND
                 MsRequest.ReqType = iiReqType AND
                 MsRequest.Custnum = iiCustNum AND
                (iiReqstatus EQ ? OR MsRequest.ReqStatus = iiReqStatus)
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN DO: 
          IF iiReqstatus NE ? THEN
          FIND LAST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.ReqStatus = iiReqStatus
          USE-INDEX ReqType NO-LOCK NO-ERROR.
 
          ELSE 
          FIND LAST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType 
          USE-INDEX BrandType NO-LOCK NO-ERROR.
       END.   
       ELSE IF order = 2 THEN FIND LAST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX ReqType
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiMSRequest > 0 THEN DO: 
      
      IF iiMSSeq > 0 THEN 
      FIND NEXT  MsRequest WHERE 
                 MSRequest.MSrequest = iiMSRequest NO-LOCK NO-ERROR.
      ELSE 
      FIND NEXT  MsRequest WHERE
                 MSRequest.OrigRequest = iiMSRequest NO-LOCK NO-ERROR.
   END.
   ELSE IF iiMSSeq > 0 THEN DO:
      IF iiReqType EQ 1 AND lcReqParamValue NE ""  THEN DO:
         FIND NEXT MsRequest
             WHERE MsRequest.MSSeq  = iiMSSeq AND
                   MsRequest.ReqType = iiReqType AND
                   IF iiReqstatus NE ? THEN
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam1 EQ  lcReqParamValue
         NO-LOCK NO-ERROR.
      END. 

 
      ELSE IF iiReqType >= 0 THEN 
         FIND NEXT MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   MsRequest.ReqType = iiReqType AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE
         NO-LOCK NO-ERROR.
      
      ELSE IF lcReqParamValue NE "" THEN 
         FIND NEXT MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   LOOKUP(STRING(MsRequest.ReqType),"8,9") > 0 AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam3 = lcReqParamValue 
         NO-LOCK NO-ERROR.
      
      ELSE FIND NEXT MsRequest 
               WHERE MsRequest.MSSeq = iiMSSeq AND
               IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
               USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND NEXT MsRequest USE-INDEX Custnum
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.Custnum = iiCustNum AND
               (iiReqstatus EQ ? OR MsRequest.ReqStatus = iiReqStatus)
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN DO: 
          IF iiReqstatus NE ? THEN
          FIND NEXT MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.ReqStatus = iiReqStatus
          USE-INDEX ReqType NO-LOCK NO-ERROR.
 
          ELSE 
          FIND NEXT MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType 
          USE-INDEX BrandType NO-LOCK NO-ERROR.
       END.   
       ELSE IF order = 2 THEN FIND NEXT MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX ReqType
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF iiMSRequest > 0 THEN DO: 
      
      IF iiMSSeq > 0 THEN 
      FIND Prev MsRequest WHERE 
                 MSRequest.MSrequest = iiMSRequest NO-LOCK NO-ERROR.
      ELSE 
      FIND Prev MsRequest WHERE
                MSRequest.OrigRequest = iiMSRequest NO-LOCK NO-ERROR.
   END.
   ELSE IF iiMSSeq > 0 THEN DO:
      IF iiReqType EQ 1 AND lcReqParamValue NE ""  THEN DO:
         FIND PREV MsRequest
             WHERE MsRequest.MSSeq  = iiMSSeq AND
                   MsRequest.ReqType = iiReqType AND
                   IF iiReqstatus NE ? THEN
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam1 EQ  lcReqParamValue
         NO-LOCK NO-ERROR.
      END. 
     ELSE IF iiReqType >= 0 THEN 
         FIND PREV MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   MsRequest.ReqType = iiReqType AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
         NO-LOCK NO-ERROR.
      ELSE IF lcReqParamValue NE "" THEN 
         FIND PREV MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   LOOKUP(STRING(MsRequest.ReqType),"8,9") > 0 AND
                   IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                   ELSE TRUE AND
                   MsRequest.ReqCParam3 = lcReqParamValue 
         NO-LOCK NO-ERROR.
      ELSE FIND PREV MsRequest 
               WHERE MsRequest.MSSeq = iiMSSeq AND
               IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
                USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND PREV MsRequest USE-INDEX Custnum
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.Custnum = iiCustNum AND
               (iiReqstatus EQ ? OR MsRequest.ReqStatus = iiReqStatus)
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN DO: 
          IF iiReqstatus NE ? THEN
          FIND PREV MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                MsRequest.ReqStatus = iiReqStatus
          USE-INDEX ReqType NO-LOCK NO-ERROR.
 
          ELSE 
          FIND PREV MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType 
          USE-INDEX BrandType NO-LOCK NO-ERROR.
       END.   
       ELSE IF order = 2 THEN FIND PREV MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = iiReqType AND
                IF iiReqstatus NE ? THEN 
                     MsRequest.ReqStatus = iiReqStatus
                 ELSE TRUE
          USE-INDEX ReqType
        NO-LOCK NO-ERROR.
   END.
        
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   IF iiReqType EQ 1 AND lcReqParamValue NE "" THEN DO:
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY
       MsRequest.MsRequest
       MsRequest.CLI
       MsRequest.CustNum
       MsRequest.UserCode  @ lcCustName COLUMN-LABEL "Changed by"
       ldtActivate
       ldtHandled
       MsRequest.ReqType
       MsRequest.ReqStatus
       WITH FRAME sel.

   END.
   ELSE DO: /*old functionality*/
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MsRequest.MsRequest
       MsRequest.CLI 
       MsRequest.CustNum
       lcCustName WHEN AVAILABLE Customer
       ldtActivate
       ldtHandled
       MsRequest.ReqType
       MsRequest.ReqStatus
       WITH FRAME sel.
   END.
END PROCEDURE.

PROCEDURE local-find-others.

    FIND Customer OF MsRequest NO-LOCK NO-ERROR.

    IF AVAILABLE Customer THEN    
       lcCustName = Func.Common:mDispCustName(BUFFER Customer).
    ELSE lcCustName = "".
          
    IF   MSrequest.OrigRequest > 0 
    THEN lcMainRequest = "(Main request:" + STRING(MSrequest.OrigRequest) + ")".
    ELSE lcMainRequest = "".

    IF MSRequest.Mandatory = 1 THEN lcMandatory = "MANDATORY".
    ELSE                            lcMandatory = "".

    Func.Common:mSplitTS(MsRequest.ActStamp,
             OUTPUT ldtActivate,
             OUTPUT liStampTime).

    IF MsRequest.DoneStamp = 0 
    THEN ldtHandled = ?.
    ELSE Func.Common:mSplitTS(MsRequest.DoneStamp,
                  OUTPUT ldtHandled,
                  OUTPUT liStampTime).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   PAUSE 0.
   VIEW FRAME lis.

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.

      ASSIGN
         lcCreated  = fDispStamp(MsRequest.CreStamp)
         lcActivate = fDispStamp(MsRequest.ActStamp)
         lcUpdated  = fDispStamp(MsRequest.UpdateStamp)
         lcDone     = fDispStamp(MsRequest.DoneStamp)
         llSendSMS  = (MsRequest.SendSMS = 1).

      fReqStatus(MsRequest.ReqStatus).
      
      FIND TMSUser WHERE
           TMSUser.UserCode = MsRequest.UserCode 
      NO-LOCK NO-ERROR.
      
      FIND RequestType WHERE
           RequestType.Brand   = Syst.Var:gcBrand AND
           RequestType.ReqType = MsRequest.ReqType NO-LOCK NO-ERROR.
      IF AVAILABLE RequestType 
      THEN lcReqType = RequestType.ReqName.
      ELSE lcReqType = "Unknown".
         
      lcReqSource = Func.Common:mTMSCodeName("MsRequest",
                                     "ReqSource",
                                     MsRequest.ReqSource).
     
      DISP MsRequest.MsRequest 
           lcMainRequest
           lcMandatory
           MsRequest.MSSeq
           MsRequest.CLI
           MsRequest.CustNum
           lcCustName WHEN AVAILABLE Customer
           MsRequest.UserCode
           TMSUser.UserName WHEN AVAILABLE TMSUser
           MsRequest.ReqType lcReqType
           MsRequest.ReqSource lcReqSource
           MsRequest.CreateFees
           llSendSMS
           MsRequest.SMSText
           MsRequest.ReqStatus
           MsRequest.CreStamp lcCreated
           MsRequest.ActStamp lcActivate
           MsRequest.UpdateStamp lcUpdated
           MsRequest.DoneStamp lcDone
      WITH FRAME lis.
      
      ASSIGN Syst.Var:ufk    = 0 
             Syst.Var:ehto   = 0
             Syst.Var:ufk[1] = 9861 /* SAPC-46 */
             Syst.Var:ufk[2] = 1643
             Syst.Var:ufk[4] = 927
             Syst.Var:ufk[5] = 1697
             Syst.Var:ufk[6] = 1752
             Syst.Var:ufk[8] = 8.

      IF MSRequest.OrigReQuest > 0 THEN Syst.Var:ufk[3] = 2953.
      ELSE DO:
         FIND FIRST SubRequest WHERE 
                    SubRequest.OrigRequest = MSRequest.MSRequest 
         NO-LOCK NO-ERROR.
         IF AVAIL SubRequest THEN Syst.Var:ufk[3] = 2954.           
      END.
      
      /* 2nd loop */
      IF iiMSRequest > 0 THEN DO:
         ASSIGN
         Syst.Var:ufk[3] = 0.
      END.
      
      IF lcRight = "RW" THEN DO:
         
         /* Check if statuses and functions are allowed for this type */

         IF LOOKUP(STRING(MsRequest.ReqType),lcTypes) > 0 THEN DO:
            FIND MsReqStatFunc NO-LOCK WHERE
                 MsReqStatFunc.ReqType   = MsRequest.ReqType AND
                 MsReqStatFunc.ReqStatus = MsRequest.ReqStatus
            NO-ERROR.

            IF AVAILABLE MsReqStatFunc THEN 
            DO liLoop = 2 TO NUM-ENTRIES(MsReqStatFunc.FuncGroup,","):
               FIND MsReqFuncItem NO-LOCK WHERE
                    MsReqFuncItem.ItemId = 
                    ENTRY(liLoop,MsReqStatFunc.FuncGroup,",")
               NO-ERROR.
               IF AVAILABLE MsReqFuncItem THEN Syst.Var:ufk[7] = 9074. 
            END. 
         END.
      END.
                
      RUN Syst/ufkey.p. 

      /* SAPC-46  SAPC Commands */     
      IF Syst.Var:toimi = 1 THEN DO:
          RUN Mc/ProCommandView.p(INPUT 0,  /* mobsub.msseq */
                                  INPUT MSRequest.MSrequest). 
       
      END. 
      ELSE 
      /* parameters */  
      IF Syst.Var:toimi = 2 THEN DO:
         RUN Mm/msreqparam.p(MSRequest.MSrequest).
      END.
      
      ELSE IF Syst.Var:toimi = 3 THEN DO:

         /* search main-request*/
         IF   MSRequest.origRequest > 0 
         THEN RUN Mm/msrequest.p(?,
                            ?, 
                            MSRequest.MSrequest, 
                            ?, 
                            Msrequest.OrigRequest,
                            "").
         /* search sub-requests */
         ELSE RUN Mm/msrequest.p(?,
                            ?,
                            ?,
                            ?,
                            Msrequest.MSRequest,
                            "").
      END.
      ELSE IF Syst.Var:toimi = 4 THEN DO:

        RUN Mc/memo.p(INPUT MsRequest.CustNum,
                 INPUT "MsRequest",
                 INPUT STRING(MsRequest.MsRequest),
                 INPUT "Request").
      END.

      /* internal memo */
      ELSE IF Syst.Var:toimi = 5 THEN DO:
      
            PAUSE 0.
            DISP MsRequest.Memo WITH FRAME fIntMemo.
            PAUSE MESSAGE "Press ENTER to continue".
            HIDE FRAME fIntMemo NO-PAUSE.

      END.
      
      /* show eventlog */
      ELSE IF Syst.Var:toimi = 6 THEN DO:

        RUN Mc/eventsel.p("MsRequest",
                     STRING(MsRequest.MsRequest)).
      END.

      /* change status */
      ELSE IF Syst.Var:toimi = 7 THEN DO:
         RUN Syst/msreqfuncmenu.p(MsRequest.MsRequest).
      END.

      ELSE IF Syst.Var:toimi = 8 THEN LEAVE. 
   
   END.
   
END PROCEDURE.

