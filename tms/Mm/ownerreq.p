/* ----------------------------------------------------------------------
  MODULE .......: ownerreq
  TASK .........: subscription owner change requests
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.02.06
  CHANGED ......: 27.04.06/aam cli-find with 'iiReqStat NE ?'
                  27.09.06/aam adv.payment invoice,
                               status 16 means approved (before 15),
                               display adv.payment invoice date when status 14
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable MsRequest

{commali.i}
{timestamp.i}
{cparam2.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MsRequest'}
{msreqfunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhMsRequest).
   END.      

END.

DEF INPUT PARAMETER iiMSSeq   AS INT NO-UNDO.
DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.
DEF INPUT PARAMETER iiReqStat AS INT NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

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
DEF VAR maxOrder     AS INT                    NO-UNDO  init 4.
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
DEF VAR lcStatusLst  AS CHAR                   NO-UNDO. 
DEF VAR lcCreated    AS CHAR                   NO-UNDO.
DEF VAR lcActivate   AS CHAR                   NO-UNDO. 
DEF VAR lcDone       AS CHAR                   NO-UNDO. 
DEF VAR ldtStampDate AS DATE                   NO-UNDO.
DEF VAR liStampTime  AS INT                    NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR ldtActivate  AS DATE                   NO-UNDO. 
DEF VAR lcInfo       AS CHAR                   NO-UNDO. 
DEF VAR llSendSMS    AS LOG                    NO-UNDO. 
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcDispParam  AS CHAR                   NO-UNDO.
DEF VAR llGenBrowse  AS LOG                    NO-UNDO.
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR lcNewName    AS CHAR                   NO-UNDO. 
DEF VAR liReqType    AS INT                    NO-UNDO INIT 10.
DEF VAR lcError      AS CHAR                   NO-UNDO. 
DEF VAR lcPassword   AS CHAR                   NO-UNDO. 
DEF VAR lcAskPassWd  AS CHAR                   NO-UNDO.
DEF VAR lcCancelTxt  AS CHAR                   NO-UNDO.
DEF VAR ldtAdvDate   AS DATE                   NO-UNDO.
DEF VAR liAdvDays    AS INT                    NO-UNDO.
DEF VAR liAdvLimit   AS INT                    NO-UNDO.
DEF VAR lcNote       AS CHAR                   NO-UNDO.


form
    MsRequest.MsRequest   FORMAT ">>>>>>>9"
    MsRequest.CLI         COLUMN-LABEL "MSISDN"    FORMAT "X(11)"   
    MsRequest.CustNum     COLUMN-LABEL "Old Agr."
    lcCustName            COLUMN-LABEL "Old Name"  FORMAT "X(14)" 
    ldtActivate           COLUMN-LABEL "Activate"  FORMAT "99-99-99"
    SPACE(0)
    lcNote                NO-LABEL FORMAT "X"
    SPACE(0)
    MsRequest.ReqIParam1  COLUMN-LABEL "New Agr."  FORMAT ">>>>>>>9"
    lcNewName             COLUMN-LABEL "New Name"  FORMAT "X(12)"
    MsRequest.ReqStatus   COLUMN-LABEL "S"         FORMAT ">9"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  OWNER CHANGES  "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    MsRequest.MsRequest  COLON 18  FORMAT ">>>>>>>9"
    MsRequest.MSSeq      COLON 18 LABEL "Subscription ID"  
    MsRequest.CLI        COLON 18
    MsRequest.CustNum    COLON 18  LABEL "Old Agr.Customer"
       lcCustName NO-LABEL FORMAT "X(30)" SKIP
    MsRequest.CreStamp   COLON 18
       lcCreated NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.UserCode   COLON 18 FORMAT "X(14)"
       TMSUser.UserName NO-LABEL SKIP
    MsRequest.ActStamp   COLON 18 
       lcActivate NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.ReqType    COLON 18 FORMAT ">9" SKIP
    MsRequest.ReqIParam1  COLON 18 LABEL "New Agr.Customer"
    MsRequest.ReqCParam1  COLON 18 LABEL "New Agr.Name" FORMAT "X(50)" SKIP
    MsRequest.CreateFees  COLON 18 
    llSendSMS             COLON 18 LABEL "Send SMS" FORMAT "Yes/No"
    MsRequest.ReqStatus   COLON 18 FORMAT ">9" 
       lcStatus NO-LABEL FORMAT "X(30)" SKIP
    MsRequest.DoneStamp   COLON 18
       lcDone NO-LABEL FORMAT "X(20)" SKIP
    MsRequest.Memo        COLON 18 
       FORMAT "X(55)" 
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  MsRequest */
    "Brand .:" lcBrand skip
    "Request:" liRequest FORMAT ">>>>>>>9"
    HELP "Enter request ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  MsRequest */
    "Brand:" lcBrand skip
    "CLI .:" lcCLI FORMAT "X(15)"
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum  FORMAT ">>>>>>>9"
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  ActStamp */
    "Brand :" lcBrand skip
    "Status:" liStatus FORMAT "9"
    HELP "Enter Status"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Status "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

FUNCTION fDispReqStatus RETURNS LOGICAL
   (iiStatus AS INT).

   IF iiStatus >= 0 AND iiStatus <= 16 
   THEN lcStatus = ENTRY(iiStatus + 1,lcStatusLst).
   ELSE lcStatus = "".
   
   DISPLAY lcStatus WITH FRAME lis.
   
   RETURN (lcStatus > ""). 
   
END FUNCTION.

FUNCTION fDispStamp RETURNS CHARACTER
  (idTimeStamp AS DEC).

  DEF VAR lcStamp AS CHAR NO-UNDO.
  
  IF idTimeStamp > 0 THEN DO:
     fSplitTS(idTimeStamp,
              OUTPUT ldtStampDate,
              OUTPUT liStampTime).
     lcStamp = "(" + STRING(ldtStampDate,"99-99-99") + " " +
                     STRING(liStampTime,"hh:mm:ss") + ")".
  END.
  ELSE lcStamp = "".

  RETURN lcStamp.

END FUNCTION.

DO i = 1 TO 17:
   lcStatusLst = lcStatusLst + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "MsRequest",
                                  "ReqStatus",
                                  STRING(i - 1)) + ",". 
END.

ASSIGN lcPassword = fCParamC("MSAddressChg")
       liAdvLimit = fCParamI("APOpenDays").
       
IF lcPassword = ? THEN lcPassword = "".
 
/* display adv.payment invoice data when status is 14 */
IF iiReqStat = 14 THEN ASSIGN 
   ldtActivate:LABEL IN FRAME sel          = "Inv.Date"
   MsRequest.ReqIParam1:LABEL IN FRAME sel = "    Days".
   
cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Request ," +
         "  By CLI     ," +
         "  By CustNum ," +
         "  By Status  ,".

IF iiMSSeq > 0 OR iiCustNum > 0 OR iiReqStat NE ? THEN MaxOrder = 1.

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
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

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
           MsRequest.UserCode = katun.
           MsRequest.CreStamp = fMakeTS().

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
        ufk[1]= 135  ufk[2]= 653 ufk[3]= 714  ufk[4]= 559
        ufk[5]= (IF lcRight = "RW" THEN 90 ELSE 0)
        ufk[6]= 927 
        ufk[7]= 1752 
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF iiMSSeq > 0 OR iiCustNum > 0 OR iiReqStat NE ? THEN ASSIGN
           ufk[1] = 0
           ufk[3] = 0
           ufk[4] = 0.
        
        IF iiMSSeq > 0 OR iiCustNum > 0 THEN ASSIGN
           ufk[2] = 0.

        IF iiReqStat NE ? THEN DO:
           IF iiReqStat NE 16 THEN ufk[5] = 0.
        END. 
        
        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MsRequest.MsRequest ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MsRequest.MsRequest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MsRequest.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MsRequest.CLI WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW MsRequest.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MsRequest.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MsRequest.ReqStatus ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MsRequest.ReqStatus WITH FRAME sel.
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
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
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
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
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
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
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
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              liRequest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liRequest > 0 THEN DO:
       
          FIND MsRequest WHERE 
               MsRequest.MsRequest = liRequest
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE MsRequest OR MsRequest.Brand NE gcBrand OR
             MsRequest.ReqType NE liReqType           
          THEN FIND LAST MsRequest WHERE
                         MsRequest.Brand     = gcBrand   AND
                         MsRequest.ReqType   = liReqType AND
                         MsRequest.MsRequest > liRequest NO-LOCK NO-ERROR.
                    
          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              lcCLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcCLI > "" THEN DO:
       
          IF iiReqStat NE ? THEN 
          FIND FIRST MsRequest USE-INDEX CLI WHERE 
                     MsRequest.Brand   = lcBrand   AND
                     MsRequest.ReqType = liReqType AND
                     MsRequest.CLI     = lcCLI     AND
                     MsRequest.ReqStat = iiReqStat 
          NO-LOCK NO-ERROR.

          ELSE 
          FIND FIRST MsRequest WHERE 
                     MsRequest.Brand   = lcBrand   AND
                     MsRequest.ReqType = liReqType AND
                     MsRequest.CLI    >= lcCLI
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.
          
          IF iiReqStat NE ? THEN order = 1.

          NEXT LOOP.
       END.
     END. /* Search-2 */


     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              liCustNum WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF liCustNum > 0 THEN DO:
          FIND FIRST MsRequest WHERE 
                     MsRequest.Brand    = lcBrand   AND
                     MsRequest.ReqType  = liReqType AND
                     MsRequest.CustNum >= liCustNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* Search BY col 4 */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F4.
       DISPLAY lcBrand WITH FRAME F4.
       UPDATE lcBrand WHEN gcAllBrand
              liStatus WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       FIND FIRST MsRequest WHERE 
                  MsRequest.Brand     = lcBrand   AND
                  MsRequest.ReqType   = liReqType AND
                  MsRequest.ReqStatus >= liStatus
       NO-LOCK NO-ERROR.

       IF NOT fRecFound(4) THEN NEXT BROWSE.

       NEXT LOOP.
     END. /* Search-4 */

     /* run a request immediately */
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:
     
        RUN local-find-this(FALSE).
        
        IF NOT AVAILABLE MsRequest THEN NEXT.
        
        IF MsRequest.ReqStat NE 16 THEN DO:
           MESSAGE "This is not an approved, unhandled request"
           VIEW-AS ALERT-BOX ERROR.
           NEXT.
        END.

        RUN local-find-others.
        
        ok = FALSE.
        
        IF ldtActivate > TODAY OR 
           (ldtActivate = TODAY AND liStampTime > TIME) 
        THEN MESSAGE "Request is scheduled to be run on" 
                     STRING(ldtActivate,"99-99-99")
                     STRING(liStampTime,"hh:mm:ss") "." SKIP
                     "Request will be rescheduled to today." SKIP 
                     "Do You want to bypass original scheduling and"
                     "run this request immediately ?" 
             VIEW-AS ALERT-BOX QUESTION
             BUTTONS YES-NO
             TITLE " RUN REQUEST "
             SET ok.
                     
        ELSE MESSAGE "Run this request now ?" 
             VIEW-AS ALERT-BOX QUESTION
             BUTTONS YES-NO
             TITLE " RUN REQUEST "
             SET ok.
            
        IF ok THEN DO:

           RUN runreqim(MsRequest.MsRequest).
           
           IF RETURN-VALUE > "" THEN
              MESSAGE RETURN-VALUE
              VIEW-AS ALERT-BOX ERROR.
           ELSE MESSAGE "Request has been handled"
                VIEW-AS ALERT-BOX
                TITLE " DONE".
 
           must-print = TRUE.
           
           NEXT LOOP.
        END. 
     
     END. 
     
     /* memo */
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 THEN DO:
     
        RUN local-find-this(FALSE).
        
        IF NOT AVAILABLE MsRequest THEN NEXT.
        
        ufkey = TRUE.
     
        RUN memo(0,
                 "MsRequest",
                 STRING(MsRequest.MsRequest),
                 "Owner Change").
     END.
      
     /* eventlog */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
     
        RUN local-find-this(FALSE).
        
        IF NOT AVAILABLE MsRequest THEN NEXT.
        
        ufkey = TRUE.
        
        RUN eventsel ("MsRequest",
                      STRING(MsRequest.MsRequest)).
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
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

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MsRequest) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MsRequest) must-print = TRUE.
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
      FIND MsRequest WHERE recid(MsRequest) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MsRequest WHERE recid(MsRequest) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiReqStat NE ? THEN DO:
       FIND FIRST MsRequest 
          WHERE MsRequest.Brand     = lcBrand   AND
                MsRequest.ReqType   = liReqType AND
                MsRequest.ReqStatus = iiReqStat 
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END. 
   ELSE IF iiMSSeq > 0 THEN DO:
      IF liReqType >= 0 THEN 
         FIND FIRST MsRequest 
              WHERE MsRequest.MSSeq   = iiMSSeq AND
                    MsRequest.ReqType = liReqType
         NO-LOCK NO-ERROR.
      ELSE FIND FIRST MsRequest 
                WHERE MsRequest.MSSeq = iiMSSeq USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND FIRST MsRequest 
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = liReqType AND
                MsRequest.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND FIRST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX BrandType
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CLI
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX ReqStatus
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiReqStat NE ? THEN DO:
       FIND LAST MsRequest 
          WHERE MsRequest.Brand     = lcBrand   AND
                MsRequest.ReqType   = liReqType AND
                MsRequest.ReqStatus = iiReqStat 
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END. 
   ELSE IF iiMSSeq > 0 THEN DO:
      IF liReqType >= 0 THEN 
         FIND LAST MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   MsRequest.ReqType = liReqType
         NO-LOCK NO-ERROR.
      ELSE FIND LAST MsRequest 
               WHERE MsRequest.MSSeq = iiMSSeq USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND LAST MsRequest 
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = liReqType AND
                MsRequest.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND LAST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX BrandType
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CLI
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX ReqStatus
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiReqStat NE ? THEN DO:
       FIND NEXT MsRequest 
          WHERE MsRequest.Brand     = lcBrand   AND
                MsRequest.ReqType   = liReqType AND
                MsRequest.ReqStatus = iiReqStat 
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END. 
   ELSE IF iiMSSeq > 0 THEN DO:
      IF liReqType >= 0 THEN 
         FIND NEXT MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   MsRequest.ReqType = liReqType
         NO-LOCK NO-ERROR.
      ELSE FIND NEXT MsRequest 
               WHERE MsRequest.MSSeq = iiMSSeq USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND NEXT MsRequest 
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = liReqType AND
                MsRequest.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND NEXT MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX BrandType
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CLI
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX ReqStatus
        NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF iiReqStat NE ? THEN DO:
       FIND PREV MsRequest 
          WHERE MsRequest.Brand     = lcBrand   AND
                MsRequest.ReqType   = liReqType AND
                MsRequest.ReqStatus = iiReqStat 
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END. 
   ELSE IF iiMSSeq > 0 THEN DO:
      IF liReqType >= 0 THEN 
         FIND PREV MsRequest 
             WHERE MsRequest.MSSeq   = iiMSSeq AND
                   MsRequest.ReqType = liReqType
         NO-LOCK NO-ERROR.
      ELSE FIND PREV MsRequest 
               WHERE MsRequest.MSSeq = iiMSSeq USE-INDEX MsActStamp
           NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND PREV MsRequest 
          WHERE MsRequest.Brand = lcBrand     AND
                MsRequest.ReqType = liReqType AND
                MsRequest.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND PREV MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX BrandType
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV MsRequest
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CLI
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV MsRequest 
          WHERE MsRequest.Brand   = lcBrand AND
                MsRequest.ReqType = liReqType
          USE-INDEX ReqStatus
        NO-LOCK NO-ERROR.
   END.
        
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MsRequest.MsRequest
       MsRequest.CLI 
       MsRequest.CustNum
       lcCustName 
       ldtActivate WHEN iiReqStat NE 14
       ldtAdvDate  WHEN iiReqStat = 14 @ ldtActivate
       lcNote
       MsRequest.ReqIParam1 WHEN iiReqStat NE 14
       liAdvDays            WHEN iiReqStat = 14 @ MsRequest.ReqIParam1 
       lcNewName
       MsRequest.ReqStatus
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    FIND Customer OF MsRequest NO-LOCK NO-ERROR.

    IF AVAILABLE Customer THEN    
       lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                     BUFFER Customer).
    ELSE lcCustName = "".
                
    lcNewName = ENTRY(1,MsRequest.ReqCParam1,";") + " " + 
                ENTRY(2,MsRequest.ReqCParam1,";").
                
    fSplitTS(MsRequest.ActStamp,
             OUTPUT ldtActivate,
             OUTPUT liStampTime).
    
    lcNote = "".
    
    /* if adv.payment invoice sent but not paid -> display invoice date */
    IF iiReqStat = 14 THEN DO:
       ASSIGN ldtAdvDate = ?
              liAdvDays  = 0.
              
       FOR EACH Invoice NO-LOCK WHERE
                Invoice.Brand   = gcBrand              AND
                Invoice.CustNum = MsRequest.ReqIParam1 AND
                Invoice.InvType = 4,
          FIRST SingleFee NO-LOCK WHERE
                SingleFee.InvNum    = Invoice.InvNum  AND
                SingleFee.HostTable = "MsRequest"     AND
                SingleFee.KeyValue  = STRING(MsRequest.MsRequest):
          ASSIGN ldtAdvDate = Invoice.InvDate
                 liAdvDays  = TODAY - Invoice.InvDate.
          LEAVE.
       END.
       
       IF liAdvLimit > 0 AND liAdvDays >= liAdvLimit THEN lcNote = "*".
    END.
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR ldCurrStamp AS DEC NO-UNDO.

   PAUSE 0.
   VIEW FRAME lis.

   LocalDisp:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      ASSIGN lcCreated  = fDispStamp(MsRequest.CreStamp)
             lcActivate = fDispStamp(MsRequest.ActStamp)
             lcDone     = fDispStamp(MsRequest.DoneStamp)
             llSendSMS  = (MsRequest.SendSMS = 1).

     
      FIND TMSUser WHERE TMSUser.UserCode = MsRequest.UserCode 
          NO-LOCK NO-ERROR.

      DISP MsRequest.MsRequest 
           MsRequest.MSSeq
           MsRequest.CLI
           MsRequest.CustNum
           lcCustName WHEN AVAILABLE Customer
           MsRequest.CreStamp
           lcCreated
           MsRequest.ActStamp
           lcActivate
           MsRequest.UserCode
           TMSUser.UserName WHEN AVAILABLE TMSUser
           MsRequest.ReqType 
           MsRequest.ReqIParam1
           MsRequest.ReqCParam1
           MsRequest.CreateFees
           llSendSMS
           MsRequest.DoneStamp
           lcDone
           MsRequest.Memo
      WITH FRAME lis.
      
      HandleRequest:
      REPEAT:
      
          fDispReqStatus(MsRequest.ReqStatus).
          DISPLAY MsRequest.ReqStatus
                  WITH FRAME lis.
          
          ASSIGN ufk    = 0 
                ehto   = 0
                ufk[2] = 1060
                ufk[8] = 8.
         
         IF (MsRequest.ReqStatus >= 2 AND MsRequest.ReqStatus <= 4) OR
            MsRequest.ReqStatus = 16
         THEN ufk[2] = 1053.
         
         IF lcRight = "RW" AND 
            (MsRequest.ReqStatus < 2 OR MsRequest.ReqStatus > 4)
         THEN ufk[1] = 7.
         
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,11") = 0 THEN DO: 
            /* invoice customer */
            IF SUBSTRING(MsRequest.ReqCparam4,2,1) = "2" THEN ufk[3] = 2242.
         
            /* user customer */
            IF SUBSTRING(MsRequest.ReqCparam4,3,1) = "3" THEN ufk[4] = 2241.
         END.

         /* accept request */   
         IF MsRequest.ReqStatus >= 12 AND MsRequest.ReqStatus <= 15 THEN DO:
             /* ssn is mandatory */
             IF ENTRY(11,MsRequest.ReqCParam1,";") > "" 
             THEN ufk[6] = 1057.
         END.
         
         /* cancel request */
         IF MsRequest.ReqStatus = 0 OR MsRequest.ReqStatus >= 10
         THEN ufk[7] = 1059.
         
         RUN ufkey. 
               
         IF toimi = 1 THEN DO:      
           
            IF lcPassword > "" THEN DO:
             
               lcAskPassWd = "".
            
               PAUSE 0.
               UPDATE lcAskPassWd 
                  BLANK
                  FORMAT "X(20)" 
                  LABEL "Password"
                  HELP "Password for changing request"
                  WITH OVERLAY ROW 10 CENTERED TITLE " REQUEST CHANGE "
                       SIDE-LABELS FRAME fPassword.
              
               IF lcAskPassWd NE lcPassword THEN NEXT. 
            END.
            
            FIND CURRENT MsRequest EXCLUSIVE-LOCK.
            
            ehto = 9. RUN ufkey.
      
            UPDATE
            MsRequest.CreateFees
            llSendSMS
            MsRequest.ReqStatus
            MsRequest.Memo
            WITH FRAME lis EDITING:
            
               READKEY.
         
               IF KEYLABEL(LASTKEY) = "F9" AND 
                  FRAME-FIELD = "ReqStatus" 
               THEN DO:
            
                  RUN h-tmscodes(INPUT "MsRequest",  /* TableName */
                                       "ReqStatus",     /* FieldName */
                                       "Request",       /* GroupCode */
                                 OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     fDispReqStatus(INTEGER(lcCode)).
                     DISPLAY INTEGER(lcCode) ;& MsRequest.ReqStatus
                     WITH FRAME lis.   
                  END.
 
                  ehto = 9.
                  RUN ufkey.
                  NEXT. 
               END.

               ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
               THEN DO WITH FRAME lis:
             
                  PAUSE 0.
                
                  IF FRAME-FIELD = "ReqStatus" THEN DO:
                     fDispReqStatus(INPUT INPUT FRAME lis MsRequest.ReqStatus).
                  END.

                  ELSE IF FRAME-FIELD = "ActStamp" THEN DO:
                     lcActivate = 
                        fDispStamp(INPUT INPUT FRAME lis MsRequest.ActStamp).
                     DISPLAY lcActivate WITH FRAME lis.   
                  END. 
 
                  ELSE IF FRAME-FIELD = "DoneStamp" THEN DO:
                     lcDone = 
                        fDispStamp(INPUT INPUT FRAME lis MsRequest.DoneStamp).
                     DISPLAY lcDone WITH FRAME lis.   
                  END. 
               
               END.
             
               APPLY LASTKEY.
       
            END. /* EDITING */
            
            MsRequest.SendSms = INTEGER(llSendSMS).
            
         END.

         /* agreement customer */
         ELSE IF toimi = 2 THEN DO:

            RUN chgmsowner (MsRequest.MsSeq,
                            MsRequest.MsRequest,
                            IF lcRight = "RW" AND 
                               LOOKUP(STRING(MsRequest.ReqStatus),
                                      "2,3,4,16") = 0
                            THEN "change"
                            ELSE "view").

            NEXT LocalDisp.
         END. 

         
         /* invoice customer */
         ELSE IF toimi = 3 THEN DO:
         
            RUN ownerinvc (MsRequest.MsRequest,
                           IF lcRight = "RW" AND 
                              LOOKUP(STRING(MsRequest.ReqStatus),
                                      "2,3,4,16") = 0
                           THEN "change"
                           ELSE "view").
         END. 
          
         /* user customer */
         ELSE IF toimi = 4 THEN DO:
         
            RUN owneruser (MsRequest.MsRequest,
                           IF lcRight = "RW" AND 
                              LOOKUP(STRING(MsRequest.ReqStatus),
                                     "2,3,4,16") = 0
                           THEN "change"
                           ELSE "view").
         END. 
          
         /* approve request */
         ELSE IF TOIMI = 6 THEN DO:
         
            Ok = FALSE.
            
            /* check that names have been given */
            IF ENTRY(1,MsRequest.ReqCParam1,";") = "" THEN DO:
               MESSAGE "Name for new agreement customer has not been given"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
            
            /* separate invoice customer */
            IF SUBSTRING(MsRequest.ReqCParam4,2,1) NE "1" AND
               ENTRY(1,MsRequest.ReqCParam2,";") = "" THEN DO:
               MESSAGE "Name for new invoice customer has not been given"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            /* separate user */
            IF SUBSTRING(MsRequest.ReqCParam4,3,1) NE "1" AND
               ENTRY(1,MsRequest.ReqCParam3,";") = "" THEN DO:
               MESSAGE "Name for new user has not been given"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
                 
            /* advance payment invoice made but not paid */
            IF MsRequest.ReqStatus = 14 THEN DO:
               MESSAGE "Advance payment invoice has been sent, but it"
                       "hasn't been paid."
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END. 
                
            /* r.i.p. */
            IF MsRequest.ReqDParam1 = 3 THEN DO:
               MESSAGE "New owner has died. Request can not be approved."
               VIEW-AS ALERT-BOX ERROR.
               NEXT. 
            END. 
               
            /* vrk failed or not run */
            ELSE IF MsRequest.ReqDParam1 NE 1 THEN DO:
            
               IF MsRequest.ReqDParam1 = 0 
               THEN lcError = "VRK check has not been done.".
               ELSE lcError = "VRK check has failed".
               
               Ok = FALSE.
               MESSAGE lcError SKIP
                       "Approve request anyway ?" 
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE  " VRK "
               SET Ok.
    
               IF NOT Ok THEN NEXT.         
            END.

            /* sat failed or not run */
            IF MsRequest.ReqDParam2 NE 1 THEN DO:
            
               IF MsRequest.ReqDParam2 = 0 
               THEN lcError = "SAT check has not been done.".
               ELSE lcError = "SAT check has failed".
               
               Ok = FALSE.
               MESSAGE lcError SKIP
                       "Approve request anyway ?" 
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE  " SAT "
               SET Ok.
    
               IF NOT Ok THEN NEXT.         
            END.

            IF NOT ok THEN 
            MESSAGE "Approve request for final handling ?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " APPROVE "
            SET ok.
            
            IF NOT ok THEN NEXT.
            
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).
            
            FIND CURRENT MsRequest EXCLUSIVE-LOCK.
            MsRequest.ReqStatus = 16.
            
            /* set activation to now (if not scheduled into future) */
            ldCurrStamp = fMakeTS().
            IF MsRequest.ActStamp < ldCurrStamp 
            THEN  MsRequest.ActStamp = ldCurrStamp.
      
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
            
            MESSAGE "Request has been marked as approved"
            VIEW-AS ALERT-BOX INFORMATION.

         END. 
         
         /* cancel request */
         ELSE IF toimi = 7 THEN DO:
            ok = FALSE.
            
            MESSAGE "Request will be cancelled, i.e. it cannot be handled"
                    "any further." SKIP
                    "Continue with cancellation ?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET ok. 
            
            IF ok THEN DO:
               REPEAT WITH FRAME fCancel ON ENDKEY UNDO, NEXT HandleRequest:
                  ehto = 9.
                  RUN ufkey.
                  
                  PAUSE 0.
                  UPDATE lcCancelTxt 
                     FORMAT "X(60)" HELP "Reason for cancellation"
                  WITH NO-LABELS TITLE " CANCELLATION " 
                       OVERLAY ROW 10 CENTERED 1 DOWN FRAME fCancel.
                  
                  IF lcCancelTxt = "" THEN DO:
                     MESSAGE "Explanation for cancellation is mandatory"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  
                  HIDE FRAME fCancel NO-PAUSE.
                  LEAVE.
               END.
               
               fReqStatus(4,lcCancelTxt).
            
               MESSAGE "Request has been marked to status 4"
               VIEW-AS ALERT-BOX
               TITLE " CANCELLED ".
               
            END.    
         END.
         
         ELSE IF toimi = 8 THEN LEAVE.
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.
                     
