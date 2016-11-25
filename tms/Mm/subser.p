/* ----------------------------------------------------------------------
  MODULE .......: SubSer.P
  TASK .........: UPDATE Mobile subs. Services
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-06-99
  CHANGED ......: 21.07.99 pt f5 / add allowed 
                  12.08.99 pt F6 restricted
                  13.08.99 pt RUN setms when exiting
                  13.10.99 pt setfee operations
                  31.01.00 jp ScChgable added
                  22.02.00 pt setfee OBOPRI corrected
                  15.05.02 jp  RUN setfee, more PARAMETER
                  11.11.02 jp sprule
                  13.03.03 tk tokens
                  01.04.03 jp f3
                  16.05.03 tk eventlog
                  16.05.03 jp Generate automatically data&fax number
                  27.05.03 jp b70
                  21.08.03 jp remove data&gprs fixedfee
                  05.09.03 jp brand
                  30.09.03 jp setfees; feemodel activation from 
                                       service component or Service pack
                  07.11.03 jp sprule isolated                     
                  13.02.03 jp subserpara handling
                  08.03.04 jp service request
                  14.04.04/aam create contract (fFeeContract)
                  15.04.04 jp  barring validation
                  01.07.04 tk  temp-table in can-finds
                  07.12.04/aam SubSer.ServEl removed,
                  10.12.04/aam time element added to ttSubSer
                               -> use temptable on browser (current settings),
                               new procedures from service.i,
                               ScUpdRule, MsRequest etc.
                  14.12.04 tk  message if SLTx services
                  11.02.05/aam Salesman to fServiceRequest
                  06.04.04 jp  llsendsms
                  15.12.05/aam new parameter to runscreqim
                  12.12.06/mvi new param to run msrequest (reqstat = ?)
                  06.09.07 vk  Added restrictions to ServCom updating
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
 ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'SubSer'}
{cparam2.i}
{eventval.i}
{msisdn.i}
{ffeecont.i}
{timestamp.i}
{fmakemsreq.i}
{service.i}

DEF TEMP-TABLE ttSubSer NO-UNDO
   LIKE SubSer.
 
DEF TEMP-TABLE ttsubserpara NO-UNDO 
   LIKE subserpara.

DEF TEMP-TABLE ttRequest NO-UNDO
   FIELD ReqId   AS INT
   FIELD Service AS CHAR
   INDEX ReqID ReqID.
   
{sername.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSubSer AS HANDLE NO-UNDO.
   lhSubSer = BUFFER SubSer:HANDLE.
   RUN StarEventInitialize(lhSubSer).

   DEFINE VARIABLE lhSubSerPara AS HANDLE NO-UNDO.
   lhSubSerPara = BUFFER SubSerPara:HANDLE.
   RUN StarEventInitialize(lhSubSerPara).

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhSubSer).
   END.

END.

DEF INPUT PARAMETER  iiMsSeq LIKE MobSub.MsSeq NO-UNDO .

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF BUFFER bSubser   FOR ttSubser.

DEF VAR ServCom  LIKE SubSer.ServCom  NO-UNDO.
DEF VAR ServPac  LIKE SubSer.ServPac NO-UNDO.
DEF VAR xrecid         AS RECID                           init ?.
DEF VAR FIRSTrow       AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow         AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown        AS INT                    NO-UNDO  init 11.
DEF VAR order          AS INT                    NO-UNDO  init 1.
DEF VAR orders         AS CHAR                   NO-UNDO.
DEF VAR maxOrder       AS INT                    NO-UNDO  init 2.
DEF VAR lcinfo         AS CHAR                   NO-UNDO.

DEF VAR ufkey          AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow         AS INT                    NO-UNDO  init 0.
DEF VAR pr-order       AS INT                    NO-UNDO.
DEF VAR Memory         AS RECID                  NO-UNDO.
DEF VAR RowNo          AS INT                    NO-UNDO.
DEF VAR must-print     AS LOG                    NO-UNDO.
DEF VAR must-add       AS LOG                    NO-UNDO.

DEF VAR ac-hdr         AS CHAR                   NO-UNDO.
DEF VAR rtab           AS RECID EXTENT 24        NO-UNDO.
DEF VAR i              AS INT                    NO-UNDO.
DEF VAR ok             AS log format "Yes/No"    NO-UNDO.
DEF VAR rc             AS INT                    NO-UNDO.

DEF VAR datalist       AS C                      NO-UNDO. 
DEF VAR data_found     AS LO                     NO-UNDO INIT FALSE.
DEF VAR ActiveStatus   AS LO FORMAT "Immediate/Scheduled" NO-UNDO INIT TRUE.
DEF VAR ActivateDate   AS DATE  NO-UNDO FORMAT "99.99.99".
DEF VAR ActivateTime   AS CHAR                   NO-UNDO.

DEF VAR lcContract      AS CHAR               NO-UNDO. 
DEF VAR llChange        AS LOGI               NO-UNDO. 
DEF VAR lcSologStat     AS CHAR               NO-UNDO. 
DEF VAR ldActStamp      AS DECI               NO-UNDO. 
DEF VAR liOldStat       AS INT                NO-UNDO. 
DEF VAR llUpdate        AS LOGI               NO-UNDO. 
DEF VAR ldAmt           AS DECI               NO-UNDO. 
DEF VAR liDefValue      AS INT                NO-UNDO.
DEF VAR ldCurrent       AS DECI               NO-UNDO.
DEF VAR llRequest       AS LOGI               NO-UNDO. 
DEF VAR liReq           AS INT                NO-UNDO. 
DEF VAR liNewStat       AS INT                NO-UNDO. 
DEF VAR lcSalesman      AS CHAR               NO-UNDO. 
DEF VAR llSendSMS       AS LOGI               NO-UNDO.
DEF VAR llStop          AS LOGI               NO-UNDO.
DEF VAR lcRestrictList  AS CHAR               NO-UNDO.

form
    ttSubSer.ServPac  format "x(3)" column-label "SP"
    ttSubSer.ServCom  COLUMN-LABEL "Service" 
    ttSubSer.SSStat   format ">>>9" column-label "St"
    llRequest         format "*/"   NO-LABEL SPACE(0)
    ttSubSer.SSDate   format "99-99-99" column-label "Date"
    ServCom.ScName    format "x(31)" 
    ttSubSer.SSParam  format "x(13)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)  " Services of MobSub " + MobSub.CLI + " "
    FRAME sel.


FORM
   "You have just changed or added service parameters."         skip
   "These changes are next updated to HLR which can"            SKIP
   "take a few moments."                                        skip(1)
   "Do  you want to do this IMMEADIATELLY or SCHEDULE it"      skip 
   "to be done later?                                    "      SKIP(1)
   "Activation........:" ActiveStatus NO-LABEL                   
HELP "(I)mmediate Activation or a (S)cheduled Activation ?"     SKIP(1)

   "Activation Date ..:" Activatedate NO-LABEL 
HELP "Date when service parameter shall be activated"           SKIP
   "Activation Time ..:" ActivateTime No-LABEL FORMAT "99:99:99"
HELP "Excact time when a SCHEDULED Activation shall be performed"   

WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "UPDATE TO HLR" 
    SIDE-LABELS 

    FRAME hlr.

form
    ttSubSer.ServPac  
    ServPac.SPName
    skip(1)
    ttSubSer.ServCom  
    ServCom.ScName 
    ServCom.ScLocalName
    SKIP(1)
    ttSubSer.SSStat
    ttSubSer.SSParam
    ttSubSer.SSDate
      VALIDATE(INPUT ttSubSer.SSDate NE ? AND
               INPUT ttSubSer.SSDate >= TODAY,
              "Date is mandatory and cannot be in the past")
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Subscriber's Service  BY  ServCom */
    ServCom
    HELP "Enter service component code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SERVICE COMPONENT "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Subscriber's Service  BY ServPac */
    ttSubSer.ServPac
    HELP "Enter Service Package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SERVICE PACKAGE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK.

/* get current definitions, servpac index is sorted by date */
RUN pSetTempTable.
        
cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Service,By ServPac,By 3, By 4".
lcRestrictList = "BCG,OBA,OBO,OBR,RSA".


RUN local-find-first.

IF AVAILABLE ttSubSer THEN ASSIGN
   Memory       = recid(ttSubSer)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No services available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND FIRST ttSubSer WHERE
           ttSubSer.MsSeq = iiMsSeq AND
           ttSubSer.ServCom BEGINS "SLT" AND
           ttSubSer.SsStat = 1
NO-LOCK NO-ERROR.
IF AVAIL ttSubSer THEN DO:
   MESSAGE "This subscriber has credit control barrings !"
   VIEW-AS ALERT-BOX TITLE " Note ! ".
END.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a SubSer  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
      
           CLEAR FRAME lis NO-PAUSE.
           DISPLAY TODAY @ ttSubSer.SSDate WITH FRAME lis.
           
           PROMPT-FOR ttSubSer.ServCom ttSubSer.SSDate
           WITH FRAME lis EDITING:

              READKEY.

              IF lookup(keylabel(LASTKEY),"f4") > 0 
              THEN undo add-row, leave add-row.

              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 if frame-field = "ServCom" THEN DO:
                    if input ttSubSer.ServCom = "" THEN LEAVE add-row.
                    FIND ServCom where 
                         ServCom.Brand   = gcBrand    AND 
                         ServCom.ServCom = INPUT ttSubSer.ServCom
                    no-lock no-error.
                    IF NOT AVAIL ServCom THEN DO:
                       bell. message "Unknown Service Component !".
                       NEXT.
                    END.

                    DISP fGetServiceName(ttSubSer.ServCom) @  ServCom.ScName 
                    fGetLocalName(ttSubSer.ServCom) @ ServCom.ScLocalName.
                    
                END.
              END.         
              APPLY LASTKEY.
           END.      

           IF CAN-FIND(ttSubSer where
                       ttSubSer.MsSeq   = iiMsSeq                  AND
                       ttSubSer.ServCom = INPUT ttSubSer.ServCom AND
                       ttSubSer.SSDate  = INPUT ttSubSer.SSDate)
           THEN DO:     
               MESSAGE
               "Service '" + INPUT FRAME lis ttSubSer.ServCom +
               "' already exists with date " INPUT FRAME lis ttSubSer.SSDate
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.

           /* check if component is defined for clitype and get 
              default value */
           FIND FIRST CTServEl WHERE
                      CTServEl.Brand    = gcBrand        AND
                      CTServEl.ServCom  = INPUT FRAME lis ttSubSer.ServCom AND
                      CTServEl.CLIType  = MobSub.CLIType AND
                      CTServEl.FromDate <= INPUT FRAME lis ttSubSer.SSDate
           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE CTServEl THEN DO:
              MESSAGE "Service component is not available for this CLI type"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
           
           CREATE ttSubSer.
           ASSIGN
              ttSubSer.MsSeq   = iiMsSeq
              ttSubSer.ServCom = INPUT FRAME lis ttSubSer.ServCom
              ttSubSer.SSDate  = INPUT FRAME lis ttSubSer.SSDate
              ttSubSer.ServPac = CTServEl.ServPac
              ttSubSer.SSStat  = CTServEl.DefValue.
              
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSubSer).

           ASSIGN
              Memory = recid(ttSubSer)
              xrecid = Memory.

           LEAVE.
        END.
      END.  /* ADD-ROW */
 
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttSubSer THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttSubSer WHERE recid(ttSubSer) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttSubSer THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttSubSer).
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
        
        DOWN FirstRow.
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
        ufk[1]= 245  ufk[2]= 246 ufk[3]= 2350 
        ufk[4]= (IF lcRight = "RW" THEN 248 ELSE 0)
        ufk[5]= (IF lcRight = "RW" THEN 60 ELSE 0) 
        ufk[6]= 66
        ufk[7]= 781  
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttSubSer.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttSubSer.ServCom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttSubSer.ServPac ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttSubSer.ServPac WITH FRAME sel.
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
        FIND ttSubSer WHERE recid(ttSubSer) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttSubSer THEN
              ASSIGN FIRSTrow = i Memory = recid(ttSubSer).
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
           IF NOT AVAILABLE ttSubSer THEN DO:
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
                rtab[1] = recid(ttSubSer)
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
           IF NOT AVAILABLE ttSubSer THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttSubSer).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttSubSer WHERE recid(ttSubSer) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttSubSer THEN DO:
           Memory = recid(ttSubSer).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttSubSer THEN Memory = recid(ttSubSer).
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
           FIND ttSubSer WHERE recid(ttSubSer) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 2 BUT ORDER IS STILL 1 !!!! */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServCom ENTERED THEN DO:
          FIND FIRST ttSubSer WHERE 
                     ttSubSer.ServCom >= ServCom AND
                     ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttSubSer THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SubSer/ServCom was found */
          ASSIGN order = 1 Memory = recid(ttSubSer) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 1 BUT ORDER IS 2 !!!! */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET ServPac WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ServPac ENTERED THEN DO:
          FIND FIRST ttSubSer WHERE 
                     ttSubSer.ServPac >= ServPac AND
                     ttSubSer.MsSeq    = iiMsSeq 
          USE-INDEX ServPac  NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttSubSer THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SubSer/ServPac was found */
          ASSIGN order = 2 Memory = recid(ttSubSer) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* attributes */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:  
        RUN local-find-this(FALSE).
        IF AVAILABLE ttSubSer THEN DO:
           FIND ServCom WHERE 
                ServCom.Brand   = gcBrand AND
                ServCom.ServCom = ttSubSer.ServCom NO-LOCK NO-ERROR.
           IF NOT AVAILABLE ServCom OR 
              NOT ServCom.ServAttrL 
           THEN DO:
              MESSAGE "This service does not include attributes"
              VIEW-AS ALERT-BOX
              INFORMATION.
           END.

           ELSE DO:
               IF LOOKUP(ttSubSer.ServCom,lcRestrictList) > 0
                   THEN llStop = TRUE.
                   ELSE llStop = FALSE.

               IF NOT llStop THEN 
                   RUN subserpara (INPUT-OUTPUT TABLE ttSubserPara,
                                                      ttSubSer.MsSeq,
                                                      ttSubSer.ServCom).
                             ELSE MESSAGE "Use the package !" 
                             VIEW-AS ALERT-BOX.
          END.                   
        END. 
        
        ufkey = TRUE.
        NEXT loop.
     END.


     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  /* COPY */
        RUN local-copy-ServPac.
        ufkey = TRUE.
        NEXT loop.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* request */
        RUN local-find-this(FALSE).
        RUN local-find-others.
        IF NOT llRequest THEN 
        MESSAGE "There is no pending request for this service."
        VIEW-AS ALERT-BOX ERROR.

        ELSE DO:
           RUN msrequest (1,
                          ?, /* reqstat ? for all */
                          iiMsSeq,
                          0,
                          0,
                          "").
        
           ufkey = TRUE.
           NEXT LOOP.
        END.
        
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 THEN DO:  /* history */
        RUN subserhist (iiMsSeq).
        
        ufkey = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  /* history */
        RUN local-find-this(FALSE).


        RUN msrequest (1,
                          ?, /* reqstat ? for all */
                          iiMsSeq,
                          0,
                          0,
                          ttSubSer.ServCom).


        ufkey = TRUE.
        NEXT LOOP.
     END.
    
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this((lcRight = "RW")).
       IF LOOKUP(ttSubSer.ServCom,lcRestrictList) > 0
            THEN llStop = TRUE.
            ELSE llStop = FALSE.
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttSubSer.ServCom.
       IF NOT llStop THEN DO:
           RUN local-UPDATE-record.                                  
           HIDE FRAME lis NO-PAUSE.

           /*YPR-1973*/
           IF ttSubSer.ServCom EQ "NAM" AND 
              MobSub.BarrCode EQ "D_HOTLP" AND 
              ttSubSer.SSSTat > 0 THEN DO:
              MESSAGE "D_HOTLP blocks NAM change" VIEW-AS ALERT-BOX.
              UNDO, LEAVE.
           END.

           /*YPR-4773*/
           IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR /*16*/ 
               MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN DO:
              MESSAGE "Mobile line provisioning is not complete" 
                 VIEW-AS ALERT-BOX.
              UNDO, LEAVE.
           END.      
           /* IF  User Wanted TO Cancel this Change TRANSACTION */
           IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
           KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

           RUN local-disp-row.

           xrecid = recid(ttSubSer).

           IF must-print THEN NEXT LOOP.
           LEAVE.
       END.
       ELSE DO:
           MESSAGE "Use the package !" VIEW-AS ALERT-BOX.
           LEAVE.
       END.    
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttSubSer) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttSubSer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN  LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


/* update temp-table into subser */
RUN pUpdateSubSer.

/*******************************************************
* The user has now pressed F8.  IF Services            *
* have been added OR modified during this              *
* session, their codes are gathered consequently       *
* into the STRING 'codelist'.                          *
* IF codelist contains anything we RUN the             *
* setms.p that UPDATEs those parameters into HLR       *
* via SOG Gateway.  IN CASE THIS MobSub IS ACTIVE.     *
*******************************************************/

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK.

IF MobSub.ICC = "" THEN RETURN.  /* NOT active */ 


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttSubSer WHERE recid(ttSubSer) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttSubSer WHERE recid(ttSubSer) = rtab[frame-line(sel)] 
       NO-LOCK.
    FIND ServCom WHERE 
         ServCom.Brand   = gcBrand AND 
         ServCom.ServCom = ENTRY(1,ttSubSer.ServCom,".") 
    NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttSubSer
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttSubSer USE-INDEX ServPac
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttSubSer
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttSubSer USE-INDEX ServPac
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttSubSer
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttSubSer USE-INDEX ServPac
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttSubSer
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ttSubSer USE-INDEX ServPac
       WHERE ttSubSer.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY
       ttSubSer.ServCom
       llRequest
       ttSubSer.SSDate
       ttSubSer.ServPac
       ttSubSer.SSStat
       ttSubSer.SSParam
       fGetServiceName(ttSubSer.ServCom) @ ServCom.ScName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND ServPac WHERE 
        ServPac.Brand   = gcBrand   AND 
        ServPac.ServPac = ttSubSer.ServPac NO-LOCK NO-ERROR.
   
   FIND ServCom WHERE 
        ServCom.Brand   = gcBrand  AND 
        ServCom.ServCom = ENTRY(1,ttSubSer.ServCom,".") NO-LOCK NO-ERROR.

   llRequest = CAN-FIND(FIRST MsRequest WHERE      
                              MsRequest.MsSeq      = iiMsSeq AND
                              MsRequest.ReqType    = 1       AND
                              MsRequest.ReqCParam1 = ttSubSer.ServCom AND
                              MsRequest.ReqStat    < 2).
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      llChange = (lcRight = "RW").
      
      IF llChange THEN DO:
         /* can service be changed from here */
         liDefValue = fServComValue(MobSub.CLIType,
                                    ttSubSer.ServCom,
                                    OUTPUT ok).
                                   
         IF liDefValue = ? OR
            (liDefValue = 0 AND NOT ok) 
         THEN llChange = FALSE.
      END.

      IF llChange AND llRequest THEN DO:
         MESSAGE "There is an active change request for service." SKIP
                 "Change is not allowed before request is handled."
         VIEW-AS ALERT-BOX
         TITLE " PENDING REQUEST ".
         llChange = FALSE.
      END. 

      CASE ttSubSer.SologStat:
      WHEN 1 THEN lcSologStat = "Should be sent".
      WHEN 2 THEN lcSologStat = "Solog created".
      OTHERWISE   lcSologStat = "".
      END CASE. 
      
      DISP
         ttSubSer.ServPac
         ServPac.SPName  WHEN AVAIL ServPac
         fGetServiceName(ttSubSer.ServCom) @ ServCom.ScName 
         fGetLocalName(ttSubSer.ServCom)   @ ServCom.ScLocalName
         ttSubSer.SSStat
         ttSubSer.SSDate
         ttSubSer.SSParam 
      WITH FRAME lis. 
          
      IF llChange THEN DO:

         liOldStat = ttSubSer.SSStat.
         
         ehto = 9.
         RUN ufkey.
         
         UPDATE
             ttSubSer.SSStat
             ttSubSer.SSParam when ServCom.SCParameter
             ttSubSer.SSDate
         WITH FRAME lis EDITING:

            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "SSStat" THEN DO:

                  /* suggest date for changes */
                  IF INPUT ttSubSer.SSStat NE ttSubSer.SSStat AND
                     INPUT ttSubSer.SSDate < TODAY
                  THEN DO:
                      ldActStamp = 
                         fServiceActStamp(ttSubSer.MsSeq,
                                          ttSubSer.ServCom,
                                          INPUT INPUT ttSubSer.SSStat).
                      IF ldActStamp > 0 
                      THEN fSplitTS(ldActStamp,
                                    OUTPUT ldtActDate,
                                    OUTPUT liReq).
                      ELSE ldtActDate = TODAY.             
                     
                      DISPLAY ldtActDate @ ttSubSer.SSDate WITH FRAME lis.
                  END.
                  
                  IF INPUT ttSubSer.SSStat < ServCom.SCValueRange[1]  OR
                     INPUT ttSubSer.SSStat > ServCom.SCValueRange[2] THEN DO:
                     MESSAGE
                     "The value must be within range"
                     ServCom.ScValueRange[1] "-" ServCom.ScValueRange[2] "!".
                     NEXT.
                  END.

                  ELSE IF INDEX(ttSubSer.servcom,"DELAY") > 0 THEN  DO:

                     IF LOOKUP(INPUT ttSubSer.SSStat,"5,10,15,20,25,30") = 0 
                     THEN DO:
                        MESSAGE 
                        "You can only choose from 5,10,15,20,25 and 30."
                        view-as alert-box error title "INVALID DELAY TIME".
                        NEXT.
                     END.
                  END.

                  IF LOOKUP(ttSubSer.ServCom,"B16,T62") > 0 THEN DO:
                     IF ttSubSer.Servcom = "B16" AND 
                        ttSubSer.SSparam = ""    AND
                        INPUT ttSubSer.SSStat = 1 
                     THEN ttSubSer.SSParam = 
                             fSearchGenServNumber(MobSub.cli,"DATA").

                     ELSE IF ttSubSer.ServCom = "T62"   AND
                             ttSubSer.SSParam = ""      AND
                             INPUT ttSubSer.SSStat = 1 
                     THEN ttSubSer.SSParam = 
                             fSearchGenServNumber(MobSub.cli,"FAX").           
                  
                     ELSE IF INPUT ttSubSer.SSStat = 0 
                     THEN ttSubSer.ssparam = "".
                  
                     DISP ttSubSer.ssparam WITH FRAME lis.
                  END.
                  
               END.

               ELSE IF FRAME-FIELD = "SSParam" THEN DO:

                  IF INPUT ttSubSer.SSStat NE 0 AND 
                     INPUT ttSubSer.SSparam = "" 
                  THEN DO:
                     BELL.
                     MESSAGE
                     "Parameter must be defined for this service"
                     VIEW-AS ALERT-BOX TITLE "PARAMETER".
                     NEXT-PROMPT ttSubSer.ssparam. NEXT.
                  ENd. 
               END.

            END.
            APPLY LASTKEY.
         END. /* EDITING */ 

         IF liOldStat NE ttSubSer.SSStat THEN DO:

            /* check the validity of change date */
            ldActStamp = fServiceActStamp(ttSubSer.MsSeq,
                                          ttSubSer.ServCom,
                                          ttSubSer.SSStat).
            IF ldActStamp > 0 THEN DO:
               fSplitTS(ldActStamp,
                        OUTPUT ldtActDate,
                        OUTPUT liReq).

               IF ldtActDate > ttSubSer.SSDate OR
                  (DAY(ldtActDate) = 1 AND liReq < TIME - 120 AND
                   DAY(ttSubSer.SSDate) NE 1) 
               THEN DO:
                  ok = FALSE.
                  MESSAGE "Date for change should be" 
                          STRING(ldtActDate,"99.99.9999") SKIP
                          "Do You still want to keep the given date ?"
                  VIEW-AS ALERT-BOX QUESTION
                  BUTTONS YES-NO
                  SET ok.
                  IF NOT ok THEN ttSubSer.SSDate = ldtActDate.
               END.
            END.
            
            
            ASSIGN liOldStat = MIN(liOldStat,1)
                   liNewStat = MIN(ttSubSer.SSStat,1).
            
            /* are there rules for updating other components due
               to the change of this one */
            FOR EACH ScUpdRule NO-LOCK WHERE
                     ScUpdRule.Brand    = gcBrand          AND
                     ScUpdRule.ServCom  = ttSubSer.ServCom AND
                     ScUpdRule.OldValue = liOldStat        AND
                     ScUpdRule.NewValue = liNewStat:
                           
               FIND FIRST bSubSer WHERE
                          bSubSer.MsSeq   = MobSub.MsSeq AND
                          bSubSer.ServCom = ScUpdRule.UpdServCom 
               NO-ERROR.

               IF AVAILABLE bSubSer AND 
                  bSubSer.SSStat NE ScUpdRule.UpdValue
               THEN DO:
   
                  ASSIGN bSubSer.SSStat = ScUpdRule.UpdValue
                         bSubSer.SSDate = ttSubSer.SSDate.
                        
                  IF bSubSer.SSStat = 0 THEN bSubSer.SSParam = "".
                        
                  MESSAGE "Service component" bSubSer.servcom 
                         (IF bSubSer.SSStat = 0
                          THEN "de-"
                          ELSE "") + "activated"
                  VIEW-AS ALERT-BOX
                  TITLE " Effect To Other Components ".
               END.
            END.

         END.
 
         /****************************************************
         * IF a Service that was copied from some ServPack   *
         * was now changed  AND the NEW settings differ from *
         * those in original  ServEl  record, we  shall now  *
         * erase the ServCom FIELD in order TO  indicate     *
         * that this  Service  is now no longer SIMilar      *
         * WITH the original servel  record.                 *
         ****************************************************/

         ttSubSer.ServPac = "".
         FOR FIRST CTServEl NO-LOCK WHERE
                   CTServEl.Brand   = gcBrand          AND 
                   CTServEl.ServCom = ttSubSer.ServCom AND
                   CTServEl.CLIType = MobSub.CLIType   AND
                   CTServEl.FromDate <= TODAY:
            IF ttSubSer.SSStat = CTServEl.DefValue
            THEN ttSubSer.ServPac = CTServEl.ServPac.
         END.   

         ASSIGN must-print = TRUE
                FirstRow   = MAX(0,FRAME-LINE(sel) - 1).
      END.  

      ELSE DO:
         ehto = 5.
         RUN ufkey.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
    
      LEAVE.
   END.

END PROCEDURE.

PROCEDURE local-copy-ServPac:

   DEF VAR lcServPac AS CHAR NO-UNDO.
   DEF VAR ldtDate   AS DATE NO-UNDO. 
   DEF VAR llFees    AS LOG  NO-UNDO. 

   form
     skip(1)
     "You can copy a set of Services from CLI Type onto " AT 2 SKIP
     "Mobile Subscriber's personal Service Profile."      AT 2 SKIP(1)
     "Service Package:" AT 2
     lcServPac FORMAT "X(12)"   
         help "Code of the package You want to copy"
     ServPac.SPName SKIP
     "Date ..........:" AT 2
     ldtDate FORMAT "99-99-99"
         HELP "Valid from date for components" 
         SKIP
     "Create Fees ...:" AT 2
     llFees FORMAT "Yes/No"
         HELP "Create fees according to feemodel on service package"
     skip(1)
   WITH
      overlay centered row 2 title " Copy a Service Package " NO-LABELS
      FRAME c-sp.

   PAUSE 0.   

   ldtDate = TODAY.
   
   COPY:
   REPEAT WITH FRAME c-sp ON ENDKEY UNDO, LEAVE:
      ehto = 9. RUN ufkey.

      UPDATE lcServPac 
         VALIDATE(input lcServPac = "" OR 
                  CAN-FIND(ServPac where
                           ServPac.Brand   = gcBrand AND 
                           ServPac.ServPac = INPUT lcServPac),
                 "UNKNOWN Service PACK !")
         ldtDate.
      if lcServPac = "" THEN LEAVE.

      IF ldtDate = ? THEN DO:
         MESSAGE "Date is mandatory"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      FIND ServPac WHERE
           ServPac.Brand   = gcBrand AND 
           ServPac.ServPac = lcServPac NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServPac THEN DO:
         MESSAGE "Service package record not found for: " lcServPac
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END. /* IF NOT AVAILABLE ServPac THEN DO: */
      DISP ServPac.SPName.

      FIND FIRST ServCom WHERE ServCom.ServCom = lcServPac NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServCom THEN DO:
         MESSAGE "Service component record not found for: " lcServPac
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END. /* IF NOT AVAILABLE ServCom THEN DO: */

      llFees = (ServPac.FeeModel > "" OR ServCom.FeeModel > "").
      IF llFees THEN UPDATE llFees.
      
      /* package must be defined to clitype */
      IF NOT CAN-FIND(FIRST CTServPac WHERE
                            CTServPac.Brand     = gcBrand        AND
                            CTServpac.CLIType   = MobSub.CLIType AND
                            CTServPac.ServPac   = lcServPac      AND
                            CTServPac.FromDate <= TODAY)
      THEN DO:
         MESSAGE "Subscriptions CLI type" MobSub.CLIType "doesn't contain"
                 "service package" lcServPac
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      MESSAGE "Start copying service package ?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      TITLE " Copy Package "
      SET ok.

      IF ok THEN DO:

         IF ServPac.ServPac = "BB" AND
            NOT fIsBBAllowed(Mobsub.MsSeq,fMakeTS()) THEN DO:
            MESSAGE "BB service can not be activated since subscription" skip
                    "does not have active data bundle"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END. /* IF ServPac.ServPac = "BB" */

         RUN pCopyPackage(MobSub.CLIType,
                          ServPac.ServPac, 
                          "",
                          MobSub.MsSeq,
                          ldtDate,
                          FALSE,   /* all changed ones */
                          llFees,  /* create fees */
                          TRUE,    /* solog */
                          0,
                          FALSE,
                          OUTPUT i).

         /* refresh temp-table */
         RUN pSetTempTable.
         
         RUN local-find-first.
         Memory = RECID(ttSubSer).
      END.
      
      ASSIGN
      Must-print = TRUE
      Must-add   = FALSE
      ufkey      = TRUE
      order      = 1.

      LEAVE.
   END.

   HIDE FRAME c-sp.

END PROCEDURE.

PROCEDURE pSetTempTable:

   EMPTY TEMP-TABLE ttSubSer.
   EMPTY TEMP-TABLE ttSubSerPara.

   FOR EACH SubSer NO-LOCK USE-INDEX ServPac WHERE
            SubSer.MSSeq = MobSub.MSSeq:

      /* show all scheduled changes, but only newest from already activated */
      IF SubSer.SSDate <= TODAY AND
         CAN-FIND(FIRST ttSubSer WHERE
                        ttSubSer.MSSeq   = SubSer.MSSeq   AND
                        ttSubSer.ServCom = SubSer.ServCom AND
                        ttSubSer.SSDate <= TODAY)
      THEN NEXT.
   
      CREATE ttSubSer.
      BUFFER-COPY SubSer TO ttSubSer.
   END.
   
   /* attributes */    
   FOR EACH SubSerPara NO-LOCK WHERE
            SubserPara.MsSeq = MobSub.MsSeq:

      /* show all scheduled changes, but only newest from already activated */
      IF SubSerPara.SSDate <= TODAY AND
         CAN-FIND(FIRST ttSubSerPara WHERE
                        ttSubSerPara.MsSeq    = SubSerPara.MsSeq    AND
                        ttSubSerPara.ServCom  = SubSerPara.ServCom  AND
                        ttSubSerPara.ParaName = SubSerPara.ParaName AND
                        ttSubSerPara.SSDate  <= TODAY)
      THEN NEXT. 
            
      CREATE ttsubserpara.
      BUFFER-COPY subserpara to ttSubserpara.
   END.
   
END PROCEDURE.

/* update temp-table changes to db */
PROCEDURE pUpdateSubSer:

   EMPTY TEMP-TABLE ttRequest.
    
   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK.
    
   ldCurrent = fMakeTS().
   
   FOR EACH ttSubSer
   BREAK BY ttSubSer.SSStat   /* deactivations first */
         BY ttSubSer.ServCom
         BY ttSubSer.SSDate DESC:
   
      IF NOT FIRST-OF(ttSubSer.ServCom) THEN NEXT. 
      
      FIND FIRST SubSer NO-LOCK WHERE
                 SubSer.MsSeq   = ttSubSer.MsSeq   AND
                 SubSer.ServCom = ttSubSer.ServCom NO-ERROR.

      IF NOT AVAILABLE SubSer OR 
         ttSubser.SSStat  NE SubSer.SSStat OR
         ttSubSer.SSParam NE Subser.SSParam 
      THEN DO:

         IF AVAILABLE SubSer AND SubSer.SSDate > ttSubSer.SSDate
         THEN DO:
            MESSAGE "There is a newer setting for service" ttSubSer.ServCom
                    SKIP
                    "This change is cancelled."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         IF ttSubSer.ServCom = "BB" AND
            ttSubSer.SSStat  = 1    AND
            NOT fIsBBAllowed(Mobsub.MsSeq,fMakeTS()) THEN DO:
            MESSAGE "BB service can not be activated since subscription" skip
                    "does not have active data bundle"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END. /* IF ServPac.ServPac = "BB" */

         /* TODO: Can be removed after services clean up (barring migration) */
         IF LOOKUP(ttSubSer.ServCom,"NAM,BARRING,LP,BPSUB") > 0 THEN DO:

            MESSAGE "Service" ttSubSer.ServCom "is retired" skip
                    "This change is cancelled"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         
         END.
         
         IF ttSubSer.SSDate = TODAY 
         THEN ldActStamp = ldCurrent.
         ELSE ldActStamp = fMake2DT(ttSubSer.SSDate,1).

         /* create change request */
         liReq = fServiceRequest(MobSub.MsSeq,
                                 ttSubSer.ServCom,
                                 ttSubSer.SSStat,
                                 ttSubSer.SSParam,
                                 ldActStamp,
                                 lcSalesman,
                                 TRUE,      /* fees */
                                 TRUE,      /* sms */          
                                 "",
                                 "4",
                                 0,
                                 FALSE,
                                 OUTPUT lcInfo).
         
         IF liReq = 0 THEN DO:
            MESSAGE "Change request was not accepted for service" 
                    ttSubSer.ServCom ";" SKIP
                    lcInfo
            VIEW-AS ALERT-BOX
            TITLE "REQUEST FAILED".
            NEXT.
         END. 
                             
         /* if change date is not in the future, run request immediately */
         IF ldActStamp <= ldCurrent THEN DO:
            CREATE ttRequest.
            ASSIGN ttRequest.ReqID   = liReq
                   ttRequest.Service = ttSubSer.ServCom.
         END. 
         
      END.
                                
   END.

   /* attributes */
   FOR EACH ttSubSerPara
   BREAK BY ttSubSerPara.ServCom
         BY ttSubSerPara.ParaName
         BY ttSubSerPara.SSDate DESC:
   
      IF NOT FIRST-OF(ttSubSerPara.ParaName) THEN NEXT. 
     
      FIND FIRST SubSerPara NO-LOCK WHERE
                 SubSerPara.MsSeq    = ttSubSerPara.MsSeq    AND
                 SubSerPara.ServCom  = ttSubSerPara.ServCom  AND
                 SubSerPara.ParaName = ttSubSerPara.ParaName NO-ERROR.
          
      IF NOT AVAILABLE SubSerPara OR 
         ttSubSerPara.ParaValue NE SubSerPara.ParaValue
      THEN DO:
       
         IF AVAILABLE SubSerPara AND SubSerPara.SSDate > ttSubSerPara.SSDate
         THEN DO:
            MESSAGE "There is a newer setting for attribute" 
                    ttSubSerPara.ServCom + "." + ttSubSerPara.ParaName
                    SKIP
                    "This change is cancelled."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         IF ttSubSerPara.SSDate = TODAY 
         THEN ldActStamp = ldCurrent.
         ELSE ldActStamp = fMake2DT(ttSubSerPara.SSDate,1).

         /* create change request */
         liReq = fServAttrRequest(MobSub.MsSeq,
                                  ttSubSerPara.ServCom,
                                  ttSubSerPara.ParaName,
                                  ttSubSerPara.ParaValue,
                                  ldActStamp,
                                  TRUE,      /* fees */
                                  FALSE,     /* sms */    
                                  "",
                                  "4",
                                  0,
                                  FALSE,
                                  OUTPUT lcInfo).
         
         IF liReq = 0 THEN DO:
            MESSAGE "Change request was not accepted for attribute" 
                    ttSubSerPara.ServCom + "." + ttSubSerPara.ParaName 
                    ";" SKIP
                    lcInfo
            VIEW-AS ALERT-BOX
            TITLE "REQUEST FAILED".
            NEXT.
         END. 
                                  
         /* if change date is not in the future, run request immediately */
         IF ldActStamp <= ldCurrent THEN DO:
            CREATE ttRequest.
            ASSIGN ttRequest.ReqID   = liReq
                   ttRequest.Service = ttSubSerPara.ServCom + "." +
                                       ttSubSerPara.ParaName.
         END.
         
      END.
   END.

   /* run requests that are not scheduled 
      (better to create all requests first before handling them, this way 
      e.g. alleviation of barring level can be handled) 
   */ 

   IF CAN-FIND(FIRST ttRequest) THEN DO:
      PAUSE 0.
      MESSAGE "Performing requests, wait ..".

      ehto = 5.
      RUN ufkey.
      
      FOR EACH ttRequest:

         RUN runreqim(ttRequest.ReqID).
           
         IF RETURN-VALUE > "" THEN
            MESSAGE "Service" 
                    (IF INDEX(ttRequest.Service,".") > 0
                     THEN "attribute"
                     ELSE "") 
                    "could not be updated;" SKIP
                    ttRequest.Service       SKIP
                    RETURN-VALUE
            VIEW-AS ALERT-BOX
            TITLE "REQUEST FAILED".
      END.
   
      HIDE MESSAGE NO-PAUSE. 
   END.
   
END PROCEDURE.

