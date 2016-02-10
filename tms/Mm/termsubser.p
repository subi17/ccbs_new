/* ----------------------------------------------------------------------
  MODULE .......: SubSer.P
  TASK .........: UPDATE Mobile subs. Services
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-06-99
  CHANGED ......: 21.07.99 pt f5 / add allowed 
                  12.08.99 pt F6 restricted
                  13.08.99 pt RUN Mm/setms when exiting
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
                  12.12.06/mvi new param to RUN Mm/msrequest (reqstat = ?)
                  19.03.07 kl  all updates removed

  Version ......: M15
 ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SubSer'}
{Syst/eventval.i}
{Func/msisdn.i}
{Func/service.i}
{Func/timestamp.i}

DEF TEMP-TABLE ttSubSer NO-UNDO
   LIKE SubSer.
 
DEF TEMP-TABLE ttsubserpara NO-UNDO 
   LIKE subserpara.

DEF TEMP-TABLE ttRequest NO-UNDO
   FIELD ReqId   AS INT
   FIELD Service AS CHAR
   INDEX ReqID ReqID.
   
{Func/sername.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSubSer AS HANDLE NO-UNDO.
   lhSubSer = BUFFER SubSer:HANDLE.
   RUN StarEventInitialize(lhSubSer).

   DEFINE VARIABLE lhSubSerPara AS HANDLE NO-UNDO.
   lhSubSerPara = BUFFER SubSerPara:HANDLE.
   RUN StarEventInitialize(lhSubSerPara).

   DEFINE VARIABLE lhTermMobsub AS HANDLE NO-UNDO.
   lhTermMobsub = BUFFER TermMobsub:HANDLE.
   RUN StarEventInitialize(lhTermMobsub).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSubSer).
   END.

END.

DEF INPUT PARAMETER   iiMsSeq LIKE TermMobsub.MsSeq NO-UNDO .

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
DEF VAR llChange        AS LOG                NO-UNDO. 
DEF VAR lcSologStat     AS CHAR               NO-UNDO. 
DEF VAR ldActStamp      AS DEC                NO-UNDO. 
DEF VAR liOldStat       AS INT                NO-UNDO. 
DEF VAR llUpdate        AS LOG                NO-UNDO. 
DEF VAR ldAmt           AS DEC                NO-UNDO. 
DEF VAR liDefValue      AS INT                NO-UNDO.
DEF VAR ldCurrent       AS DEC                NO-UNDO.
DEF VAR llRequest       AS LOG                NO-UNDO. 
DEF VAR liReq           AS INT                NO-UNDO. 
DEF VAR liNewStat       AS INT                NO-UNDO. 
DEF VAR lcSalesman      AS CHAR               NO-UNDO. 
DEF VAR llSendSMS       AS LOG                NO-UNDO.

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
    TITLE COLOR VALUE(ctc)  " Services of TermMobsub " + TermMobsub.CLI + " "
    FRAME sel.

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


FIND TermMobsub WHERE TermMobsub.MsSeq = iiMsSeq NO-LOCK.

/* get current definitions, servpac index is sorted by date */
RUN pSetTempTable.
        
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Service,By ServPac,By 3, By 4".


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
           ufk[1] = 245
           ufk[2] = 246
           ufk[3] = 2350 
           ufk[4] = 0
           ufk[5] = 0 
           ufk[6] = 66
           ufk[7] = 0 
           ufk[8] = 8
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.
      
        RUN Syst/ufkey.p.

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
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
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

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
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

           ELSE RUN Mm/subserpara (INPUT-OUTPUT TABLE ttSubserPara,
                                ttSubSer.MsSeq,
                                ttSubSer.ServCom).
        END. 
        
        ufkey = TRUE.
        NEXT loop.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 THEN DO:  /* history */
        RUN Mm/subserhist (iiMsSeq).
        
        ufkey = TRUE.
        NEXT LOOP.
     END.
    
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttSubSer.ServCom.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.

       xrecid = recid(ttSubSer).

       IF must-print THEN NEXT LOOP.
       LEAVE.
       
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
      
      llChange = FALSE.

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
          
      ehto = 5.
      RUN Syst/ufkey.
      PAUSE MESSAGE "Press ENTER to continue".
   
      LEAVE.

   END.

END PROCEDURE.

PROCEDURE pSetTempTable:

   EMPTY TEMP-TABLE ttSubSer.
   EMPTY TEMP-TABLE ttSubSerPara.

   FOR EACH SubSer NO-LOCK USE-INDEX ServPac WHERE
            SubSer.MSSeq = TermMobsub.MSSeq:

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
            SubserPara.MsSeq = TermMobsub.MsSeq:

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

