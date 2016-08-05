/* ----------------------------------------------------------------------
  MODULE .......: ttSubserPara
  TASK .........: UPDATEs table ttSubserPara
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 27.05.03
  CHANGED ......: 14.01.04 jp servattr
                  01.07.04 tk subser.i moved
                  13.12.04/aam use ttSubserPara
                  12.12.06/mvi new param to RUN Mm/msrequest (reqstat = ?)
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'} 
{Syst/eventval.i}

DEF  NEW  shared VAR siirto AS CHAR.

DEF TEMP-TABLE ttSubserPara NO-UNDO
   LIKE SubserPara.

DEF INPUT-OUTPUT PARAMETER TABLE FOR ttSubserPara.
DEF INPUT        PARAMETER iiMsSeq   AS INT  NO-UNDO.
DEF INPUT        PARAMETER icServCom AS CHAR NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhttSubserPara AS HANDLE NO-UNDO.
   lhttSubserPara = BUFFER ttSubserPara:HANDLE.
   RUN StarEventInitialize(lhttSubserPara).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhttSubserPara).
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
    ttSubserPara.servcom   COLUMN-LABEL "Service"            FORMAT "X(10)" 
    ttSubserPara.ParaName  COLUMN-LABEL "Name of Attribute"  FORMAT "X(20)" 
    ttSubserPara.ParaValue COLUMN-LABEL "Value of Attribute" FORMAT "X(20)"
    llRequest              FORMAT "*/"   NO-LABEL SPACE(0)
    ttSubSerPara.SSDate    COLUMN-LABEL "Date" 
WITH ROW FrmRow OVERLAY CENTERED FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    " ATTRIBUTES FOR " + icServCom
    FRAME sel.

form
    ttSubserPara.servcom    /* LABEL FORMAT */
    ttSubserPara.ParaName  LABEL "Attribute" FORMAT "X(16)"
    ttSubserPara.ParaValue LABEL "Value of attribute" 
    ttSubSerPara.SSDate 
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
IF AVAILABLE ttSubserPara THEN ASSIGN
   Memory       = recid(ttSubserPara)
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
        FIND ttSubserPara WHERE recid(ttSubserPara) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttSubserPara THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttSubserPara).
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
        ufk[5]= 60 
        ufk[6]= 66
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttSubserPara.servcom {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttSubserPara.servcom WITH FRAME sel.
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
        FIND ttSubserPara WHERE recid(ttSubserPara) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttSubserPara THEN
              ASSIGN FIRSTrow = i Memory = recid(ttSubserPara).
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
           IF NOT AVAILABLE ttSubserPara THEN DO:
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
                rtab[1] = recid(ttSubserPara)
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
           IF NOT AVAILABLE ttSubserPara THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttSubserPara).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttSubserPara WHERE recid(ttSubserPara) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttSubserPara THEN DO:
           Memory = recid(ttSubserPara).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttSubserPara THEN Memory = recid(ttSubserPara).
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
           FIND ttSubserPara WHERE recid(ttSubserPara) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* request */
        RUN local-find-this(FALSE).
        RUN local-find-others.
        IF NOT llRequest THEN 
        MESSAGE "There is no pending request for this service."
        VIEW-AS ALERT-BOX ERROR.

        ELSE DO:
           RUN Mm/msrequest (1,
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
        RUN Mm/ssparahist (iiMsSeq,
                        icServCom).
        
        ufkey = TRUE.
        NEXT LOOP.
     END.
     
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhttSubserPara).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhttSubserPara).

       RUN local-disp-row.
       xrecid = recid(ttSubserPara).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttSubserPara) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttSubserPara) must-print = TRUE.
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
      FIND ttSubserPara WHERE recid(ttSubserPara) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttSubserPara WHERE recid(ttSubserPara) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttSubserPara where 
                                    ttSubserPara.msseq = iiMsSeq AND 
                                    ttSubserPara.servcom = icservcom
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttSubserPara where 
                                   ttSubserPara.msseq = iiMsSeq AND 
                                   ttSubserPara.servcom = icservcom
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttSubserPara where 
                                   ttSubserPara.msseq = iiMsSeq AND 
                                   ttSubserPara.servcom = icservcom
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttSubserPara where 
                                   ttSubserPara.msseq = iiMsSeq AND 
                                   ttSubserPara.servcom = icservcom
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       ttSubserPara.servcom
       ttSubserPara.ParaName 
       ttSubserPara.ParaValue
       llRequest
       ttSubSerPara.SSDate
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   llRequest = CAN-FIND(FIRST MsRequest WHERE      
                              MsRequest.MsSeq      = iiMsSeq AND
                              MsRequest.ReqType    = 1       AND
                              MsRequest.ReqCParam1 = ttSubSerPara.ServCom + 
                                                     "." +
                                                     ttSubSerPara.ParaName AND
                              MsRequest.ReqStat    < 2).

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
 
   CASE ttSubSerPara.SologStat:
   WHEN 1 THEN lcSologStat = "Should be sent".
   WHEN 2 THEN lcSologStat = "Solog created".
   OTHERWISE   lcSologStat = "".
   END CASE. 
 
   DISP
       ttSubserPara.servcom
       ttSubserPara.ParaName 
       ttSubserPara.ParaValue
       ttSubSerPara.SSDate
   WITH FRAME lis.   PAUSE 0.

   llChange = (lcRight = "RW").
   
   FIND MobSub WHERE MobSub.MsSeq = ttSubSerPara.MsSeq NO-LOCK NO-ERROR.
   
   IF llChange THEN
   /* can service be changed from here */
   FOR FIRST CTServEl NO-LOCK WHERE 
             CTServEl.Brand     = gcBrand              AND
             CTServEl.ServCom   = ttSubSerPara.ServCom AND
             CTServEl.CLIType   = MobSub.CLIType       AND
             CTServEl.FromDate <= TODAY,
       FIRST CTServAttr OF CTServEl NO-LOCK WHERE
             CTServAttr.ServAttr  = ttSubSerPara.ParaName AND
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
        
         UPDATE  
         ttSubserPara.Paravalue
         ttSubSerPara.SSDate
         WITH FRAME lis EDITING:
      
            READKEY.
         
            IF FRAME-FIELD = "PAraValue" AND 
               keylabel(lastkey) = "F9"
            THEN DO:
               RUN Help/h-tmscodes.p(INPUT "ServAttr",        /* TableName*/
                                     ttSubSerPara.paraname, /* FieldName */
                                     ttSubserPara.ServCom, /* GroupCode */
                              OUTPUT siirto).
            
               ASSIGN
               ttSubserPara.paravalue = siirto WHEN siirto NE ?.
               
               DISP ttSubserPara.paravalue  with frame lis.
               
            END.
          
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME LIS:
            
               PAUSE 0.
               
               IF FRAME-FIELD = "Paravalue" THEN DO:

                  /* suggest today for changes */
                  IF INPUT ttSubSerPara.ParaValue NE ttSubSerPara.ParaValue AND
                     INPUT ttSubSerPara.SSDate < TODAY
                  THEN DO:
                      DISPLAY TODAY @ ttSubSerPara.SSDate WITH FRAME lis.
                  END.
             
                  ldPara = DECIMAL(INPUT ttSubserPara.paravalue) NO-ERROR.
                  IF ldPara > 0 THEN DO:
                     FIND FIRST ServAttr WHERE 
                                ServAttr.Brand    = gcBrand            AND 
                                ServAttr.ServCom  = ttSubserPara.servcom AND 
                                ServAttr.ServAttr = ttSubserPara.paraname
                     NO-LOCK NO-ERROR.

                     IF ldPara < ServAttr.SCValueRange[1]      OR
                     (ldPara > ServAttr.SCValueRange[2] AND
                      ServAttr.SCValueRange[2] > 0)
                     THEN DO:
                        MESSAGE
                        "The value MUST be within range"
                        ServAttr.ScValueRange[1] "-" 
                        ServAttr.ScValueRange[2] "!".
                        NEXT-PROMPT ttSubserPara.paraname. NEXT.
                     END.
                  END.
                  
               END.
            END.   
            APPLY LASTKEY.
         END.
            
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.   

END PROCEDURE.

