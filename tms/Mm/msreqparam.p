/* ----------------------------------------------------------------------
  MODULE .......: msreqparam.p
  TASK .........: view parameters on msrequest
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 29.11.07
  CHANGED ......:
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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

DEF VAR lhRequest  AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttParam NO-UNDO
   FIELD ParamCode   AS CHAR
   FIELD Usage       AS CHAR
   FIELD ParamValue  AS CHAR
   FIELD Description AS CHAR
   INDEX ParamCode ParamCode.


FORM
    ttParam.ParamCode   COLUMN-LABEL "Parameter"
    ttParam.Usage       COLUMN-LABEL "Usage"        FORMAT "X(20)"
    ttParam.ParamValue  COLUMN-LABEL "Value"        FORMAT "X(20)"
    ttParam.Description COLUMN-LABEL "Description"  FORMAT "X(20)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) "  REQUEST PARAMETERS " 
    FRAME sel.

FORM
    ttParam.ParamCode   COLON 15 LABEL "Parameter"
    ttParam.Usage       COLON 15 LABEL "Usage"        FORMAT "X(50)"
    ttParam.ParamValue  COLON 15 LABEL "Value"        FORMAT "X(50)"
    ttParam.Description COLON 15 LABEL "Description"  FORMAT "X(50)"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
    ttParam.ParamValue VIEW-AS EDITOR SIZE 60 BY 4 
    WITH OVERLAY ROW 11 CENTERED TITLE " VALUE " NO-LABELS FRAME fEditor.
        

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN DO:
   MESSAGE "Unknown request"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lhRequest = BUFFER MsRequest:HANDLE.

RUN pGetParameters(lhRequest).

RUN local-Find-First.

IF AVAILABLE ttParam THEN ASSIGN
   Memory       = recid(ttParam)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No parameters defined" VIEW-AS ALERT-BOX.
   RETURN.
END.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ttParam  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(ttParam)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttParam THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttParam WHERE recid(ttParam) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttParam THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttParam).
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
        Syst.Var:ufk    = 0
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey = FALSE.
       
        FIND FIRST MsRequestParam NO-LOCK WHERE 
                   MsRequestParam.MsRequest EQ iiMsRequest NO-ERROR.

        IF AVAIL MsRequestParam THEN 
           Syst.Var:ufk[4] = 9859.  

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttParam.ParamCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) ttParam.ParamCode WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
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
        FIND ttParam WHERE recid(ttParam) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttParam THEN
              ASSIGN FIRSTrow = i Memory = recid(ttParam).
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
           IF NOT AVAILABLE ttParam THEN DO:
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
                rtab[1] = recid(ttParam)
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
           IF NOT AVAILABLE ttParam THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttParam).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttParam WHERE recid(ttParam) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttParam THEN DO:
           Memory = recid(ttParam).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttParam THEN Memory = recid(ttParam).
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
           FIND ttParam WHERE recid(ttParam) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttParam).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttParam) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttParam) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.
     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 THEN DO:
        RUN Mm/msrequestparam.p(iiMsRequest).
        ufkey = TRUE. 
     END.   

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttParam WHERE recid(ttParam) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttParam WHERE recid(ttParam) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST ttParam NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST ttParam NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT ttParam NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV ttParam NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttParam.ParamCode
       ttParam.ParamValue
       ttParam.Usage
       ttParam.Description
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      PAUSE 0.
      DISPLAY 
         ttParam.ParamCode
         ttParam.ParamValue
         ttParam.Usage
         ttParam.Description
      WITH FRAME lis.

      IF LENGTH(ttParam.ParamValue) > 50 THEN DO:
          PAUSE 0.
          DISP ttParam.ParamValue WITH FRAME fEditor.
      END.
      
      ASSIGN 
         Syst.Var:ufk    = 0
         Syst.Var:ufk[8] = 8
         Syst.Var:ehto   = 0.
         
      RUN Syst/ufkey.p.
         
      IF Syst.Var:toimi = 8 THEN LEAVE.
   END.
   
   HIDE FRAME fEditor NO-PAUSE.
   
END PROCEDURE.

PROCEDURE pGetParameters:

   DEF INPUT PARAMETER ihRequest  AS HANDLE NO-UNDO.

   DEF VAR lhType        AS HANDLE NO-UNDO.
   DEF VAR lhField       AS HANDLE NO-UNDO.
   DEF VAR lcProcedure   AS CHAR   NO-UNDO.
   DEF VAR lcParam       AS CHAR   NO-UNDO EXTENT 5. 
   DEF VAR liCnt         AS INT    NO-UNDO.
   DEF VAR lcDescription AS CHAR   NO-UNDO.
   
   lhType = ihRequest:BUFFER-FIELD("ReqType"). 
   
   /* check what fields are used for this request type and get their values */
   FOR EACH RequestParam NO-LOCK WHERE
            RequestParam.Brand     = Syst.Var:gcBrand             AND
            RequestParam.ReqType   = lhType:BUFFER-VALUE AND
            RequestParam.DispParam = TRUE                AND
            RequestParam.Usage     > "":

      CREATE ttParam.
      ASSIGN 
         ttParam.ParamCode   = RequestParam.ParamField
         ttParam.ParamCode   = REPLACE(ttParam.ParamCode,"Req","")
         ttParam.ParamCode   = REPLACE(ttParam.ParamCode,"Param","")
         ttParam.Usage       = RequestParam.Usage
         ttParam.Description = RequestParam.Description
         lhField             = ihRequest:BUFFER-FIELD(RequestParam.ParamField)
         ttParam.ParamValue  = lhField:BUFFER-VALUE.
     
      /* is there a procedure for getting description */
      IF RequestParam.CharConfig > "" THEN DO:
      
         ASSIGN
            lcProcedure = "p" + ENTRY(1,RequestParam.CharConfig)
            lcParam     = "".
            
         IF NUM-ENTRIES(RequestParam.CharConfig) > 1 THEN 
         DO liCnt = 2 TO MIN(NUM-ENTRIES(RequestParam.CharConfig),6):
            lcParam[liCnt - 1] = ENTRY(liCnt,RequestParam.CharConfig).
         END.      
               
         IF LOOKUP(lcProcedure,THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN DO:
            RUN VALUE(lcProcedure) (ttParam.ParamValue,
                                    lcParam,
                                    OUTPUT lcDescription).
                                   
            IF lcDescription > "" THEN ttParam.Description = lcDescription.
         END.
            
      END.
      
   END.
   
END PROCEDURE.


/* procedures for getting ttParam.Description */

PROCEDURE pTimeStamp:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
   
   DEF VAR ldStamp AS DEC NO-UNDO.

   ldStamp = DECIMAL(icValue) NO-ERROR.
   
   IF ldStamp > 0 THEN 
      ocDescription = Func.Common:mTS2HMS(ldStamp).
   
END PROCEDURE.

PROCEDURE pTMSCodes:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
 
   ocDescription = Func.Common:mTMSCodeName(icParam[1],
                                    icParam[2],
                                    icValue).
 
END PROCEDURE.

PROCEDURE pCustName:
 
   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
 
   DEF VAR liCustNum AS INT NO-UNDO.
   
   liCustNum = INTEGER(icValue) NO-ERROR.
   
   IF liCustNum > 0 THEN DO:
      FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         ocDescription = Func.Common:mDispCustName(BUFFER Customer).
   END.
   
END PROCEDURE.

PROCEDURE pExtInvID:
 
   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
 
   DEF VAR liInvNum AS INT NO-UNDO.
   
   liInvNum = INTEGER(icValue) NO-ERROR.
   
   IF liInvNum > 0 THEN DO:
      FIND Invoice WHERE Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.
      IF AVAILABLE Invoice THEN 
         ocDescription = Invoice.ExtInvID.
   END.
   
END PROCEDURE.

PROCEDURE pCLIType:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
 
   FIND CLIType WHERE 
        CLIType.Brand   = Syst.Var:gcBrand AND
        CLIType.CLIType = icValue NO-LOCK NO-ERROR.
   IF AVAILABLE CLIType THEN 
      ocDescription = CLIType.CLIName.
   
END PROCEDURE.

PROCEDURE pServCom:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.
 
   /* attribute change */
   IF NUM-ENTRIES(icValue,".") > 1 THEN 
      icValue = ENTRY(1,icValue,".").

   FIND ServCom WHERE 
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = icValue NO-LOCK NO-ERROR.
   IF AVAILABLE ServCom THEN 
      ocDescription = ServCom.SCName.
   
END PROCEDURE.

PROCEDURE pServPac:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.

   FIND ServPac WHERE
        ServPac.Brand   = Syst.Var:gcBrand  AND
        ServPAc.ServPac = icValue NO-LOCK NO-ERROR.
   IF AVAILABLE ServPac THEN 
      ocDescription = ServPac.SPName.

END PROCEDURE.

PROCEDURE pDayCampaign:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.

   FIND DayCampaign WHERE
        DayCampaign.Brand   = Syst.Var:gcBrand  AND
        DayCampaign.DCEvent = icValue NO-LOCK NO-ERROR.
   IF AVAILABLE DayCampaign THEN 
      ocDescription = DayCampaign.DCName.

END PROCEDURE.

PROCEDURE pBillCode:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.

   FIND BillItem WHERE
        BillItem.Brand    = Syst.Var:gcBrand  AND
        BillItem.BillCode = icValue NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN 
      ocDescription = BillItem.BIName.

END PROCEDURE.

PROCEDURE pTMRule:

   DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icParam       AS CHAR NO-UNDO EXTENT 5.
   DEF OUTPUT PARAMETER ocDescription AS CHAR NO-UNDO.

   FIND FIRST TMRule WHERE TMRule.TMRuleSeq = INTEGER(icValue)
      NO-LOCK NO-ERROR.
   IF AVAILABLE TMRule THEN 
      ocDescription = TMRule.Name.

END PROCEDURE.







