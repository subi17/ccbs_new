/* ----------------------------------------------------------------------
  MODULE .......: RequestAction
  TASK .........: UPDATEs table RequestAction
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.02.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RequestAction

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'RequestAction'}

{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRequestAction AS HANDLE NO-UNDO.
   lhRequestAction = BUFFER RequestAction:HANDLE.
   RUN StarEventInitialize(lhRequestAction).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRequestAction).
   END.

END.

DEF INPUT PARAMETER iiReqType    AS INT  NO-UNDO.
DEF INPUT PARAMETER icCLIType    AS CHAR NO-UNDO.
DEF INPUT PARAMETER icActionType AS CHAR NO-UNDO.
DEF INPUT PARAMETER icActionKey  AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCLIType AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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
DEF VAR liActionID   AS INT                    NO-UNDO.

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcAction     AS CHAR NO-UNDO.
DEF VAR lcPayType    AS CHAR NO-UNDO.
DEF VAR lcActionType AS CHAR NO-UNDO.
DEF VAR llActive     AS LOG  NO-UNDO INIT TRUE. 

DEF TEMP-TABLE ttAction NO-UNDO
   FIELD RequestActionID AS INT
   FIELD PayType         AS INT
   FIELD CLIType         AS CHAR
   FIELD ActionType      AS CHAR
   FIELD ActionKey       AS CHAR
   FIELD ValidTo         AS DATE
   INDEX PayType PayType CLIType ActionType ActionKey ValidTo DESC
   INDEX CLIType CLIType ActionType ActionKey.
   
FORM
    RequestAction.RequestActionID FORMAT ">>>>9" 
    lcPayType  FORMAT "X(9)" COLUMN-LABEL "PayType"
    RequestAction.CLIType  FORMAT "X(9)"
    RequestAction.ActionType FORMAT "X(18)" 
    RequestAction.ActionKey
    lcAction FORMAT "X(11)" COLUMN-LABEL "Action"
    RequestAction.ValidTo
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  ACTIONS OF TYPE " + STRING(iiReqType) + " (Active) "
    FRAME sel.

{brand.i}

FORM
    RequestAction.Brand        COLON 20
    RequestAction.RequestActionID COLON 20 
    RequestAction.ReqType      COLON 20
       RequestType.ReqName NO-LABEL SKIP
    RequestAction.PayType      COLON 20 FORMAT ">>9"
       lcPayType NO-LABEL FORMAT "X(30)" SKIP
    RequestAction.CLIType      COLON 20 
       CLIType.CLIName NO-LABEL SKIP
    RequestAction.ActionType   COLON 20
       lcActionType NO-LABEL FORMAT "X(30)" SKIP
    RequestAction.ActionKey    COLON 20  FORMAT "x(256)" VIEW-AS FILL-IN SIZE 40 BY 1
    RequestAction.Action       COLON 20
       lcAction NO-LABEL FORMAT "X(30)" SKIP
    RequestAction.ValidFrom    COLON 20
    RequestAction.ValidTo      COLON 20    
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "CLIType:" lcCLIType FORMAT "X(15)"
    HELP "Enter CLIType"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLIType"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fActionDesc RETURNS CHAR
   (INPUT iiAction AS INT):
   
   IF iiAction = 0 THEN RETURN "".
   
   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "RequestAction",
                           "Action",
                           STRING(iiAction)).
END FUNCTION.

FUNCTION fActionTypeDesc RETURNS CHAR
   (INPUT icActionType AS CHAR):
   
   IF icActionType = "" THEN RETURN "".
   
   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "RequestAction",
                           "ActionType",
                           icActionType).
END FUNCTION.

FUNCTION fPayType RETURNS CHAR
   (INPUT iiPayType AS INT):
   
   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "CLIType",
                           "PayType",
                           STRING(iiPayType)).
END FUNCTION.
   
   

RUN pInitTempTable.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE ttAction THEN ASSIGN
   Memory       = recid(ttAction)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No actions available" VIEW-AS ALERT-BOX.
      RETURN.
   END.

   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a RequestAction  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY gcBrand @ RequestAction.Brand
                   iiReqType @ RequestAction.ReqType.

           PROMPT-FOR 
              RequestAction.PayType
              RequestAction.CLIType WITH FRAME lis
           EDITING:
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "PayType" THEN DO:

                 RUN h-tmscodes(INPUT "CLIType",     /* TableName*/
                                      "PayType",       /* FieldName */
                                      "MobSub",     /* GroupCode */
                                OUTPUT lcCode).

                 IF lcCode ne "" AND lcCode NE ? THEN 
                    DISPLAY INTEGER(lcCode) ;& RequestAction.PayType
                            WITH FRAME lis.

                 ehto = 9.
                 RUN ufkey.
                 NEXT. 
              END.
                 
              APPLY LASTKEY.
           END.
            
           IF INPUT RequestAction.PayType = 0 AND 
              INPUT RequestAction.CLIType = "" THEN UNDO, LEAVE ADD-ROW.

           IF INPUT RequestAction.PayType > 0 AND
              fPayType(INPUT INPUT RequestAction.PayType) = ""
           THEN DO:
              MESSAGE "Unknown payment type"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
           
           IF INPUT RequestAction.CLIType NE "*" AND 
              INPUT RequestAction.CLIType > "" AND 
                                 NOT CAN-FIND(FIRST CLIType WHERE
                                 CLIType.Brand = gcBrand AND
                                 CLIType.CLIType = INPUT RequestAction.CLIType) 
           THEN DO:
               MESSAGE "Unknown CLI type"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.
           
           FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
           IF AVAILABLE RequestAction THEN 
              liActionID = RequestAction.RequestActionID + 1.
           ELSE liActionID = 1.
           
           CREATE RequestAction.
           ASSIGN 
              RequestAction.Brand   = gcBrand
              RequestAction.RequestActionID = liActionID
              RequestAction.ReqType = iiReqType
              RequestAction.PayType = INPUT FRAME lis RequestAction.PayType
              RequestAction.CLIType = INPUT FRAME lis RequestAction.CLIType.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           CREATE ttAction.
           BUFFER-COPY RequestAction TO ttAction.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRequestAction).

           ASSIGN
           Memory = recid(ttAction)
           xrecid = RECID(RequestAction).  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttAction THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttAction WHERE recid(ttAction) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttAction THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttAction).
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
        ufk[1]= 739 
        ufk[2]= 0  
        ufk[3]= 0  
        ufk[4]= 1827 WHEN NOT llActive
        ufk[4]= 1828 WHEN llActive
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[4] = 0
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RequestAction.CLIType ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RequestAction.CLIType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"4,f4,5,f5,8,f8") = 0 THEN DO:
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
        FIND ttAction WHERE recid(ttAction) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttAction THEN
              ASSIGN FIRSTrow = i Memory = recid(ttAction).
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
           IF NOT AVAILABLE ttAction THEN DO:
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
                rtab[1] = recid(ttAction)
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
           IF NOT AVAILABLE ttAction THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttAction).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttAction WHERE recid(ttAction) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttAction THEN DO:
           Memory = recid(ttAction).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttAction THEN Memory = recid(ttAction).
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
           FIND ttAction WHERE recid(ttAction) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET lcCLIType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcCLIType > "" THEN DO:
          FIND FIRST ttAction USE-INDEX CLIType WHERE 
                     ttAction.CLIType >= lcCLIType
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttAction THEN NEXT BROWSE.

          ASSIGN
             order      = 1 
             memory     = RECID(ttAction) 
             must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* display filter */
        llActive = NOT llActive.
        RUN pInitTempTable.
        RUN local-find-first.
        ASSIGN 
           Memory = recid(ttAction) 
           must-print = TRUE
           ufkey = TRUE.
        IF llActive THEN    
           FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"All","Active").
        ELSE FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"Active","All").
   
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           RUN local-find-this(FALSE).
           xRecid = RECID(RequestAction).
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       lcPayType
       RequestAction.CLIType
       RequestAction.ActionType.

       RUN local-find-NEXT.
       IF AVAILABLE ttAction THEN Memory = recid(ttAction).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttAction THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttAction).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       lcPayType
       RequestAction.CLIType
       RequestAction.ActionType.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRequestAction).

           DELETE RequestAction.
           DELETE ttAction.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE RequestAction THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = RECID(RequestAction).
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequestAction).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequestAction).

       RUN local-disp-row.
       xrecid = recid(RequestAction).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttAction) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttAction) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
       FIND ttAction WHERE recid(ttAction) = rtab[frame-line(sel)].
       FIND FIRST RequestAction WHERE 
          RequestAction.RequestActionID = ttAction.RequestActionID
       EXCLUSIVE-LOCK.
    END.  
    ELSE DO:
       FIND ttAction WHERE recid(ttAction) = rtab[frame-line(sel)] NO-LOCK.
       FIND FIRST RequestAction WHERE 
          RequestAction.RequestActionID = ttAction.RequestActionID NO-LOCK.
    END.
    
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiReqType ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND FIRST ttAction USE-INDEX PayType NO-LOCK NO-ERROR.
         IF AVAILABLE ttAction THEN 
            FIND FIRST RequestAction WHERE 
               RequestAction.RequestActionID = ttAction.RequestActionID 
               NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiReqType ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND LAST ttAction USE-INDEX PayType NO-LOCK NO-ERROR.
         IF AVAILABLE ttAction THEN 
            FIND FIRST RequestAction WHERE 
               RequestAction.RequestActionID = ttAction.RequestActionID 
               NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiReqType ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND NEXT ttAction USE-INDEX PayType NO-LOCK NO-ERROR.
         IF AVAILABLE ttAction THEN 
            FIND FIRST RequestAction WHERE 
               RequestAction.RequestActionID = ttAction.RequestActionID 
               NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiReqType ne ?  THEN DO:
      IF order = 1 THEN DO:
         FIND PREV ttAction USE-INDEX PayType NO-LOCK NO-ERROR.
         IF AVAILABLE ttAction THEN 
            FIND FIRST RequestAction WHERE 
               RequestAction.RequestActionID = ttAction.RequestActionID 
               NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-disp-row:

       FIND FIRST RequestAction WHERE 
             RequestAction.RequestActionID = ttAction.RequestActionID NO-LOCK.
       
       RUN local-find-others.
       IF RequestAction.PayType = 0 THEN lcPayType = "".
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RequestAction.RequestActionID
       lcPayType
       RequestAction.CLIType
       RequestAction.ActionType
       RequestAction.ActionKey
       lcAction
       RequestAction.ValidTo
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcAction = fActionDesc(RequestAction.Action).
   lcPayType = fPayType(RequestAction.PayType).
  
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bAction FOR RequestAction.
    
   ActionDetails:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND RequestType WHERE 
           RequestType.Brand   = gcBrand AND
           RequestType.ReqType = RequestAction.ReqType NO-LOCK NO-ERROR.
      
      FIND CLIType WHERE 
           CLIType.Brand   = gcBrand AND
           CLIType.CLIType = RequestAction.CLIType NO-LOCK NO-ERROR.
           
      lcActionType = fActionTypeDesc(RequestAction.ActionType).

      DISP 
         RequestAction.Brand          
         RequestAction.RequestActionID
         RequestAction.ReqType        
         RequestType.ReqName WHEN AVAILABLE RequestType
         RequestAction.PayType
         lcPayType 
         RequestAction.CLIType
         CLIType.CLIName WHEN AVAILABLE CLIType
         RequestAction.ActionType lcActionType
         RequestAction.ActionKey
         RequestAction.Action lcAction
         RequestAction.ValidFrom
         RequestAction.ValidTo
       WITH FRAME lis.

      
      IF NOT NEW RequestAction THEN REPEAT:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[4] = 1865
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.

         IF toimi = 1 THEN LEAVE.
         
         ELSE IF toimi = 4 THEN 
            RUN requestactionrule(RequestAction.RequestActionID).
            
         ELSE IF toimi = 8 THEN LEAVE ActionDetails.
      END.

      FIND CURRENT RequestAction EXCLUSIVE-LOCK.
      
      UPDATE
         RequestAction.ActionType  
         RequestAction.ActionKey
         RequestAction.Action
         RequestAction.ValidFrom
         RequestAction.ValidTo      
      WITH FRAME lis EDITING:
 
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND 
            LOOKUP(FRAME-FIELD,"Action,ActionType") > 0 
         THEN DO:

            IF FRAME-FIELD = "Action" THEN DO:

               RUN h-tmscodes(INPUT "RequestAction",     /* TableName*/
                                    "Action",       /* FieldName */
                                    "Request",     /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ? THEN 
                  DISPLAY INTEGER(lcCode) ;& RequestAction.Action 
                  WITH FRAME lis.
            END.

            ELSE IF FRAME-FIELD = "ActionType" THEN DO:

               RUN h-tmscodes(INPUT "RequestAction",     /* TableName*/
                                    "ActionType",       /* FieldName */
                                    "Request",     /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ? THEN 
                  DISPLAY lcCode ;& RequestAction.ActionType
                  WITH FRAME lis.
            END.

            ehto = 9.
            RUN ufkey.
            NEXT. 
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.

            IF FRAME-FIELD = "Action" THEN DO:
               lcAction = fActionDesc(INPUT INPUT RequestAction.Action).
               DISP lcAction WITH FRAME lis.

               IF lcAction = "" THEN DO:
                  MESSAGE "Unknown action"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
 
            ELSE IF FRAME-FIELD = "ActionType" THEN DO:
               lcActionType = 
                  fActionTypeDesc(INPUT INPUT RequestAction.ActionType).
               DISP lcActionType WITH FRAME lis.

               IF lcActionType = "" THEN DO:
                  MESSAGE "Unknown action type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
             
         END.
            
         APPLY LASTKEY.
      END.

      FIND FIRST bAction NO-LOCK WHERE
                 bAction.Brand      = gcBrand AND
                 bAction.ReqType    = RequestAction.ReqType    AND
                 bAction.CLIType    = RequestAction.CLIType    AND
                 bAction.PayType    = RequestAction.PayType    AND
                 bAction.ActionType = RequestAction.ActionType AND
                 bAction.ActionKey  = RequestAction.ActionKey  AND
                 bAction.ValidFrom <= RequestAction.ValidTo    AND
                 bAction.ValidTo   >= RequestAction.ValidFrom  AND
                 RECID(bAction) NE RECID(RequestAction) NO-ERROR.

      IF AVAIL bAction THEN DO:
         
         IF bAction.Action EQ RequestAction.Action THEN DO:
            MESSAGE "Check dates. Another configuration row already exists"
                    "with overlapping effective period."
            VIEW-AS ALERT-BOX ERROR.
            NEXT. 
         END.
         ELSE DO:
            MESSAGE "Another configuration row already exists"
                  "with overlapping effective period but with different action."
            VIEW-AS ALERT-BOX TITLE "WARNING".
         END.
      END.
      
      LEAVE.
   
   END.
   
END PROCEDURE.

PROCEDURE pInitTempTable:

   EMPTY TEMP-TABLE ttAction.
   
   FOR EACH RequestAction NO-LOCK WHERE
            RequestAction.Brand   = gcBrand AND
            RequestAction.ReqType = iiReqType:

      IF llActive AND RequestAction.ValidTo < TODAY THEN NEXT.
            
      CREATE ttAction.
      BUFFER-COPY RequestAction TO ttAction.
   END.

END PROCEDURE.

