/* ----------------------------------------------------------------------
  MODULE .......: RequestStatus
  TASK .........: UPDATEs table RequestStatus
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 02.11.07
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RequestStatus

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RequestStatus'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRequestStatus AS HANDLE NO-UNDO.
   lhRequestStatus = BUFFER RequestStatus:HANDLE.
   RUN StarEventInitialize(lhRequestStatus).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRequestStatus).
   END.

END.

DEF INPUT PARAMETER iiReqType AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liReqStat    AS INT                    NO-UNDO.
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

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 


FORM
    RequestStatus.ReqStat
    lcStatus                FORMAT "X(25)" COLUMN-LABEL "Description"
    RequestStatus.Program   FORMAT "X(12)"
    RequestStatus.InUse 
    RequestStatus.LogOn     COLUMN-LABEL "Log"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  STATUS HANDLING FOR TYPE " + STRING(iiReqType) + " "
    FRAME sel.

{Func/brand.i}

FORM
    RequestStatus.Brand          COLON 15
    RequestStatus.ReqType        COLON 15
       RequestType.ReqName NO-LABEL SKIP
    RequestStatus.ReqStat        COLON 15 
       HELP "Request status, ?=quit insert mode"
       lcStatus NO-LABEL FORMAT "X(30)" SKIP
    RequestStatus.Program        COLON 15
    RequestStatus.InUse          COLON 15 
    RequestStatus.LogOn          COLON 15 LABEL "Logging"
    RequestStatus.LogFile        COLON 15
    RequestStatus.LogEntry       COLON 15 FORMAT "X(40)"
    RequestStatus.LogClear       COLON 15
    RequestStatus.LogThreshold   COLON 15
     
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand :" lcBrand skip
    "Status:" liReqStat FORMAT ">>>>>9" 
    HELP "Enter status"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Status"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE RequestStatus THEN ASSIGN
   Memory       = recid(RequestStatus)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No request status available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a RequestStatus  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ RequestStatus.Brand
                   iiReqType @ RequestStatus.ReqType.

           PROMPT-FOR RequestStatus.ReqStat WITH FRAME lis
           EDITING:

              READKEY.
           
              IF KEYLABEL(LASTKEY) = "F9" THEN DO:
            
                 RUN h-tmscodes(INPUT "MsRequest",  /* TableName */
                                      "ReqStatus",     /* FieldName */
                                      "Request",       /* GroupCode */
                                OUTPUT lcCode).
              
                 IF lcCode ne "" AND lcCode NE ? THEN DO:
                    DISPLAY INTEGER(lcCode) ;& RequestStatus.ReqStat
                    WITH FRAME lis.   
                 END.
 
                 ehto = 9.
                 RUN ufkey.
                 NEXT. 
              END.

              APPLY LASTKEY.
           END.
            
           IF INPUT RequestStatus.ReqStat = ? THEN UNDO, LEAVE ADD-ROW.

           lcStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "MsRequest",
                                       "ReqStatus",
                                       STRING(INPUT FRAME lis 
                                              RequestStatus.ReqStat)).
           IF lcStatus = "" THEN DO:
              MESSAGE "Unknown request status"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.

           IF CAN-FIND(FIRST RequestStatus WHERE
                             RequestStatus.Brand   = gcBrand AND
                             RequestStatus.ReqType = iiReqType 
                             USING FRAME lis RequestStatus.ReqStat)
           THEN DO:
              MESSAGE "Status has already been defined for this type"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
                             

           CREATE RequestStatus.
           ASSIGN 
              RequestStatus.Brand    = gcBrand
              RequestStatus.ReqType  = iiReqType
              RequestStatus.ReqStat  = INPUT FRAME lis RequestStatus.ReqStat
              RequestStatus.InUse    = TRUE.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRequestStatus).

           ASSIGN
           Memory = recid(RequestStatus)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RequestStatus THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND RequestStatus WHERE recid(RequestStatus) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RequestStatus THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RequestStatus).
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
        ufk[1]= 36 ufk[2]= 0  ufk[3]= 0  
        ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RequestStatus.ReqStat ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RequestStatus.ReqStat WITH FRAME sel.
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
        FIND RequestStatus WHERE recid(RequestStatus) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RequestStatus THEN
              ASSIGN FIRSTrow = i Memory = recid(RequestStatus).
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
           IF NOT AVAILABLE RequestStatus THEN DO:
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
                rtab[1] = recid(RequestStatus)
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
           IF NOT AVAILABLE RequestStatus THEN DO:
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
              rtab[FRAME-DOWN] = recid(RequestStatus).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RequestStatus WHERE recid(RequestStatus) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RequestStatus THEN DO:
           Memory = recid(RequestStatus).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RequestStatus THEN Memory = recid(RequestStatus).
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
           FIND RequestStatus WHERE recid(RequestStatus) = Memory NO-LOCK.
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
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           liReqStat WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liReqStat > 0 THEN DO:
          FIND FIRST RequestStatus WHERE 
                     RequestStatus.Brand   = lcBrand AND
                     RequestStatus.ReqType = iiReqType AND
                     RequestStatus.ReqStat >= liReqStat
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
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
       RequestStatus.ReqStat
       RequestStatus.InUse RequestStatus.Program.

       RUN local-find-NEXT.
       IF AVAILABLE RequestStatus THEN Memory = recid(RequestStatus).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RequestStatus THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RequestStatus).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RequestStatus.ReqStat
       RequestStatus.InUse RequestStatus.Program.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRequestStatus).

           DELETE RequestStatus.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE RequestStatus THEN DO:
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
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequestStatus).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequestStatus).

       RUN local-disp-row.
       xrecid = recid(RequestStatus).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RequestStatus) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RequestStatus) must-print = TRUE.
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

    IF exlock THEN
      FIND RequestStatus WHERE recid(RequestStatus) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RequestStatus WHERE recid(RequestStatus) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND FIRST RequestStatus 
          WHERE RequestStatus.Brand = lcBrand AND
                RequestStatus.ReqType = iiReqType
          USE-INDEX ReqStat NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND LAST RequestStatus
          WHERE RequestStatus.Brand = lcBrand AND
                RequestStatus.ReqType = iiReqType
          USE-INDEX ReqStat NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND NEXT RequestStatus
          WHERE RequestStatus.Brand = lcBrand AND
                RequestStatus.ReqType = iiReqType
          USE-INDEX ReqStat NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiReqType ne ?  THEN DO:
       IF order = 1 THEN FIND PREV RequestStatus
          WHERE RequestStatus.Brand = lcBrand AND
                RequestStatus.ReqType = iiReqType
          USE-INDEX ReqStat NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RequestStatus.ReqStat
       lcStatus
       RequestStatus.InUse
       RequestStatus.Program
       RequestStatus.LogOn
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
   lcStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "MsRequest",
                               "ReqStatus",
                               STRING(RequestStatus.ReqStat)).
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND RequestType WHERE 
           RequestType.Brand   = gcBrand AND
           RequestType.ReqType = RequestStatus.ReqType NO-LOCK NO-ERROR.
      DISP 
         RequestStatus.Brand          
         RequestStatus.ReqType        
         RequestType.ReqName WHEN AVAILABLE RequestType
         RequestStatus.ReqStat
         lcStatus
         RequestStatus.Program        
         RequestStatus.InUse           
         RequestStatus.LogOn          
         RequestStatus.LogFile        
         RequestStatus.LogEntry       
         RequestStatus.LogClear       
         RequestStatus.LogThreshold   
       WITH FRAME lis.

      
      IF NOT NEW RequestStatus THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      FIND CURRENT RequestStatus EXCLUSIVE-LOCK.
      
      UPDATE
         RequestStatus.Program        
         RequestStatus.InUse           
         RequestStatus.LogOn          
         RequestStatus.LogFile        
         RequestStatus.LogEntry       
         RequestStatus.LogClear       
         RequestStatus.LogThreshold   
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.
 
      LEAVE.
   
   END.
   
END PROCEDURE.
