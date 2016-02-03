/* ----------------------------------------------------------------------
  MODULE .......: RequestType
  TASK .........: UPDATEs table RequestType
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 02.11.07
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RequestType

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RequestType'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRequestType AS HANDLE NO-UNDO.
   lhRequestType = BUFFER RequestType:HANDLE.
   RUN StarEventInitialize(lhRequestType).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRequestType).
   END.

END.

DEF INPUT PARAMETER iiQueue AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liReqType    AS INT                    NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR llShowHistory AS LOG NO-UNDO INIT TRUE. 

DEF VAR lcQueueName      AS CHAR NO-UNDO.
DEF VAR lcField          AS CHAR NO-UNDO. 
DEF VAR lcCode           AS CHAR NO-UNDO. 


FORM
    RequestType.ReqType
    RequestType.ReqName
    RequestType.Program   FORMAT "X(18)"
    RequestType.InUse 
    RequestType.Queue
    RequestType.LogOn     COLUMN-LABEL "Log"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  REQUEST TYPES " + 
      (IF iiQueue > 0 
       THEN "IN QUEUE " + STRING(iiQueue)
       ELSE "") + "  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    RequestType.Brand          COLON 15
    RequestType.ReqType        COLON 15
    RequestType.ReqName        COLON 15
    RequestType.Queue          COLON 15 RequestQueue.QName NO-LABEL FORMAT "X(20)"
    RequestType.InUse          COLON 15 
    RequestType.Program        COLON 15
    RequestType.UserCode       COLON 15 
    RequestType.LogOn          COLON 15 LABEL "Logging"
    RequestType.LogFile        COLON 15
    RequestType.LogEntry       COLON 15 FORMAT "X(40)"
    RequestType.LogClear       COLON 15
    RequestType.LogThreshold   COLON 15 
    RequestType.Mode           COLON 15
    
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Queue:" liReqType FORMAT ">>>>>9" 
    HELP "Enter type number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Type"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


IF iiQueue > 0 OR gcHelpParam > "" THEN ASSIGN
   FrmRow  = 2
   FrmDown = 14.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE RequestType THEN ASSIGN
   Memory       = recid(RequestType)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No request types available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a RequestType  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
        KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ RequestType.Brand
                   iiQueue @ RequestType.Queue.

           PROMPT-FOR RequestType.ReqType WITH FRAME lis.
           IF INPUT RequestType.ReqType = 0 THEN UNDO, LEAVE ADD-ROW.
           
           CREATE RequestType.
           ASSIGN 
              RequestType.Brand    = lcBrand
              RequestType.Queue    = iiQueue
              RequestType.ReqType  = INPUT FRAME lis RequestType.ReqType
              RequestType.InUse    = TRUE.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRequestType).

           ASSIGN
           Memory = recid(RequestType)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RequestType THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND RequestType WHERE recid(RequestType) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RequestType THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RequestType).
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
        ufk[1]= 36 
        ufk[2]= 1646  
        ufk[3]= 1643  
        ufk[4]= 6911
        ufk[5]= (IF lcRight = "RW" AND iiQueue = 0 THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" AND iiQueue = 0 THEN 4 ELSE 0)  
        ufk[7] = (IF llShowHistory THEN 46 ELSE 1828) 
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
        CHOOSE ROW RequestType.ReqType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RequestType.ReqType WITH FRAME sel.
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
        FIND RequestType WHERE recid(RequestType) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RequestType THEN
              ASSIGN FIRSTrow = i Memory = recid(RequestType).
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
           IF NOT AVAILABLE RequestType THEN DO:
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
                rtab[1] = recid(RequestType)
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
           IF NOT AVAILABLE RequestType THEN DO:
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
              rtab[FRAME-DOWN] = recid(RequestType).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RequestType WHERE recid(RequestType) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RequestType THEN DO:
           Memory = recid(RequestType).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RequestType THEN Memory = recid(RequestType).
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
           FIND RequestType WHERE recid(RequestType) = Memory NO-LOCK.
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
           liReqType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liReqType > 0 THEN DO:
          FIND FIRST RequestType WHERE 
                     RequestType.Brand = lcBrand AND
                     RequestType.ReqType >= liReqType
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO:
        RUN local-find-this (FALSE).
       
        IF AVAILABLE RequestType THEN 
           RUN requestaction(RequestType.ReqType,
                             "",
                             "",
                             "").
         
        ufkey = TRUE.
        NEXT browse.
     END.

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        RUN local-find-this (FALSE).
       
        IF AVAILABLE RequestType THEN 
           RUN requestparam(RequestType.ReqType).
         
        ufkey = TRUE.
        NEXT browse.
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this (FALSE).
       
        IF AVAILABLE RequestType THEN 
           RUN requeststatus(RequestType.ReqType).
         
        ufkey = TRUE.
        NEXT browse.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       RequestType.ReqType RequestType.ReqName
       RequestType.InUse RequestType.Queue .

       RUN local-find-NEXT.
       IF AVAILABLE RequestType THEN Memory = recid(RequestType).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RequestType THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RequestType).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RequestType.ReqType RequestType.ReqName
       RequestType.InUse RequestType.Queue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRequestType).

           DELETE RequestType.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE RequestType THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */
     
     ELSE IF nap = "7" OR nap = "f7" THEN DO:
        llShowHistory = NOT llShowHistory.
        CLEAR FRAME sel ALL no-pause.
        RUN local-find-first.
        ASSIGN
           memory = recid(RequestType)
           ufkey = true
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequestType).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY RequestType.ReqType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequestType).

       RUN local-disp-row.
       xrecid = recid(RequestType).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RequestType) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RequestType) must-print = TRUE.
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
      FIND RequestType WHERE recid(RequestType) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RequestType WHERE recid(RequestType) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiQueue > 0 THEN DO:
       IF order = 1 THEN FIND FIRST RequestType 
          WHERE RequestType.Brand = lcBrand AND
                RequestType.Queue = iiQueue AND
                (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX Queue NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND FIRST RequestType 
          WHERE RequestType.Brand = lcBrand AND
                (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiQueue > 0 THEN DO:
       IF order = 1 THEN FIND LAST RequestType
          WHERE RequestType.Brand = lcBrand AND
                RequestType.Queue = iiQueue AND
                (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX Queue NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND LAST RequestType 
          WHERE RequestType.Brand = lcBrand AND
               (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiQueue > 0 THEN DO:
       IF order = 1 THEN FIND NEXT RequestType
          WHERE RequestType.Brand = lcBrand AND
                RequestType.Queue = iiQueue AND
                (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX Queue NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND NEXT RequestType 
          WHERE RequestType.Brand = lcBrand AND
               (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiQueue > 0 THEN DO:
       IF order = 1 THEN FIND PREV RequestType
          WHERE RequestType.Brand = lcBrand AND
                RequestType.Queue = iiQueue AND
               (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX Queue NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND PREV RequestType 
          WHERE RequestType.Brand = lcBrand AND
               (IF llShowHistory THEN TRUE ELSE RequestType.InUse EQ TRUE)
          USE-INDEX ReqType NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RequestType.ReqType 
       RequestType.ReqName
       RequestType.InUse
       RequestType.Queue
       RequestType.LogOn
       RequestType.Program
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

       FIND RequestQueue No-LOCK WHERE
            RequestQueue.Queue = RequestType.Queue NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         RequestType.Brand          
         RequestType.ReqType        
         RequestType.ReqName
         RequestType.Queue RequestQueue.QName WHEN AVAILABLE RequestQueue
         RequestType.InUse           
         RequestType.Program        
         RequestType.UserCode        
         RequestType.LogOn          
         RequestType.LogFile        
         RequestType.LogEntry       
         RequestType.LogClear       
         RequestType.LogThreshold
         RequestType.Mode
       WITH FRAME lis.

      
      IF NOT NEW RequestType THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      FIND CURRENT RequestType EXCLUSIVE-LOCK.

      UPDATE
         RequestType.ReqName        
         RequestType.Queue 
         RequestType.InUse           
         RequestType.Program        
         RequestType.UserCode        
         RequestType.LogOn          
         RequestType.LogFile        
         RequestType.LogEntry       
         RequestType.LogClear       
         RequestType.LogThreshold   
         RequestType.Mode
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            IF FRAME-FIELD = "Queue" THEN DO:
               FIND RequestQueue No-LOCK WHERE
                    RequestQueue.Queue = INPUT RequestType.Queue NO-ERROR.
               IF AVAILABLE RequestQueue THEN
                  DISPLAY   RequestQueue.QName.
               ELSE
                  DISPLAY "N/A" @ RequestQueue.QName.
            END.
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.
 
              
      IF RequestType.LogOn AND 
         (RequestType.LogFile = "" OR RequestType.LogEntry = "") THEN DO:
         MESSAGE "Logging definitions are incomplete."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      LEAVE.
   
   END.
   
END PROCEDURE.

