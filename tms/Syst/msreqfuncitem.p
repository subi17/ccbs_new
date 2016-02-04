/* -----------------------------------------------
  MODULE .......: msreqfuncitem.p
  FUNCTION .....: Request queue browser 
  APPLICATION ..: TMS
  AUTHOR .......: JT 
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}

DEFINE INPUT PARAMETER icFuncGroup AS CHAR  NO-UNDO.

DEFINE OUTPUT PARAMETER ocReturn   AS CHAR NO-UNDO.

DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liCodeValue  AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR icCoName2    AS CHAR                   NO-UNDO.
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
DEF VAR llQueRights  AS LOG                    NO-UNDO. /* User rights OK ?  */
DEF VAR lcTemp       AS CHAR                   NO-UNDO.
DEF VAR liBarr       AS INT                    NO-UNDO. 
DEF VAR icTitle      AS CHAR                   NO-UNDO.
DEF VAR lcRowText    AS CHAR                   NO-UNDO.
DEF VAR lcSpace      AS CHAR                   NO-UNDO.
DEF VAR lcSelected   AS CHAR                   NO-UNDO.
DEF VAR icFrameTitle AS CHAR                   NO-UNDO.
DEF VAR llAllow      AS LOGICAL                NO-UNDO.
DEF VAR liLoop       AS INT                    NO-UNDO.
DEF VAR ok           AS LOGICAL                NO-UNDO.

icTitle = "Request status function items".

DEFINE TEMP-TABLE ttSelected
  FIELD ItemId AS CHAR.

icFuncGroup = SUBSTRING(icFuncGroup,3).

/* Create selections from input */
DO liLoop = 1 TO NUM-ENTRIES(icFuncGroup):
   CREATE ttSelected.
   ASSIGN ttSelected.ItemId = ENTRY(liLoop,icFuncGroup).
END.

/* Define browsing form */
FORM
    
    MsReqFuncItem.ItemId   FORMAT "x(3)"  NO-LABEL 
    MsReqFuncItem.ItemDesc FORMAT "x(41)" NO-LABEL 
    MsReqFuncItem.Module   FORMAT "x(13)" NO-LABEL
    MsReqFuncItem.CParam   FORMAT "x(12)" NO-LABEL
    MsReqFuncItem.IParam   FORMAT "zz9"   NO-LABEL
    lcSelected             FORMAT "x(1)"  NO-LABEL
   
WITH ROW FrmRow OVERLAY 15 DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + icTitle + " " 
    CENTERED
    FRAME sel.

FORM "Item identifer..:" MsReqFuncItem.ItemId   SKIP
     "Description.....:" MsReqFuncItem.ItemDesc SKIP
     "Module of action:" MsReqFuncItem.Module  FORMAT "X(20)" SKIP
     "Char. parameter.:" MsReqFuncItem.Cparam   SKIP
     "Int. parameter..:" MsReqFuncItem.IParam   SKIP

WITH  OVERLAY ROW 4 centered
 COLOR VALUE(cfc)
 TITLE COLOR VALUE(ctc) " Add/Update "
 NO-LABELS 
 FRAME upd.

IF icTitle = "" THEN icTitle = "Requests".

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

/* column-labels for parameters */


DEFINE VARIABLE lcRight AS CHAR INIT "R".

RUN local-find-first.
llQueRights = FALSE.
       
IF AVAILABLE MsReqFuncItem THEN ASSIGN
   Memory       = RECID(MsReqFuncItem)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

LOOP:
REPEAT WITH FRAME sel:
                           
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
   
   IF must-add THEN DO:  /* Add a record  */
      ASSIGN cfc = "upd" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.   
   ADD-ROW:
      REPEAT WITH FRAME upd ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME upd:
           
           CLEAR FRAME upd NO-PAUSE.
           PROMPT-FOR MsReqFuncItem.ItemId.
           IF INPUT FRAME upd MsReqFuncItem.ItemId = "" THEN
           LEAVE add-row.
           
            CREATE MsReqFuncItem.
           ASSIGN
           MsReqFuncItem.ItemId = INPUT FRAME upd MsReqFuncItem.ItemId.

           RUN pUpdateRecord.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.
           
           ASSIGN  Memory = RECID(MsReqFuncItem)
                   xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME upd NO-PAUSE.
      ASSIGN must-print = TRUE
             must-add   = FALSE.

      /* is there ANY record ? */
      FIND FIRST MsReqFuncItem 
      NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE MsReqFuncItem THEN LEAVE LOOP.
      NEXT LOOP.
   END. 
   PrintPage:
   DO :
  
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MsReqFuncItem WHERE RECID(MsReqFuncItem) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MsReqFuncItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = RECID(MsReqFuncItem).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        UP FRAME-LINE - 1.
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
           ufk[1] = 0 
           ufk[2] = 0
           ufk[5] = 5 
           ufk[6] = 7
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.
      
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      
      IF order = 1 THEN DO:
         CHOOSE ROW MsReqFuncItem.ItemDesc ;(uchoose.i;) 
         NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) MsReqFuncItem.ItemDesc WITH FRAME sel.
      END.
      
      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"5,f5") > 0 THEN DO:
         must-add = TRUE.
         NEXT LOOP.
      END.
      
      
      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.
      
      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MsReqFuncItem THEN DO:
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
                rtab[1] = RECID(MsReqFuncItem)
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
           IF NOT AVAILABLE MsReqFuncItem THEN DO:
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
              rtab[FRAME-DOWN] = RECID(MsReqFuncItem).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.        
        END.
        ELSE DO:
           DOWN 1.
        END.
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MsReqFuncItem WHERE RECID(MsReqFuncItem) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MsReqFuncItem THEN DO:
           Memory = RECID(MsReqFuncItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MsReqFuncItem THEN Memory = RECID(MsReqFuncItem).
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
           FIND MsReqFuncItem WHERE RECID(MsReqFuncItem) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:
        RUN local-find-this(TRUE).
        RUN pUpdateRecord.
        RUN local-disp-ROW.
     END.
     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = RECID(MsReqFuncItem) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = RECID(MsReqFuncItem) must-print = TRUE.
        NEXT LOOP.
     END.
     IF LOOKUP(nap,"enter,return") > 0 THEN DO:
        
        RUN local-find-this(FALSE).
     
        FIND FIRST ttSelected WHERE
                   ttSelected.ItemId = MsReqFuncItem.ItemId
        EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAILABLE ttSelected THEN DO:
           DELETE ttSelected.
           RUN local-disp-ROW.
        END.
        ELSE DO:
           CREATE ttSelected.
           ASSIGN ttSelected.ItemId = MsReqFuncItem.ItemId.
           RUN local-disp-ROW.
        END.
        
     END.
     
     IF LOOKUP(nap,"delete") > 0 THEN DO TRANSACTION:
           
        delrow = FRAME-LINE.
        RUN local-find-this (FALSE).
        FOR EACH MsReqStatFunc NO-LOCK:
           IF INDEX(MsReqStatFunc.FuncGroup,MsReqFuncItem.ItemId) > 0 THEN DO:
              MESSAGE "Delete not allowed!" SKIP
                      "This item is attached to request type:"
                      MsReqStatFunc.ReqType
                      " and status:"
                      MsReqStatFunc.ReqStatus
              VIEW-AS ALERT-BOX.
              NEXT LOOP.
           END.
        END. 
        /* Highlight */
        COLOR DISPLAY VALUE(ctc)
        MsReqFuncItem.ItemId MsReqFuncItem.ItemDesc.
         
        RUN local-find-NEXT.
        IF AVAILABLE MsReqFuncItem THEN Memory = recid(MsReqFuncItem).
        
        ELSE DO:
           /* read back the record that is TO be  removed */
           RUN local-find-this (FALSE).
           RUN local-find-PREV.
        END.

        IF AVAILABLE MsReqFuncItem THEN DO:
            ASSIGN delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
                   Memory = recid(MsReqFuncItem).
         END.
        
         /* FIND back the ROW that is TO be removed */
         RUN local-find-this(TRUE).
         
         /* Check if any request has these functions, 
            if so delete not allowed */
         
         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
         COLOR DISPLAY VALUE(ccc) MsReqFuncItem.ItemId MsReqFuncItem.ItemDesc.
           
         IF ok THEN DO:
            FIND ttSelected EXCLUSIVE-LOCK WHERE
                 ttSelected.ItemId = MsReqFuncItem.ItemId
            NO-ERROR.
            IF AVAILABLE ttSelected THEN DELETE ttSelected.
            DELETE MsReqFuncItem.
              
            /* was LAST record DELETEd ? */
            IF NOT CAN-FIND(FIRST MsReqFuncItem
            /* srule */) THEN DO:
               CLEAR FRAME sel NO-PAUSE.
               PAUSE 0 NO-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
      ELSE delrow = 0. /* UNDO DELETE */
   END. /* DELETE */
     
   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      
      ocReturn = "".
      
      FOR EACH ttSelected NO-LOCK:
         ocReturn = ocReturn + ttSelected.ItemId + ",".
      END.
      /* 0 from the begining allows status to be seen even if there
           if no actual function */
      ocReturn = SUBSTRING(ocReturn,1,LENGTH(ocReturn) - 1).
      ocReturn = "0," + ocReturn.
      HIDE FRAME sel NO-PAUSE.
      RETURN. 
   END.
  
END.  /* BROWSE */

END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MsReqFuncItem WHERE RECID(MsReqFuncItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    
    ELSE FIND MsReqFuncItem WHERE 
        RECID(MsReqFuncItem) = rtab[frame-line(sel)] 
    NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF lcRight = "R" THEN DO:
      FIND FIRST MsReqFuncItem NO-LOCK NO-ERROR.
   END.   

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF lcRight = "R" THEN DO:
      FIND LAST MsReqFuncItem NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF lcRight = "R" THEN DO:
     FIND NEXT MsReqFuncItem NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF lcRight = "R" THEN DO:
      FIND PREV MsReqFuncItem NO-LOCK NO-ERROR.
   END.                  

END PROCEDURE.

PROCEDURE local-disp-row:

    lcSelected = "".

    FOR EACH ttSelected NO-LOCK:
       IF LOOKUP(MsReqFuncItem.ItemId,ttSelected.ItemId) > 0 THEN 
       lcSelected = "*".
    END.
    
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       
       MsReqFuncItem.ItemId   LABEL " Id"
       MsReqFuncItem.ItemDesc LABEL "Description"
       MsReqFuncItem.Module   LABEL "Module/Action"
       MsReqFuncItem.CParam   LABEL "Char param"
       MsReqFuncItem.IParam   LABEL "Ip"
       lcSelected             LABEL "S" 
    WITH FRAME sel.
END PROCEDURE.

PROCEDURE pUpdateRecord.
    
   PAUSE 0. 
    
   REPEAT ON ENDKEY UNDO, LEAVE:
      DISP
       MsReqFuncItem.ItemId
       MsReqFuncItem.ItemDesc
       MsReqFuncItem.Module 
       MsReqFuncItem.CParam
       MsReqFuncItem.IParam
      WITH FRAME upd.
       

      UPDATE
       MsReqFuncItem.ItemDesc FORMAT "x(50)"
       MsReqFuncItem.Module
       MsReqFuncItem.CParam FORMAT "x(30)"
       MsReqFuncItem.IParam
      WITH FRAME upd.
      LEAVE.
   END.
END.

