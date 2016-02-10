/* ----------------------------------------------------------------------
  MODULE .......: RequestActionRule
  TASK .........: UPDATEs table RequestActionRule
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.03.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RequestActionRule

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RequestActionRule'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRequestActionRule AS HANDLE NO-UNDO.
   lhRequestActionRule = BUFFER RequestActionRule:HANDLE.
   RUN StarEventInitialize(lhRequestActionRule).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhRequestActionRule).
   END.

END.

DEF INPUT PARAMETER iiRequestActionID AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcParamField AS CHAR                   NO-UNDO.
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
DEF VAR llActive     AS LOG                    NO-UNDO INIT TRUE. 

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcFields     AS CHAR NO-UNDO.
DEF VAR lcParamDesc  AS CHAR NO-UNDO.

DEF VAR lhRequest    AS HANDLE NO-UNDO.
DEF VAR lhField      AS HANDLE NO-UNDO.

FORM
    RequestActionRule.ParamField
    lcParamDesc FORMAT "X(20)" COLUMN-LABEL "Description"
    RequestActionRule.ParamValue     FORMAT "X(20)" 
    RequestActionRule.ExclParamValue FORMAT "X(10)"
    RequestActionRule.ToDate 
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  ACTION RULES FOR " + STRING(iiRequestActionID) + " (Active) "
    FRAME sel.

FORM
    RequestActionRule.RequestActionID COLON 25
    RequestActionRule.ParamField      COLON 25 
    lcParamDesc NO-LABEL FORMAT "X(35)" SKIP
    RequestActionRule.ParamValue      COLON 25 FORMAT "x(320)" VIEW-AS FILL-IN SIZE 45 BY 1 
    RequestActionRule.ExclParamValue  COLON 25 FORMAT "x(256)" VIEW-AS FILL-IN SIZE 45 BY 1
    RequestActionRule.FromDate        COLON 25
    RequestActionRule.ToDate          COLON 25    
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Parameter:" lcParamField FORMAT "X(15)"
    HELP "Enter parameter"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Parameter"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fParamDescription RETURNS LOGIC
   (icParamField AS CHAR):

   lcParamDesc = "".

   IF icParamField begins "#" THEN lcParamDesc = "Special rule".
   ELSE 
   FOR FIRST RequestParam NO-LOCK WHERE
             RequestParam.Brand   = gcBrand AND
             RequestParam.ReqType = RequestAction.ReqType AND
             RequestParam.ParamField = icParamField:
      lcParamDesc = RequestParam.Usage.      
   END.
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST RequestAction WHERE 
           RequestAction.RequestActionID = iiRequestActionID NO-LOCK NO-ERROR.
IF NOT AVAILABLE RequestAction THEN DO:
   MESSAGE "Unknown action" 
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FOR EACH RequestParam NO-LOCK WHERE
         RequestParam.Brand   = gcBrand AND
         RequestParam.ReqType = RequestAction.ReqType AND
         RequestParam.Usage > "":
   lcFields = lcFields + (IF lcFields > "" THEN "," ELSE "") + 
              RequestParam.ParamField.
END.

RUN local-Find-First.

IF AVAILABLE RequestActionRule THEN ASSIGN
   Memory       = recid(RequestActionRule)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rules available" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a RequestActionRule  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY iiRequestActionID @ RequestActionRule.RequestActionID.

           CREATE RequestActionRule.
           ASSIGN 
              RequestActionRule.RequestActionID = iiRequestActionID
              RequestActionRule.FromDate        = TODAY.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              RequestActionRule.ParamField = "" 
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRequestActionRule).

           ASSIGN
           Memory = recid(RequestActionRule)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RequestActionRule THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND RequestActionRule WHERE recid(RequestActionRule) = Memory 
        NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RequestActionRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RequestActionRule).
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
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RequestActionRule.ParamField ;(uchoose.i;) NO-ERROR 
          WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RequestActionRule.ParamField WITH FRAME sel.
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
        FIND RequestActionRule WHERE recid(RequestActionRule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RequestActionRule THEN
              ASSIGN FIRSTrow = i Memory = recid(RequestActionRule).
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
           IF NOT AVAILABLE RequestActionRule THEN DO:
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
                rtab[1] = recid(RequestActionRule)
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
           IF NOT AVAILABLE RequestActionRule THEN DO:
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
              rtab[FRAME-DOWN] = recid(RequestActionRule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RequestActionRule WHERE recid(RequestActionRule) = Memory 
        NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RequestActionRule THEN DO:
           Memory = recid(RequestActionRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RequestActionRule THEN 
                 Memory = recid(RequestActionRule).
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
           FIND RequestActionRule WHERE recid(RequestActionRule) = Memory 
              NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET lcParamField WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcParamField > "" THEN DO:
          FIND FIRST RequestActionRule WHERE 
                     RequestActionRule.RequestActionID = iiRequestActionID AND
                     RequestActionRule.ParamField >= lcParamField
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE RequestActionRule THEN NEXT BROWSE.

          ASSIGN 
             order      = 1 
             memory     = RECID(RequestActionRule) 
             must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* display filter */
        llActive = NOT llActive.
        RUN local-find-first.
        ASSIGN 
           Memory = recid(RequestActionRule) 
           must-print = TRUE
           ufkey = TRUE.
        IF llActive THEN    
           FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"All","Active").
        ELSE FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"Active","All").
   
        NEXT LOOP.
     END.

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
       RequestActionRule.ParamField
       RequestActionRule.ParamValue
       RequestActionRule.ExclParamValue.

       RUN local-find-NEXT.
       IF AVAILABLE RequestActionRule THEN Memory = recid(RequestActionRule).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RequestActionRule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RequestActionRule).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RequestActionRule.ParamField
       RequestActionRule.ParamValue
       RequestActionRule.ExclParamValue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRequestActionRule).

           DELETE RequestActionRule.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE RequestActionRule THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequestActionRule).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequestActionRule).

       RUN local-disp-row.
       xrecid = recid(RequestActionRule).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RequestActionRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RequestActionRule) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND RequestActionRule WHERE 
         recid(RequestActionRule) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND RequestActionRule WHERE 
          recid(RequestActionRule) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiRequestActionID ne ? THEN DO:
       IF order = 1 THEN DO:
          IF llActive THEN
             FIND FIRST RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID AND
                  RequestActionRule.ToDate >= TODAY NO-LOCK NO-ERROR.
          ELSE
             FIND FIRST RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID
                  NO-LOCK NO-ERROR.
       END. /* IF order = 1 THEN DO: */
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiRequestActionID ne ? THEN DO:
       IF order = 1 THEN DO:
          IF llActive THEN
             FIND LAST RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID AND
                  RequestActionRule.ToDate >= TODAY NO-LOCK NO-ERROR.
          ELSE
             FIND LAST RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID
                  NO-LOCK NO-ERROR.
       END. /* IF order = 1 THEN DO: */
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiRequestActionID ne ? THEN DO:
       IF order = 1 THEN DO:
          IF llActive THEN
             FIND NEXT RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID AND
                  RequestActionRule.ToDate >= TODAY NO-LOCK NO-ERROR.
          ELSE
             FIND NEXT RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID
                  NO-LOCK NO-ERROR.
       END. /* IF order = 1 THEN DO: */
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiRequestActionID ne ? THEN DO:
       IF order = 1 THEN DO:
          IF llActive THEN
             FIND PREV RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID AND
                  RequestActionRule.ToDate >= TODAY NO-LOCK NO-ERROR.
          ELSE
             FIND PREV RequestActionRule WHERE 
                  RequestActionRule.RequestActionID = iiRequestActionID
                  NO-LOCK NO-ERROR.
       END. /* IF order = 1 THEN DO: */
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RequestActionRule.ParamField
       lcParamDesc
       RequestActionRule.ParamValue
       RequestActionRule.ExclParamValue
       RequestActionRule.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
   fParamDescription(RequestActionRule.ParamField).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bRule FOR RequestActionRule.
    
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         RequestActionRule.RequestActionID        
         RequestActionRule.ParamField
         lcParamDesc
         RequestActionRule.ParamValue        
         RequestActionRule.ExclParamValue
         RequestActionRule.FromDate
         RequestActionRule.ToDate
       WITH FRAME lis.

      
      IF NOT NEW RequestActionRule THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      FIND CURRENT RequestActionRule EXCLUSIVE-LOCK.
      
      ehto = 9.
      RUN Syst/ufkey.
      
      UPDATE
         RequestActionRule.ParamField WHEN NEW RequestActionRule
         RequestActionRule.ParamValue  
         RequestActionRule.ExclParamValue
         RequestActionRule.FromDate   
         RequestActionRule.ToDate      
      WITH FRAME lis EDITING:
 
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "ParamField" THEN DO:

            ASSIGN
               gcHelpParam = "RequestActionRule"
               si-recid    = ?.
            RUN Syst/requestparam(RequestAction.ReqType).
            gcHelpParam = "".
            
            IF si-recid NE ? THEN DO:
            
               FIND RequestParam WHERE RECID(RequestParam) = si-recid
                  NO-LOCK NO-ERROR.
               IF AVAILABLE RequestParam THEN 
                  DISPLAY RequestParam.ParamField @ 
                          RequestActionRule.ParamField WITH FRAME lis.
            END.           

            ehto = 9.
            RUN Syst/ufkey.
            NEXT. 
         END.

         ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:

            PAUSE 0.
            
            IF FRAME-FIELD = "ParamField" THEN DO:
               IF NOT INPUT RequestActionRule.ParamField BEGINS "#" AND
                  LOOKUP(INPUT RequestActionRule.ParamField,lcFields) = 0
               THEN DO:
                  MESSAGE "Unknown parameter field"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               fParamDescription(INPUT INPUT RequestActionRule.ParamField).
               DISPLAY lcParamDesc WITH FRAME lis.
            END.
            
         END.
            
         APPLY LASTKEY.
      END.

      IF CAN-FIND(
         FIRST bRule WHERE
               bRule.RequestActionID = iiRequestActionID AND
               bRule.ParamField      = RequestActionRule.ParamField AND
               bRule.ParamValue      = RequestActionRule.ParamValue AND
               bRule.ToDate         >= RequestActionRule.FromDate   AND
               bRule.FromDate       <= RequestActionRule.ToDate     AND
               RECID(bRule) NE RECID(RequestActionRule))
      THEN DO:
         MESSAGE "A rule already exists for the same parameter and period"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      LEAVE.
   
   END.
   
END PROCEDURE.


