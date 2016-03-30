/* ----------------------------------------------------------------------
  MODULE .......: RequestParam
  TASK .........: UPDATEs table RequestParam
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 29.11.07
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RequestParam

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RequestParam'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRequestParam AS HANDLE NO-UNDO.
   lhRequestParam = BUFFER RequestParam:HANDLE.
   RUN StarEventInitialize(lhRequestParam).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhRequestParam).
   END.

END.

DEF INPUT PARAMETER iiReqType AS INT  NO-UNDO.

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

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcFields     AS CHAR NO-UNDO.

DEF VAR lhRequest    AS HANDLE NO-UNDO.
DEF VAR lhField      AS HANDLE NO-UNDO.

FORM
    RequestParam.ParamField
    RequestParam.Usage
    RequestParam.DispParam COLUMN-LABEL "View"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  PARAMETERS OF TYPE " + STRING(iiReqType) + " "
    FRAME sel.

{Func/brand.i}

FORM
    RequestParam.Brand          COLON 20
    RequestParam.ReqType        COLON 20
       RequestType.ReqName NO-LABEL SKIP
    RequestParam.ParamField     COLON 20 
    RequestParam.Usage          COLON 20
    RequestParam.Description    COLON 20
    RequestParam.CharConfig     COLON 20
       LABEL "Procedure"
       FORMAT "X(50)"
       HELP "Procedure that is used for getting description for parameter"
    RequestParam.DispParam      COLON 20    
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Param:" lcParamField FORMAT "X(15)"
    HELP "Enter status"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Parameter"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

/* get valid fields */
FOR FIRST mobile._file NO-LOCK WHERE
          mobile._file._file-name = "msrequest",
     EACH mobile._field OF mobile._file NO-LOCK:
   lcFields = lcFields + 
              (IF lcFields > "" THEN "," ELSE "") + 
              mobile._field._field-name.
END.

/* make sure that all parameter fields are available */
DO i = 1 TO NUM-ENTRIES(lcFields) TRANS:
   
   IF CAN-FIND(RequestParam WHERE
               RequestParam.Brand      = gcBrand   AND
               RequestParam.ReqType    = iiReqType AND
               RequestParam.ParamField = ENTRY(i,lcFields))
   THEN NEXT.
      
   CREATE RequestParam.
   ASSIGN RequestParam.Brand      = gcBrand 
          RequestParam.ReqType    = iiReqType
          RequestParam.ParamField = ENTRY(i,lcFields).
END.



RUN local-Find-First.

IF AVAILABLE RequestParam THEN ASSIGN
   Memory       = recid(RequestParam)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No parameters available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a RequestParam  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY gcBrand @ RequestParam.Brand
                   iiReqType @ RequestParam.ReqType.

           PROMPT-FOR RequestParam.ParamField WITH FRAME lis.
            
           IF INPUT RequestParam.ParamField = "" THEN UNDO, LEAVE ADD-ROW.

           IF LOOKUP(INPUT RequestParam.ParamField,lcFields) = 0 THEN DO:
               MESSAGE "Unknown parameter field"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.
           
           IF CAN-FIND(FIRST RequestParam WHERE
                             RequestParam.Brand   = gcBrand AND
                             RequestParam.ReqType = iiReqType 
                             USING FRAME lis RequestParam.ParamField)
           THEN DO:
              MESSAGE "Parameter has already been defined for this type"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.

           CREATE RequestParam.
           ASSIGN 
              RequestParam.Brand    = gcBrand
              RequestParam.ReqType  = iiReqType
              RequestParam.ParamField = INPUT FRAME lis 
                                        RequestParam.ParamField.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRequestParam).

           ASSIGN
           Memory = recid(RequestParam)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE RequestParam THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND RequestParam WHERE recid(RequestParam) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RequestParam THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RequestParam).
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
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RequestParam.ParamField {Syst/uchoose.i} NO-ERROR 
          WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RequestParam.ParamField WITH FRAME sel.
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
        FIND RequestParam WHERE recid(RequestParam) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RequestParam THEN
              ASSIGN FIRSTrow = i Memory = recid(RequestParam).
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
           IF NOT AVAILABLE RequestParam THEN DO:
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
                rtab[1] = recid(RequestParam)
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
           IF NOT AVAILABLE RequestParam THEN DO:
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
              rtab[FRAME-DOWN] = recid(RequestParam).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RequestParam WHERE recid(RequestParam) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RequestParam THEN DO:
           Memory = recid(RequestParam).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RequestParam THEN Memory = recid(RequestParam).
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
           FIND RequestParam WHERE recid(RequestParam) = Memory NO-LOCK.
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
          FIND FIRST RequestParam WHERE 
                     RequestParam.Brand   = gcBrand AND
                     RequestParam.ReqType = iiReqType AND
                     RequestParam.ParamField >= lcParamField
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
       RequestParam.ParamField
       RequestParam.Usage.

       RUN local-find-NEXT.
       IF AVAILABLE RequestParam THEN Memory = recid(RequestParam).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RequestParam THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RequestParam).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RequestParam.ParamField
       RequestParam.Usage.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRequestParam).

           DELETE RequestParam.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE RequestParam THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequestParam).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequestParam).

       RUN local-disp-row.
       xrecid = recid(RequestParam).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RequestParam) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RequestParam) must-print = TRUE.
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
      FIND RequestParam WHERE recid(RequestParam) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RequestParam WHERE recid(RequestParam) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND FIRST RequestParam 
          WHERE RequestParam.Brand = gcBrand AND
                RequestParam.ReqType = iiReqType
          NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND LAST RequestParam
          WHERE RequestParam.Brand = gcBrand AND
                RequestParam.ReqType = iiReqType
          NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiReqType ne ? THEN DO:
       IF order = 1 THEN FIND NEXT RequestParam
          WHERE RequestParam.Brand = gcBrand AND
                RequestParam.ReqType = iiReqType
          NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiReqType ne ?  THEN DO:
       IF order = 1 THEN FIND PREV RequestParam
          WHERE RequestParam.Brand = gcBrand AND
                RequestParam.ReqType = iiReqType
          NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RequestParam.ParamField
       RequestParam.Usage
       RequestParam.DispParam
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND RequestType WHERE 
           RequestType.Brand   = gcBrand AND
           RequestType.ReqType = RequestParam.ReqType NO-LOCK NO-ERROR.
      
      DISP 
         RequestParam.Brand          
         RequestParam.ReqType        
         RequestType.ReqName WHEN AVAILABLE RequestType
         RequestParam.ParamField
         RequestParam.Usage        
         RequestParam.Description
         RequestParam.CharConfig
         RequestParam.DispParam
       WITH FRAME lis.

      
      IF NOT NEW RequestParam THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      FIND CURRENT RequestParam EXCLUSIVE-LOCK.
      
      UPDATE
         RequestParam.Usage  
         RequestParam.Description
         RequestParam.CharConfig
         RequestParam.DispParam      
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


