/* ----------------------------------------------------------------------
  MODULE .......: CTCTServEl.P
  TASK .........: Elements (Services) of a CLI Type
  APPLICATION ..: TMS
  AUTHOR .......: aam (from CTServEl1)
  CREATED ......: 07.12.04
  CHANGED ......: 23.03.05/aam DefParam
                  27.03.07/aam FromDate must be aligned with CTServPac.Dates,
                               input Dates
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CTServEl'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCTServEl AS HANDLE NO-UNDO.
   lhCTServEl = BUFFER CTServEl:HANDLE.
   RUN StarEventInitialize(lhCTServEl).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCTServEl).
   END.

END.


DEF INPUT PARAMETER icCLIType   AS CHAR NO-UNDO.
DEF INPUT PARAMETER icServPac   AS CHAR NO-UNDO.
DEF INPUT PARAMETER idtFromDate AS DATE NO-UNDO.
DEF INPUT PARAMETER idtToDate   AS DATE NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcServCom    LIKE CTServEl.ServCom  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
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
DEF VAR lcTarget     AS CHAR                   NO-UNDO FORMAT "x(20)".
DEF VAR lcActType    AS CHAR                   NO-UNDO FORMAT "x(20)".
DEF VAR llServType   AS LOG                    NO-UNDO. 

form
    CTServEl.ServCom      /* COLUMN-LABEL FORMAT */
    CTServEl.FromDate   
    CTServEl.DefValue   column-label "V"
    llServType          column-label "S.Type" FORMAT "Basic/Addit"
    ServCom.ScName      FORMAT "X(40)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    icCLIType + ": Components of '" + icServPac + "' "
    FRAME sel.

form
    CTServEl.CLIType  COLON 20
       CLIType.CLIName NO-LABEL SKIP
    CTServEl.ServPac  COLON 20
       ServPac.SPName NO-LABEL SKIP
    CTServEl.ServCom  COLON 20 SKIP
       ServCom.ScName AT 22 NO-LABEL FORMAT "X(45)" SKIP
    ServCom.ScLocalName AT 22 NO-LABEL FORMAT "X(45)" SKIP
    ServCom.ActType  COLON 20 
        lcActType NO-LABEL SKIP
    ServCom.Target   COLON 20 
        lcTarget NO-LABEL SKIP
    CTServEl.FromDate COLON 20   
    CTServEl.DefValue COLON 20
    CTServEl.DefParam COLON 20 FORMAT "X(50)"
    llServType           COLON 20 
       LABEL "Service Type"
       HELP "Type of service; basic / additional"
       FORMAT "Basic/Additional" SKIP
    CTServEl.ChgAllowed COLON 20
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form 
    lcServCom
    HELP "Enter Code of Service"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-find-first.
IF AVAILABLE CTServEl THEN ASSIGN
   Memory       = recid(CTServEl)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a CTServEl  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
       
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
        
           DISPLAY icCLIType @ CTServEl.CLIType
                   icServPac @ CTServEl.ServPac WITH FRAME lis.
                   
           PROMPT-FOR CTServEl.ServCom CTServEl.FromDate 
           WITH FRAME lis EDITING:
           
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 
                 PAUSE 0.

                 if frame-field = "ServCom" THEN DO:
                 
                    if input CTServEl.ServCom = "" THEN LEAVE add-row.
                    
                    FIND ServCom where 
                         ServCom.Brand   = gcBrand AND 
                         ServCom.ServCom = INPUT CTServEl.ServCom
                    no-lock no-error.
                    IF NOT AVAIL ServCom THEN DO:
                       bell. message "Unknown Service Component !".
                       NEXT.
                    END.

                    /* must be part of the general package */
                    IF NOT CAN-FIND(FIRST ServEl WHERE
                             ServEl.Brand   = gcBrand   AND
                             ServEl.ServPac = icServPac AND
                             ServEl.ServCom = INPUT CTServEl.ServCom)
                    THEN DO:
                       BELL.
                       MESSAGE "This component does not belong to package"
                               icServPac.
                       NEXT.
                    END.
                    
                    DISP ServCom.ScName ServCom.ScLocalName.
                 END.
                    
                 ELSE IF FRAME-FIELD = "FromDate" THEN DO:
                    IF INPUT CTServEl.FromDate = ? THEN DO:
                       BELL.
                       MESSAGE "Date is mandatory".
                       NEXT.
                    END.

                    IF CAN-FIND(CTServEl WHERE
                         CTServEl.Brand    = gcBrand                AND 
                         CTServEl.CLIType  = icCLIType              AND
                         CTServEl.ServPac  = icServPac              AND
                         CTServEl.ServCom  = INPUT CTServEl.ServCom AND
                         CTServEl.FromDate = INPUT CTServEl.FromDate)
                    THEN DO:
                       BELL. MESSAGE
                       "Service '" + INPUT FRAME lis CTServEl.ServCom +
                       "' already exists with date " + 
                       STRING(INPUT CTServEl.FromDate,"99-99-99").
                       NEXT.
                    END.
                 END.
              END.         
              APPLY LASTKEY.
           END.      

           IF INPUT FRAME lis CTServEl.FromDate < idtFromDate OR
              INPUT FRAME lis CTServEl.FromDate > idtToDate
           THEN DO:
              MESSAGE "Beginning date must be between" 
                     idtFromDate "-" idtToDate
              VIEW-AS ALERT-BOX INFORMATION.
              NEXT.
           END. 

           CREATE CTServEl.
           ASSIGN
           CTServEl.Brand    = gcBrand
           CTServEl.CTServEl = NEXT-VALUE(CTServEl)
           CTServEl.CLIType  = icCLIType
           CTServEl.ServPac  = icServPac  
           CTServEl.ServCom  = INPUT FRAME lis CTServEl.ServCom
           CTServEl.FromDate = INPUT FRAME lis CTServEl.FromDate.

           IF AVAILABLE ServCom THEN ASSIGN 
               CTServEl.ServType   = ServCom.ServType
               CTServEl.ChgAllowed = ServCom.SCChgable. 
               
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCTServEl).

           ASSIGN
           Memory = recid(CTServEl)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE CTServEl THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CTServEl WHERE recid(CTServEl) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CTServEl THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CTServEl).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 2350.
        
        /* find, new and delete keys only for Syst group */
        FIND FIRST tmsuser NO-LOCK WHERE
                   tmsuser.usercode = katun NO-ERROR.
        
        IF AVAIL tmsuser AND tmsuser.usergroup = "Syst" THEN 
           ASSIGN 
              ufk[1]= 35
              ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
              ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0).
        
        ASSIGN      
           ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
           ehto = 3 ufkey = FALSE.
           
        {Syst/uright1.i '"5,6"'}
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CTServEl.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CTServEl.ServCom WITH FRAME sel.
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
        FIND CTServEl WHERE recid(CTServEl) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CTServEl THEN
              ASSIGN FIRSTrow = i Memory = recid(CTServEl).
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
           IF NOT AVAILABLE CTServEl THEN DO:
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
                rtab[1] = recid(CTServEl)
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
           IF NOT AVAILABLE CTServEl THEN DO:
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
              rtab[FRAME-DOWN] = recid(CTServEl).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CTServEl WHERE recid(CTServEl) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CTServEl THEN DO:
           Memory = recid(CTServEl).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CTServEl THEN Memory = recid(CTServEl).
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
           FIND CTServEl WHERE recid(CTServEl) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcServCom > "" THEN DO:
          FIND FIRST CTServEl WHERE 
                     CTServEl.Brand    = gcBrand   AND
                     CTServEl.CLIType  = icCLIType AND
                     CTServEl.ServPac  = icServPac AND 
                     CTServEl.ServCom >= lcServCom  
                     NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CTServEl THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CTServEl/ServCom was found */
          ASSIGN order = 1 Memory = recid(CTServEl) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* attributes */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        
        RUN local-find-this(FALSE).


        FIND ServCom WHERE
             ServCom.Brand   = gcBrand AND
             ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.
        IF AVAILABLE ServCom AND ServCom.ServAttr = TRUE THEN 
        RUN Mm/ctservattr(CTServEl.CTServEl).

        ELSE MESSAGE 
             "Service component does not have any attributes"
             VIEW-AS ALERT-BOX TITLE "SERVICE ATTRIBUTES".
        
        ufkey = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CTServEl.ServCom. 

       RUN local-find-NEXT.
       IF AVAILABLE CTServEl THEN Memory = recid(CTServEl).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CTServEl THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CTServEl).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CTServEl.ServCom.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCTServEl).
           
           DELETE CTServEl.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CTServEl
           WHERE CTServEl.ServPac = ServPac AND CTServEl.Brand = gcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this((lcRight = "RW")).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CTServEl.ServCom.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCTServEl).
       
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCTServEl).

       RUN local-disp-row.
       xrecid = recid(CTServEl).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CTServEl) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CTServEl) must-print = TRUE.
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
      FIND CTServEl WHERE recid(CTServEl) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CTServEl WHERE recid(CTServEl) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CTServEl WHERE 
          CTServEl.Brand   = gcBrand   AND
          CTServEl.CLIType = icCLIType AND
          CTServEl.ServPac = icServPac NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CTServEl WHERE 
          CTServEl.Brand   = gcBrand   AND
          CTServEl.CLIType = icCLIType AND
          CTServEl.ServPac = icServPac NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CTServEl WHERE 
          CTServEl.Brand   = gcBrand   AND
          CTServEl.CLIType = icCLIType AND
          CTServEl.ServPac = icServPac NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CTServEl WHERE 
          CTServEl.Brand   = gcBrand   AND
          CTServEl.CLIType = icCLIType AND
          CTServEl.ServPac = icServPac NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CTServEl.ServCom
       CTServEl.DefValue
       CTServEl.FromDate
       llServType
       ServCom.ScName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

       FIND ServCom where
            ServCom.Brand   = gcBrand  AND 
            ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.

       llServType = (CTServEl.ServType = 0). 
            
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      FIND CLIType WHERE
           CLIType.Brand   = gcBrand AND
           CLIType.CLIType = icCLIType NO-LOCK NO-ERROR.
      FIND ServPac WHERE 
           ServPac.Brand   = gcBrand AND 
           ServPac.ServPac = icServPac NO-LOCK NO-ERROR.

      IF AVAILABLE ServCom THEN DO:

         lcActType = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                      "ServCom",
                                      "ActType",
                                      STRING(ServCom.ActType)).

         lcTarget = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                     "ServCom",
                                     "Target",
                                     STRING(ServCom.Target)).
         
         DISPLAY ServCom.ScName ServCom.ScLocalName 
                 ServCom.ActType lcActType
                 ServCom.Target  lcTarget
         WITH FRAME lis.
      END.
       
      DISP 
        CTServEl.CLIType
        CLIType.CLIName WHEN AVAILABLE CLIType
        CTServEl.ServPac
        ServPac.SPName WHEN AVAILABLE ServPac
        CTServEl.ServCom
        CTServEl.FromDate
        CTServEl.DefValue
        CTServEl.DefParam
        llServType
        CTServEl.ChgAllowed
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN DO:
         ehto = 9.
         RUN Syst/ufkey.
         
         UPDATE
         CTServEl.DefValue
         CTServEl.DefParam
         llServType
         CTServEl.ChgAllowed
         WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "DefValue" THEN DO:
                   IF INPUT CTServEl.DefValue < ServCom.scvaluerange[1] OR
                      INPUT CTServEl.DefValue > ServCom.scvaluerange[2] THEN DO:
                      BELL.
                      message "Value MUST be within range" 
                              ServCom.scvaluerange[1]
                              "-" ServCom.scvaluerange[2] "!".
                      NEXT.
                   END.
                END.   
             END.
             APPLY LASTKEY.
         END. /* EDITING */
                 
         CTServEl.ServType = INTEGER(NOT llServType).
      
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
 
      LEAVE.
  END.
      
END PROCEDURE.


