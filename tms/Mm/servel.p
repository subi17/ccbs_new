/* ----------------------------------------------------------------------
  MODULE .......: ServEl
  TASK .........: ALL elements in Service Packages
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 18-06-99
  CHANGED ......: 28.06.99 pt ServEl record changed
                  07.10.99 jp urights added  
                  21.05.02/tk Event logging added
                  10.03.03 tk tokens
                  07.12.04/aam attributes (f4),
                               input icServPac (ServEl1.p removed)
                  15.12.06/mvi find,new,delete ufkeys only for Syst group             
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable ServEl

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ServEl'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhServEl AS HANDLE NO-UNDO.
   lhServEl = BUFFER ServEl:HANDLE.
   RUN StarEventInitialize(lhServEl).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhServEl).
   END.
END.

DEF INPUT PARAMETER icServPac AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcServPac  LIKE ServEl.ServPac  NO-UNDO.
DEF VAR lcServCom  LIKE ServEl.ServCom NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lcTarget     AS CHAR                   NO-UNDO FORMAT "x(20)".
DEF VAR lcActType    AS CHAR                   NO-UNDO FORMAT "x(20)".

form
    ServEl.ServPac      /* COLUMN-LABEL FORMAT */ FORMAT "X(12)"
    ServPac.SPName     format "x(10)"
    ServEl.ServCom
    ServEl.SeValue     column-label "V"
    ServCom.ScName     format "x(31)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Service Components "
    + (IF icServPac > "" THEN "of '" + icServPac + "'  " ELSE "") 
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    ServEl.ServPac COLON 20 FORMAT "X(12)"
       ServPac.SPName NO-LABEL format "x(30)"     SKIP
    ServEl.ServCom COLON 20 SKIP
        ServCom.ScName colon 20 format "x(40)"     SKIP

        ServCom.ScLocalName colon 20 format "x(40)"     SKIP
        ServCom.ActType  COLON 20 
           lcActType NO-LABEL SKIP
        ServCom.Target   COLON 20 
           lcTarget NO-LABEL SKIP
    ServEl.SeValue COLON 20
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    /*1 columns*/
    FRAME lis.

form /* seek ServEl  BY  ServPac */
    "Brand .:" gcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = gcBrand),"Unknown brand") SKIP
    "Package:" lcServPac
    HELP "Enter Code of Service Package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND PACKAGE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ServEl  BY ServEl */
    "Brand ...:" gcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = gcBrand),"Unknown brand") SKIP
    "Component:" lcServCom             
    HELP "Enter Code of Service Component"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND COMPONENT "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Package  ,By Component, By 4".

IF icServPac > "" THEN ASSIGN
    MaxOrder = 1
    order    = 2
    FrmRow   = 3
    FrmDown  = 11. 
    

RUN local-find-first.
IF AVAILABLE ServEl THEN ASSIGN
   Memory       = recid(ServEl)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No service elements available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a ServEl  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           IF icServPac > "" 
           THEN DISPLAY icServPac @ ServEl.ServPac WITH FRAME lis.
           
           PROMPT-FOR 
            ServEl.ServPac WHEN icServPac = ""
            ServEl.ServCom

           WITH FRAME lis EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "ServPac" THEN DO:
                    if input frame lis ServEl.ServPac = "" THEN
                    UNDO add-row, LEAVE add-row.

                    FIND ServPac WHERE 
                         ServPac.Brand = gcBrand   AND 
                         ServPac.ServPac =
                    INPUT FRAME lis ServEl.ServPac NO-LOCK NO-ERROR.

                    IF NOT AVAIL ServPac THEN DO:
                       BELL.
                       MESSAGE "Unknown Service Package !".
                       NEXT.
                    END.
                    DISP ServPac.SPName WITH FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "ServCom" THEN DO:
                   IF INPUT FRAME lis ServEl.ServCom = "" THEN DO:
                      IF icServPac = "" THEN DO:
                         NEXT-PROMPT ServEl.ServPac.
                         NEXT.
                      END.
                      ELSE UNDO add-row, LEAVE add-row.
                   END.

                   FIND ServCom WHERE 
                        ServCom.Brand   = gcBrand AND 
                        ServCom.ServCom =
                   INPUT FRAME lis ServEl.ServCom NO-LOCK NO-ERROR.
                   IF NOT AVAIL ServCom THEN DO:
                      BELL.
                      MESSAGE "Unknown Service Component !".
                      NEXT.
                   END.
                   DISP ServCom.ScName WITH FRAME lis.
                END.

                /* Check that there is no such record already */
                FIND ServEl WHERE
                     ServEl.Brand    = gcBrand                         AND 
                     ServEl.ServPac  = INPUT FRAME lis ServEl.ServPac  AND
                     ServEl.ServCom  = INPUT FRAME lis ServEl.ServCom
                NO-LOCK NO-ERROR.
                IF AVAIL ServEl THEN DO:
                   MESSAGE 
                   "A Service Element record with equal key values"
                   "exists already !"
                   VIEW-AS ALERT-BOX error.
                   NEXT.
               END.    

             END.
             APPLY LASTKEY.
           END. /* EDITING */

           CREATE ServEl.
           ASSIGN
           ServEl.Brand    = gcBrand 
           ServEl.ServPac  = caps(INPUT FRAME lis ServEl.ServPac)
           ServEl.ServCom  = CAPS(INPUT FRAME lis ServEl.ServCom).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServEl).
           ASSIGN
           Memory = recid(ServEl)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ServEl
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServEl THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServEl THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServEl).
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
        ufk[1]= 0 ufk[2]= 246 ufk[3]= 0 ufk[4]= 2350.
        
        
        /* find, new and delete keys only for Syst group */
        FIND FIRST tmsuser NO-LOCK WHERE
                   tmsuser.usercode = katun NO-ERROR.
        IF AVAIL tmsuser AND tmsuser.usergroup = "Syst" THEN 
           ASSIGN 
              ufk[1] = 245
              ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
              ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0).
        
        ASSIGN
           ufk[7]= 814 ufk[8]= 8 ufk[9]= 1
           ehto = 3 ufkey = FALSE.
        
        IF icServPac > "" THEN ufk[1] = 0.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServEl.ServPac ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServEl.ServPac WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ServEl.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServEl.ServCom WITH FRAME sel.
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

      IF LOOKUP(nap,"cursor-right") > 0 AND MaxOrder > 1 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      ELSE IF LOOKUP(nap,"cursor-left") > 0 AND MaxOrder > 1 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServEl THEN
              ASSIGN FIRSTrow = i Memory = recid(ServEl).
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
           IF NOT AVAILABLE ServEl THEN DO:
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
                rtab[1] = recid(ServEl)
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
           IF NOT AVAILABLE ServEl THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServEl).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServEl THEN DO:
           Memory = recid(ServEl).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServEl THEN Memory = recid(ServEl).
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
           FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISP gcBrand WITH FRAME f1.
       SET  gcBrand WHEN gcAllBrand = TRUE
            lcServPac WITH FRAME f1.          
       HIDE FRAME f1 NO-PAUSE.
       IF lcServPac > "" THEN DO:
          FIND FIRST ServEl WHERE 
                     ServEl.ServPac >= lcServPac   AND 
                     ServEl.Brand    = gcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISP gcBrand WITH FRAME f2.
       SET gcBrand WHEN gcAllBrand = TRUE AND icServPac = ""
           lcServCom WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcServCom > "" THEN DO:
          
          IF icServPac > "" THEN   
          FIND FIRST ServEl Use-index servcom WHERE 
                     ServEl.ServPac  = icServPac  AND
                     ServEl.ServCom >= lcServCom  AND 
                     ServEl.Brand    = gcBrand 
          NO-LOCK NO-ERROR.
 
          ELSE
          FIND FIRST ServEl Use-index servcom WHERE 
                     ServEl.ServCom >= lcServCom  AND 
                     ServEl.Brand    = gcBrand 
          NO-LOCK NO-ERROR.
          
          IF NOT  fRecFound(2) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* attributes */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        
        RUN local-find-this(FALSE).

        FIND ServCom WHERE 
             ServCom.Brand = gcBrand AND
             ServCom.ServCom = ServEl.ServCom NO-LOCK NO-ERROR.
             
        IF AVAILABLE ServCom AND ServCom.ServAttr = TRUE THEN 
        RUN Mc/servattr(ServCom.servcom).

        ELSE MESSAGE 
             "Service component does not have any attributes"
             VIEW-AS ALERT-BOX TITLE "SERVICE ATTRIBUTES".
        
        ufkey = TRUE.
        NEXT LOOP.
     END.

     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ServEl.ServPac ServEl.ServCom .

       RUN local-find-NEXT.
       IF AVAILABLE ServEl THEN Memory = recid(ServEl).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServEl THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServEl).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServEl.ServPac ServEl.ServCom .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServEl).

           DELETE ServEl.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServEl
           WHERE ServEl.Brand = gcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* translations (for components) */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  
        FIND ServEl WHERE RECID(ServEl) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invlang(13,ServEl.ServCom).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       {Syst/uright2.i}
       /* change */
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServEl).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ServEl.ServPac.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServEl).

       RUN local-disp-row.
       xrecid = recid(ServEl).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServEl) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServEl) must-print = TRUE.
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
      FIND ServEl WHERE recid(ServEl) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServEl WHERE recid(ServEl) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icServPac > "" THEN DO:
      FIND FIRST ServEl WHERE
                 ServEl.Brand   = gcBrand AND
                 ServEl.ServPac = icServPac NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      IF order = 1 THEN FIND FIRST ServEl
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND FIRST ServEl USE-INDEX ServCom
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
       
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icServPac > "" THEN DO:
      FIND LAST ServEl WHERE
                ServEl.Brand   = gcBrand AND
                ServEl.ServPac = icServPac NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      IF order = 1 THEN FIND LAST ServEl
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND LAST ServEl USE-INDEX ServCom
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
   END.   
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icServPac > "" THEN DO:
      FIND NEXT ServEl WHERE
                ServEl.Brand   = gcBrand AND
                ServEl.ServPac = icServPac NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      IF order = 1 THEN FIND NEXT ServEl
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND NEXT ServEl USE-INDEX ServCom
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF icServPac > "" THEN DO:
      FIND PREV ServEl WHERE
                ServEl.Brand   = gcBrand AND
                ServEl.ServPac = icServPac NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      IF order = 1 THEN FIND PREV ServEl
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND PREV ServEl USE-INDEX ServCom
      WHERE ServEl.Brand = gcBrand NO-LOCK NO-ERROR.
   END.    
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServEl.ServPac
       ServPac.SPName WHEN AVAIL ServPac
       ServEl.ServCom
       ServCom.ScName WHEN AVAIL ServCom
       ServEl.SeValue
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND ServPac WHERE
            ServPac.ServPac = ServEl.ServPac AND 
            ServPac.Brand   = ServEl.Brand NO-LOCK NO-ERROR.
       
       FIND ServCom WHERE
            ServCom.ServCom = ServEl.ServCom AND 
            ServCom.Brand   = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      IF AVAILABLE ServPac 
      THEN DISPLAY ServPac.SPName WITH FRAME lis.
      
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
      
      DISP  ServEl.ServPac
            ServEl.ServCom
            ServEl.SeValue
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9.
         RUN Syst/ufkey.

         UPDATE ServEl.SeValue
         WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "SeValue" THEN DO:
                   IF INPUT ServEl.SeValue < ServCom.SCValueRange[1] OR
                      INPUT ServEl.SeValue > ServCom.SCValueRange[2] THEN DO:
                      BELL.
                      message "Value MUST be within range" 
                              ServCom.SCValueRange[1]
                              "-" 
                              ServCom.SCValueRange[2] "!".
                      NEXT.
                   END.
                END.   
             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.

      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.

