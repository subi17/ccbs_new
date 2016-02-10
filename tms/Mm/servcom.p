/*/----------------------------------------------------------------------
  MODULE .......: ServCom.P
  TASK .........: Service Components
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 18-06-99
  CHANGED ......: 28-06-99 pt added FIELD ScLocalName 
                  05-10-99 jp urights added  
                  31-01-00 jp cp-chgable added
                  19.03.03 tk RUN Mc/memo
                  14.01.03 jp servattr
                  06.02.04 jp custnum for memo
                  07.12.04/aam tokens & eventlog,
                               ServType added
                  21.12.04/aam SepHLR, ClFeeModel,ChgFeeModel, ServiceLimit
                  19.01.05/aam CloseTime
                  08.05.06/aam SMSTxt-fields
                  19.12.06/aam ScPosition
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ServCom'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhServCom AS HANDLE NO-UNDO.
   lhServCom = BUFFER ServCom:HANDLE.
   RUN StarEventInitialize(lhServCom).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhServCom).
   END.

END.

DEF INPUT PARAMETER  Service LIKE Service.Service No-UNDO.
DEF NEW shared VAR siirto AS CHAR.

DEF VAR ServCom  LIKE ServCom.ServCom  NO-UNDO.
DEF VAR ScName  LIKE ServCom.ScName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
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
def var Memo-exists  AS log format "M/"        NO-UNDO.
DEF VAR lcActType    AS CHAR                   NO-UNDO FORMAT "x(20)".
DEF VAR lcTarget     AS CHAR                   NO-UNDO FORMAT "x(20)".
DEF VAR llFind       AS LOG                    NO-UNDO.
DEF VAR llServType   AS LOG                    NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO.
DEF VAR lcOpenName   AS CHAR                   NO-UNDO. 
DEF VAR lcCloseName  AS CHAR                   NO-UNDO. 
DEF VAR lcChangeName AS CHAR                   NO-UNDO. 
DEF VAR llCloseTime  AS LOG                    NO-UNDO. 
form
    ServCom.ServCom     
    ServCom.ScName      column-label "Service Name" format "x(17)"
    ServCom.SCParameter column-label "Param"     
    ServCom.ServAttr    column-label "Attr"
    ServCom.ScChgable   column-label "ChgA" format "Yes/No"
    ServCom.ActType     column-label "HLR"
    llServType          column-label "S.Type" FORMAT "Basic/Addit"
    ServCom.Target      column-label "T" 
    ServCom.FeeModel    column-label "FeeModel" format "x(7)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    "  Components of Service '" + Service + "'  "
    + " " 
    FRAME sel.

form
    ServCom.ServCom      COLON 20                  SKIP
    ServCom.ScName       COLON 20 FORMAT "x(45)"   SKIP
    ServCom.ScLocalName  COLON 20 FORMAT "X(45)"   SKIP
    ServCom.SCParameter  COLON 20  
       LABEL "Parameter" SKIP
    ServCom.ServAttr     COLON 20                  SKIP
    ServCom.SCValueRange[1] COLON 20 
       LABEL "Range"
    ServCom.SCValueRange[2] 
       NO-LABEL
       VALIDATE (INPUT ServCom.SCValueRange[2] >= 
                 input ServCom.SCValueRange[1],"Invalid Order !")     SKIP
    ServCom.ScChgable    COLON 20                  SKIP
    ServCom.ActType      COLON 20
       lcActType no-label                          SKIP
    ServCom.SepHLR       COLON 20                      
    ServCom.ScPosition   COLON 20 
       LABEL "HLR Order"
       HELP "Relative order (inside a package) when sent to HLR" 
       FORMAT ">>>9" SKIP
    llServType           COLON 20 
       LABEL "Service Type"
       HELP "Type of service; basic / additional"
       FORMAT "Basic/Additional" SKIP
    ServCom.Target       COLON 20 
       lcTarget no-label                           SKIP
    llCloseTime          COLON 20 FORMAT "Yes/No"
       LABEL "Closed on last day" 
       HELP "Can service be closed only on last day of month"
 
    ServCom.FeeModel     COLON 20 FORMAT "X(16)"
       LABEL "OPEN    Fee Model"
       SPACE(2) 
    ServCom.SMSTxt 
       LABEL "SMS" 
       FORMAT "X(15)"
       HELP "SMS for opening service" SKIP
    
    ServCom.ClFeeModel   COLON 20 FORMAT "X(16)"
       LABEL "CLOSE   Fee Model"
       SPACE(2) 
    ServCom.ClSMSTxt 
       LABEL "SMS" 
       FORMAT "X(15)"
       HELP "SMS for closing service" SKIP

    ServCom.ChgFeeModel  COLON 20 FORMAT "X(16)"
       LABEL "CHANGE  Fee Model"
       SPACE(2) 
    ServCom.ChgSMSTxt 
       LABEL "SMS" 
       FORMAT "X(15)"
       HELP "SMS for changing service's value" SKIP
    
    ServCom.ServiceLimit COLON 20 
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek ServCom  BY  ServCom */
    ServCom
    HELP "Enter Code of component"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ServCom  BY ScName */
    SCName
    HELP "Enter Name of component"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


form /* memo */
    ServCom.Memo
    WITH OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Memo: " + ServCom.ServCom + " " WITH NO-LABELS 1 columns
    FRAME memo.

FIND Service WHERE 
     Service.Service = Service AND 
     Service.Brand   = gcBrand NO-LOCK.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST ServCom WHERE 
           ServCom.Service = Service AND 
           ServCom.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE ServCom THEN ASSIGN
   Memory       = recid(ServCom)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = (lcRight = "RW").

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ServCom  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
      
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR ServCom.ServCom
           VALIDATE
              (ServCom.ServCom NOT ENTERED OR
              NOT CAN-FIND(ServCom using  ServCom.ServCom WHERE  
                           Servcom.Brand = gcBrand ),
              "ServCom " + string(INPUT ServCom.ServCom) +
              " already exists !").
           IF INPUT FRAME lis ServCom.ServCom NOT ENTERED THEN 
           LEAVE add-row.
           CREATE ServCom.
           ASSIGN
           ServCom.Service  = Service  
           ServCom.Brand   = gcBrand 
           ServCom.ServCom = INPUT FRAME lis ServCom.ServCom.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServCom).

           ASSIGN
           Memory = recid(ServCom)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.
      /* is there ANY record ? */
      FIND FIRST ServCom
      WHERE ServCom.Service = Service AND 
            ServCom.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServCom THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServCom WHERE recid(ServCom) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel: 
           IF AVAILABLE ServCom THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServCom).
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
        ufk[1]= 816  ufk[2]= 251 ufk[3]= 2350 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 814 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"3,5,6"'}
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServCom.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServCom.ServCom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ServCom.ScName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServCom.ScName WITH FRAME sel.
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
        FIND ServCom WHERE recid(ServCom) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServCom THEN
              ASSIGN FIRSTrow = i Memory = recid(ServCom).
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
           IF NOT AVAILABLE ServCom THEN DO:
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
                rtab[1] = recid(ServCom)
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
           IF NOT AVAILABLE ServCom THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServCom).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServCom WHERE recid(ServCom) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServCom THEN DO:
           Memory = recid(ServCom).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServCom THEN Memory = recid(ServCom).
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
           FIND ServCom WHERE recid(ServCom) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO:

        ASSIGN 
           ufk    = 0
           ufk[1] = 35
           ufk[2] = 30
           ufk[8] = 8
           ehto   = 0
           ufkey  = TRUE.
        RUN Syst/ufkey.
        
        /* Search BY column 1 */
        IF toimi = 1 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           CLEAR FRAME f1.
           SET ServCom WITH FRAME f1.
           HIDE FRAME f1 NO-PAUSE.
           IF ServCom ENTERED THEN DO:
              FIND FIRST ServCom WHERE 
                         ServCom.Brand    = gcBrand AND 
                         ServCom.ServCom >= ServCom AND 
                         ServCom.Service   = Service NO-LOCK NO-ERROR.
              IF NOT AVAILABLE ServCom THEN DO:
                 BELL.
                 MESSAGE "NOT FOUND !".
                 PAUSE 1 NO-MESSAGE.
                 NEXT BROWSE.
              END.
              /* some ServCom/ServCom was found */
              ASSIGN order = 1 Memory = recid(ServCom) must-print = TRUE.
              NEXT LOOP.
           END.
        END. /* Search-1 */

        /* Search BY col 2 */
        ELSE IF toimi = 2 THEN DO ON ENDKEY UNDO, NEXT LOOP:

           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           CLEAR FRAME f2.
           SET ScName WITH FRAME f2.
           HIDE FRAME f2 NO-PAUSE.
           IF ScName ENTERED THEN DO:
              FIND FIRST ServCom WHERE 
                         ServCom.Brand   = gcBrand AND 
                         ServCom.ScName >= ScName AND 
                         ServCom.Service  = Service NO-LOCK NO-ERROR.
              IF NOT AVAILABLE ServCom THEN DO:
                 BELL. MESSAGE "NOT FOUND !".
                 PAUSE 1 NO-MESSAGE.
                 NEXT BROWSE.
              END.
              /* some ServCom/SCName was found */
              ASSIGN order = 2 Memory = recid(ServCom) must-print = TRUE.
              NEXT LOOP.
           END.
        END. /* Search-2 */
     END.
          
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO TRANS:  /* Within ServPac */
       RUN local-find-this(FALSE).                                        
       RUN Mm/servel2(ServCom.ServCom).
       ufkey = TRUE.
       NEXT LOOP.
     END.

     /* attributes */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        RUN local-find-this(FALSE).
        IF ServCom.ServAttr = TRUE THEN 
        RUN Mc/servattr(INPUT ServCom.servcom).
        ELSE  
        MESSAGE 
        "There are no defined attributes to service component" 
        servcom.servcom                                            SKIP
        "Change the 'attribute' value in service component,"       SKIP
        "then You can add attributes to the component."   
        VIEW-AS ALERT-BOX TITLE "SERVICE ATTRIBUTES".
        
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN Mc/memo(INPUT 0,
                 INPUT "ServCom",
                 INPUT STRING(ServCom.ServCom),
                 INPUT "Service component").
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
       ServCom.ServCom ServCom.ScName .

       RUN local-find-NEXT.
       IF AVAILABLE ServCom THEN Memory = recid(ServCom).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServCom THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServCom).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServCom.ServCom ServCom.ScName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServCom).

           DELETE ServCom.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServCom
           WHERE ServCom.Service = Service AND 
                 ServCom.Brand = gcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* translations */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  
        FIND ServCom WHERE RECID(ServCom) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invlang(13,ServCom.ServCom).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       {Syst/uright2.i}
       /* change */
       RUN local-find-this((lcRight = "RW")).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ServCom.ServCom.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServCom).

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServCom).
       
       RUN local-disp-row.
       xrecid = recid(ServCom).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServCom) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServCom) must-print = TRUE.
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
      FIND ServCom WHERE recid(ServCom) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServCom WHERE recid(ServCom) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServCom
       WHERE ServCom.Service = Service AND 
             ServCom.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ServCom USE-INDEX SCName
       WHERE ServCom.Service = Service AND 
             ServCom.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServCom
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand 
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ServCom USE-INDEX SCName
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServCom
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand 
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ServCom USE-INDEX SCName
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServCom
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ServCom USE-INDEX SCName
       WHERE ServCom.Service = Service AND ServCom.Brand = gcBrand 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServCom.ServCom
       ServCom.ScName
       ServCom.SCParameter
       ServCom.ServAttr
       ServCom.ScChgable
       ServCom.ActType
       llServType
       ServCom.Target
       ServCom.Feemodel 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   llServType = (ServCom.ServType = 0). 

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcField AS CHAR NO-UNDO.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      llCloseTime = (ServCom.CloseTime = 1).

      lcActType = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                  "ServCom",
                                  "ActType",
                                  STRING(ServCom.ActType)).

      lcTarget = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                  "ServCom",
                                  "Target",
                                  STRING(ServCom.Target)).
      DISP 
      ServCom.ScName
      ServCom.ScLocalName
      ServCom.SCParameter
      ServCom.ServAttr
      ServCom.SCValueRange           
      ServCom.ScChgable
      ServCom.ActType
      lcActType 
      ServCom.SepHLR
      ServCom.ScPosition
      llServType
      ServCom.Target
      lcTarget
      llCloseTime
      ServCom.FeeModel
      ServCom.ClFeeModel
      ServCom.ChgFeeModel
      ServCom.SMSTxt
      ServCom.ClSMSTxt
      ServCom.ChgSMSTxt
      ServCom.ServiceLimit
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
         ehto = 9.
         RUN Syst/ufkey.
      
         UPDATE
           ServCom.ScName
           ServCom.ScLocalName
           ServCom.SCParameter
           ServCom.ServAttr
           ServCom.SCValueRange           
           ServCom.ScChgable
           ServCom.ActType
           ServCom.SepHLR
           ServCom.ScPosition
           llServType
           ServCom.Target
           llCloseTime
           ServCom.FeeModel
           ServCom.SMSTxt
           ServCom.ClFeeModel
           ServCom.ClSMSTxt
           ServCom.ChgFeeModel
           ServCom.ChgSMSTxt
           ServCom.ServiceLimit
         WITH FRAME lis EDITING:
         
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" AND 
                FRAME-FIELD = "Target" 
             THEN DO:
            
                RUN Help/h-tmscodes(INPUT "ServCom",     /* TableName */
                                     "Target",  /* FieldName */
                                     "Service", /* GroupCode */
                               OUTPUT lcCode).
              
                IF lcCode ne "" AND lcCode NE ? THEN DO:
                   lcTarget = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                               "ServCom",
                                               "Target",
                                               lcCode).
                   DISPLAY INTEGER(lcCode) ;& ServCom.Target
                           lcTarget
                   WITH FRAME lis.   
                END.
               
                ehto = 9.
                RUN Syst/ufkey.
                NEXT. 
             END.

             ELSE IF keylabel(lastkey) = "F9" AND
                FRAME-FIELD MATCHES("*SMSTxt")
             THEN DO WITH FRAME lis:

                ASSIGN gcHelpParam = "prt"
                       si-recid    = 0
                       lcField     = FRAME-FIELD.
                RUN Mc/invotxt ("",
                             "").
                gcHelpParam = "".
                   
                ehto = 9.
                RUN Syst/ufkey.
       
                IF si-recid > 0 THEN DO:
                   FIND InvText WHERE RECID(InvText) = si-recid 
                   NO-LOCK NO-ERROR.
                   IF AVAILABLE InvText THEN DO:

                      IF InvText.Target NE "SMS" THEN DO:
                         MESSAGE "This is not an SMS text"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                      
                      CASE lcField:
                      WHEN "SMSTxt" THEN 
                         DISPLAY InvText.KeyValue @ ServCom.SMSTxt.
                      WHEN "ClSMSTxt" THEN 
                         DISPLAY InvText.KeyValue @ ServCom.ClSMSTxt.
                      WHEN "ChgSMSTxt" THEN 
                         DISPLAY InvText.KeyValue @ ServCom.ChgSMSTxt.
                      END CASE.   
                   END. 
                   
                END.
                ELSE NEXT. 
             END.

 
             ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
             DO WITH FRAME lis:
             
                PAUSE 0.

                IF FRAME-FIELD = "FeeModel" THEN DO: 
                   IF INPUT FRAME lis ServCom.Feemodel NE "" THEN DO:
                      FIND Feemodel WHERE 
                           FeeModel.Brand    = gcBrand AND 
                           FeeModel.FeeModel = INPUT FRAME lis ServCom.FeeModel
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL FeeModel THEN DO:
                         BELL.
                         MESSAGE "Unknown FeeModel !".
                         NEXT.
                      END.
                      lcOpenName = FeeModel.FeeName.
                   END.
                   ELSE lcOpenName = "".
                END.

                ELSE IF FRAME-FIELD = "ClFeeModel" THEN DO: 
                   IF INPUT FRAME lis ServCom.ClFeemodel NE "" THEN DO:
                      FIND Feemodel WHERE 
                           FeeModel.Brand    = gcBrand AND 
                           FeeModel.FeeModel = INPUT FRAME lis 
                                               ServCom.ClFeeModel
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL FeeModel THEN DO:
                         BELL.
                         MESSAGE "Unknown FeeModel !".
                         NEXT.
                      END.
                      lcCloseName = FeeModel.FeeName.
                   END.
                   ELSE lcCloseName = "".
                END.

                ELSE IF FRAME-FIELD = "ChgFeeModel" THEN DO: 
                   IF INPUT FRAME lis ServCom.ChgFeemodel NE "" THEN DO:
                      FIND Feemodel WHERE 
                           FeeModel.Brand    = gcBrand AND 
                           FeeModel.FeeModel = INPUT FRAME lis 
                                               ServCom.ChgFeeModel
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL FeeModel THEN DO:
                         BELL.
                         MESSAGE "Unknown FeeModel !".
                         NEXT.
                      END.
                      lcChangeName = FeeModel.FeeName.
                   END.
                   ELSE lcChangeName = "".
                END.

                ELSE IF FRAME-FIELD = "SMSTxt" THEN DO: 
                   IF INPUT FRAME lis ServCom.SMSTxt NE "" THEN DO:
                      IF NOT CAN-FIND(FIRST InvText WHERE
                         InvText.Brand    = gcBrand AND
                         InvText.Target   = "SMS"   AND
                         InvText.KeyValue = INPUT FRAME lis ServCom.SMSTxt)
                      THEN DO:
                         MESSAGE "Unknown SMS text"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END.
                END.

                ELSE IF FRAME-FIELD = "ClSMSTxt" THEN DO: 
                   IF INPUT FRAME lis ServCom.ClSMSTxt NE "" THEN DO:
                      IF NOT CAN-FIND(FIRST InvText WHERE
                         InvText.Brand    = gcBrand AND
                         InvText.Target   = "SMS"   AND
                         InvText.KeyValue = INPUT FRAME lis ServCom.ClSMSTxt)
                      THEN DO:
                         MESSAGE "Unknown SMS text"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END.
                END.

                ELSE IF FRAME-FIELD = "ChgSMSTxt" THEN DO: 
                   IF INPUT FRAME lis ServCom.ChgSMSTxt NE "" THEN DO:
                      IF NOT CAN-FIND(FIRST InvText WHERE
                         InvText.Brand    = gcBrand AND
                         InvText.Target   = "SMS"   AND
                         InvText.KeyValue = INPUT FRAME lis ServCom.ChgSMSTxt)
                      THEN DO:
                         MESSAGE "Unknown SMS text"
                         VIEW-AS ALERT-BOX ERROR.
                         NEXT.
                      END.
                   END.
                END.
  
                ELSE IF FRAME-FIELD = "ActType" THEN DO:
                   ASSIGN INPUT servcom.ActType.

                   RUN Syst/v-tmscodes(INPUT "ServCom", 
                                        "ActType", 
                                        "Service",
                                    INPUT INPUT ActType,
                                    OUTPUT llFind).

                   IF NOT llFind THEN  DO:
                      NEXT-PROMPT ActType.
                      NEXT.
                   END.
                   FIND FIRST TMSCodes WHERE
                              TMSCodes.TableName = "ServCom" AND
                              TMSCodes.FieldName = "ActType" AND
                              TMSCodes.CodeGroup = "Service" AND
                              TMSCodes.CodeValue = STRING(ServCom.ActType)
                   NO-LOCK NO-ERROR.
                   IF AVAIL tmscodes THEN lcActType = TMSCodes.CodeName.

                   pause 0.
                   DISP lcActType With FRAME lis.
                END.
                     
                ELSE IF FRAME-FIELD = "Target" THEN DO:

                   IF NOT 
                   DYNAMIC-FUNCTION("fTMSCodeChk" in ghFunc1,
                                    "ServCom",
                                    "Target",
                                    STRING(INPUT FRAME lis ServCom.Target))
                   THEN DO:
                      BELL.
                      MESSAGE "Unknown target".
                      NEXT.
                   END.

                   lcTarget = 
                   DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                    "ServCom",
                                    "Target",
                                    STRING(INPUT FRAME lis ServCom.Target)).

                   DISP lcTarget WITH FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "ServiceLimit" THEN DO:
                   IF INPUT ServCom.ServiceLimit > "" THEN DO:
                      FIND ServiceLimitGroup WHERE 
                           ServiceLimitGroup.Brand     = gcBrand AND
                           ServiceLimitGroup.GroupCode = 
                               INPUT ServCom.ServiceLimit
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL ServiceLimitGroup THEN DO:
                         BELL.
                         MESSAGE "Unknown Service limit group !".
                         NEXT.
                      END. 
                   END.
                END.
 
             END.
             APPLY LASTKEY.
         END. /* EDITING */
         
         ASSIGN ServCom.ServType  = INTEGER(NOT llServType)
                ServCom.CloseTime = INTEGER(llCloseTime).
         
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.

END PROCEDURE.

