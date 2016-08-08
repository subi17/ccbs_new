/* ----------------------------------------------------------------------
  MODULE .......: ServFee
  TASK .........: UPDATEs table ServFee
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 29.01.04
  CHANGED ......: 10.02.04 jp  f4 memo
                  28.06.04/aam InvInfo  
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable ServFee

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ServFee'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhServFee AS HANDLE NO-UNDO.
   lhServFee = BUFFER ServFee:HANDLE.
   RUN StarEventInitialize(lhServFee).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhServFee).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcServType  LIKE ServFee.ServType      NO-UNDO. 

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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

DEF VAR lcServName   AS CHAR  NO-UNDO.
DEF VAR lcEventName  AS CHAR  NO-UNDO. 
DEF VAR lcKeyName    AS CHAR  NO-UNDO. 
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR lcSTypeLst   AS CHAR  NO-UNDO. 
DEF VAR lcSNameLst   AS CHAR  NO-UNDO.
DEF VAR lcETypeLst   AS CHAR  NO-UNDO. 
DEF VAR lcENameLst   AS CHAR  NO-UNDO.
DEF VAR lcHelpTable  AS CHAR  NO-UNDO.
DEF VAR lcHelpField  AS CHAR  NO-UNDO.
DEF VAR lcHelpGrp    AS CHAR  NO-UNDO. 
DEF VAR liInvInfo    AS INT   NO-UNDO. 

form
    ServFee.Brand     FORMAT "X(5)"  COLUMN-LABEL "Brand"
    ServFee.ServType
    ServFee.ServKey
    ServFee.EventType
    lcEventName       FORMAT "X(15)" COLUMN-LABEL "Event"
    ServFee.FeeModel  FORMAT "X(8)" 
    ServFee.FromDate
    ServFee.ToDate

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " SERVICE FEES "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ServFee.ServType    COLON 20 
        lcServName NO-LABEL FORMAT "X(30)" AT 35 SKIP
    ServFee.ServKey     COLON 20 
        lcKeyName NO-LABEL FORMAT "X(30)" AT 35 SKIP
    ServFee.EventType   COLON 20 
        lcEventName NO-LABEL FORMAT "X(30)" AT 35 SKIP
    ServFee.FeeModel    COLON 20
       VALIDATE(INPUT ServFee.FeeModel = "" OR
                CAN-FIND(FeeModel WHERE 
                         FeeModel.Brand = gcBrand AND
                         FeeModel.FeeModel = INPUT ServFee.FeeModel),
                "Unknown fee model")
       FeeModel.FeeName NO-LABEL   SKIP
    liInvInfo           COLON 20
          FORMAT ">>9"
          LABEL "Info to Invoice" 
          HELP "Header text number" 
       HdrText.te-text NO-LABEL FORMAT "X(20)"    
       SKIP
    ServFee.FromDate    COLON 20
       VALIDATE(INPUT ServFee.FromDate NE ?,
                "Date is mandatory") SKIP
    ServFee.ToDate      COLON 20
       VALIDATE(INPUT ServFee.ToDate NE ? AND 
                INPUT ServFee.ToDate >= INPUT ServFee.FromDate,
                "Date is mandatory, and cannot be less than begin date") SKIP
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form /* seek  ServFee */
    "Brand:" lcBrand skip
    "Type :" lcServType
    HELP "Enter Service Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fServType RETURNS LOGICAL
   (icServType AS CHAR).
   
   DEF VAR liType AS INT NO-UNDO.
   
   liType = LOOKUP(icServType,lcSTypeLst).
   
   IF liType = 0 THEN RETURN FALSE.
   
   lcServName = ENTRY(liType,lcSNameLst).
   
   DISPLAY lcServName WITH FRAME lis.
   
   RETURN TRUE. 
END.

FUNCTION fEventType RETURNS LOGICAL
   (icEventType AS CHAR,
    ilDisp      AS LOG).
   
   DEF VAR liEType AS INT NO-UNDO.
   
   liEType = LOOKUP(icEventType,lcETypeLst).
   
   IF liEType = 0 THEN RETURN FALSE.
   
   lcEventName = ENTRY(liEType,lcENameLst).
   
   IF ilDisp THEN 
   DISPLAY lcEventName WITH FRAME lis.
   
   RETURN TRUE.   
END.

FUNCTION fServKey RETURNS LOGICAL
   (icServKey  AS CHAR).
   
   lcKeyName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                lcHelpTable,lcHelpField,icServKey).
   DISPLAY lcKeyName WITH FRAME lis.
   
END FUNCTION.

FUNCTION fServKeyHelp RETURNS LOGICAL
   (icServType AS CHAR):

   ASSIGN lcHelpTable = ""
          lcHelpField = ""
          lcHelpGrp   = "".
          
   CASE icServType:
   WHEN "CLISpec" OR 
   WHEN "InvSpec" THEN ASSIGN lcHelpTable = "Mobsub"
                              lcHelpField = "RepCodes"
                              lcHelpGrp   = "Report".
   END CASE.
   
END FUNCTION.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Type  ,".

/* explanations and validations for codes */
lcServName  = DYNAMIC-FUNCTION("fTMSCodeList" IN ghFunc1,
                               "ServFee","ServType").
lcEventName = DYNAMIC-FUNCTION("fTMSCodeList" IN ghFunc1,
                               "ServFee","EventType").

DO i = 1 TO NUM-ENTRIES(lcServName,CHR(1)):
   ASSIGN 
   lcSTypeLst = lcSTypeLst + ENTRY(1,ENTRY(i,lcServName,CHR(1)),CHR(9)) + ","
   lcSNameLst = lcSNameLst + ENTRY(2,ENTRY(i,lcServName,CHR(1)),CHR(9)) + ",".
END.          
DO i = 1 TO NUM-ENTRIES(lcEventName,CHR(1)):
   ASSIGN 
   lcETypeLst = lcETypeLst + ENTRY(1,ENTRY(i,lcEventName,CHR(1)),CHR(9)) + ","
   lcENameLst = lcENameLst + ENTRY(2,ENTRY(i,lcEventName,CHR(1)),CHR(9)) + ",".
END.          

ASSIGN lcServName  = ""
       lcEventName = "".


RUN local-find-first.

IF AVAILABLE ServFee THEN ASSIGN
   Memory       = recid(ServFee)
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ServFee  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:

           PROMPT-FOR ServFee.ServType
                      ServFee.ServKey
           WITH FRAME lis EDITING:
              
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" THEN DO:
                 IF FRAME-FIELD = "ServType" THEN DO:

                    RUN Help/h-tmscodes.p(INPUT "ServFee",    /* TableName */
                                         "ServType",   /* FieldName */
                                         "Service",   /* GroupCode */
                                   OUTPUT lcCode).

                    IF lcCode ne "" AND lcCode NE ?
                    THEN DO:
                       DISPLAY lcCode ;& ServFee.ServType WITH FRAME lis.
                       fServType(lcCode). 
                    END.   
                 END.
                 
                 ELSE IF FRAME-FIELD = "ServKey" THEN DO:

                    fServKeyHelp(INPUT INPUT FRAME lis ServFee.ServType).
                    
                    RUN Help/h-tmscodes.p(INPUT lcHelpTable,    /* TableName */
                                         lcHelpField,    /* FieldName */
                                         lcHelpGrp,      /* GroupCode */
                                   OUTPUT lcCode).

                    IF lcCode ne "" AND lcCode NE ?
                    THEN DO:
                       DISPLAY lcCode ;& ServFee.ServKey WITH FRAME lis.
                       fServKey(lcCode). 
                    END.   
                 END. 
                 
                 ehto = 9.
                 RUN Syst/ufkey.
                 NEXT. 
              END.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "ServType" THEN DO:
                    IF INPUT ServFee.ServType NE "" AND
                       NOT fServType(INPUT INPUT FRAME lis ServFee.ServType)
                    THEN DO:
                       BELL.
                       MESSAGE "Unknown service type".
                       NEXT.
                    END.
                 END.
              END. 
              
              APPLY LASTKEY.
              
           END.

           IF INPUT FRAME lis ServFee.ServType = "" OR
              INPUT FRAME lis ServFee.ServKey = ""
           THEN LEAVE add-row.

           CREATE ServFee.
           ASSIGN
           ServFee.Brand    = lcBrand
           ServFee.ServType = INPUT FRAME lis ServFee.ServType
           ServFee.ServKey  = INPUT FRAME lis ServFee.ServKey.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServFee).

           ASSIGN
           Memory = recid(ServFee)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ServFee THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServFee WHERE recid(ServFee) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServFee THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServFee).
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
        ufk[1]= 816  ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
        ufk[4] = 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ServFee.ServType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ServFee.ServType WITH FRAME sel.
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
        FIND ServFee WHERE recid(ServFee) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServFee THEN
              ASSIGN FIRSTrow = i Memory = recid(ServFee).
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
           IF NOT AVAILABLE ServFee THEN DO:
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
                rtab[1] = recid(ServFee)
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
           IF NOT AVAILABLE ServFee THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServFee).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServFee WHERE recid(ServFee) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServFee THEN DO:
           Memory = recid(ServFee).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServFee THEN Memory = recid(ServFee).
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
           FIND ServFee WHERE recid(ServFee) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

          cfc = "puyr". RUN Syst/ufcolor.
          ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
          CLEAR FRAME f1.
          DISPLAY lcBrand WITH FRAME F1.
          UPDATE lcBrand WHEN gcAllBrand
                 lcServType WITH FRAME f1.
          HIDE FRAME f1 NO-PAUSE.

          IF lcServType NE "" THEN DO:
             FIND FIRST ServFee WHERE 
                        ServFee.Brand     = lcBrand AND
                        ServFee.ServType >= lcServType
             NO-LOCK NO-ERROR.

             IF NOT fRecFound(1) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        RUN local-find-this (FALSE).
        RUN Mc/memo(INPUT 0,
                 INPUT "ServFee",
                 INPUT ServFee.ServKey,
                 INPUT Servfee.ServType).
        ufkey = TRUE.
        NEXT.
                                                                                     END.
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
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
       ServFee.ServType ServFee.ServKey ServFee.EventType ServFee.FeeModel.

       RUN local-find-NEXT.
       IF AVAILABLE ServFee THEN Memory = recid(ServFee).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServFee THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServFee).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServFee.ServType ServFee.ServKey ServFee.EventType ServFee.FeeModel.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServFee).

           DELETE ServFee.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServFee WHERE ServFee.Brand = gcBrand) 
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

     ELSE IF LOOKUP(nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServFee).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServFee).

       RUN local-disp-row.
       xrecid = recid(ServFee).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServFee) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServFee) must-print = TRUE.
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
      FIND ServFee WHERE recid(ServFee) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServFee WHERE recid(ServFee) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   FIND FIRST ServFee WHERE 
              ServFee.Brand = lcBrand
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   FIND LAST ServFee WHERE 
             ServFee.Brand = lcBrand
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:

   FIND NEXT ServFee WHERE 
             ServFee.Brand = lcBrand
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:

   FIND PREV ServFee WHERE 
             ServFee.Brand = lcBrand
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServFee.Brand
       ServFee.ServType
       ServFee.ServKey
       ServFee.EventType
       lcEventName
       ServFee.FeeModel
       ServFee.FromDate
       ServFee.ToDate
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   fEventType(STRING(ServFee.EventType),FALSE).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      FIND FeeModel NO-LOCK WHERE
           FeeModel.Brand    = lcBrand AND
           FeeModel.FeeModel = ServFee.FeeModel NO-ERROR.

      fServType(ServFee.ServType).
      fServKeyHelp(ServFee.ServType).
      fServKey(ServFee.ServKey).
      fEventType(STRING(ServFee.EventType),TRUE).
      
      liInvInfo = INTEGER(ServFee.InvInfo) NO-ERROR.

      IF liInvInfo > 0 THEN DO:
         FIND FIRST HdrText NO-LOCK WHERE
                    HdrText.Brand  = gcBrand AND
                    HdrText.te-nro = liInvInfo AND
                    HdrText.te-kie = 1 NO-ERROR.
                              
         IF AVAILABLE HdrText THEN DISPLAY HdrText.te-text WITH FRAME lis.
      END.
      ELSE DISPLAY "" @ HdrText.Te-text WITH FRAME lis.
       
      DISP ServFee.ServType
           ServFee.ServKey 
           ServFee.EventType
           ServFee.FeeModel
           FeeModel.FeeName WHEN AVAILABLE FeeModel
           ServFee.FromDate
           ServFee.ToDate
           liInvInfo
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN Syst/ufkey.
         
         UPDATE
         ServFee.EventType
         ServFee.FeeModel
         liInvInfo
         ServFee.FromDate
         ServFee.ToDate
         WITH FRAME lis
         EDITING:

            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"EventType,liInvInfo") > 0
            THEN DO:

               IF FRAME-FIELD = "EventType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "ServFee",    /* TableName */
                                       "EventType",  /* FieldName */
                                       "Service",    /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& ServFee.EventType WITH FRAME lis.   
                     fEventType(lcCode,TRUE).
                  END.   
               END.
               
               ELSE IF FRAME-FIELD = "liInvInfo" THEN DO:
                  ASSIGN gcHelpParam = "prt"
                         si-recid    = 0.
                  RUN Mc/nnteyp.
                  gcHelpParam = "".
                  
                  IF si-recid > 0 THEN DO:
                     FIND HdrText WHERE RECID(HdrText) = si-recid NO-LOCK.
                     DISPLAY HdrText.te-nro @ liInvInfo WITH FRAME lis.
                  END.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.


            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "ServKey" THEN DO:
               
               END.

               ELSE IF FRAME-FIELD = "EventType" THEN DO:

                  IF NOT fEventType(INPUT 
                                STRING(INPUT FRAME lis ServFee.EventType),
                                TRUE)
                  THEN DO:
                     BELL.
                     MESSAGE "Unknown event type".
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "FeeModel" THEN DO:

                  IF INPUT ServFee.FeeModel = "" 
                  THEN DISPLAY "" @ FeeModel.FeeName.

                  ELSE DO:
                     FIND FeeModel NO-LOCK WHERE
                          FeeModel.Brand    = lcBrand AND
                          FeeModel.FeeModel = INPUT ServFee.FeeModel 
                          NO-ERROR.
                     IF AVAILABLE FeeModel THEN DISPLAY FeeModel.FeeName.
                  END.
               END. 

               ELSE IF FRAME-FIELD = "liInvInfo" THEN DO:
                  IF INPUT liInvInfo > 0 THEN DO:
                     FIND FIRST HdrText NO-LOCK WHERE
                                HdrText.Brand  = gcBrand AND
                                HdrText.te-nro = INPUT liInvInfo AND
                                HdrText.te-kie = 1  /* 1 must always exist */
                     NO-ERROR.
                              
                     IF NOT AVAILABLE HdrText THEN DO:
                        BELL.
                        MESSAGE "Unknown text".
                        NEXT.
                     END.
                     DISPLAY HdrText.te-text WITH FRAME lis.
                                
                  END.
                  ELSE DISPLAY "" @ HdrText.Te-text WITH FRAME lis.

               END.

            END.

            APPLY LASTKEY.
            
         END. /* EDITING */
         
         ServFee.InvInfo = STRING(liInvInfo).
         
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
   
END PROCEDURE.

