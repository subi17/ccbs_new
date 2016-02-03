/* ----------------------------------------------------------------------
  MODULE .......: PaymConfig
  TASK .........: UPDATEs table PaymConfig
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 11.09.07
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable PaymConfig

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PaymConfig'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPaymConfig AS HANDLE NO-UNDO.
   lhPaymConfig = BUFFER PaymConfig:HANDLE.
   RUN StarEventInitialize(lhPaymConfig).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhPaymConfig).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liPaymType   LIKE PaymConfig.PaymType  NO-UNDO.
DEF VAR lcPaymSrc    LIKE PaymConfig.PaymSrc   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcPaymTypeName   AS CHAR NO-UNDO.
DEF VAR lcPaymSrcName    AS CHAR NO-UNDO.
DEF VAR lcDebitAccName   AS CHAR NO-UNDO.
DEF VAR lcCreditAccName  AS CHAR NO-UNDO.
DEF VAR lcField          AS CHAR NO-UNDO. 
DEF VAR lcCode           AS CHAR NO-UNDO. 

DEF BUFFER bConfig FOR PaymConfig.

FORM
    PaymConfig.PaymType
    lcPaymTypeName      FORMAT "X(18)" COLUMN-LABEL "Name" 
    PaymConfig.PaymSrc
    PaymConfig.FromDate 
    PaymConfig.ToDate
    PaymConfig.DebitAccNum
    PaymConfig.CreditAccNum
    PaymConfig.TaxRules
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " PAYMENT POSTING RULES "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    PaymConfig.Brand     COLON 18
    PaymConfig.PaymConfig COLON 18 
    PaymConfig.PaymType  COLON 15 
       lcPaymTypeName NO-LABEL FORMAT "X(30)" SKIP
    PaymConfig.PaymSrc   COLON 18
       lcPaymSrcName  NO-LABEL FORMAT "X(30)" SKIP
    PaymConfig.FromDate  COLON 18 
    PaymConfig.ToDate    COLON 18
    PaymConfig.DebitAccNum COLON 18
       lcDebitAccName NO-LABEL FORMAT "X(30)" SKIP
    PaymConfig.CreditAccNum COLON 18
       lcCreditAccName NO-LABEL FORMAT "X(30)" SKIP
    PaymConfig.TaxRules  COLON 18   
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " RULE "
    SIDE-LABELS 
    FRAME lis.

FORM
    PaymConfig.Description 
    VIEW-AS EDITOR SIZE 70 BY 6
    NO-LABEL 
    WITH OVERLAY ROW 12 CENTERED TITLE " Description " FRAME fDesc.
    
FORM 
    "Brand:" lcBrand skip
    "Type :" liPaymType
    HELP "Enter payment type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM  
    "Brand :" lcBrand skip
    "Source:" lcPaymSrc
    HELP "Enter payment source"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Source "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fPaymTypeName RETURNS LOGIC
   (iiPaymType AS INT):
   
   lcPaymTypeName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                     "Payment",
                                     "PaymType",
                                     STRING(iiPaymType)).
END FUNCTION.

FUNCTION fDispPaymTypeName RETURNS LOGIC
   (iiPaymType AS INT):
   
   fPaymTypeName(iiPaymType).
   
   DISP lcPaymTypeName WITH FRAME lis.
   
END FUNCTION.

FUNCTION fDispPaymSrcName RETURNS LOGIC
   (icPaymSrc AS CHAR):
   
   IF icPaymSrc = "" THEN lcPaymSrcName = "".
   ELSE DO:
      lcPaymSrcName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "Payment",
                                       "PaymSrc",
                                       icPaymSrc).
      IF lcPaymSrcName = "" THEN lcPaymSrcName = ?.
   END.
      
   DISP lcPaymSrcName WITH FRAME lis.
   
END FUNCTION.

FUNCTION fDispDebitAccName RETURNS LOGIC
   (iiDebitAcc AS INT):
   
   IF iiDebitAcc = 0 THEN lcDebitAccName = "".
   
   ELSE DO:
      FIND Account WHERE 
           Account.Brand  = gcBrand AND
           Account.AccNum = iiDebitAcc NO-LOCK NO-ERROR.
      IF AVAILABLE Account 
      THEN lcDebitAccName = Account.AccName.
      ELSE lcDebitAccName = ?.
   END.
   
   DISP lcDebitAccName WITH FRAME lis.
   
END FUNCTION.

FUNCTION fDispCreditAccName RETURNS LOGIC
   (iiCreditAcc AS INT):
   
   IF iiCreditAcc = 0 THEN lcCreditAccName = "".
   
   ELSE DO:
      FIND Account WHERE 
           Account.Brand  = gcBrand AND
           Account.AccNum = iiCreditAcc NO-LOCK NO-ERROR.
      IF AVAILABLE Account 
      THEN lcCreditAccName = Account.AccName.
      ELSE lcCreditAccName = ?.
   END.
   
   DISP lcCreditAccName WITH FRAME lis.
   
END FUNCTION.



cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE PaymConfig THEN ASSIGN
   Memory       = recid(PaymConfig)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No payment configurations available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a PaymConfig  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ PaymConfig.Brand.

           CREATE PaymConfig.
           ASSIGN 
              PaymConfig.Brand    = lcBrand
              PaymConfig.FromDate = TODAY
              PaymConfig.ToDate   = 12/31/2049.

           i = 1. 
           FOR EACH bConfig NO-LOCK 
           BY bConfig.PaymConfig DESC:
              i = bConfig.PaymConfig + 1.
              LEAVE.
           END.
           
           PaymConfig.PaymConfig = i.
           
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              PaymConfig.PaymType = ? THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymConfig).

           ASSIGN
           Memory = recid(PaymConfig)
           xrecid = Memory.  

           /* add only one at a time */
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PaymConfig THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND PaymConfig WHERE recid(PaymConfig) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PaymConfig THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PaymConfig).
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
        ufk[1]= 759 ufk[2]= 1018 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PaymConfig.PaymType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PaymConfig.PaymType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PaymConfig.PaymSrc ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PaymConfig.PaymSrc WITH FRAME sel.
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
        FIND PaymConfig WHERE recid(PaymConfig) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PaymConfig THEN
              ASSIGN FIRSTrow = i Memory = recid(PaymConfig).
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
           IF NOT AVAILABLE PaymConfig THEN DO:
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
                rtab[1] = recid(PaymConfig)
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
           IF NOT AVAILABLE PaymConfig THEN DO:
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
              rtab[FRAME-DOWN] = recid(PaymConfig).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PaymConfig WHERE recid(PaymConfig) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PaymConfig THEN DO:
           Memory = recid(PaymConfig).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PaymConfig THEN Memory = recid(PaymConfig).
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
           FIND PaymConfig WHERE recid(PaymConfig) = Memory NO-LOCK.
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
           liPaymType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liPaymType NE ? THEN DO:
          FIND FIRST PaymConfig WHERE 
                     PaymConfig.Brand = lcBrand AND
                     PaymConfig.PaymType >= liPaymType
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       SET lcBrand WHEN gcAllBrand
           lcPaymSrc WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcPaymSrc NE ? THEN DO:
          FIND FIRST PaymConfig WHERE 
                     PaymConfig.Brand    = lcBrand AND
                     PaymConfig.PaymSrc >= lcPaymSrc
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

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
       PaymConfig.PaymType PaymConfig.PaymSrc
       PaymConfig.FromDate PaymConfig.ToDate .

       RUN local-find-NEXT.
       IF AVAILABLE PaymConfig THEN Memory = recid(PaymConfig).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PaymConfig THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PaymConfig).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PaymConfig.PaymType PaymConfig.PaymSrc
       PaymConfig.FromDate PaymConfig.ToDate.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPaymConfig).

           DELETE PaymConfig.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST PaymConfig WHERE PaymConfig.Brand = lcBrand) 
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

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPaymConfig).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PaymConfig.PaymType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPaymConfig).

       RUN local-disp-row.
       xrecid = recid(PaymConfig).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PaymConfig) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PaymConfig) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PaymConfig WHERE recid(PaymConfig) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PaymConfig WHERE recid(PaymConfig) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PaymConfig 
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymType NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST PaymConfig 
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PaymConfig
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymType NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST PaymConfig 
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PaymConfig
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymType NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT PaymConfig 
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PaymConfig
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymType NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV PaymConfig 
          WHERE PaymConfig.Brand = lcBrand
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       PaymConfig.PaymType 
       PaymConfig.PaymSrc
       lcPaymTypeName
       PaymConfig.FromDate
       PaymConfig.ToDate
       PaymConfig.DebitAccNum
       PaymConfig.CreditAccNum
       PaymConfig.TaxRules
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
   fPaymTypeName(PaymConfig.PaymType).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llNew AS LOGIC NO-UNDO.
    
   llNew = NEW PaymConfig.
    
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP PaymConfig.Brand
           PaymConfig.PaymConfig
           PaymConfig.PaymType
           PaymConfig.PaymSrc
           PaymConfig.FromDate
           PaymConfig.ToDate
           PaymConfig.DebitAccNum
           PaymConfig.CreditAccNum
           PaymConfig.TaxRules
      WITH FRAME lis.

      PAUSE 0.
      DISP PaymConfig.Description WITH FRAME fDesc.
      
      fDispPaymTypeName(PaymConfig.PaymType).
      fDispPaymSrcName(PaymConfig.PaymSrc).
      fDispDebitAccName(PaymConfig.DebitAccNum).
      fDispCreditAccName(PaymConfig.CreditAccNum).
      
      
      IF NOT llNew THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[2] = 1644 WHEN PaymConfig.TaxRules
            ufk[3] = 1698 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.
      ELSE ASSIGN 
         toimi = 1
         llNew = FALSE.
         
      
      IF toimi = 1 THEN DO:
      
         ehto = 9.
         RUN ufkey.
      
         UPDATE
            PaymConfig.PaymType WHEN NEW PaymConfig
            PaymConfig.PaymSrc  WHEN NEW PaymConfig
            PaymConfig.FromDate 
            PaymConfig.ToDate  
            PaymConfig.DebitAccNum
            PaymConfig.CreditAccNum
            PaymConfig.TaxRules
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"PaymSrc,PaymType") > 0 
            THEN DO:

               lcField = FRAME-FIELD.
               RUN h-tmscodes(INPUT "Payment",  /* TableName */
                                    lcField,    /* FieldName */
                                    "AccRec",   /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ? THEN DO:
            
                  CASE lcField:
                  WHEN "PaymType" THEN DO:
                     DISPLAY INTEGER(lcCode) @ PaymConfig.PaymType 
                     WITH FRAME lis.   
                     fDispPaymTypeName(INTEGER(lcCode)).
                  END.   
                  WHEN "PaymSrc" THEN DO:
                     DISPLAY lcCode @ PaymConfig.PaymSrc 
                     WITH FRAME lis.   
                     fDispPaymSrcName(lcCode).
                  END.
                  END CASE. 
               END.
            
               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.


            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
               PAUSE 0.

               IF FRAME-FIELD = "PaymType" THEN DO:
                  IF INPUT PaymConfig.PaymType = ? THEN LEAVE.
                  
                  fDispPaymTypeName(INPUT INPUT PaymConfig.PaymType).
                  
                  IF lcPaymTypeName = "" THEN DO:
                     MESSAGE "Unknown payment type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "PaymSrc" THEN DO:
               
                  fDispPaymSrcName(INPUT INPUT PaymConfig.PaymSrc).
                   
                  IF lcPaymSrcName = ? THEN DO:
                     MESSAGE "Unknown payment source"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DebitAccNum" THEN DO:
                  fDispDebitAccName(INPUT INPUT PaymConfig.DebitAccNum).

                  IF lcDebitAccName = ? THEN DO:
                     MESSAGE "Unknown account"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "CreditAccNum" THEN DO:
                  fDispCreditAccName(INPUT INPUT PaymConfig.CreditAccNum).

                  IF lcCreditAccName = ? THEN DO:
                     MESSAGE "Unknown account"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

            END.
            
            APPLY LASTKEY.
         END.
 
         IF PaymConfig.PaymType = ? THEN LEAVE.   
              
         IF PaymConfig.FromDate = ? OR PaymConfig.ToDate = ? THEN DO:
            MESSAGE "Dates are mandatory."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         IF PaymConfig.ToDate < PaymConfig.FromDate THEN DO:
            MESSAGE "End date cannot be earlier than beginning date."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         /* overlapping time period */
         IF CAN-FIND(FIRST bConfig WHERE 
                           bConfig.Brand     = lcBrand AND
                           bConfig.PaymType  = PaymConfig.PaymType AND
                           bConfig.PaymSrc   = PaymConfig.PaymSrc  AND
                           bConfig.FromDate <= PaymConfig.ToDate   AND
                           bConfig.ToDate   >= PaymConfig.FromDate AND
                           RECID(bConfig) NE RECID(PaymConfig))      
         THEN DO:
            MESSAGE "Configuration has already been defined for"
                    "this time period"
            VIEW-AS ALERT-BOX ERROR. 
            NEXT. 
         END.

      END.

      ELSE IF toimi = 2 THEN RUN paymconftax(PaymConfig.PaymConfig).

      ELSE IF toimi = 3 THEN DO:

         ehto = 9.
         RUN ufkey.
         
         REPEAT ON ENDKEY UNDO, LEAVE:
            UPDATE PaymConfig.Description WITH FRAME fDesc.
            LEAVE.
         END.
      END.
      
   END.
   
   HIDE FRAME fDesc NO-PAUSE.
   
END PROCEDURE.

