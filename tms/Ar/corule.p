/* ----------------------------------------------------------------------
  MODULE .......: CoRule
  TASK .........: UPDATEs table CoRule
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 13.12.02
  CHANGED ......: 13.10.03/aam tokens
                  11.11.03/aam billcode & ccn to CoBasis
                  19.11.03/aam OpenDays
                  20.02.04/aam use ParentRule for showing template
                  01.06.04/aam CLIType,FtGrp
                  30.08.04/aam brand
                  27.06.05/aam CoRule.CLIType can contain a list
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable CoRule

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'corule'}

DEF BUFFER bCoRule  FOR CoRule.
DEF BUFFER bCoBasis FOR CoBasis.
DEF BUFFER bCoTarg  FOR CoTarg.
DEF BUFFER bCoShare FOR CoShare.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCoRule AS HANDLE NO-UNDO.
   lhCoRule = BUFFER CoRule:HANDLE.
   RUN StarEventInitialize(lhCoRule).

   DEFINE VARIABLE lhbCoRule AS HANDLE NO-UNDO.
   lhbCoRule = BUFFER bCoRule:HANDLE.
   RUN StarEventInitialize(lhbCoRule).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhCoRule).
   END.

END.


DEF VAR lcDesc     LIKE CoRule.RuleDesc        NO-UNDO.
DEF VAR liRule     LIKE CoRule.CoRuleID        NO-UNDO. 
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
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR lcBasis      AS CHAR                   NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR lcPoint      AS CHAR                   NO-UNDO. 
DEF VAR lcParent     AS CHAR                   NO-UNDO. 
DEF VAR llTemplate   AS LOG                    NO-UNDO. 
DEF VAR lcPayType    AS CHAR                   NO-UNDO.
DEF VAR lcFtgName    AS CHAR                   NO-UNDO.
DEF VAR lcRuleType   AS CHAR                   NO-UNDO. 
DEF VAR lcField      AS CHAR                   NO-UNDO. 

DEF VAR lcActivationSMS AS CHAR NO-UNDO. 
DEF VAR lcCreationSMS   AS CHAR NO-UNDO. 


form
    CoRule.Brand      FORMAT "X(2)" COLUMN-LABEL "Br"
    CoRule.CoRuleID
    CoRule.RuleDesc   FORMAT "X(16)"
    lcRuleType        FORMAT "X(13)" COLUMN-LABEL "Rule Type"
    lcPayType         FORMAT "X(8)"  COLUMN-LABEL "PayType"
    CoRule.CoFrom     FORMAT "99-99-99"
    CoRule.CoTo       FORMAT "99-99-99"
    CoRule.CommAmount FORMAT ">>>>>9" 
    CoRule.Priority   
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " COMMISSION RULES "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form            
    CoRule.CoRuleID    COLON 20 
    CoRule.RuleDesc    COLON 20 
    CoRule.RuleType   COLON 20 
       lcRuleType NO-LABEL FORMAT "X(30)" SKIP
       
    CoRule.CoFrom     COLON 20
       LABEL "Effective"
       VALIDATE(INPUT CoRule.CoFrom NE ?,
                "Date is mandatory")
    "-"            
    CoRule.CoTo    
       NO-LABEL 
       VALIDATE(INPUT CoRule.CoTo NE ?,
                "Date is mandatory") 
    CoRule.Priority SKIP 
    
    CoRule.PayType    COLON 20 
       lcPayType NO-LABEL FORMAT "X(30)" SKIP
    CoRule.CLIType    COLON 20 
       LABEL "CLI Types"
       FORMAT "X(50)" 
       HELP "List of CLI types that this rule applies to (EMPTY=all)" SKIP
    CoRule.AllowedDNI COLON 20
       FORMAT "X(50)"

    CoRule.MaxPendingDays COLON 20 
    CoRule.BasisType  COLON 20
       VALIDATE(INPUT CoRule.BasisType >= 1 AND
                INPUT CoRule.BasisType <= 3,
                "Valid values are 1-3")
       lcBasis NO-LABEL FORMAT "X(30)"     
    CoRule.CommPoint  COLON 20
       VALIDATE(INPUT CoRule.CommPoint < 3,
                "Valid choices are 0-2")
       lcPoint NO-LABEL FORMAT "X(30)"                
    
    CoRule.OpenDays   COLON 20                
    CoRule.AmtBilled  COLON 20
       LABEL "Min.Amount Paid "
       HELP "Minimum amount paid (billed or topup)" SKIP

    CoRule.CommAmount COLON 20 
    CoRule.CoNoInst 
       LABEL "Divided Into Months"
       HELP "In how many months is commission amount divided to" SKIP
       
    CoRule.FtGrp COLON 20
       LABEL "Create FAT"
       HELP "FAT group that is used for creating FAT from events"
       FORMAT "X(12)" 
    lcFtgName NO-LABEL FORMAT "X(30)" SKIP
    CoRule.PPReqPrefix COLON 20
       LABEL "Create Topup"
    CoRule.PPSource FORMAT "X(12)" 
       NO-LABEL SKIP
    CoRule.CreationSMS COLON 20 FORMAT "X(20)"
       lcCreationSMS NO-LABEL FORMAT "X(30)" SKIP
    CoRule.ActivationSMS COLON 20 FORMAT "X(20)"
       lcActivationSMS NO-LABEL FORMAT "X(30)" SKIP
WITH OVERLAY ROW 1 centered
    COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS FRAME lis.

form            
    CoRule.RuleDesc    COLON 20 
    CoRule.CoFrom     COLON 20
       VALIDATE(INPUT CoRule.CoFrom NE ?,
                "Date is mandatory")
    CoRule.CoTo       COLON 20
       VALIDATE(INPUT CoRule.CoTo NE ?,
                "Date is mandatory")
WITH  OVERLAY ROW 7 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " Copy a New Rule From Template " 
    SIDE-LABELS 
    FRAME fTemplate.

{Func/brand.i}

form /* seek  */
    "Brand :" lcBrand skip
    "RuleID:" liRule
    HELP "Enter rule ID "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ID"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* seek  */
    "Brand .....:" lcBrand skip
    "Description:" lcDesc
    HELP "Enter description"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND description "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fBasisType RETURNS LOGICAL
   (iiType AS INT):

   lcBasis = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                              "CoRule",
                              "BasisType",
                              STRING(iiType)).
                              
   DISPLAY lcBasis WITH FRAME lis.
END FUNCTION.

FUNCTION fCommPoint RETURNS LOGICAL
   (iiPoint AS INT).

   lcPoint = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                              "CoRule",
                              "CommPoint",
                              STRING(iiPoint)).
    
   DISPLAY lcPoint WITH FRAME lis.
END FUNCTION.

FUNCTION fFatGroup RETURNS LOGIC
   (icGroup AS CHAR):
   
   lcFtgName = "".
   IF icGroup > "" THEN DO:
      FIND FIRST FatGroup WHERE 
                 FatGroup.Brand = gcBrand AND
                 FatGroup.FTGrp = icGroup 
      NO-LOCK NO-ERROR.
      IF AVAILABLE FatGroup THEN
         lcFtgName = FatGroup.FtgName.
   END.
   
   DISP lcFtgName WITH FRAME lis.
END FUNCTION.
 
FUNCTION fPayType RETURNS LOGICAL
   (iiType AS INT):

   lcPayType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "CLIType",
                                "PayType",
                                STRING(iiType)).
                              
END FUNCTION.

FUNCTION fRuleType RETURNS LOGICAL
   (iiType AS INT):

   lcRuleType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "CoRule",
                                 "RuleType",
                                 STRING(iiType)).
                              
END FUNCTION.

FUNCTION fDispCreationSMS RETURNS LOGIC
   (icSMSText AS CHAR):

   lcCreationSMS = "".
   
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand   AND 
             InvText.Target    = "SMS"     AND
             InvText.KeyValue  = icSMSText AND
             InvText.FromDate <= TODAY     AND
             InvText.ToDate   >= TODAY     AND
             InvText.Language  = 1:
      lcCreationSMS = InvText.TxtTitle.
      IF lcCreationSMS = "" THEN 
         lcCreationSMS = SUBSTRING(InvText.InvText,1,30).
   END.
                                  
   DISP lcCreationSMS WITH FRAME lis.
       
END FUNCTION.

FUNCTION fDispActivationSMS RETURNS LOGIC
   (icSMSText AS CHAR):

   lcActivationSMS = "".
   
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand   AND 
             InvText.Target    = "SMS"     AND
             InvText.KeyValue  = icSMSText AND
             InvText.FromDate <= TODAY     AND
             InvText.ToDate   >= TODAY     AND
             InvText.Language  = 1:
      lcActivationSMS = InvText.TxtTitle.
      IF lcActivationSMS = "" THEN 
         lcActivationSMS = SUBSTRING(InvText.InvText,1,30).
   END.
                                  
   DISP lcActivationSMS WITH FRAME lis.
       
END FUNCTION.




cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE CoRule THEN ASSIGN
   Memory       = recid(CoRule)
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

   IF must-add THEN DO:  /* Add a CoRule  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR CoRule.RuleDesc CoRule.RuleType CoRule.PayType
           EDITING:
              READKEY.
         
              IF KEYLABEL(LASTKEY) = "F9" AND 
                 LOOKUP(FRAME-FIELD,"RuleType,PayType") > 0 THEN DO:
            
                 IF FRAME-FIELD = "PayType" THEN DO:
                    RUN h-tmscodes(INPUT "CLIType",     /* TableName */
                                         "PayType",  /* FieldName */
                                         "MobSub", /* GroupCode */
                                   OUTPUT lcCode).
              
                    IF lcCode ne "" AND lcCode NE ? THEN DO:
                       DISPLAY INTEGER(lcCode) ;& CoRule.PayType
                       WITH FRAME lis.   
                    END.
                 END.
                                      
                 ELSE IF FRAME-FIELD = "RuleType" THEN DO:
                    RUN h-tmscodes(INPUT "CoRule",     /* TableName */
                                         "RuleType",  /* FieldName */
                                         "Commission", /* GroupCode */
                                   OUTPUT lcCode).
              
                    IF lcCode ne "" AND lcCode NE ? THEN DO:
                       DISPLAY INTEGER(lcCode) ;& CoRule.RuleType
                       WITH FRAME lis.   
                    END.
                 END.
 
                 ehto = 9.
                 RUN ufkey.
                 NEXT. 
              END.
           
              APPLY LASTKEY.
           END.

           IF INPUT FRAME lis CoRule.RuleDesc = "" 
           THEN LEAVE add-row.

           CREATE CoRule.
           ASSIGN
           CoRule.Brand      = gcBrand 
           CoRule.CoRuleID   = NEXT-VALUE(CoRule)
           CoRule.CustNum    = 0
           CoRule.RuleDesc   = INPUT FRAME lis CoRule.RuleDesc
           CoRule.PayType    = INPUT FRAME lis CoRule.PayType
           CoRule.RuleType   = INPUT FRAME lis CoRule.RuleType
           CoRule.BasisType  = 3
           CoRule.CommPoint  = 2
           CoRule.QtyPaidInv = 1.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCoRule).

           ASSIGN
           Memory = recid(CoRule)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE CoRule THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CoRule WHERE recid(CoRule) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CoRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CoRule).
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
        ufk   = 0
        ufk[1]= 816  
        ufk[4]= 1872
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8]= 8 
        ehto = 3 ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CoRule.CoRuleID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoRule.CoRuleID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CoRule.RuleDesc ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoRule.RuleDesc WITH FRAME sel.
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
        FIND CoRule WHERE recid(CoRule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CoRule THEN
              ASSIGN FIRSTrow = i Memory = recid(CoRule).
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
           IF NOT AVAILABLE CoRule THEN DO:
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
                rtab[1] = recid(CoRule)
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
           IF NOT AVAILABLE CoRule THEN DO:
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
              rtab[FRAME-DOWN] = recid(CoRule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CoRule WHERE recid(CoRule) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CoRule THEN DO:
           Memory = recid(CoRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CoRule THEN Memory = recid(CoRule).
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
           FIND CoRule WHERE recid(CoRule) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* search */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO:

        ASSIGN
        ufk = 0
        ufk[1]= 35  ufk[2]= 717  
        ufk[8]= 8 
        ehto = 0
        ufkey = TRUE.
        RUN ufkey.

        /* Search BY column 1 */
        IF toimi = 1 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". run ufcolor.
           ehto = 9. RUN ufkey. ufkey = TRUE.
           CLEAR FRAME f1.
           DISPLAY lcBrand WITH FRAME F1.
           UPDATE lcBrand WHEN gcAllBrand
                  liRule WITH FRAME f1.
           HIDE FRAME f1 NO-PAUSE.

           IF liRule > 0 THEN DO:
              FIND FIRST CoRule WHERE 
                         CoRule.Brand = lcBrand AND
                         CoRule.CoRuleID >= liRule
              NO-LOCK NO-ERROR.

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
           
        END. /* Search-1 */

        /* Search BY column 2 */
        ELSE IF toimi = 2 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". run ufcolor.
           ehto = 9. RUN ufkey. ufkey = TRUE.
           CLEAR FRAME f2.
           DISPLAY lcBrand WITH FRAME F2.
           UPDATE lcBrand WHEN gcAllBrand
                  lcDesc WITH FRAME f2.
           HIDE FRAME f2 NO-PAUSE.

           IF lcDesc > "" THEN DO:
              FIND FIRST CoRule WHERE 
                         CoRule.Brand = lcBrand AND
                         CoRule.RuleDesc >= lcDesc
              NO-LOCK NO-ERROR.

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              NEXT LOOP.

           END.
        END. /* Search-2 */

     END.

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:  /* BASIS */
       {Syst/uright2.i}
       RUN local-find-this (FALSE).
       IF AVAILABLE CoRule 
       THEN RUN cobasis (CoRule.CoRuleID). 
       ufkey = true. 
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* TARGETS */
       {Syst/uright2.i}
       RUN local-find-this (FALSE).
       IF AVAILABLE CoRule 
       THEN RUN cotarg (CoRule.CoRuleID, "rule"). 
       ufkey = true. 
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
       CoRule.CoRuleID CoRule.RuleDesc .

       RUN local-find-NEXT.
       IF AVAILABLE CoRule THEN Memory = recid(CoRule).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CoRule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CoRule).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       IF CAN-FIND(FIRST CoTarg OF CoRule) OR
          CAN-FIND(FIRST CoBasis OF CoRule) 
       THEN DO:
          MESSAGE "There are commission targets/basis defined for this rule."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT LOOP.
       END.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CoRule.CoRuleID CoRule.RuleDesc .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCoRule).

           DELETE CoRule.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CoRule /* srule */ ) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* copy from template */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
     
        RUN local-find-this(FALSE).
        
        IF NOT AVAILABLE CoRule OR CoRule.RuleType NE 1 THEN DO:
           MESSAGE "This is not a template rule."
           VIEW-AS ALERT-BOX
           ERROR.
           NEXT.
        END.
     
        ehto = 9.
        RUN ufkey.
        ufkey = TRUE.
        
        REPEAT WITH FRAME fTemplate ON ENDKEY UNDO, NEXT LOOP:
           PAUSE 0.
           PROMPT-FOR CoRule.RuleDesc
                      CoRule.CoFrom 
                      CoRule.CoTo
           WITH FRAME fTemplate.
           LEAVE.
        END.
        
        IF INPUT FRAME fTemplate CoRule.RuleDesc = "" THEN NEXT LOOP.
        
        ok = TRUE.
        MESSAGE "Copy a new rule from this template ?"
        VIEW-AS ALERT-BOX 
        QUESTION
        BUTTONS YES-NO
        TITLE " Start Copying "
        set ok.
        
        IF ok THEN DO TRANS:
        
           FIND bCoRule WHERE RECID(bCoRule) = RECID(CoRule) NO-LOCK.
           CREATE CoRule.
           ASSIGN 
           CoRule.Brand     = gcBrand 
           CoRule.CoRuleID  = NEXT-VALUE(CoRule)
           CoRule.RuleDesc  = INPUT FRAME fTemplate CoRule.RuleDesc
           CoRule.CoFrom    = INPUT FRAME fTemplate CoRule.CoFrom
           CoRule.CoTo      = INPUT FRAME fTemplate CoRule.CoTo
           CoRule.RuleType  = 0
           Memory           = RECID(CoRule).
           
           BUFFER-COPY bCoRule 
              EXCEPT CoRuleId RuleDesc CoFrom CoTo RuleType
              TO CoRule.
              
           FOR EACH bCoBasis OF bCoRule NO-LOCK:
              CREATE CoBasis.
              BUFFER-COPY bCoBasis EXCEPT CoRuleID TO CoBasis.
              CoBasis.CoRuleID = CoRule.CoRuleID.
           END.
           FOR EACH bCoTarg OF bCoRule NO-LOCK:
              CREATE CoTarg.
              BUFFER-COPY bCoTarg EXCEPT CoTargID CoRuleID TO CoTarg.
              ASSIGN CoTarg.CoTargID = NEXT-VALUE(CommSeq)
                     CoTarg.CoRuleID = CoRule.CoRuleID.
                     
              FOR EACH bCoShare NO-LOCK WHERE
                       bCoShare.CoTargID = bCoTarg.CoTargID:
                 CREATE CoShare.
                 BUFFER-COPY bCoShare EXCEPT CoTargId TO CoShare.
                 CoShare.CoTargID = CoTarg.CoTargID.
              END.
           END.
           
           must-print = TRUE.
           NEXT LOOP.
        END.      
        
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCoRule).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       must-print = FALSE.
       
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCoRule).

       RUN local-disp-row.
       xrecid = recid(CoRule).
       
       IF must-print THEN NEXT LOOP.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CoRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CoRule) must-print = TRUE.
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
      FIND CoRule WHERE recid(CoRule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CoRule WHERE recid(CoRule) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CoRule USE-INDEX CoRuleID
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST CoRule USE-INDEX RuleDesc
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CoRule USE-INDEX CoRuleID
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST CoRule USE-INDEX RuleDesc
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CoRule USE-INDEX CoRuleID
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT CoRule USE-INDEX RuleDesc
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CoRule USE-INDEX CoRuleID
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV CoRule USE-INDEX RuleDesc
          WHERE CoRule.Brand = gcBrand
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       CoRule.Brand 
       CoRule.CoRuleID
       CoRule.RuleDesc
       lcRuleType
       lcPayType
       CoRule.CoFrom
       CoRule.CoTo
       CoRule.CommAmount
       CoRule.Priority
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    llTemplate = (CoRule.RuleType = 1).
    fPayType(CoRule.PayType).
    fRuleType(CoRule.RuleType).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE. 
      
      lcCustName = "".
      IF CoRule.CustNum > 0 THEN DO:
         FIND Customer NO-LOCK WHERE 
              Customer.CustNum = CoRule.CustNum NO-ERROR.
         IF AVAILABLE Customer THEN lcCustName = Customer.CustName.
      END. 

      lcParent = "".
      IF CoRule.ParentRule > 0 THEN DO:
         FIND bCoRule NO-LOCK WHERE 
              bCoRule.Brand    = gcBrand AND  
              bCoRule.CoRuleId = CoRule.ParentRule NO-ERROR.
         IF AVAILABLE bCoRule THEN lcParent = bCoRule.RuleDesc.
      END.
      
      fBasisType(CoRule.BasisType).
      fCommPoint(CoRule.CommPoint).
      fFatGroup(CoRule.FtGrp).
      fDispCreationSMS(CoRule.CreationSMS).
      fDispActivationSMS(CoRule.ActivationSMS).
      
      DISP CoRule.CoRuleID
           CoRule.RuleDesc
           CoRule.RuleType lcRuleType
           CoRule.PayType lcPayType
           CoRule.CLIType
           CoRule.AllowedDNI
           CoRule.MaxPendingDays
           CoRule.CoFrom
           CoRule.CoTo
           CoRule.Priority
           CoRule.BasisType
           CoRule.CommPoint
           CoRule.CommAmount
           CoRule.CoNoInst
           CoRule.FtGrp
           CoRule.PPReqPrefix
           CoRule.PPSource
           CoRule.OpenDays
           CoRule.AmtBilled
           CoRule.CreationSMS
           CoRule.ActivationSMS
      WITH FRAME lis.
      
      IF NEW CoRule THEN toimi = 1.
      ELSE DO: 
         ASSIGN 
            ehto   = 0
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND gcHelpParam = ""
            ufk[3] = 1550 WHEN lcRight = "RW" AND gcHelpParam = ""
            ufk[8] = 8.
         RUN ufkey.
      END.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT CoRule EXCLUSIVE-LOCK.
            
         ehto = 9.
         RUN ufkey.
      
         UPDATE
            CoRule.RuleDesc WHEN NOT NEW CoRule
            CoRule.CoFrom
            CoRule.CoTo
            CoRule.Priority
            CoRule.CLIType
            CoRule.AllowedDNI
            CoRule.MaxPendingDays
            CoRule.OpenDays
            CoRule.AmtBilled
            CoRule.CommAmount
            CoRule.CoNoInst
            CoRule.FtGrp       WHEN CoRule.PayType = 1
            CoRule.PPReqPrefix WHEN CoRule.PayType = 2
            CoRule.PPSource    WHEN CoRule.PayType = 2
            CoRule.CreationSMS
            CoRule.ActivationSMS
         WITH FRAME lis EDITING:
         
            READKEY.
         
            IF KEYLABEL(LASTKEY) = "F9" AND 
               (LOOKUP(FRAME-FIELD,"PPReqPrefix,PPSource") > 0 OR
                INDEX(FRAME-FIELD,"SMS") > 0)
            THEN DO:
            
               IF FRAME-FIELD = "PPReqPrefix" THEN DO:
               
                  RUN h-tmscodes(INPUT "PrepaidRequest",     /* TableName */
                                       "PPReqPrefix",  /* FieldName */
                                       "Prepaid", /* GroupCode */
                                 OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& CoRule.PPReqPrefix
                     WITH FRAME lis.   
                  END.
               END.
                                      
               ELSE IF FRAME-FIELD = "PPSource" THEN DO:
               
                  RUN h-tmscodes(INPUT "PrepaidRequest",     /* TableName */
                                       "Source",  /* FieldName */
                                       "Prepaid", /* GroupCode */
                                 OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& CoRule.PPSource
                     WITH FRAME lis.   
                  END.
               END.

               ELSE IF INDEX(FRAME-FIELD,"SMS") > 0 THEN DO:
                  ASSIGN
                     gcHelpParam = "tmrlimit"
                     si-recid    = ?
                     lcField     = FRAME-FIELD.
               
                  RUN invotxt("SMS","").
        
                  gcHelpParam = "".
                        
                  IF si-recid NE ? THEN DO:
                     FIND InvText WHERE RECID(InvText) = si-recid
                        NO-LOCK NO-ERROR.
                     IF AVAILABLE InvText THEN DO WITH FRAME lis:
                        IF lcField = "CreationSMS" THEN 
                           DISP InvText.KeyValue @ CoRule.CreationSMS.
                        ELSE DISP InvText.KeyValue @ CoRule.ActivationSMS.
                     END.
                  END.   
               END.
                
               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.
            
            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "CLIType" THEN DO:
                  IF INPUT CoRule.CLIType > "" THEN DO:
                  
                     DO i = 1 TO NUM-ENTRIES(INPUT CoRule.CLIType):
                        
                        IF NOT CAN-FIND(CLIType WHERE 
                             CLIType.Brand   = gcBrand AND
                             CLIType.CLIType = ENTRY(i,INPUT CoRule.CLIType))
                        THEN DO:
                           MESSAGE "Unknown CLI type" 
                                   ENTRY(i,INPUT CoRule.CLIType)
                           VIEW-AS ALERT-BOX ERROR.
                           i = -1.
                           LEAVE.
                        END. 
                     END.
                           
                     IF i < 0 THEN NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "FtGrp" THEN DO:
                  fFatGroup(INPUT INPUT CoRule.FtGrp).
                  
                  IF INPUT CoRule.FtGrp > "" AND lcFtgName = "" THEN DO: 
                     MESSAGE "Unknown FAT group" VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "PPReqPrefix" THEN DO:
                  IF INPUT CoRule.PPReqPrefix > "" THEN DO:
                     IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                         "PrepaidRequest",
                                         "PPReqPrefix",
                                         INPUT INPUT CoRule.PPReqPrefix) = ""
                     THEN DO:
                        MESSAGE "Unknown prefix"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
               END.

               ELSE IF FRAME-FIELD = "PPSource" THEN DO:
                  IF INPUT CoRule.PPSource > "" THEN DO:
                     IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                         "PrepaidRequest",
                                         "Source",
                                         INPUT INPUT CoRule.PPSource) = ""
                     THEN DO:
                        MESSAGE "Unknown source"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
               END.
                 
               ELSE IF FRAME-FIELD = "CreationSMS" THEN DO:
                  IF INPUT CoRule.CreationSMS = "" THEN 
                      DISP "" @ lcCreationSMS WITH FRAME lis.
 
                  ELSE DO: 
                     fDispCreationSMS(INPUT INPUT CoRule.CreationSMS).

                     IF lcCreationSMS = "" THEN DO:
                        MESSAGE "Unknown text"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
                     
               END.
               
               ELSE IF FRAME-FIELD = "ActivationSMS" THEN DO:
                   IF INPUT CoRule.ActivationSMS = "" THEN 
                      DISP "" @ lcActivationSMS WITH FRAME lis.
 
                  ELSE DO: 
                     fDispActivationSMS(INPUT INPUT CoRule.ActivationSMS).

                     IF lcActivationSMS = "" THEN DO:
                        MESSAGE "Unknown text"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
               
               END.
            END.
          
            APPLY LASTKEY.
      
         END. /* EDITING */

         LEAVE MaintMenu.   
      END.

      /* copy to a new rule */
      ELSE IF toimi = 3 THEN DO:
 
         ehto = 9.
         RUN ufkey.
        
         REPEAT WITH FRAME fTemplate ON ENDKEY UNDO, LEAVE:
            PAUSE 0.
            DISPLAY "" @ CoRule.RuleDesc WITH FRAME fTemplate.
            PROMPT-FOR CoRule.RuleDesc
                       CoRule.CoFrom 
                       CoRule.CoTo
            WITH FRAME fTemplate.
            LEAVE.
         END.

         HIDE FRAME fTemplate NO-PAUSE.
         
         IF INPUT FRAME fTemplate CoRule.RuleDesc = "" THEN LEAVE.
        
         ok = TRUE.
         MESSAGE "Copy a new rule using this one as a template?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         TITLE " Start Copying "
         SET ok.
        
         IF ok THEN DO TRANS:
        
            CREATE bCoRule.
            ASSIGN 
               bCoRule.CoRuleID  = NEXT-VALUE(CoRule)
               bCoRule.RuleDesc  = INPUT FRAME fTemplate CoRule.RuleDesc
               bCoRule.CoFrom    = INPUT FRAME fTemplate CoRule.CoFrom
               bCoRule.CoTo      = INPUT FRAME fTemplate CoRule.CoTo
               must-print        = TRUE.
           
            BUFFER-COPY CoRule 
               EXCEPT CoRuleId RuleDesc CoFrom CoTo TO bCoRule.

            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhbCoRule).
               
            FOR EACH bCoBasis OF CoRule NO-LOCK:
               CREATE CoBasis.
               BUFFER-COPY bCoBasis EXCEPT CoRuleID TO CoBasis.
               CoBasis.CoRuleID = bCoRule.CoRuleID.
            END.
         END.
        
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.

