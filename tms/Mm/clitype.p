/* ----------------------------------------------------------------------
  MODULE .......: clitype.p
  TASK .........: Update table CLIType
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 28.03.03
  CHANGED ......: 10.06.2003 jp Priceplan -> rateplan
                  12.08.2003 tk maxorder = 1
                  04.09.2003 jp brand
                  08.10.2003 jp Watchdog Limit
                  10.10.2003 jp PenaltyFee,ContrType
                  06.11.2003 aam DiscPlan not mandatory
                  06.02.2004 jp  Custnum for memo
                  06.08.2004 aam WebDisp and WebInfo added 
                  07.12.2004 aam service packages (ctservpac)
                  04.01.2005 aam FeeModels removed
                  21.12.2006 aam BillCode, MinimAmt
                  03.01.2007 aam Accounts,
                                 translations (invlang)
                  09.03.2007 jp  FeeModel               
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Clitype

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CLIType'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

{Func/lib/eventlog.i}

   DEFINE VARIABLE lhCLIType AS HANDLE NO-UNDO.
   lhCLIType = BUFFER CLIType:HANDLE.
   RUN StarEventInitialize(lhCLIType).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCLIType).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CLIType      LIKE CLIType.CLIType        NO-UNDO.
DEF VAR CLIName      LIKE CLIType.CLIName        NO-UNDO.
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
DEF VAR SPName       AS CHAR                   NO-UNDO.
DEF VAR DPName       LIKE Discplan.DPName      NO-UNDO.
DEF VAR FeeName1     LIKE Feemodel.FeeName     NO-UNDO.
DEF VAR FeeName2     LIKE Feemodel.FeeName     NO-UNDO.
DEF VAR FeeName3     LIKE FeeModel.FeeName     NO-UNDO.
DEF VAR PLName       LIKE Pricelist.PLName     NO-UNDO.

DEF VAR lcPerName    AS CHAR NO-UNDO.
DEF VAR lcUbName     AS CHAR NO-UNDO.
DEF VAR lcAccName    AS CHAR NO-UNDO. 
DEF VAR lcPayType    AS CHAR NO-UNDO.
DEF VAR lcCode       AS CHAR NO-UNDO.
DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcWebStatus  AS CHAR NO-UNDO.
DEF VAR lcUsageType  AS CHAR NO-UNDO.
DEF VAR lcBundleType AS CHAR NO-UNDO.
DEF VAR lcLineType   AS CHAR NO-UNDO.
DEF VAR lcFixedLineType  AS CHAR NO-UNDO.
DEF VAR lcTariffType     AS CHAR NO-UNDO. 

form
    CliType.Brand      FORMAT "X(2)" COLUMN-LABEL "Br"
    CLIType.CLIType    FORMAT "X(12)"    
    CLIType.CLIName    format "x(18)"
    CLIType.PricePlan  COLUMN-LABEL "RatePlan" FORMAT "X(13)"
    CLIType.DiscPlan   COLUMN-LABEL "Disc.Plan" FORMAT "X(9)"
    lcPayType          FORMAT "X(10)" COLUMN-LABEL "PayType"
    CliType.BillTarget COLUMN-LABEL "B.Target"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  SUBSCRIPTION TYPE MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    "CLIType ......:"  CLIType.CLIType FORMAT "X(12)" SKIP
    "Name .........:"  CLIType.CLIName              SKIP
    "Base bundle...:"  CLIType.BaseBundle SKIP
    "Payment Type .:"  CLIType.PayType
       HELP "1=Postpaid, 2=Prepaid"
       lcPayType NO-LABEL FORMAT "X(15)"
    "Usage Type....:"  AT 35 CLIType.UsageType
        HELP "1=Voice, 2=Data"
        lcUsageType NO-LABEL FORMAT "X(15)" SKIP
    "Rate plan ....:"  CLIType.PricePlan FORMAT "X(20)" PLName   SKIP
    "Disc. plan ...:"  CLIType.DiscPlan    DPName   SKIP
    
    "Service pack .:"  CliType.ServicePack FORMAT "x(2)" 
      SPName FORMAT "x(10)" 
    "Service Class :"  AT 35 CliType.ServiceClass     SKIP
    
    "BillingTarget :"  Clitype.BillTarget           SKIP
    "DOC1 Code ....:"  Clitype.ContrType 
       HELP "Used in invoice printing"
       FORMAT ">9" SKIP
    "A/R Account ..:" 
       CLIType.ARAccNum     
       FORMAT ">>>>>>>9"
       lcAccName NO-LABEL FORMAT "X(30)" SKIP

    "Bundle Type...:" CLIType.BundleType
        HELP "Bundle based type" SKIP
    "Commercial Fee:"  CLIType.CommercialFee
        HELP "Commercial Fee"
    "Comparison Fee:"  AT 35 CLIType.CompareFee
        HELP "Fee used for determining penalty fee in STC/BTC" SKIP
    "Min. Billing .:"  CLIType.MinimAmt 
        HELP "Minimum consumption"
    "Billing Item .:"  AT 35 CLIType.BillCode
        FORMAT "X(16)"
        HELP "Billing item for minimum consumption" SKIP
    "Web Status....:"  CLIType.WebStatusCode FORMAT "9"
        HELP "0=Inactive, 1=Active, 2=Retired, 3=Hidden"
        lcWebStatus NO-LABEL FORMAT "X(15)"
    "STC Status ...:"  AT 35 CLIType.StatusCode FORMAT "9"
        HELP "0=Inactive, 1=Active, 2=Retired"
        lcStatus NO-LABEL FORMAT "X(15)" SKIP
    "LineType......:"  CLIType.LineType
        HELP "0=Entry, 1=Main, 2=Additional"
        lcLineType NO-LABEL FORMAT "X(15)"
    "Fixed LineType:"  AT 35 CLIType.FixedLineType
        HELP "1=ADSL, 2=FIBER"
        lcFixedLineType NO-LABEL FORMAT "X(15)" SKIP
    "Tariff Type...:" CLIType.TariffType  
        HELP "0=MobileOnly, 1=Convergent, 2=FixedOnly, 3=Fusion"
        lcTariffType NO-LABEL FORMAT "X(15)" 

WITH OVERLAY ROW 2 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   ac-hdr  WITH no-labels
FRAME lis.


form /* seek  CLIType */
   "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "CliType ..:"  CLIType FORMAT "X(12)"               
    HELP "Enter Code of Cli Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fAccName RETURNS CHARACTER
   (iiAccNum AS INT).

   IF iiAccNum = 0 THEN RETURN "".

   FIND Account WHERE 
        Account.Brand  = gcBrand AND
        Account.AccNum = iiAccNum NO-LOCK NO-ERROR.
   IF AVAILABLE Account THEN RETURN Account.AccName.
   ELSE RETURN "?".

END FUNCTION.

FUNCTION fPayTypeName RETURNS LOGIC
   (iiPayType AS INT):

   lcPayType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "CLIType",
                                "PayType",
                                STRING(iiPayType)).
END FUNCTION.

FUNCTION fStatusName RETURNS LOGIC
   (iiStatusCode AS INT):

   lcStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "CLIType",
                               "StatusCode",
                               STRING(iiStatusCode)).
END FUNCTION.

FUNCTION fWebStatusName RETURNS LOGIC
   (iiStatusCode AS INT):

   lcWebStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "CLIType",
                                  "WebStatusCode",
                                  STRING(iiStatusCode)).
END FUNCTION.

FUNCTION fLineType RETURNS LOGIC
   (iiLineType AS INT):

   lcLineType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "CLIType",
                                  "LineType",
                                  STRING(iiLineType)).
END FUNCTION.

FUNCTION fFixedLineType RETURNS LOGIC
   (iiFixedLineType AS INT):

   lcFixedLineType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "CLIType",
                                      "FixedLineType",
                                      STRING(iiFixedLineType)).
END FUNCTION.

FUNCTION fTariffType RETURNS LOGIC
   (iiTariffType AS INT):

   lcTariffType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "CLIType",
                                   "TariffType",
                                    STRING(iiTariffType)).
END FUNCTION.   

FUNCTION fUsageType RETURNS LOGIC
   (iiUsageType AS INT):

   lcUsageType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "CLIType",
                                      "UsageType",
                                      STRING(iiUsageType)).
END FUNCTION.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST CLIType Where 
           CliType.Brand = lcBrand 
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN ASSIGN
   Memory       = recid(CLIType)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No CLI types available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a CLIType  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR CLIType.CLIType
           VALIDATE
              (CLIType.CLIType NOT ENTERED OR
              NOT CAN-FIND(CLIType using  CLIType.CLIType WHERE 
                           CLIType.Brand  = gcBrand ),
              "CLIType " + string(INPUT CLIType.CLIType) +
              " already exists !").
           IF INPUT FRAME lis CLIType.CLIType = "" THEN 
           LEAVE add-row.
           CREATE CLIType.
           ASSIGN
           Clitype.Brand   = lcBrand
           CLIType.CLIType = INPUT FRAME lis CLIType.CLIType.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCLIType).

           ASSIGN
           Memory = recid(CLIType)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CLIType WHERE 
                 clitype.Brand = lcBrand 
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CLIType THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CLIType WHERE recid(CLIType) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CLIType THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CLIType).
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
        ufk[1]= 35  ufk[2]= 927 ufk[3]= 253 ufk[4]= 814
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CLIType.CLIType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CLIType.CLIType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CLIType.CLIName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CLIType.CLIName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND CLIType WHERE recid(CLIType) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CLIType THEN
              ASSIGN FIRSTrow = i Memory = recid(CLIType).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE CLIType THEN DO:
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
                rtab[1] = recid(CLIType)
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
           IF NOT AVAILABLE CLIType THEN DO:
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
              rtab[FRAME-DOWN] = recid(CLIType).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CLIType WHERE recid(CLIType) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CLIType THEN DO:
           Memory = recid(CLIType).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CLIType THEN Memory = recid(CLIType).
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
           FIND CLIType WHERE recid(CLIType) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISP CLIType With FRAME f1.
       SET  lcBrand WHEN gcallbrand = TRUE CLIType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CLIType ENTERED THEN DO:
          IF lcBrand ne "*" THEN 
          FIND FIRST CLIType WHERE 
                     CLIType.CLIType >= CLIType AND
                     CliType.Brand    = lcBrand
           NO-LOCK NO-ERROR.
           ELSE            
           FIND FIRST CLIType WHERE
                     CLIType.CLIType >= CLIType 
          /* srule */ NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* service packages */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        RUN local-find-this(FALSE).

        RUN Mm/ctservpac.p (CLIType.CLIType).
        
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* translations */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
        RUN local-find-this(FALSE).
        RUN Mc/invlang.p(9,CLIType.CLIType).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* memo */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN Mc/memo.p(INPUT 0,
                 INPUT "CLIType",
                 INPUT STRING(CLIType.CLIType),
                 INPUT "CLI Type").
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
       CLIType.CLIType CLIType.CLIName CliType.Brand.

       RUN local-find-NEXT.
       IF AVAILABLE CLIType THEN Memory = recid(CLIType).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CLIType THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CLIType).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CLIType.CLIType CLIType.CLIName CliType.Brand.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCLIType).

           DELETE CLIType.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CLIType
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

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CLIType.CLIType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(CLIType).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CLIType) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CLIType) must-print = TRUE.
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
      FIND CLIType WHERE recid(CLIType) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CLIType WHERE recid(CLIType) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CLIType
      WHERE CLiType.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CLIType
      WHERE CLiType.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CLIType
      WHERE CLiType.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CLIType
      WHERE CLiType.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others. 
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
          CliType.Brand
          CLIType.CLIType 
          CLIType.CLIName
          CLIType.BillTarget
          CLIType.PricePlan
          CLIType.DiscPlan
          lcPayType
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   
   fPayTypeName(CLIType.PayType).

   fStatusName(CLIType.StatusCode).

   fWebStatusName(CLIType.WebStatusCode).

   fLineType(CLIType.LineType).

   fFixedLineType(CLIType.FixedLineType).

   fTariffType(CLIType.TariffType).
   
   fUsageType(CLIType.UsageType).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      FIND DiscPlan WHERE
           DiscPlan.Brand    = gcBrand AND 
           DiscPlan.DiscPlan = CLIType.DiscPlan
      NO-LOCK NO-ERROR.
      IF AVAIL Discplan THEN DPName = Discplan.DPName.
      ELSE                   DPName = "".

      FIND FIRST TMSCodes WHERE
                 TMSCodes.TableName = "CliType"     AND
                 TmsCodes.FieldName = "ServicePack" AND
                 TmsCodes.CodeGroup = "Profile"     AND
                 TMSCodes.CodeValue = CliType.Servicepack
      NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN SPName = TMSCodes.CodeName.               
 
      FIND RatePlan WHERE
           RatePlan.Brand    = gcBrand AND
           RatePlan.RatePlan = CLIType.Priceplan
      NO-LOCK NO-ERROR.
      IF AVAIL RatePlan THEN PLName = RatePlan.RPName.
      ELSE                   PLName = "".

      RUN local-find-others.

      DISP
          CLIType.CLIType
          CLIType.BaseBundle
          CLIType.CLIName
          CLIType.PayType lcPayType
          CLIType.UsageType lcUsageType
          CLIType.PricePlan
          PLName
          CLIType.DiscPlan
          DPName
          CLIType.ServicePack
          SPName
          CliType.ServiceClass
          CLIType.BillTarget
          CliType.Contrtype
          CLIType.ARAccNum
          CLIType.BundleType
          CLIType.CommercialFee
          CLIType.CompareFee
          CLIType.MinimAmt
          CLIType.BillCode
          CLIType.WebStatusCode lcWebStatus
          CLIType.StatusCode lcStatus
          CLIType.LineType lcLineType
          CLIType.FixedLineType lcFixedLineType
          CLIType.TariffType  lcTariffType
      WITH FRAME lis.

      ASSIGN lcAccName = fAccName(CLIType.ARAccNum)
             lcPerName = fAccName(CLIType.PerAccNum)
             lcUBName  = fAccName(CLIType.UnbillAccNum).

      DISPLAY lcAccName WITH FRAME lis.

      ASSIGN
         ufk    = 0
         ufk[1] = 7 WHEN lcRight = "RW"
         ufk[8] = 8
         ehto   = 0.
      RUN Syst/ufkey.p.
         
      IF toimi = 1 THEN 
      ChangeType:
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         ehto = 9.
         RUN Syst/ufkey.p.
         
         PROMPT-FOR
            CLIType.CLIName
            CLIType.BaseBundle
            CLIType.PayType
            CLIType.UsageType
            CLIType.PricePlan
            CLIType.DiscPlan
            CLIType.ServicePack
            CliType.ServiceClass
            CliType.BillTarget
            CliType.Contrtype
            CLIType.ARAccNum
            CLIType.BundleType
            CLIType.CommercialFee
            CLIType.CompareFee
            CLIType.MinimAmt
            CLIType.BillCode
            CLIType.WebStatusCode
            CLIType.StatusCode
            CLIType.LineType
            CLIType.FixedLineType
            CLIType.TariffType
         WITH FRAME lis EDITING:
         
            READKEY.
         
            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"PayType,servicepack") > 0 
            THEN DO:

               IF FRAME-FIELD = "PayType" THEN DO:
                  RUN Help/h-tmscodes.p("CLIType",
                                 "PayType",
                                 "MobSub",
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DISP INTEGER(lcCode) ;& CLIType.PayType 
                  WITH FRAME lis.
               END.
               ELSE IF FRAME-FIELD = "ServicePack" THEN DO:
                  RUN Help/h-tmscodes.p("CLIType",
                                 "ServicePack",
                                 "Profile",
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DISP lcCode ;& CLIType.ServicePack 
                  WITH FRAME lis.
               END.
             
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
               
               IF FRAME-FIELD = "PayType" THEN DO:
                  fPayTypeName(INPUT INPUT CLIType.PayType).
                  DISP lcPayType WITH FRAME lis.

                  IF lcPayType = "" THEN DO:
                     MESSAGE "Unknown payment type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "UsageType" THEN DO:
                  fUsageType(INPUT INPUT CLIType.UsageType).
                  DISP lcUsageType WITH FRAME lis.

                  IF lcUsageType = "" THEN DO:
                     MESSAGE "Unknown usage type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "ServicePack" THEN DO:
                  FIND FIRST TMSCodes WHERE
                             TMSCodes.TableName = "CliType"     AND
                             TmsCodes.FieldName = "ServicePack" AND
                             TmsCodes.CodeGroup = "Profile"     AND
                             TMSCodes.CodeValue = INPUT CliType.ServicePack
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TmsCodes THEN DO:
                     MESSAGE "Incorrect value for profile!" VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
                  ELSE DISP TmsCodes.CodeName @ SPName WITH FRAME lis.
                     
               END.

               ELSE IF FRAME-FIELD = "Discplan" THEN DO:
                  IF INPUT CLIType.Discplan = "" 
                  THEN DISPLAY  "" @ DPName with frame lis.
                  ELSE DO:
                     FIND Discplan WHERE 
                          DiscPlan.Brand    = gcBRand AND 
                          Discplan.Discplan = INPUT CLIType.Discplan
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL Discplan THEN DO:
                        MESSAGE "Unknown Discount plan !".
                        NEXT.
                     END.
                     ELSE DISP Discplan.DPName @ DPName with frame lis.
                  END.
                  
               END.

               ELSE IF FRAME-FIELD = "Priceplan" THEN DO:
                  FIND rateplan  WHERE 
                       rateplan.Brand     = gcBrand AND 
                       rateplan.rateplan  = input PricePlan
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL rateplan THEN DO:
                     MESSAGE "Unknown Rate plan !".
                     NEXT.
                  END.
                  ELSE DISP rateplan.rpname @ PLName with frame lis.
               END.

               ELSE IF FRAME-FIELD = "ARAccNum" THEN DO:

                  lcAccName = fAccName(INPUT INPUT FRAME lis CLIType.ARAccNum).

                  IF lcAccName = "?" THEN DO:
                     BELL.
                     MESSAGE "Unknown Account !".
                     NEXT.
                  END.
                  DISPLAY lcAccName WITH FRAME lis.  
               END.   

               ELSE IF FRAME-FIELD = "StatusCode" THEN DO:
                  fStatusName(INPUT INPUT CLIType.StatusCode).
                  DISP lcStatus WITH FRAME lis.

                  IF lcStatus = "" THEN DO:
                     MESSAGE "Unknown status"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "WebStatusCode" THEN DO:
                  fWebStatusName(INPUT INPUT CLIType.WebStatusCode).
                  DISP lcWebStatus WITH FRAME lis.

                  IF lcWebStatus = "" THEN DO:
                     MESSAGE "Unknown status"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "LineType" THEN DO:
                  fLineType(INPUT INPUT CLIType.LineType).
                  DISP lcLineType WITH FRAME lis.

                  IF lcLineType = "" THEN DO:
                     MESSAGE "Unknown line type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "FixedLineType" THEN DO:
                  fFixedLineType(INPUT INPUT CLIType.FixedLineType).
                  DISP lcFixedLineType WITH FRAME lis.

                  IF lcFixedLineType = "" THEN DO:
                     MESSAGE "Unknown fixed line type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

               ELSE IF FRAME-FIELD = "TariffType" THEN DO:
                  fTariffType(INPUT INPUT CLIType.TariffType).
                  DISP lcTariffType WITH FRAME lis.

                  IF lcTariffType = "" THEN DO:
                     MESSAGE "Unknown Tariff type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.    
               END.

            END.
            APPLY LASTKEY.
         END.
      
         FIND CURRENT CLIType EXCLUSIVE-LOCK.
         
         IF CURRENT-CHANGED CLIType THEN DO:
            
            FIND CURRENT CLIType NO-LOCK.
            
            MESSAGE 
               "This record has been changed elsewhere while updating" 
            VIEW-AS ALERT-BOX TITLE " UPDATE CANCELLED ".

            UNDO ChangeType, LEAVE ChangeType.
         END. 
         
         ELSE DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCLIType).
        
            ASSIGN FRAME lis 
               CLIType.CLIName
               CLIType.BaseBundle
               CLIType.PayType
               CLIType.UsageType
               CLIType.PricePlan
               CLIType.DiscPlan
               CLIType.ServicePack
               CliType.ServiceClass
               CliType.BillTarget
               CliType.Contrtype
               CLIType.ARAccNum
               CLIType.BundleType
               CLIType.CommercialFee
               CLIType.CompareFee
               CLIType.MinimAmt
               CLIType.BillCode
               CLIType.WebStatusCode
               CLIType.StatusCode
               CLIType.LineType
               CLIType.FixedLineType
               CLIType.TariffType.

            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCLIType).
            
            FIND CURRENT CLIType NO-LOCK.   
         END. 
      
         LEAVE ChangeType.
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.

