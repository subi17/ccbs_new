/* ----------------------------------------------------------------------
  MODULE .......: PPREQBR
  TASK .........: Browse and add topup requests
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 05.02.07
  CHANGED ......: 20.02.07/aam don't allow field updates
                  09.03.07 kl  allow updates with new records
                  22.03.07 kl  negative adjustment
                  28.03.07/aam new update logic and layout,
                               tax handling,
                               cancelling,
                               minus adjustment using original request
                  11.04.07/aam pShowXML into a procedure, 
                               call it from detail view also
                  22.05.07/aam TaxZone to request             
                  24.05.07 kl  add prefix to PPRequest sequence field
                  26.06.07/aam deduct 0.01 from current balance,
                               checking of max adjustment amount corrected
                  05.07.07 kl  PrePaidRequest.UserCode
                  06.07.07 kl  previous display fixed
                  30.07.07 kl  Disp TaxPerc with negative value also
                  17.08.07/aam 1. phase 4B info,
                               don't allow compensation for such request
                  22.08.07/aam prefix 998->98, 999->99             
                  23.08.07/aam don't ask for 'add' if no requests available,
                               allow F7 on an empty screen
                  22.10.07/ vk Added field topuplimit to the TMSUser and used                                it instead of hard coding.
                  09.11.07/   previous never tested or worked
                              
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PrePaidRequest'}
{Func/xmlfunction.i}
{Func/ftaxdata.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/matrix.i}
{Func/ftaxdata.i}

{Syst/tmsconst.i}
{Func/fuserright.i}
{Func/fcustpl.i}
{Func/dialog.i}

DEFINE INPUT PARAMETER  iiMsSeq AS INT  NO-UNDO.
                      
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPrePaidRequest AS HANDLE NO-UNDO.
   lhPrePaidRequest = BUFFER PrePaidRequest:HANDLE.
   RUN StarEventInitialize(lhPrePaidRequest).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPrePaidRequest).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.


DEFINE VARIABLE lcCLI        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcRequest    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.
DEFINE VARIABLE lcTSRequest  AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lmXML        AS MEMPTR                  NO-UNDO.
DEFINE VARIABLE lhDoc        AS HANDLE                  NO-UNDO.
DEFINE VARIABLE lhRoot       AS HANDLE                  NO-UNDO.
DEFINE VARIABLE lcXML        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcPostCode   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE ldeVATPerc   AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE ldeTopUpAmt  AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE ldeVatAmt    AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE ldeTemp      AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE ldaDate      AS DATE                    NO-UNDO.
DEFINE VARIABLE lcTime       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE liTime       AS INTEGER                 NO-UNDO.
DEFINE VARIABLE lcMenu       AS CHARACTER               NO-UNDO 
   EXTENT 3 FORMAT "X(30)".
DEFINE VARIABLE llNegative   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE lcPassword   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcAskPassWd  AS CHARACTER               NO-UNDO.  
DEFINE VARIABLE lcRespCode   AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcDispResp   AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcDone       AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcCustName   AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcTaxZone    AS CHAR                    NO-UNDO.
DEFINE VARIABLE ldTaxPerc    AS DEC                     NO-UNDO.
DEFINE VARIABLE lcStatus     AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcTransID    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcUserName   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE lcFirst4B    AS CHAR                    NO-UNDO.
DEFINE VARIABLE lc4BInfo     AS CHAR                    NO-UNDO.
DEFINE VARIABLE lcMobCLI     AS CHAR                    NO-UNDO.
DEFINE VARIABLE ldUpLimit    AS DEC                     NO-UNDO.
DEFINE VARIABLE ldUpMonthLimit AS DEC                   NO-UNDO.
DEFINE VARIABLE llTaxable    AS LOG                     NO-UNDO.
DEFINE VARIABLE llIsAdmin    AS LOG                     NO-UNDO.
DEFINE VARIABLE ldFItemAmt   AS DEC                     NO-UNDO.
DEFINE VARIABLE lcBEventId   AS CHAR                    NO-UNDO. 
DEFINE VARIABLE ldeLoaded    AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE ocResult     AS CHAR                    NO-UNDO.



/* check admin user rights */
IF getTMSRight("CCSUPER,SYST") = "RW" THEN llIsAdmin = TRUE. ELSE llIsAdmin = FALSE.


DEFINE VARIABLE lcCCPrepaidPriceList AS CHARACTER NO-UNDO.

FIND TMSParam WHERE TMSParam.Brand = "1" AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "FMItemPriceListPrepaid" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN lcCCPrepaidPriceList = TMSParam.CharVal. 


 /* create records in ttable */
 FOR EACH FeeModel WHERE FeeModel.Brand = gcBrand AND 
                        FeeModel.FMGroup = 1,
    FIRST FMItem OF FeeModel WHERE FMItem.ToDate >= TODAY AND
                                   FMItem.FromDate <= TODAY AND
                                   FMItem.PriceList = lcCCPrepaidPriceList NO-LOCK :
       CREATE ttable. 
       ASSIGN ttable.ValueId = FeeModel.FeeModel
              ttable.Description = FeeModel.FeeName.
 END.


DEFINE BUFFER bufPP FOR PrePaidRequest.

FORM
   PrePaidRequest.CLI         COLUMN-LABEL "MSISDN"   FORMAT "X(9)"
   PrePaidRequest.Request     COLUMN-LABEL "Req.Type" FORMAT "X(12)"
   SPACE(0)
   lcFirst4B                  NO-LABEL FORMAT "X"
   SPACE(0)
   PrePaidRequest.Source                              
   lcTSRequest FORMAT "X(19)" COLUMN-LABEL "Request Time"
   PrePaidRequest.PPStatus    COLUMN-LABEL "St"
   lcDispResp                 COLUMN-LABEL "Resp"     FORMAT "X(4)"
   PrePaidRequest.TopUpAmt    COLUMN-LABEL "TopUpAmt" FORMAT "->>9.99"
   PrePaidRequest.VatAmt      COLUMN-LABEL "TaxAmt"   FORMAT "->>9.99"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
   COLOR VALUE(cfc)   
   TITLE COLOR VALUE(ctc) " " + ynimi +
   " TOPUP REQUESTS  "
   + string(pvm,"99-99-99") + " "
FRAME sel.

FORM
    PrePaidRequest.PPRequest  COLON 18  
       LABEL "Request ID" 
       FORMAT ">>>>>>>>>>9"
    PrePaidRequest.PPReqPrefix COLON 40
       LABEL "Prefix" 
       SKIP
    PrePaidRequest.MSSeq      COLON 18   
       LABEL "Subscription ID" 
    PrePaidRequest.UserCode   COLON 18
       LABEL "User"
    lcUserName
       NO-LABEL 
       FORMAT "X(30)" 
       SKIP
    PrePaidRequest.CLI        COLON 18
       LABEL "MSISDN"
       FORMAT "X(13)" 
    MobSub.InvCust            COLON 18 
       LABEL "Invoice Customer"
    lcCustName 
       NO-LABEL 
       FORMAT "X(30)" 
       SKIP
    PrePaidRequest.TsRequest  COLON 18
       LABEL "Request Time"
       SPACE(2) 
    ldaDate                 
       NO-LABEL 
       HELP "Date when TopUp should be activated"
       FORMAT "99.99.9999" 
    lcTime
       NO-LABEL
       HELP "Time when TopUp should be activated"
       FORMAT "XX:XX:XX"
    PrePaidRequest.Request    COLON 18 
       LABEL "Type"
       FORMAT "X(30)" 
    PrePaidRequest.Source     COLON 18 
       FORMAT "X(30)" 
    PrePaidRequest.Reference  COLON 18 
       LABEL "Operation Nbr"
       FORMAT "X(30)" 
    ldeTopUpAmt               COLON 18 
       LABEL "TopUp Amount"
       FORMAT  "->>,>>>,>>9.99" 
    lcTaxZone                 COLON 18 
       LABEL "Tax Zone" 
       FORMAT "X(30)" 
    ldTaxPerc                 COLON 18 
       LABEL "Tax%" 
       FORMAT "->9.9" 
    ldeVatAmt                 COLON 18 
       LABEL "Tax Amount" 
       FORMAT "->>,>>>,>>9.99" 
    PrePaidRequest.TSResponse  COLON 18
       LABEL "Response Time"
       SPACE(2) 
    lcDone 
       NO-LABEL 
       FORMAT "X(20)" 
    PrePaidRequest.RespCode   COLON 18
       LABEL "Response"
    lcRespCode 
       NO-LABEL
       FORMAT "X(30)"
    PrePaidRequest.PPStatus   COLON 18 
       FORMAT ">9"
    lcStatus 
       NO-LABEL 
       FORMAT "X(30)" 

WITH  OVERLAY ROW 1 centered COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr SIDE-LABELS FRAME lis.

FORM /* seek  PrePaidRequest */
   lcCLI
      HELP "Enter code"
WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
   COLOR VALUE(cfc) NO-LABELS OVERLAY
FRAME f1.

FUNCTION fGetRequestID RETURNS INTEGER:

   DEF VAR liNextID AS INT NO-UNDO.
   
   DO WHILE TRUE:
      liNextID = NEXT-VALUE(PrePaidReq).
   
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liNextID)
      THEN LEAVE.
   END.
   
   RETURN liNextID.
   
END.
 /* how much has been loaded already on this month */
FUNCTION fTopUpLoaded RETURNS DECIMAL
   (idtDate AS DATE):

   DEF VAR ldTS1     AS DEC  NO-UNDO.
   DEF VAR ldTS2     AS DEC  NO-UNDO.
   DEF VAR ldLoaded  AS DEC  NO-UNDO.
   
   fMonthlyStamps(TODAY,
                  OUTPUT ldTS1, 
                  OUTPUT ldTS2).
   
   ldLoaded = 0.
   
   FOR EACH bufPP WHERE
            bufPP.Brand      = PrePaidRequest.Brand  AND
            bufPP.CLI        = PrePaidRequest.CLI    AND
            bufPP.Source     = PrePaidRequest.Source AND
            bufPP.TSRequest >= ldTS1                 AND
            bufPP.TSRequest <= ldTS2                 AND
            RECID(bufPP) NE RECID(PrePaidRequest):

      ldLoaded = ldLoaded + bufPP.TopUpAmt / 100.

   END.
 
   RETURN ldLoaded.

END FUNCTION.

FUNCTION fMinusRequest RETURNS LOGIC
  (INPUT iiMsSeq AS INT,
   INPUT icCLI   AS CHAR):

   ASSIGN
      PrePaidRequest.PPRequest   = fGetRequestID()
      PrePaidRequest.Brand       = gcBrand
      PrePaidRequest.MsSeq       = iiMsSeq
      PrePaidRequest.CLI         = icCLI
      PrePaidRequest.PPReqPrefix = "999"
      PrePaidRequest.Request     = "AdjustmentTRequest" 
      PrePaidRequest.CommLine    = "AdjustmentTRequest" 
      PrePaidRequest.Source      = "CC"
      PrePaidRequest.PPStatus    = 99.
      PrePaidRequest.TSRequest   = fMakeTS().

   RETURN TRUE.
   
END FUNCTION.

lcPassword = fCParamC("AdminUser").
 
cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST MobSub WHERE
           MobSub.MsSeq = iiMsSeq
NO-LOCK NO-ERROR.

IF NOT MobSub.PayType THEN DO:
   MESSAGE
      "This is not a prepaid subscription!"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

lcMobCLI = MobSub.CLI.

RUN local-find-first.
IF AVAILABLE PrePaidRequest THEN ASSIGN
   Memory       = recid(PrePaidRequest)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   must-add = FALSE. 
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a PrePaidRequest  */
      
      CHOISES:
      DO WHILE TRUE:
   
         ASSIGN
            ufk       = 0
            ufk[8]    = 8
            ehto      = 3.
      
         RUN Syst/ufkey.p. 

          /* dialog to fetch the charge/compensation  amount using 
             FeeModel and FMItem  */
       
          DEFINE VARIABLE lctitle AS CHARACTER INITIAL "Charge and Compensation Type " NO-UNDO.
          DEFINE VARIABLE lrecid AS RECID NO-UNDO.
          DEFINE VARIABLE loutValueId AS CHARACTER NO-UNDO. 


          RUN Help/h-dialog.p (INPUT TABLE ttable BY-REFERENCE ,
                          INPUT lctitle,
                          OUTPUT lrecid,
                          OUTPUT loutValueId).
          
          FIND ttable WHERE RECID(ttable) = lrecid NO-LOCK NO-ERROR.

          IF NOT AVAIL ttable THEN DO:
             llNegative = ?.
             ASSIGN
                must-add   = FALSE
                must-print = TRUE
                ufkey      = TRUE.
             NEXT LOOP.
           END.
          
         FIND FeeModel WHERE FeeModel.Brand = gcBrand AND 
                             FeeModel.FeeModel  = ttable.ValueId NO-LOCK NO-ERROR.
                            
         /* Fetch default charge */
        DEFINE VARIABLE lcPriceList AS CHARACTER NO-UNDO. 
        lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                         MobSub.BillTarget,
                                         FeeModel.FeeModel,
                                         TODAY).


        FIND FIRST FMItem NO-LOCK  WHERE
                   FMItem.Brand     = gcBrand       AND
                   FMItem.FeeModel  = FeeModel.FeeModel AND
                   FMItem.PriceList = lcPriceList AND
                   FMItem.FromDate <= TODAY     AND
                   FMItem.ToDate   >= TODAY NO-ERROR.
   
        IF AVAIL FMItem AND FMItem.Amount <> 0 THEN DO:
            ldFItemAmt = -1 * FMItem.Amount * 100 . /* all top up are in cents and oposite sign to FMItem */
            lcBEventId = FeeModel.FeeModel. 
        END.
        ELSE DO: 
          MESSAGE "Charge/Compensation Event doesn't contain active item or its amount is cero !"
          VIEW-AS ALERT-BOX TITLE "INFO".
          ldFItemAmt = 0.
          lcBEventId = "".
          llNegative = ?.
          ASSIGN
             must-add   = FALSE
             must-print = TRUE
             ufkey      = TRUE.
          NEXT LOOP.
        END.
        IF ldFItemAmt < 0 THEN

           ASSIGN llNegative = TRUE
                 llTaxable = FALSE.
        ELSE
          ASSIGN llNegative = FALSE
                 llTaxable = FALSE.

         LEAVE CHOISES.
         
      END.
      
      /* user limits */
      DEFINE VARIABLE liOneTimeLimitType AS INTEGER NO-UNDO.
      DEFINE VARIABLE liMonthlyLimitType AS INTEGER NO-UNDO. 

      IF  ldFItemAmt > 0 THEN DO:
          liOneTimeLimitType = {&PREP_COMP_LIMIT_TYPE}. 
          liMonthlyLimitType = {&PREP_COMP_MONTHLY_LIMIT_TYPE}.
      END.
      ELSE DO:
        liOneTimeLimitType =  {&PREP_CHARGE_LIMIT_TYPE}.
        liMonthlyLimitType =  {&PREP_CHARGE_MONTHLY_LIMIT_TYPE}.
      END.
      /* one time user limit */
      ldUpLimit = fUserLimitAmt(katun,liOneTimeLimitType).
      /* monthly user limit */
      ldUpMonthLimit = fUserLimitAmt(katun,liMonthlyLimitType).

      IF ldUpLimit < 0 OR ldUpMonthLimit < 0  THEN DO:
         MESSAGE "One time / monthly limit is not defined in your account or group "
         VIEW-AS ALERT-BOX ERROR.
         ldFItemAmt = 0.
         lcBEventId = "".
         llNegative = ?.
         ASSIGN
             must-add   = FALSE
             must-print = TRUE
             ufkey      = TRUE.
         NEXT LOOP.
      END.

      ASSIGN
         cfc      = "lis"
         ufkey    = true
         ac-hdr   = " ADD "
         must-add = FALSE.

      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.

        PAUSE 0 NO-MESSAGE.
        
        ehto = 9. RUN Syst/ufkey.p.
        
        REPEAT TRANSACTION WITH FRAME lis:
           
           CLEAR FRAME lis NO-PAUSE.

           CREATE PrePaidRequest.

           ASSIGN
              PrePaidRequest.PPRequest  = fGetRequestID()
              PrePaidRequest.Brand      = gcBrand
              PrePaidRequest.MsSeq      = iiMsSeq
              PrePaidRequest.CLI        = lcMobCLI
              PrePaidRequest.UserCode   = katun
              PrePaidRequest.ReqCParam1 = lcBEventId
              PrePaidRequest.TopUpAmt   = ldFItemAmt.
       
           /* include prefix */
           IF ldFItemAmt < 0 
           THEN PrePaidRequest.PPReqPrefix = "978".
           ELSE PrePaidRequest.PPReqPrefix = "979".

           RUN local-UPDATE-record(TRUE).

           IF ERROR-STATUS:ERROR = TRUE THEN DO:
              MESSAGE RETURN-VALUE  VIEW-AS ALERT-BOX TITLE "INFO".
              UNDO add-row, LEAVE add-row.
           END.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              KEYLABEL(lastkey) = "F4" OR 
              RETURN-VALUE = "CANCEL" THEN
           UNDO add-row, LEAVE add-row.

           PrePaidRequest.PPStatus = 0.

         
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPrePaidRequest).

           ASSIGN
              Memory = recid(PrePaidRequest)
              xrecid = Memory.
        
           LEAVE ADD-ROW.
      
        END.
      
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PrePaidRequest THEN LEAVE LOOP.
      NEXT LOOP.
      
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PrePaidRequest WHERE recid(PrePaidRequest) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PrePaidRequest THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PrePaidRequest).
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
           ufk    = 0
           ufk[4] = 829
           ufk[5] = 302 WHEN lcRight = "RW"
           ufk[7] = 1079
           ufk[8] = 8
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.

         RUN Syst/ufkey.p.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PrePaidRequest.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrePaidRequest.CLI WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND PrePaidRequest WHERE recid(PrePaidRequest) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PrePaidRequest THEN
              ASSIGN FIRSTrow = i Memory = recid(PrePaidRequest).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,7,F7,8,f8") = 0 THEN DO:
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
           IF NOT AVAILABLE PrePaidRequest THEN DO:
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
                rtab[1] = recid(PrePaidRequest)
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
           IF NOT AVAILABLE PrePaidRequest THEN DO:
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
              rtab[FRAME-DOWN] = recid(PrePaidRequest).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PrePaidRequest WHERE recid(PrePaidRequest) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PrePaidRequest THEN DO:
           Memory = recid(PrePaidRequest).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PrePaidRequest THEN Memory = recid(PrePaidRequest).
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
           FIND PrePaidRequest WHERE recid(PrePaidRequest) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcCLI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcCLI ENTERED THEN DO:
          FIND FIRST PrePaidRequest USE-INDEX CLI WHERE 
                     PrePaidRequest.Brand = gcBrand AND
                     PrePaidRequest.CLI >= lcCLI
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE PrePaidRequest THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some PrePaidRequest/PrePaidRequest was found */
          ASSIGN order = 1 Memory = recid(PrePaidRequest) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        
        RUN local-find-this(FALSE).
        
        FIND FIRST MobSub WHERE
                   MobSub.MsSeq = PrePaidRequest.MsSeq
        NO-LOCK NO-ERROR.

        IF NOT CAN-FIND(FIRST Payment WHERE
                              Payment.Brand  = gcBrand AND
                              Payment.RefNum = STRING(PrePaidRequest.PPRequest))
        THEN DO:
           MESSAGE
              "There are no payments for this request !"
           VIEW-AS ALERT-BOX.
        END.
        ELSE DO:
           RUN Ar/payments.p(MobSub.CustNum,0,STRING(PrePaidRequest.PPRequest)).
        END.
        
        ASSIGN
           ufkey      = TRUE
           must-print = TRUE
           memory     = RECID(PrePaidRequest).

     END.

     ELSE IF LOOKUP(nap,"<") > 0 THEN DO:

        RUN local-find-this(FALSE).
     
        RUN pShowXML(PrePaidRequest.CommLine).
        
        NEXT LOOP.

     END.
     
     ELSE IF LOOKUP(nap,">") > 0 THEN DO:
     
        RUN local-find-this(FALSE).

        RUN pShowXML(PrePaidRequest.Response).

        NEXT LOOP.

     END.
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
    
        IF fMatrixAnalyse("1",
                          "DENIED",
                          "SubsTypeFrom;Module",
                          Mobsub.clitype + ";" + "balances",
                          OUTPUT ocResult) ne ? THEN DO:
           MESSAGE
              SUBST("This function is not allowed with clitype &1!",
              Mobsub.CliType)
           VIEW-AS ALERT-BOX.
           NEXT LOOP. 
        END.

        must-add = TRUE.
        
        NEXT LOOP.
        
     END. /* ADD NEW */

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        RUN Gwy/balancequery.p(lcMobCLI).
        INT(RETURN-VALUE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
        ELSE
        MESSAGE
           "Current balance on prepaid platform:" 
           STRING(INT(RETURN-VALUE) / 100) + "e."
        VIEW-AS ALERT-BOX TITLE "Balance query".
     END. /* balance query */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrePaidRequest).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PrePaidRequest.CLI.

       llTaxable = (PrepaidRequest.VatAmt NE 0).
        
       RUN local-UPDATE-record(FALSE).
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
          KEYLABEL(lastkey) = "F4" OR 
          RETURN-VALUE = "CANCEL"
       THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrePaidRequest).

       IF RETURN-VALUE = "PRINT" THEN DO:
          must-print = TRUE.
          NEXT LOOP.
       END.
       
       ELSE DO:
          RUN local-disp-row.
          xrecid = recid(PrePaidRequest).
     
          LEAVE.
       END.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PrePaidRequest) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PrePaidRequest) must-print = TRUE.
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
      FIND PrePaidRequest WHERE recid(PrePaidRequest) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PrePaidRequest WHERE recid(PrePaidRequest) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST PrePaidRequest USE-INDEX MsSeq WHERE
                 PrePaidRequest.Brand = gcBrand AND
                 PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST PrePaidRequest USE-INDEX Source WHERE 
                 PrePaidRequest.Brand = gcBrand AND
                 PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST PrePaidRequest USE-INDEX MsSeq WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST PrePaidRequest USE-INDEX Source WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT PrePaidRequest USE-INDEX MsSeq WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT PrePaidRequest USE-INDEX MsSeq WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV PrePaidRequest USE-INDEX MsSeq WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV PrePaidRequest USE-INDEX Source WHERE 
                PrePaidRequest.Brand = gcBrand AND
                PrePaidRequest.MsSeq = iiMsSeq 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.

   DISPLAY 
      PrePaidRequest.CLI 
      PrePaidRequest.Request
      lcFirst4B
      PrePaidRequest.Source
      lcTSRequest
      PrePaidRequest.PPStatus
      lcDispResp
      PrePaidRequest.TopUpAmt   / 100 @ PrePaidRequest.TopUpAmt 
      PrePaidRequest.VatAmt     / 100 @ PrePaidRequest.VatAmt 
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   lcTSRequest = fTS2HMS(PrePaidRequest.TSRequest).
   
   lcDispResp = STRING((PrePaidRequest.RespCode = 0 OR
                        PrePaidRequest.RespCode = 1 OR
                        PrePaidRequest.RespCode = 2),"OK/NOK").

   lcFirst4B = STRING(PrePaidRequest.Response = "First4B","*/").

END PROCEDURE.


PROCEDURE pShowXML:

   DEF INPUT PARAMETER icMessage AS CHAR NO-UNDO.
   
   IF INDEX(icMessage,"<") = 0 THEN DO:
      MESSAGE
         "XML message not available" SKIP
         icMessage
      VIEW-AS ALERT-BOX INFORMATION.

      RETURN.     
   END.  

   ASSIGN
      lcXML = icMessage
      lcXML = SUBSTR(lcXML,INDEX(lcXML,"<")).
        
   SET-SIZE(lmXML) = LENGTH(lcXML) + 1.
        
   PUT-STRING(lmXML,1) = lcXML.
     
   CREATE X-DOCUMENT lhDoc.
   CREATE X-NODEREF  lhRoot.

   lhDoc:LOAD("MEMPTR",lmXML,FALSE).
   lhDoc:GET-DOCUMENT-ELEMENT(lhRoot) NO-ERROR.   

   EMPTY TEMP-TABLE ttXMLSchema.
   
   RUN pMessage2XMLTable(lhRoot,1,0).
   
   RUN Mc/xmlbrowser.p(INPUT TABLE ttXMLSchema).
        
   SET-SIZE(lmXML) = 0.

   DELETE OBJECT(lhDoc).
   DELETE OBJECT(lhRoot).

END PROCEDURE.


PROCEDURE local-UPDATE-record:

   DEFINE INPUT PARAMETER llNew AS LOGICAL NO-UNDO.

   
   DEFINE VARIABLE llUpdate  AS LOG       NO-UNDO.
   DEFINE VARIABLE ldCheckTS AS DEC       NO-UNDO.
   DEFINE VARIABLE ldCurrTS  AS DEC       NO-UNDO.
   DEFINE VARIABLE llDirect  AS LOG       NO-UNDO.
   DEFINE VARIABLE llOk      AS LOG       NO-UNDO. 
   DEFINE VARIABLE ldMinus   AS DEC       NO-UNDO. 
   DEFINE VARIABLE ldMaxAmt  AS DEC       NO-UNDO. 
   DEFINE VARIABLE ldCurrBal AS DEC       NO-UNDO. 
   DEFINE VARIABLE llChanged AS LOG       NO-UNDO.
   DEFINE VARIABLE lcReturn  AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcReqZone AS CHAR      NO-UNDO.
   
   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      MESSAGE "Invoice customer data not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   ASSIGN 
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                 BUFFER Customer)
      lcTaxZone  = ""
      ldTaxPerc  = 0
      llDirect   = FALSE
      ldMinus    = 0
      ldMaxAmt   = 0
      llChanged  = FALSE
      lcUserName = ""
      lc4BInfo   = "".

   FIND FIRST TMSUser WHERE
              TMSUser.UserCode = PrePaidRequest.UserCode
   NO-LOCK NO-ERROR.
    
   
   /*pick up amount user limit */
   IF AVAIL TMSUser THEN DO:
      lcUserName = TMSUser.UserName.
      
   END.
 
      
   /* if taxzone has been marked to request then use it */
   IF PrePaidRequest.TaxZone > "" THEN 
      lcTaxZone = PrePaidRequest.TaxZone.
      
   IF lcTaxZone = "" OR 
      NOT CAN-FIND(TaxZone WHERE TaxZone.TaxZone = lcTaxZone) 
   THEN DO:    
      FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
      IF AVAILABLE Region THEN lcTaxZone = Region.TaxZone.
   END.
 
   /* tax percent according to taxzone */
   IF llTaxable THEN ldTaxPerc = fTaxPerc(lcTaxZone, "1", TODAY).
   
   IF llNew THEN DO:
          
      ASSIGN
         PrePaidRequest.TSRequest = fMakeTS()
         PrePaidRequest.Request   = "AdjustmentTRequest"    
         PrePaidRequest.CommLine  = "AdjustmentTRequest"     
         PrePaidRequest.Source    =  (IF llNegative THEN "CHARGE" ELSE "COMP" )
         PrePaidRequest.PPStatus  = 99
         PrePaidRequest.VatAmt    = PrePaidRequest.TopUpAmt * ldTaxPerc / 100
         llUpdate                 = TRUE
         llDirect                 = TRUE.
   END.
   
   ELSE DO:

      ASSIGN llUpdate   = (PrePaidRequest.PPStatus = 0 OR
                           PrePaidRequest.PPStatus = 99)
             llNegative = (PrePaidRequest.TopUpAmt < 0).           
                  
      /* for old events get taxdata from amounts */
      IF NOT llUpdate AND PrePaidRequest.TopUpAmt NE 0 THEN DO:
         ldTaxPerc = ROUND(100 * PrePaidRequest.VatAmt / 
                                 PrePaidRequest.TopUpAmt,1).
   
         IF ldTaxPerc NE 0 AND lcTaxZone = "" THEN 
         FOR FIRST VatCode NO-LOCK WHERE
                   VatCode.TaxClass = "1" AND
                   VatCode.VatPerc  = ldTaxPerc:
            lcTaxZone = VatCode.TaxZone.
         END.
      END.
           
      /* if not yet sent to pp then*/
      ELSE IF llUpdate THEN DO:
   
                
        /* recalculate vat amount using customer's vat data  */
         FIND CURRENT PrePaidRequest EXCLUSIVE-LOCK.
         PrePaidRequest.VatAmt = PrePaidRequest.TopUpAmt * ldTaxPerc / 100.

      END.

   END.
   
   ASSIGN
      ldCurrTS  = fMakeTS()
      lcReqZone = lcTaxZone.
 
   FIND TaxZone WHERE TaxZone.TaxZone = lcTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN 
      lcTaxZone = lcTaxZone + " " + TaxZone.TZName.
    
   /* maximum amount to be adjusted  */ 
   IF PrePaidRequest.Request = "AdjustmentTRequest" AND llNegative THEN DO:
      ldMaxAmt = 1000.
      
      /* get current balance */
      RUN Gwy/balancequery.p(MobSub.CLI).
      ldCurrBal = INT(RETURN-VALUE) / 100.

      /* in case of minus adjustment */
      IF PrePaidRequest.OrigRequest > 0 THEN DO: 
         FOR FIRST bufPP NO-LOCK WHERE
                   bufPP.Brand     = gcBrand  AND
                   bufPP.PPRequest = PrePaidRequest.OrigRequest:
          ldMaxAmt =  bufPP.TopUpAmt / 100.
         END.
         ldMaxAmt = MAX(ldMaxAmt,ldCurrBal).
      END.
      /* in case of charge tool */
      ELSE DO: 
         ldMaxAmt = ldCurrBal.
         /* if your are in new charge and compensation,
         are not admin and balance is not enough then return */
         IF PrePaidRequest.Source = "CHARGE" AND
            llIsAdmin = FALSE AND
            ABSOLUTE(PrePaidRequest.TopUpAmt) < ldMaxAmt THEN
               RETURN ERROR "Charge is higher than current balance !".
      END.
   END.
   
   RequestUpdate:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      fSplitTS(PrePaidRequest.TSRequest, OUTPUT ldaDate, OUTPUT liTime).

      ASSIGN
         lcDone      = fTS2HMS(PrePaidRequest.TsResponse)
         lcTime      = REPLACE(STRING(liTime,"hh:mm:ss"),":","")
         ldeTopUpAmt = PrePaidRequest.TopUpAmt  / 100    
         ldeVatAmt   = PrePaidRequest.VatAmt    / 100 
         lcRespCode  = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                        "PrePaidRequest",
                                        "RespCode",
                                        PrePaidRequest.RespCode)
         lcStatus    = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                        "PrePaidRequest",
                                        "PPStatus",
                                        STRING(PrePaidRequest.PPStatus)).
 
      IF PrePaidRequest.PPStatus = 0 OR PrePaidRequest.PPStatus = 99
      THEN lcRespCode = "". 
      
      IF PrePaidRequest.Response = "First4B"
      THEN lc4BInfo = " (4B 1. phase)".
      ELSE lc4BInfo = "".
      
      PAUSE 0.
      DISP
         PrePaidRequest.PPRequest
         PrePaidRequest.PPReqPrefix
         PrePaidRequest.CLI
         PrePaidRequest.UserCode
         lcUserName
         PrePaidRequest.MsSeq
         MobSub.InvCust
         lcCustName
         PrePaidRequest.TSRequest
         ldaDate
         lcTime
         PrePaidRequest.Request + lc4BInfo @ PrePaidRequest.Request
         PrePaidRequest.Source
         PrePaidRequest.Reference
         ldeTopUpAmt
         lcTaxZone
         ldTaxPerc
         ldeVatAmt
         PrePaidRequest.TSResponse
         lcDone
         PrePaidRequest.RespCode
         lcRespCode
         PrePaidRequest.PPStatus
         lcStatus
      WITH FRAME lis.
   
      IF PrePaidRequest.PPStatus = 99 THEN 
         DISPLAY 0 @ PrePaidRequest.PPStatus WITH FRAME lis.
   
      ASSIGN 
         ufk    = 0
         ufk[3] = 1088
         ufk[4] = 1121
         ufk[8] = 8
         ehto   = 0.

      IF lcRight = "RW" AND LOOKUP(PrePaidRequest.Source,"CC,CHARGE,COMP") > 0 THEN DO:

         /* udpate */   
         IF PrePaidRequest.PPStatus = 0 OR
            PrePaidRequest.PPStatus = 99 THEN ufk[1] = 7.

         /* status change (cancel) */
         IF PrePaidRequest.PPStatus = 0 OR
            PrePaidRequest.PPStatus = 3 THEN ufk[7] = 1087.
      END.

      /* minus adjustment */
      IF lcRight = "RW" AND 
         PrePaidRequest.TopUpAmt > 0          AND /* minus adjustment can be done only on positive topup */
         PrePaidRequest.Response NE "First4B" AND
         PrePaidRequest.PPStatus = 2          AND
         PrePaidRequest.OrigRequest = 0 THEN DO:
         
         ldMinus = 0.   

         FOR EACH bufPP NO-LOCK WHERE
                  bufPP.Brand       = gcBrand                  AND
                  bufPP.OrigRequest = PRePaidRequest.PPRequest AND
                  bufPP.Request     = "AdjustmentTRequest":
            ldMinus = ldMinus + bufPP.TopUpAmt.      
         END.

         IF ABS(ldMinus) < PrePaidRequest.TopUpAmt THEN ufk[6] = 1086.
      END.

      /* accept / cancel */
      IF PrePaidRequest.PPStatus = 99 OR
         llChanged THEN ASSIGN
         ufk[5] = 1089
         ufk[8] = 1059.

      IF llDirect THEN ASSIGN 
         llDirect = FALSE
         toimi    = 1.
         
      ELSE RUN Syst/ufkey.p.   
      
      IF toimi = 1 THEN DO:
      
         FIND CURRENT PrePaidRequest EXCLUSIVE-LOCK.
         
         ehto = 9.
         RUN Syst/ufkey.p.
         
         REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE. 
          
            UPDATE
               ldaDate
               lcTime
               ldeTopUpAmt WHEN llIsAdmin   
            WITH FRAME lis EDITING:

               READKEY.
      
               nap = keylabel(lastkey).
      
               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:

                  PAUSE 0.

                  IF FRAME-FIELD = "ldeTopUpAmt" THEN DO:
                      
                     IF INPUT FRAME lis ldeTopUpAmt  = 0 
                     THEN DO:
                        MESSAGE "Value can not be cero !" 
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
       
                     IF llNegative AND INPUT FRAME lis ldeTopUpAmt > 0 
                     THEN DO:
                        MESSAGE "Give only negative values!"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     ELSE IF NOT llNegative AND INPUT FRAME lis ldeTopUpAmt < 0 
                     THEN DO:
                        MESSAGE "Give only positive values!"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     /* lower limit for negative amounts taking into account balance */
                     IF PrePaidRequest.Request = "AdjustmentTRequest" AND llNegative THEN DO:
                        IF ABSOLUTE(INPUT ldeTopUpAmt ) > ldMaxAmt
                        THEN DO:
                           IF PrePaidRequest.Source = "CHARGE" THEN 
                              MESSAGE "Charge is higher than current balance" VIEW-AS ALERT-BOX ERROR.
                           ELSE
                              MESSAGE "Maximum negative adjustment is " ldMaxAmt "euros!" VIEW-AS ALERT-BOX ERROR.
                          NEXT.
                        END.
                     END.   
                     /* user topup limit for charge and compensations */
                     IF (PrePaidRequest.Source = "CHARGE" OR
                         PrePaidRequest.Source = "COMP" ) AND 
                         ABSOLUTE( INPUT ldeTopUpAmt) > ldUpLimit THEN DO:
                              MESSAGE "The absolute value should be less than " ldUpLimit " euros!"
                             VIEW-AS ALERT-BOX ERROR.
                             NEXT.
                     END.
                     ldeVatAmt = ROUND(ldTaxPerc * 
                                       INPUT FRAME lis ldeTopUpAmt / 100,2).
                     DISPLAY ldeVatAmt WITH FRAME lis.

                  END.
      
                  ELSE IF FRAME-FIELD = "ldaDate" THEN DO:

                     IF INPUT ldaDate < TODAY THEN DO:
                        MESSAGE "You cannot set the request date to the past"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
                  
                  ELSE IF FRAME-FIELD = "lcTime" THEN DO:
                     IF NOT fCheckTime(INPUT INPUT FRAME lis lcTime) THEN DO:
                        MESSAGE 
                           "Invalid time: " + INPUT FRAME lis lcTime
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     
                     ldCheckTS = fHMS2TS(INPUT INPUT FRAME lis ldaDate,
                                         INPUT INPUT FRAME lis lcTime).
                                          
                     IF ldCheckTS < ldCurrTS THEN DO:
                        MESSAGE "You cannot set the request time to the past"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                      
                  END.

               END.
               
               APPLY LASTKEY.
 
            END.  /* editing */
 
            ldeLoaded = fTopUpLoaded(ldaDate).

            /* monthly limit check for charge and compensations  */   
            IF (PrePaidRequest.Source = "CHARGE" OR 
                PrePaidRequest.Source = "COMP" ) AND
               ABSOLUTE(ldeLoaded + ldeTopUpAmt) > ldUpMonthLimit THEN DO:
               
                MESSAGE
                   "Requested TopUp amount is over monthly limit!" SKIP
                   "The absolute value should be less than " ldUpMonthLimit - ABSOLUTE(ldeLoaded) "euros."
                VIEW-AS ALERT-BOX.
                NEXT.

             END.
            
            ldCheckTS = fHMS2TS(ldaDate,lcTime).
            
            IF ldeTopUpAmt * 100 NE PrePaidRequest.TopUpAmt OR
               ldeVatAmt * 100   NE PrePaidRequest.VatAmt   OR
               ldCheckTS         NE PrePaidRequest.TSRequest
            THEN ASSIGN
               PrePaidRequest.TopUpAmt  = ldeTopUpAmt * 100
               PrePaidRequest.VatAmt    = ldeVatAmt * 100
               PrePaidRequest.TSRequest = fHMS2TS(ldaDate,lcTime)
               llChanged                = TRUE.
             
            PrepaidRequest.TaxZone = lcReqZone.
                
            LEAVE.
            
         END. /* repeat */

         IF KEYLABEL(LASTKEY) = "F4" OR 
            LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN DO:
            IF llNew THEN RETURN "CANCEL".
         END.
         
      END.

      /* show xml */
      ELSE IF toimi = 3 THEN DO:
         RUN pShowXML(PrePaidRequest.Response).
      END.
       
      ELSE IF toimi = 4 THEN DO:
         RUN pShowXML(PrePaidRequest.CommLine).
      END.
      
      /* minus adjustment */
      ELSE IF toimi = 6 THEN DO:

         IF PrepaidRequest.TSRequest < 20080101 THEN DO:
            MESSAGE "Minus adjustment is not allowed for previous year's"
                    "events"
            VIEW-AS ALERT-BOX INFORMATION.
            NEXT.
         END.
 
         PAUSE 0.
         UPDATE
            lcAskPassWd BLANK FORMAT "X(20)" LABEL "Password"
               HELP "Password for making a minus adjustment"
         WITH OVERLAY ROW 10 CENTERED TITLE " Minus Adjustment "
              SIDE-LABELS FRAME fPassword.

         HIDE FRAME fPassWord NO-PAUSE.
         
         IF lcPassword NE lcAskPasswd THEN NEXT.
         
         /* get current balance */
         RUN Gwy/balancequery.p(MobSub.CLI).
         ldCurrBal = INT(RETURN-VALUE) / 100.
       
         IF ldCurrBal <= 0 THEN DO:
            MESSAGE "Current balance on prepaid platform is"
                    ldCurrBal "eur. Adjustment cannot be made."
            VIEW-AS ALERT-BOX INFORMATION.
            NEXT.
         END.

         ASSIGN
            ufk    = 0
            ufk[8] = 8
            ehto   = 3.
      
         RUN Syst/ufkey.p. 

         DISPLAY
            " A) Minus adjustment           " @ lcMenu[1]  SKIP
            " X) Quit                       " @ lcMenu[2]  SKIP
         WITH OVERLAY FRAME minchoices NO-LABELS.
       
         CHOOSE FIELD lcMenu AUTO-RETURN go-on (F8) WITH FRAME minchoices
            TITLE " Minus Adjustment Type " CENTERED WITH COL 2 ROW 8.

         HIDE FRAME minchoices.

         IF FRAME-INDEX = 1 THEN DO:
           
            llOk = FALSE.
               
            MESSAGE
               "Make a minus adjustment?"
            VIEW-AS ALERT-BOX 
            QUESTION  BUTTONS YES-NO  TITLE " MINUS ADJUSTMENT "
            SET llOk.

            IF llOk THEN DO:
               FIND bufPP WHERE RECID(bufPP) = RECID(PrePaidRequest) NO-LOCK.
                   
               CREATE PrePaidRequest.
               fMinusRequest(MobSub.MsSeq,
                             MobSub.CLI).
                  
               PrePaidRequest.TopUpAmt = -1 * MIN(bufPP.TopUpAmt + 
                                                  ldMinus * 100,
                                                  ldCurrBal * 100).
               
               IF PrePaidRequest.TopUpAmt = -1 * bufPP.TopUpAmt THEN
                  PrePaidRequest.VatAmt   = -1 * bufPP.VatAmt.
               ELSE
                  PrePaidRequest.VatAmt = ldTaxPerc * PrePaidRequest.TopUpAmt /
                                                      100.

               ASSIGN 
                  ldMaxAmt                   = PrePaidRequest.TopUpAmt / 100
                  PrePaidRequest.OrigRequest = bufPP.PPRequest
                  PrePaidRequest.TaxZone     = lcReqZone
                  llNegative                 = TRUE
                  PrePaidRequest.UserCode    = katun
                  llTaxable                  = (PrePaidRequest.VatAmt NE 0).
            END.
         END.
         
      END.

      /* change status */  
      ELSE IF toimi = 7 THEN DO:
        
         ASSIGN
            ufk    = 0
            ufk[8] = 8
            ehto   = 3.
      
         RUN Syst/ufkey.p. 

         DISPLAY
            " A) Cancel request (set status to 4)" @ lcMenu[1]  SKIP
            " B) Handle request (set status to 9)" @ lcMenu[2]  SKIP 
            " X) Quit                            " @ lcMenu[3]  SKIP
         WITH OVERLAY FRAME setchoices NO-LABELS.
       
         CHOOSE FIELD lcMenu AUTO-RETURN go-on (F8) WITH FRAME setchoices
            TITLE " Choose compensation type " CENTERED WITH COL 2 ROW 8.

         HIDE FRAME setchoices.

         IF FRAME-INDEX = 1 THEN DO:
            IF PrePaidRequest.PPStatus NE 0 THEN DO:
               MESSAGE "You can't cancel this request anymore"
               VIEW-AS ALERT-BOX INFORMATION.
            END.

            ELSE DO:
               llOk = FALSE.
               
               MESSAGE "Set status to 4?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " CANCEL REQUEST "
               SET llOk.

               IF llOk THEN DO:
                   FIND CURRENT PrePaidRequest EXCLUSIVE-LOCK.
                   ASSIGN PrePaidRequest.PPStatus = 4
                          llChanged               = TRUE.
               END.
            END.
         END.
         
         ELSE IF FRAME-INDEX = 2 THEN DO:

            IF PrePaidRequest.PPStatus = 0 THEN DO:
               MESSAGE "You can't set this request as handled"
               VIEW-AS ALERT-BOX INFORMATION.
            END.

            ELSE DO:
               llOk = FALSE.
               
               MESSAGE "Set status to 9?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " MARK REQUEST AS HANDLED "
               SET llOk.

               IF llOk THEN DO:
                   FIND CURRENT PrePaidRequest EXCLUSIVE-LOCK.
                   ASSIGN PrePaidRequest.PPStatus = 9
                          llChanged               = TRUE.
               END.
            END.
         END.

      END.

      ELSE IF toimi = 5 THEN DO:
   
         lcReturn = "".
         
         IF PrePaidRequest.PPStatus = 99 THEN DO:
            FIND CURRENT PrePaidRequest EXCLUSIVE-LOCK.
            ASSIGN 
               PrePaidRequest.PPStatus = 0
               lcReturn                = "PRINT".     
         END.
         
         Memory = RECID(PrePaidRequest).
            
         RETURN lcReturn.
      END.

      ELSE IF toimi = 8 THEN DO:
      
         IF llChanged OR PrePaidRequest.PPStatus = 99 THEN DO:
            llOk = FALSE.
            MESSAGE "Do You want to cancel your changes?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " CANCEL "
            SET llOk.
            
            IF NOT llOk THEN NEXT.
         END. 
         
         RETURN "CANCEL".
      END.
      
   END. /* repeat */
   
END PROCEDURE.


