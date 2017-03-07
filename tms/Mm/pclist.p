/* ----------------------------------------------------------------------
  MODULE .......: pclist.p 
  TASK .........: Lists customer/mobsub periodical contracts
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 02/2008 anttis
  CHANGED ......: 
  VERSION ......: xfera
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/eventval.i}
{Func/cparam2.i}
{Rate/daycampaign.i}
{Func/date.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Mc/lib/tokenlib.i}

DEF INPUT PARAMETER  icKeyType AS CHAR        NO-UNDO.
DEF INPUT PARAMETER  iiKey AS INT           NO-UNDO.                

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}
        
    DEF VAR lhDCCLI AS HANDLE NO-UNDO.
    lhDCCLI = BUFFER DCCLI:HANDLE.
    RUN StarEventInitialize(lhDCCLI).
                    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhDCCLI).
    END.
END.

DEF VAR lcFormHeader AS CHAR  NO-UNDO.
DEF VAR ldeSec    AS DEC NO-UNDO.
DEF VAR liSec     AS INT NO-UNDO.
DEF VAR ldaDate   AS DATE NO-UNDO.

DEF VAR icEvent  AS CHAR  NO-UNDO init "".
DEF VAR ldaContractDate AS DATE NO-UNDO.
DEF VAR lcTypeName AS CHARACTER NO-UNDO.

/* temp table for all periodical contracts */
DEFINE TEMP-TABLE ttContract
   FIELD Msseq       LIKE MobSub.MsSeq 
   FIELD CustNum     LIKE MobSub.CustNum
   FIELD Cli         LIKE MobSub.CLI 
   FIELD Contract    LIKE dccli.dcevent
   FIELD PerContractID AS INT
   FIELD Tablename   AS CHAR
   FIELD FieldName   AS CHAR
   FIELD ValidFrom   AS DATE
   FIELD ValidFromTime AS CHAR
   FIELD ContractDate AS DATE
   FIELD RenewalDate AS DATE
   FIELD ValidTo     AS DATE
   FIELD ValidToOrig AS CHAR
   FIELD ValidToTime AS CHAR
   FIELD Active      AS LOG
   FIELD TermDate    AS DATE
   FIELD Memo        AS LOG
   FIELD record      AS RECID 
   FIELD SLCode      AS CHAR 
   FIELD SLSeq       AS INT 
   FIELD Instances   AS INT
   FIELD AgrCust     LIKE MobSub.AgrCust
   FIELD SubsTerminated AS LOG
   FIELD PayType     AS LOG
   FIELD MSID        AS INT
INDEX MsSeq MsSeq Contract
INDEX Cli Cli
INDEX Contract Contract.

def /* new */ shared var siirto AS char.

DEF VAR liMsSeq      AS INT                    NO-UNDO.
DEF VAR lccli        LIKE MobSub.CLI           NO-UNDO.
DEF VAR lcEvent      AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 3.
DEF VAR FrmDown      AS int                    NO-UNDO  init 13.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 3.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcTarget     AS CHAR                   NO-UNDO.
DEF VAR lcDurType    AS CHAR                   NO-UNDO.
DEF VAR lcRuleName   AS CHAR                   NO-UNDO.
DEF VAR lcInclUnit   AS CHAR                   NO-UNDO.
DEF VAR ldaMaxDate   AS DATE                   NO-UNDO INIT 1/1/2049. 
DEF VAR llAdmin      AS LOG                    NO-UNDO INIT FALSE. 
DEF VAR lcMenuOptions AS CHAR NO-UNDO.
DEF VAR lcSelected AS char NO-UNDO. 

FORM
    ttContract.MsSeq       COLUMN-LABEL "Subscr.ID"
    ttContract.CLI         COLUMN-LABEL "MSISDN"
    ttContract.Contract FORMAT "x(19)"  
        COLUMN-LABEL "Contract" HELP "Periodical Contract ID"
    ttContract.ValidFrom   COLUMN-LABEL "From"        FORMAT "99-99-99"
    ttContract.ValidTo     COLUMN-LABEL "To"          FORMAT "99-99-9999"
    ttContract.Active      COLUMN-LABEL "Act"         FORMAT "YES/NO"
    ttContract.TermDate    COLUMN-LABEL "Terminate"   FORMAT "99-99-9999"
    ttContract.Memo        COLUMN-LABEL "M"           FORMAT "M/"
    
    WITH ROW FrmRow CENTERED overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + lcFormHeader + " " +  " "
FRAME sel.

FORM
   " Contract:" ttContract.Contract FORMAT "x(35)" 
      "Subscr.ID...:" AT 50 ttContract.MsSeq SKIP
   " Contr.ID:" ttContract.MSID FORMAT ">>>>>>>>9"
                               "MSISDN......:" AT 50 ttContract.CLI SKIP
   " Type....:" DayCampaign.DCType FORMAT "x(1)" lcTypeName FORMAT "x(24)" 
                                "Agr.Customer:" AT 50 ttContract.AgrCust SKIP
   " Target..:" DayCampaign.DCTarget FORMAT "x(1)" lcTarget FORMAT "x(24)" SKIP
   "--------------------------------------------------------------------------"
    SKIP
   " Period type.....:" DayCampaign.DurType FORMAT 9 
                        lcDurType FORMAT "x(35)" SKIP
   " Renewal rule....:" DayCampaign.Renewal FORMAT 9 
                        lcRuleName FORMAT "x(24)" SKIP
   
   " Period unit.....:" DayCampaign.DurUnit FORMAT 9 
                        lcInclUnit FORMAT "x(24)" SKIP
   " Period length...:" DayCampaign.DurMonths FORMAT ">>9"  SKIP(1)
   
   " Effective date..:" ttContract.ValidFrom FORMAT "99-99-9999"
                        ttContract.ValidFromTime FORMAT "x(8)" SKIP
   " Expiration date.:" ttContract.ValidTo   FORMAT "99-99-9999"
                        ttContract.ValidToTime FORMAT "x(8)"
                        ttContract.ValidToOrig FORMAT "x(35)" SKIP
   "--------------------------------------------------------------------------"
    SKIP
   " Contract date...:" ttContract.ContractDate FORMAT "99-99-9999" SKIP
   " Termination date:" ttContract.TermDate FORMAT "99-99-9999" SKIP
   " Extension date..:" ttContract.RenewalDate FORMAT "99-99-9999"  

   WITH  overlay row 2 centered
   COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) ac-hdr 
   NO-LABELS SIDE-LABELS
    /*1 columns*/
FRAME lis.

form
    liMsSeq FORMAT ">>>>>>>>9" 
    HELP "Enter subscription ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SUBSCRIPTION ID " 
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.
 
form
    lCCli FORMAT "X(12)"
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN " 
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.
    
form 
     lcEvent FORMAT "X(12)"
     HELP "Enter Contract"
     WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CONTRACT "
     COLOR VALUE(cfc) NO-labels overlay FRAME f3.


/* read all mobsub periodical contracts to temp table */ 
PROCEDURE pReadContracts:
   
   DEF INPUT PARAM ihMobSub AS HANDLE NO-UNDO.

   DEFINE VARIABLE liPools AS INTEGER NO-UNDO. 
   
   FOR EACH DCCLI WHERE
            DCCLI.MSSeq = ihMobSub::Msseq NO-LOCK:
     
      FIND FIRST Memo WHERE  
          Memo.Brand     = gcBrand                AND 
          Memo.HostTable = "MobSub"               AND 
          Memo.KeyValue  = STRING(DCCLI.MsSeq)    AND 
          Memo.Memotitle begins "Periodical" NO-LOCK NO-ERROR. 

      CREATE ttContract.
      ASSIGN
         ttContract.MSID = DCCLI.PercontractId
         ttContract.ValidFrom = DCCLI.ValidFrom
         ttContract.ValidTo   = DCCLI.ValidTo
         ttContract.ValidToOrig = (IF DCCLI.ValidToOrig NE ?
                                   THEN "Original exp. date: " + 
                                        STRING(DCCLI.ValidToOrig,"99-99-9999")
                                   ELSE "") 
         ttContract.AgrCust = ihMobSub::AgrCust
         ttContract.SubsTerminated = (ihMobSub:TABLE = "TermMobsub")
         ttContract.Contract  = DCCLI.DCEvent
         ttContract.PerContractID = DCCLI.PerContractID
         ttContract.TermDate  = DCCLI.TermDate
         ttContract.ContractDate = DCCLI.ContractDate
         ttContract.RenewalDate = DCCLI.RenewalDate

         ttContract.CLI       = ihMobSub::CLI
         ttContract.MsSeq     = ihMobSub::MsSeq
         ttContract.CustNum   = 0
         ttContract.Memo      = (AVAIL Memo)
         ttContract.Record    = RECID(DCCLI)
         ttContract.PayType   = ihMobSub::PayType
         ttContract.Active    = (IF DCCLI.ValidTo   >= TODAY  AND
                                    DCCLI.ValidFrom <= TODAY THEN TRUE 
                                 ELSE FALSE).

   END.

   FOR EACH MServiceLimit WHERE
      MServiceLimit.MSSeq = ihmobsub::msseq NO-LOCK:
      
      ldeSec = fMakeTS().
      
      FIND FIRST Memo WHERE  
          Memo.Brand     = gcBrand                AND 
          Memo.HostTable = "MobSub"               AND 
          Memo.KeyValue  = STRING(DCCLI.MsSeq)    AND 
          Memo.Memotitle begins "Periodical" NO-LOCK NO-ERROR. 
      
      CREATE ttContract.
      
      FIND FIRST ServiceLimit WHERE
                 ServiceLimit.slseq    = mServiceLimit.slseq AND 
                 ServiceLimit.dialtype = mServicelimit.dialtype
      NO-LOCK NO-ERROR.
      IF avail servicelimit THEN ASSIGN  
         ttContract.SLCode   = servicelimit.SLCode
         ttContract.Contract = servicelimit.groupcode.

      liPools = 0.
      FOR EACH MServiceLPool WHERE
               MServiceLPool.MsSeq = MServiceLimit.MsSeq AND
               MServiceLPool.SlSeq = MServiceLimit.SlSeq AND
               MServiceLPool.EndTS <= MServiceLimit.EndTs AND
               MServiceLPool.FromTS >= MServiceLimit.FromTS NO-LOCK:
         liPools = liPools + 1.
      END.
      
      ASSIGN
         ttContract.TermDate  = ? 
         ttContract.Instances = liPools 
         ttContract.AgrCust = ihMobSub::AgrCust
         ttContract.CLI       = ihMobSub::CLI
         ttContract.MsSeq     = ihMobSub::MsSeq
         ttContract.CustNum   = MServiceLimit.CustNum
         ttContract.SLSeq     = MServiceLimit.SLSeq
         ttContract.MSID      = MServiceLimit.MSID
         ttContract.SubsTerminated = (ihMobSub:TABLE = "TermMobsub")
         ttContract.Memo     = (AVAIL Memo) 
         ttContract.ContractDAte = ihMobSub::CreationDate
         ttContract.Record    = RECID(MServiceLimit)
         ttContract.PayType   = ihMobSub::PayType
         ttContract.Active    = (IF MServiceLimit.EndTS >= ldeSec AND
                                    MServiceLimit.FromTS <= ldeSec THEN TRUE
                                 ELSE FALSE)
         .
      
      fSplitTS(input MServiceLimit.FromTS, output ldaDate, output liSec).
      ttContract.ValidFrom = ldaDate.
      ttContract.ValidFromTime = STRING(liSec,"HH:MM:SS"). 
      IF MServiceLimit.EndTs >= 99999999 THEN ttContract.ValidTo = 12/31/2049.
      ELSE DO:
         fSplitTS(input MServiceLimit.EndTS, output ldaDate, output liSec).
         ttContract.ValidTo = ldaDate.
         ttContract.ValidToTime = STRING(liSec,"HH:MM:SS"). 
      END.
      
   END.

END PROCEDURE. 

IF icKeyType = "mobsub" THEN DO:
   lcFormHeader = "SUBSCRIPTION'S PERIODICAL CONTRACTS".

   FIND mobsub where
        mobsub.msseq = iiKey NO-LOCK NO-ERROR.
   IF AVAIL Mobsub THEN RUN pReadContracts((BUFFER Mobsub:HANDLE)).
   ELSE DO:
      FIND TermMobsub WHERE
           TermMobsub.MsSeq = iiKey NO-LOCK NO-ERROR.
      IF NOT AVAIL TermMobsub THEN DO:
         MESSAGE "Subscription" iiKey "not found" VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
      ELSE RUN pReadContracts((BUFFER TermMobsub:HANDLE)).       
   END.
   
END.
ELSE IF icKeyType = "customer" THEN DO:
   lcFormHeader = "CUSTOMER'S PERIODICAL CONTRACTS".
   FIND FIRST Customer WHERE Customer.Custnum = iiKey NO-LOCK NO-ERROR.
   FOR EACH Mobsub WHERE
      Mobsub.Brand   = gcBrand AND
      Mobsub.Custnum = iiKey NO-LOCK:
      RUN pReadContracts ((BUFFER Mobsub:HANDLE)).
   END.
END.   

IF getTMSRight("CCSUPER,SYST") EQ "RW" THEN llAdmin = TRUE.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE ttContract THEN ASSIGN
   memory       = recid(ttContract)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = false.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND ttContract WHERE recid(ttContract) = memory NO-LOCK NO-ERROR.
        
        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttContract THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(ttContract).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1] = 1645 ufk[2]= 209  ufk[3]= 1045 
        ufk[4]=0
        ufk[4]= 1068 WHEN llAdmin AND
                          icKeyType eq "mobsub"
        ufk[5]= 2240 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.

        IF icKeyType = "mobsub" THEN ASSIGN
           ufk[1] = 0 
           ufk[2] = 0
           ufk[3] = 0.
        
        IF icEvent > "" THEN ufk[3] = 0.
           
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row ttContract.MsSeq {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttContract.MsSeq WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row ttContract.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttContract.CLI WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        choose row ttContract.Contract {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttContract.Contract WITH FRAME sel.
      END.
      
      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"4,f4,8,f8") = 0 THEN DO:
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND ttContract WHERE recid(ttContract) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE DCCLI THEN
              ASSIGN FIRSTrow = i memory = recid(ttContract).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttContract THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttContract)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttContract THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(ttContract).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttContract WHERE recid(ttContract) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttContract THEN DO:
           memory = recid(ttContract).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttContract THEN memory = recid(ttContract).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND ttContract WHERE recid(ttContract) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */
 
     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f1.
       SET liMsSeq WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF liMsSeq ENTERED THEN DO:
          FIND FIRST ttContract WHERE ttContract.MsSeq = liMsSeq
          /* srule */ USE-INDEX MsSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttContract THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DCCLI/DCCLI was found */
          ASSIGN order = 1 memory = recid(ttContract) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */
 
     /* Search by column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f2.
       SET lCCli WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lCCli ENTERED THEN DO:
          FIND FIRST ttContract WHERE ttContract.cli = lcCli
          /* srule */ USE-INDEX CLI NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttContract THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DCCLI/DCCLI was found */
          ASSIGN order = 2 memory = recid(ttContract) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */
                   
     /* Search by col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME F3.
       SET lCEvent WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF lCEvent ENTERED THEN DO:
          FIND FIRST ttContract WHERE ttContract.Contract = lCEvent
          USE-INDEX Contract /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttContract THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DCCLI/DCEvent was found */
          ASSIGN order = 3 memory = recid(ttContract) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     IF LOOKUP(nap,"f4") > 0 AND ufk[4] > 0 THEN DO:
        RUN local-find-this(false).
        /*YPR-4775*/
        /*Operation is not allowed if fixed line provisioning is pending*/
        IF MobSub.MsStatus EQ {&MSSTATUS_FIXED_PROV_ONG} /*16*/ THEN DO:
           MESSAGE "Mobile line provisioning is not complete"
              VIEW-AS ALERT-BOX.
        END.
        ELSE DO:
           RUN Syst/selectbox.p(
              "PERIODICAL CONTRACT FUNCTION",
              " CREATE NEW CONTRACT      ",
              OUTPUT lcSelected).
            
           CASE lcSelected: 
              WHEN " CREATE NEW CONTRACT      " THEN RUN Mm/dccliadd.p(iiKey).
           END.
        END. 
        ufkey = true.
        
     END.
     
     ELSE IF LOOKUP(nap,"5,F5") > 0 THEN DO:
         
         FIND FIRST ttContract WHERE
              RECID(ttContract) = rtab[FRAME-LINE]
         NO-LOCK NO-ERROR.

         RUN Mc/memo.p
              (INPUT 0,
              "mobsub",
              STRING(ttContract.msseq),
              "MEMOS").
                             
         ufkey = true.
         
     END.
     
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis
     ON ENDKEY UNDO, LEAVE:

       /* view */
       RUN local-find-this(false).
       
       ASSIGN ac-hdr = " PERIODICAL CONTRACT : " + ttContract.Contract +
                       IF ttContract.PerContractID > 0 
                       THEN "  ID: " + STRING(ttContract.PerContractID) 
                       ELSE "".
        
       ufkey = true.
      
       PAUSE 0.
       
       CLEAR FRAME lis NO-PAUSE.
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       RUN local-disp-row.
       xrecid = recid(ttContract).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttContract) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttContract) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find ttContract WHERE recid(ttContract) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       find ttContract WHERE recid(ttContract) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-FIRST:
      IF Order = 1 THEN 
         FIND FIRST ttContract USE-INDEX MsSeq NO-LOCK NO-ERROR.
      ELSE IF Order = 2 THEN 
         FIND FIRST ttContract USE-INDEX CLI NO-LOCK NO-ERROR.
      ELSE IF Order = 3 THEN
         FIND FIRST ttContract USE-INDEX Contract NO-LOCK NO-ERROR.     

END PROCEDURE.

PROCEDURE local-find-LAST:
   
   IF Order = 1 THEN 
      FIND LAST ttContract USE-INDEX MsSeq NO-LOCK NO-ERROR.
   ELSE IF Order = 2 THEN 
      FIND LAST ttContract USE-INDEX CLI NO-LOCK NO-ERROR.
   ELSE IF Order = 3 THEN
      FIND LAST ttContract USE-INDEX Contract NO-LOCK NO-ERROR.     
            
END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   IF Order = 1 THEN 
      FIND NEXT ttContract USE-INDEX MsSeq NO-LOCK NO-ERROR.
   ELSE IF Order = 2 THEN 
      FIND NEXT ttContract USE-INDEX CLI NO-LOCK NO-ERROR.
   ELSE IF Order = 3 THEN
      FIND NEXT ttContract USE-INDEX Contract NO-LOCK NO-ERROR.     

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF Order = 1 THEN 
      FIND PREV ttContract USE-INDEX MsSeq NO-LOCK NO-ERROR.
   ELSE IF Order = 2 THEN 
      FIND PREV ttContract USE-INDEX CLI NO-LOCK NO-ERROR.
   ELSE IF Order = 3 THEN
      FIND PREV ttContract USE-INDEX Contract NO-LOCK NO-ERROR.     

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.
      

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
   ttContract.MsSeq  
   ttContract.CLI 
   ttContract.Contract + "/" + ttContract.SLCode WHEN ttContract.SLCode NE ""
   @ ttContract.Contract
   ttContract.Contract WHEN ttContract.SLCode EQ "" 
   ttContract.ValidFrom
   ttContract.ValidTo WHEN ttContract.ValidTo <= ldaMaxdate
   "99-99-9999" WHEN ttContract.ValidTo > ldaMaxDate @ ttContract.ValidTo
   ttContract.Active 
   ttContract.TermDate 
   ttContract.Memo
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
   
   FIND FIRST DayCampaign NO-LOCK WHERE
      DayCampaign.Brand = gcBrand AND 
      DayCampaign.DCEvent = ttContract.Contract NO-ERROR.
   
   IF DayCampaign.DCType = "1" THEN DO:
   FIND FIRST ServiceLimit WHERE RECID(ServiceLimit) = ttContract.Record
   NO-LOCK NO-ERROR.
   END.
   ELSE RELEASE ServiceLimit.

END PROCEDURE.

PROCEDURE local-update-record:
   UPDATE-LOOP:   
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      FIND FIRST DayCampaign NO-LOCK WHERE
         DayCampaign.Brand = gcBrand AND 
         DayCampaign.DCEvent = ttContract.Contract NO-ERROR.

      /* Get Type description */
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "SLGAanalyse"   AND
                 TMSCodes.FieldName    = "SLGAType"      AND
                 TMSCodes.CodeGroup    = "Servicelimit"  AND
                 TMSCodes.CodeValue    = DayCampaign.DCtype
      NO-LOCK NO-ERROR.
      lcTypeName = (IF AVAIL TMSCodes THEN TmsCodes.Codename ELSE "").
      
      /* Get Target description */
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "DayCampaign"   AND
                 TMSCodes.FieldName    = "DCTarget"      AND
                 TMSCodes.CodeGroup    = "PerContr"      AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.DCTarget)
      NO-LOCK NO-ERROR.   
      lcTarget = (IF AVAIL TMSCodes THEN TmsCodes.Codename ELSE "").
      
      /* Get Period type description */
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "Daycampaign"   AND
                 TMSCodes.FieldName    = "DurType"       AND
                 TMSCodes.CodeGroup    = "DCCounter"         AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.DurType)
      NO-LOCK NO-ERROR.   
      lcDurType = (IF AVAIL TMSCodes THEN TmsCodes.Codename ELSE "").
   
      /* Get Renewal rule description */
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "DayCampaign"   AND
                 TMSCodes.FieldName    = "Renewal"       AND
                 TMSCodes.CodeGroup    = "Rrule"      AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.Renewal)
      NO-LOCK NO-ERROR.   
      lcRuleName = (IF AVAIL TMSCodes THEN TmsCodes.Codename ELSE "").
   
      /* Get Period unit description */
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "Daycampaign"   AND
                 TMSCodes.FieldName    = "DurUnit"       AND
                 TMSCodes.CodeGroup    = "PerContr"      AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.DurUnit)
      NO-LOCK NO-ERROR. 
      IF AVAIL TMSCodes THEN lcInclUnit = TMSCodes.CodeName.

      DEFINE VARIABLE liUpsells AS INTEGER NO-UNDO. 

      DISP 
      
      ttContract.Contract + "/" + ttContract.SLCode 
         WHEN ttContract.SLCode NE "" AND
              ttContract.Instances = 0
              @ ttContract.Contract
      ttContract.Contract + "/" + ttContract.SLCode +
            " (" + STRING(ttContract.Instances) + ")"
         WHEN ttContract.SLCode NE "" AND
              ttContract.Instances ne 0
              @ ttContract.Contract

      ttContract.Contract WHEN ttContract.SLCode EQ "" 
      ttContract.MsSeq
      ttContract.CLI
      ttContract.MSID
      DayCampaign.DcTarget lcTarget
      DayCampaign.DurType lcDurType
      DayCampaign.DCType lcTypeName
      DayCampaign.Renewal lcRuleName
      DayCampaign.DurUnit lcInclUnit
      DayCampaign.DurMonths
      ttContract.ValidFrom
      ttContract.ValidFromTime
      ttContract.ValidTo WHEN ttContract.ValidTo <= ldaMaxDate 
         "99-99-9999" WHEN ttContract.ValidTo > ldaMaxDate @ ttContract.ValidTo
      ttContract.ValidToTime
      ttContract.ValidToOrig
      ttContract.ContractDate
      ttContract.RenewalDate
      ttContract.TermDate
      ttContract.AgrCust
      WITH FRAME lis.
   
   ASSIGN
     ufk = 0
     ufk[1]= 0    ufk[2]= 0    ufk[3]= 2244
     ufk[4]= 1068 WHEN llAdmin AND NOT ttContract.SubsTerminated
     ufk[5]= 927  ufk[6]= 1752 ufk[7]= 1036 ufk[8]= 8 ufk[9]= 1
     ehto = 3.

     RUN Syst/ufkey.p.
   READKEY. 
   ASSIGN nap = keylabel(LASTKEY).
   
   IF LOOKUP(nap,"f3") > 0 AND ufk[3] > 0 THEN DO: 
      RUN Mm/msrequest.p(?,?,ttContract.MsSeq,0,0,ttContract.Contract ).
      NEXT UPDATE-LOOP.
   END.   
   
   IF LOOKUP(nap,"f4") > 0 AND ufk[4] > 0 THEN DO:

      RUN Syst/selectbox.p(
         "PERIODICAL CONTRACT FUNCTION",
         " CONTRACT TERMINATION     " + "|" + 
         " DENY SINGLE FEE CREATION ",
         OUTPUT lcSelected).
            
      CASE lcSelected:
         
         WHEN " CONTRACT TERMINATION     " THEN RUN Mm/dccliterm.p(ttContract.MsSeq, ttContract.Contract, ttContract.PerContractID).
         WHEN " DENY SINGLE FEE CREATION " THEN RUN Mm/dcclifees.p(ttContract.MsSeq, ttContract.Contract).

      END.
      
      NEXT UPDATE-LOOP.
   
   END.
   
   IF LOOKUP(nap,"f5") > 0  AND ufk[5] > 0 THEN DO: 
      
      RUN Mc/memo.p
         (INPUT 0,
         "mobsub",
         STRING(ttContract.msseq),
         "MEMOS").
      
      NEXT UPDATE-LOOP.

   END.   
   
   ELSE IF LOOKUP(nap,"6,F6") > 0 THEN DO:
      
      IF DayCampaign.DCType = "1" THEN 
         RUN Mc/eventsel.p("mservicelimit",
         "#BEGIN" + chr(255) + STRING(ttContract.MsSeq)).
      ELSE
         RUN Mc/eventsel.p("dccli",
         "#BEGIN" + chr(255) 
         + STRING(ttContract.MsSeq) + chr(255) 
         + STRING(ttContract.Contract)).    

      NEXT UPDATE-LOOP.
   
   END.

   IF LOOKUP(nap,"f7") > 0 THEN DO:
      
      lcMenuOptions = "". 
      
      CASE DayCampaign.DCType:
         WHEN "1" THEN 
            lcMenuOptions = " SERVICE PACKAGE COUNTERS ".
         WHEN "2" THEN 
            lcMenuOptions = " RATING LIMIT COUNTERS    ".
         WHEN "3" THEN IF DayCampaign.TermFeeCalc > 0 THEN
            lcMenuOptions = " PENALTY FEE RULES        ".
         WHEN "4" OR WHEN "8" THEN DO:
            IF LOOKUP(DayCampaign.DCEvent,{&DSS_BUNDLES}) > 0 THEN
               lcMenuOptions = " RATING LIMIT COUNTERS    " + "|" +
                               " RATING POOLS             ".
            ELSE
               lcMenuOptions = " RATING LIMIT COUNTERS    ".
         END. /* WHEN "4" THEN DO: */
         WHEN "2" OR WHEN "6" THEN 
            lcMenuOptions = " RATING LIMIT COUNTERS    " + "|" +
                            " RATING POOLS             ".

      END.
      
      RUN Syst/selectbox.p(
         "PERIODICAL CONTRACT FUNCTION",
         lcMenuOptions,
         OUTPUT lcSelected).
            
      IF lcSelected NE "" THEN DO:
         CASE DayCampaign.DCType: 
            WHEN "1" OR WHEN "2" OR WHEN "4" OR WHEN "6" OR WHEN "8" THEN DO:
               IF TRIM(lcSelected) EQ "RATING POOLS" THEN DO:
                  FIND MServiceLimit NO-LOCK WHERE
                       RECID(MServiceLimit) = ttContract.Record.
                  RUN Mm/mservicelpool.p(
                     ttContract.msseq,
                     MServiceLimit.SLSeq,
                     ttContract.validFrom,
                     ttContract.validTo).
               END.
               ELSE IF ttContract.PayType THEN
               RUN Mm/pre_dccounterper.p(
                  ttContract.Msseq,
                  ttContract.Contract,
                  ttContract.SLSeq,
                  ttContract.MSID,
                  ttContract.ValidFrom,
                  ttContract.ValidTo).
               ELSE               
               RUN Mm/dccounterper.p(
                  ttContract.Msseq,
                  ttContract.CustNum,
                  ttContract.Contract,
                  ttContract.SLSeq,
                  ttContract.MSID,
                  ttContract.ValidFrom,
                  ttContract.ValidTo). 
             END.
             WHEN "3" THEN DO:
               RUN Mm/penfeerules.p(ttContract.Record).
             END.  
         END.   
      END.      

      NEXT UPDATE-LOOP.
   END.   
   
   IF LOOKUP(nap,"f8") > 0 THEN DO:
      HIDE FRAME choices NO-PAUSE.
      HIDE MESSAGE.
      LEAVE UPDATE-LOOP.
   END.   

   END.
END PROCEDURE.

