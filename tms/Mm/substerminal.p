/* ----------------------------------------------------------------------
  MODULE .......: SubsTerminal
  TASK .........: UPDATEs table SubsTerminal
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.09.09
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SubsTerminal'}
{Func/luhnchecksum.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSubsTerminal AS HANDLE NO-UNDO.
   lhSubsTerminal = BUFFER SubsTerminal:HANDLE.
   RUN StarEventInitialize(lhSubsTerminal).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhSubsTerminal).
   END.

END.

DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO.
DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.

                     
DEF VAR lcCLI         AS CHAR                NO-UNDO.
DEF VAR liMsSeq       AS INT                 NO-UNDO.
DEF VAR xrecid        AS RECID               NO-UNDO  init ?.
DEF VAR FIRSTrow      AS INT                 NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                 NO-UNDO  init 3.
DEF VAR FrmDown       AS INT                 NO-UNDO  init 12.
DEF VAR order         AS INT                 NO-UNDO  init 1.
DEF VAR orders        AS CHAR                NO-UNDO.
DEF VAR maxOrder      AS INT                 NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                 NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                 NO-UNDO  init 0.
DEF VAR pr-order      AS INT                 NO-UNDO.
DEF VAR Memory        AS RECID               NO-UNDO.
DEF VAR RowNo         AS INT                 NO-UNDO.
DEF VAR must-print    AS LOG                 NO-UNDO.
DEF VAR must-add      AS LOG                 NO-UNDO.
DEF VAR ac-hdr        AS CHAR                NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24     NO-UNDO.
DEF VAR i             AS INT                 NO-UNDO.
DEF VAR ok            AS log format "Yes/No" NO-UNDO.
DEF VAR lcBIName      AS CHAR                NO-UNDO.
DEF VAR lcHeader      AS CHAR                NO-UNDO.
DEF VAR lcPurchased   AS CHAR                NO-UNDO.
DEF VAR lcPerContract AS CHAR                NO-UNDO.
DEF VAR lcMemoText    AS CHAR                NO-UNDO.
DEF VAR lcTerminalType AS CHAR               NO-UNDO.

DEF TEMP-TABLE ttTerminal NO-UNDO LIKE SubsTerminal  
   FIELD DbRec    AS INT 
   FIELD CLI      AS CHAR
   INDEX MsSeq MsSeq PurchaseTS DESC
   INDEX CLI CLI PurchaseTS DESC
   INDEX DbRec DbRec.

DEF BUFFER bttTerminal FOR ttTerminal.
    

form
    ttTerminal.MsSeq     
    ttTerminal.CLI       FORMAT "X(9)"  COLUMN-LABEL "MSISDN"
    ttTerminal.BillCode  FORMAT "X(14)" COLUMN-LABEL "Bill.Item"
    lcBIName             FORMAT "X(18)" COLUMN-LABEL "Name"
    ttTerminal.IMEI      FORMAT "X(15)" COLUMN-LABEL "IMEI/Serial No."
    ttTerminal.OrderID   FORMAT ">>>>>>>9" COLUMN-LABEL "Order"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) lcHeader FRAME sel.

form
    SubsTerminal.TerminalID COLON 22 
    SubsTerminal.TerminalType COLON 22 
      lcTerminalType FORMAT "x(20)" NO-LABEL SKIP
    SubsTerminal.MsSeq     COLON 22
    ttTerminal.CLI         COLON 22 FORMAT "X(9)" LABEL "MSISDN"
    SubsTerminal.OrderID   COLON 22 FORMAT ">>>>>>>9" 
    SubsTerminal.PurchaseTS COLON 22
       lcPurchased FORMAT "X(19)" NO-LABEL SKIP
    SubsTerminal.BillCode  COLON 22
       FORMAT "X(16)"
       VALIDATE(CAN-FIND(FIRST BillItem WHERE 
                          BillItem.Brand   = gcBrand AND
                          BillItem.BillCode = INPUT SubsTerminal.BillCode),
                "Unknown billing item")
       lcBIName FORMAT "X(30)" NO-LABEL SKIP
    SubsTerminal.IMEI      COLON 22 FORMAT "X(20)" LABEL "IMEI/Serial No."
    SubsTerminal.Model     COLON 22
    SubsTerminal.ModelColor COLON 22
    SubsTerminal.Manufacturer COLON 22
    SubsTerminal.SimChecked   COLON 22
    SubsTerminal.PerContractID COLON 22 LABEL "Per. Contract ID"
       lcPerContract FORMAT "X(30)" NO-LABEL 
WITH  OVERLAY ROW 4 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form 
    "Subscription ID:" liMsSeq FORMAT ">>>>>>>9"
    HELP "Enter subscription ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Subscription "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form 
    "MSISDN:" lcCLI FORMAT "x(12)"
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fBIName RETURNS LOGIC
   (icBillItem AS CHAR):
   
   lcBIName = "".
   FIND FIRST BillItem WHERE   
        BillItem.Brand   = gcBrand AND
        BillItem.BillCode = icBillItem NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName. 
   
END FUNCTION.

FUNCTION fCreateTemp RETURNS LOGIC:

   IF CAN-FIND(FIRST ttTerminal WHERE 
                     ttTerminal.DbRec = INTEGER(RECID(SubsTerminal)))
   THEN RETURN FALSE.

   CREATE ttTerminal.
   BUFFER-COPY SubsTerminal TO ttTerminal.
   ASSIGN 
      ttTerminal.DbRec = RECID(SubsTerminal)
      ttTerminal.CLI   = MsOwner.CLI.

END FUNCTION.


/* collect accessories to temp-table */
RUN pFillTempTable.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-find-first.

IF AVAILABLE ttTerminal THEN ASSIGN
   Memory       = recid(ttTerminal)
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
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttTerminal WHERE recid(ttTerminal) = Memory 
          NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttTerminal THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttTerminal).
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
        ufk[1] = 35
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttTerminal.BillCode {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttTerminal.BillCode WITH FRAME sel.
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
        FIND ttTerminal WHERE recid(ttTerminal) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttTerminal THEN
              ASSIGN FIRSTrow = i Memory = recid(ttTerminal).
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
           IF NOT AVAILABLE ttTerminal THEN DO:
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
                rtab[1] = recid(ttTerminal)
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
           IF NOT AVAILABLE ttTerminal THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttTerminal).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttTerminal WHERE recid(ttTerminal) = Memory
            NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttTerminal THEN DO:
           Memory = recid(ttTerminal).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttTerminal THEN Memory = recid(ttTerminal).
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
           FIND ttTerminal WHERE recid(ttTerminal) = Memory NO-LOCK.
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
       UPDATE liMsSeq WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liMsSeq > 0 THEN DO:
       
          FIND FIRST ttTerminal WHERE 
                     ttTerminal.MsSeq >= liMsSeq NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttTerminal THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(ttTerminal) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       ttTerminal.BillCode ttTerminal.IMEI.

       RUN local-find-NEXT.
       IF AVAILABLE ttTerminal THEN Memory = recid(ttTerminal).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttTerminal THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttTerminal).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ttTerminal.BillCode ttTerminal.IMEI.

       IF ok THEN DO:

           FIND SubsTerminal WHERE RECID(SubsTerminal) = 
                ttTerminal.DbRec EXCLUSIVE-LOCK.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSubsTerminal).

           DELETE ttTerminal.
           DELETE SubsTerminal.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ttTerminal THEN DO:
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

       FIND SubsTerminal WHERE RECID(SubsTerminal) = 
               ttTerminal.DbRec NO-LOCK.
           
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSubsTerminal).

       ASSIGN ac-hdr = " TERMINAL " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSubsTerminal).

       RUN local-disp-row.
       xrecid = recid(ttTerminal).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttTerminal) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttTerminal) must-print = TRUE.
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
      FIND ttTerminal WHERE recid(ttTerminal) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttTerminal WHERE recid(ttTerminal) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
       FIND FIRST ttTerminal USE-INDEX MsSeq NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
       FIND LAST ttTerminal USE-INDEX MsSeq NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN 
       FIND NEXT ttTerminal USE-INDEX MsSeq NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN 
       FIND PREV ttTerminal USE-INDEX MsSeq NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
          ttTerminal.MsSeq     
          ttTerminal.CLI      
          ttTerminal.BillCode
          lcBIName         
          ttTerminal.IMEI  
          ttTerminal.OrderID  
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   fBIName(ttTerminal.BillCode).

   lcTerminalType = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                  "OrderAccessory",
                                  "TerminalType",
                                  STRING(ttTerminal.TerminalType)).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      ASSIGN
         lcPurchased   = fTS2HMS(SubsTerminal.PurchaseTS)
         lcPerContract = "".
      
      IF SubsTerminal.PerContractID > 0 THEN DO:
         FIND FIRST DCCLI WHERE 
                    DCCLI.MsSeq = SubsTerminal.MsSeq AND
                    DCCLI.PerContractID = SubsTerminal.PerContractID
            NO-LOCK NO-ERROR.
         IF AVAILABLE DCCLI THEN 
            lcPerContract = DCCLI.DCEvent + " " +
                            STRING(DCCLI.ValidFrom,"99.99.99") + "-" +
                            STRING(DCCLI.ValidTo,"99.99.99").
      END.
      
      DISP 
         SubsTerminal.TerminalID  
         SubsTerminal.TerminalType 
            lcTerminalType
         SubsTerminal.MsSeq     
         ttTerminal.CLI         
         SubsTerminal.OrderID 
         SubsTerminal.PurchaseTS 
            lcPurchased 
         SubsTerminal.BillCode
            lcBIName
         SubsTerminal.IMEI    
         SubsTerminal.Model   
         SubsTerminal.ModelColor
         SubsTerminal.Manufacturer 
         SubsTerminal.SimChecked   
         SubsTerminal.PerContractID 
            lcPerContract       
      WITH FRAME lis.
      
      IF NOT NEW SubsTerminal THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[6] = 0
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 6 THEN DO:
            RUN Mc/eventsel ("SubsTerminal",STRING(SubsTerminal.TerminalID)). 
            NEXT. 
         END.
         
         ELSE IF toimi = 8 THEN LEAVE.
      END.
      
      FIND CURRENT SubsTerminal EXCLUSIVE-LOCK.
      
      ehto = 9. RUN Syst/ufkey.
      
      UPDATE
         SubsTerminal.IMEI
      WITH FRAME lis EDITING:
            
         READKEY.
 
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
         THEN DO WITH FRAME lis:
             
            PAUSE 0.
            IF FRAME-FIELD = "IMEI" THEN DO:
            
               IF LENGTH(INPUT SubsTerminal.IMEI,"CHARACTER") NE 15 THEN DO:
                  MESSAGE "IMEI code doesn't contain 15 characters"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            
               IF NOT fLuhnCheckSum( INPUT INPUT SubsTerminal.IMEI) THEN DO:  
                  MESSAGE "IMEI code is not a valid identification number"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            
            END.

         END.
                
         APPLY LASTKEY.
       
      END. /* EDITING */
      
      /* create a memo in case of changes */
      IF ttTerminal.IMEI NE SubsTerminal.IMEI THEN DO:
         FIND MobSub WHERE MobSub.MSSeq = SubsTerminal.MsSeq NO-LOCK NO-ERROR.
         lcMemoText = "IMEI: " + ttTerminal.IMEI + "-->" + SubsTerminal.IMEI. 

         CREATE Memo.
         ASSIGN
               Memo.CreStamp  = fMakeTS()
               Memo.Brand     = gcBrand
               Memo.HostTable = "MobSub"
               Memo.KeyValue  = STRING(SubsTerminal.MsSeq)
               Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
               Memo.CreUser   = katun
               Memo.MemoTitle = "Informacion del cliente modificada"
               Memo.MemoText  = lcMemoText 
               Memo.CustNum   = MobSub.CustNum.

        ttTerminal.IMEI = SubsTerminal.IMEI. 

      END.

      LEAVE. 
   END.

END PROCEDURE.

PROCEDURE pFillTempTable:

   IF iiCustNum > 0 THEN DO:
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.AgrCust = iiCustNum,
          EACH SubsTerminal NO-LOCK WHERE
               SubsTerminal.MsSeq = MsOwner.MsSeq
      BY MsOwner.TsBeg DESC:
         fCreateTemp().   
      END.   

      lcHeader = " TERMINALS OF CUSTOMER " + STRING(iiCustNum) + " ".
   END.

   ELSE IF iiOrderID > 0 THEN DO:
      FOR EACH SubsTerminal NO-LOCK WHERE
               SubsTerminal.Brand   = gcBrand AND
               SubsTerminal.OrderID = iiOrderID,
         FIRST MsOwner NO-LOCK WHERE
               MsOwner.MsSeq = SubsTerminal.MsSeq:
         fCreateTemp().      
      END.   
   
      lcHeader = " TERMINALS OF ORDER " + STRING(iiOrderID) + " ".
   END.
   
   ELSE DO:
      FOR EACH SubsTerminal NO-LOCK WHERE
               SubsTerminal.MsSeq = iiMsSeq,
         FIRST MsOwner NO-LOCK WHERE
               MsOwner.MsSeq = SubsTerminal.MsSeq:
         fCreateTemp().      
      END.   
   
      lcHeader = " TERMINALS OF SUBSCRIPTION " + STRING(iiMsSeq) + " ".
   END.

END PROCEDURE.


