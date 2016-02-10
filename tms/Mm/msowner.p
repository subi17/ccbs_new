/* ----------------------------------------------------------------------
  MODULE .......: MSOwner.p
  TASK .........: Browser of mobile subscription owners
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 11-08-99
  CHANGED ......: 06.10.99 pt  Layout, F4
                  07.12.99 jp  MSOwner.IMSI
                  29.12.99 jpo Layout, F4, Added UPDATE MSOwner.TsEnd
                  14.10.02 jr  Removed BillLevel
                  06.11.02 jr  Eventlog
                  03.03.03 tk  tokens
                  04.09.03 jp  Brand 
                  02.12.03 aam Contract
                  05.11.04 tk  view frame sel moved after find
                  03.02.05 aam get username from MsOwner not MobSub,
                               disp addresses in detailed frame
                  05.08.05 aam change address with Password
                  11.08.05 aam release after updates
                  06.10.05 aam update billtarget (no checks!)
                  28.12.05 mvi billtarget checks + other fields as well
                  26.01.06 aam InvCust, AgrCust,
                               timestamp handling
                  20.11.06 jp  frame sel changed
                  07.11.07 jt  minor frame changes
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MSOwner'}
{Func/cparam2.i}
{Func/timestamp.i}

DEF /* NEW */ SHARED VAR siirto AS CHAR .

DEF /* VAR */ INPUT PARAMETER msseq LIKE MsOwner.msseq NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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
DEF VAR end-txt      AS C   format "x(20)"     NO-UNDO.
DEF VAR lcPassword   AS CHAR                   NO-UNDO. 
DEF VAR lcTLPassword AS CHAR                   NO-UNDO. 
DEF VAR lcAskPassWd  AS CHAR                   NO-UNDO.
DEF VAR lcAgrName    AS CHAR                   NO-UNDO.
DEF VAR lcInvName    AS CHAR                   NO-UNDO.
DEF VAR lcCustName   AS CHAR                   NO-UNDO FORMAT "X(25)" .
DEF VAR ldtBegDate   AS DATE                   NO-UNDO.
DEF VAR liBegTime    AS INT                    NO-UNDO.
DEF VAR ldtEndDate   AS DATE                   NO-UNDO.
DEF VAR liEndTime    AS INT                    NO-UNDO.
DEF VAR lcBegTime    AS CHAR                   NO-UNDO.
DEF VAR lcEndTime    AS CHAR                   NO-UNDO.
DEF VAR llUpdate     AS LOG                    NO-UNDO. 
DEF VAR lcDivider    AS CHAR                   NO-UNDO.
DEF VAR llTerminated AS LOG NO-UNDO. 
DEF VAR llVendor     AS LOG NO-UNDO.

llTerminated = NOT CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq = MsSeq). 

IF getTMSRight("VENDOR") EQ "RW" THEN llVendor = TRUE.

lcDivider = FILL("-",76).

DEF BUFFER bCustomer FOR Customer.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMSOwner AS HANDLE NO-UNDO.
   lhMSOwner = BUFFER MSOwner:HANDLE.
   RUN StarEventInitialize(lhMSOwner).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMSOwner).
   END.
END.

form
    MsOwner.AgrCust    FORMAT ">>>>>>>9"
    MsOwner.InvCust    FORMAT ">>>>>>>9"
    MSOwner.CustNum    format ">>>>>>>9"  COLUMN-LABEL "User"
    lcCustName         FORMAT "x(14)" COLUMN-LABEL "User Name"
    ldtBegDate         FORMAT "99-99-99" COLUMN-LABEL "DateFrom"
       HELP "Begin date"
    lcBegTime          FORMAT "xx:xx:xx" COLUMN-LABEL "TimeFrom"
       HELP "Begin time" 
    ldtEndDate         FORMAT "99-99-99" COLUMN-LABEL "DateTo  "
       HELP "End date (31-12-54 means current user)"
    lcEndTime          FORMAT "xx:xx:xx" COLUMN-LABEL "TimeTo  "
       HELP "End time"
WITH OVERLAY ROW FrmRow FrmDown DOWN centered 

    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " KNOWN OWNERS OF MOBILE SUBSCRIPTION ID" + STRING(MSOwner.MSSeq) + " "
    FRAME sel.

form
    "MSISDN Number ...:" MSOwner.CLI   NO-LABEL SKIP
    "Subscription ID .:" MsOwner.MsSeq NO-LABEL SKIP
    "Agr.Customer ....:" MSOwner.AgrCust NO-LABEL
       lcAgrName  AT 35 FORMAT "X(40)" NO-LABEL SKIP
    "Inv.Customer ....:" MsOwner.InvCust NO-LABEL
       lcInvName  AT 35 FORMAT "X(40)" NO-LABEL SKIP
    "User ............:" MSOwner.CustNum NO-LABEL   
       lcCustName AT 35 NO-LABEL FORMAT "X(40)" 
           SKIP
       Customer.Address    AT 35 NO-LABEL    SKIP
       Customer.ZipCode    AT 35 NO-LABEL
       Customer.PostOffice NO-LABEL SKIP
       Customer.Country    AT 35 NO-LABEL SKIP(1)
    "Valid from ......:" ldtBegDate 
          FORMAT "99-99-9999" NO-LABEL
          HELP "Begin date"
       " Time:" lcBegTime 
          FORMAT "xx:xx:xx" NO-LABEL 
          HELP "Begin time" SKIP              
    "Valid to ........:" ldtEndDate 
          FORMAT "99-99-9999" NO-LABEL
          HELP "End date (31-12-54 means current user)"
       " Time:" lcEndTime 
          FORMAT "xx:xx:xx" NO-LABEL
          HELP "End time"
       SPACE(2) end-txt NO-LABEL  SKIP
    "IMSI Number .....:" MSOwner.IMSI NO-LABEL        SKIP
    "Cli.type.........:" MSOwner.Clitype NO-LABEL
        CLIType.CliName AT 35 NO-LABEL
        MSOwner.TariffBundle AT 60 NO-LABEL           SKIP
    "Billing Target ..:" MSOwner.BillTarget NO-LABEL
       BillTarg.RatePlan  AT 35 NO-LABEL  SKIP
    "CliEvent ........:" MSOwner.CliEvent FORMAT "X(5)" NO-LABEL   SKIP
    "InPortOperator...:" msowner.inportOper NO-LABEL  SKIP
    "Mandate .........:" Msowner.MandateID FORMAT "x(31)" NO-LABEL
                         Msowner.MandateDate FORMAT "99-99-9999" NO-LABEL SKIP
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    /*1 columns*/
    FRAME lis.

FUNCTION fChkTime RETURNS LOGICAL
   (INPUT-OUTPUT icTime AS CHAR):

   DEF VAR liTimeChk AS INT NO-UNDO.
   
   /* seconds */
   liTimeChk = INTEGER(SUBSTRING(icTime,5,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Time cannot contain letters"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   IF liTimeChk > 59 THEN DO:
      MESSAGE "Seconds can't be more than 59"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
    
   /* minutes */
   liTimeChk = INTEGER(SUBSTRING(icTime,3,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Time cannot contain letters"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   IF DECIMAL(SUBSTRING(icTime,3,2)) > 59 THEN DO:
      MESSAGE "Minutes can't be more than 59"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
               
   /* hours */
   liTimeChk = INTEGER(SUBSTRING(icTime,1,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Time cannot contain letters"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   IF DECIMAL(SUBSTRING(icTime,1,2)) > 23 THEN DO:
      MESSAGE "Hours can't be more than 23"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   
   icTime = SUBSTRING(icTime,1,2) + ":" + 
            SUBSTRING(icTime,3,2) + ":" +
            SUBSTRING(icTime,5,2).
            
   RETURN TRUE.
   
END FUNCTION.                    


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.

orders = "By Date,By 2,By 3, By 4".

ASSIGN lcPassword   = fCParamC("MSAddressChg")    
       lcTLPassword = fCParamC("MSTimeLabelChg").
        
IF lcPassword = ? THEN lcPassword = "".

FIND FIRST MSOwner WHERE 
           MSOwner.msseq = msseq NO-LOCK NO-ERROR.
IF AVAILABLE MSOwner THEN ASSIGN
   Memory       = recid(MSOwner)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO: 
  MESSAGE
  "This MSISDN Number doesn't have any user !"
  VIEW-AS ALERT-BOX ERROR.
  LEAVE.
END.

VIEW FRAME sel.

LOOP:
REPEAT WITH FRAME sel:
    /* Changed */
    IF order <> pr-order AND MaxOrder = 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a MSOwner  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MSOwner.CLI
           VALIDATE
              (MSOwner.CLI NOT ENTERED OR
              NOT CAN-FIND(MSOwner using  MSOwner.CLI),
              "MSOwnerS " + string(INPUT MSOwner.CLI) +
              " already exists !").
           IF INPUT FRAME lis MSOwner.CLI NOT ENTERED THEN 
           LEAVE add-row.
           CREATE MSOwner.
           ASSIGN
           MSOwner.CLI   = INPUT FRAME lis MSOwner.CLI
           MSOwner.Brand = gcBrand .

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSOwner).
           ASSIGN
           Memory = recid(MSOwner)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSOwner
      WHERE MSOwner.MSSEQ = Msseq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSOwner THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSOwner THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSOwner).
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
        ufk = 0
        ufk[1]= /*35*/ 0     ufk[2]= 0 ufk[3]= 0
        ufk[4]= 7 WHEN llVendor AND NOT llTerminated
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSOwner.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSOwner.CustNum WITH FRAME sel.
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
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSOwner THEN
              ASSIGN FIRSTrow = i Memory = recid(MSOwner).
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
           IF NOT AVAILABLE MSOwner THEN DO:
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
                rtab[1] = recid(MSOwner)
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
           IF NOT AVAILABLE MSOwner THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSOwner).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSOwner THEN DO:
           Memory = recid(MSOwner).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSOwner THEN Memory = recid(MSOwner).
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
           FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"F4,4") > 0 AND NOT llTerminated THEN DO TRANS:
        RUN local-find-this(TRUE).

        llUpdate = FALSE.
          
        IF lcTLPassword > "" THEN DO:
           lcAskPassWd = "".
           PAUSE 0.
           UPDATE lcAskPassWd 
              BLANK
              FORMAT "X(20)" 
              LABEL "Password" 
              HELP "Password for changing data" 
           WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE DATA "
                   SIDE-LABELS FRAME framePassword.
             
           llUpdate = (lcAskPassWd = lcTLPassword). 
        END.

        IF llUpdate AND lcRight = "RW" THEN DO:

           ehto = 9. RUN Syst/ufkey.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOwner).

           RUN local-find-others. 
            
           REPEAT WITH FRAME sel ON ENDKEY UNDO, LEAVE:
           
              ASSIGN lcBegTime = REPLACE(lcBegTime,":","")
                     lcEndTime = REPLACE(lcEndTime,":","").
                     
              UPDATE 
                 ldtBegDate
                 lcBegTime
                 ldtEndDate
                 lcEndTime
              WITH FRAME sel EDITING:
                 READKEY.

                 IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
                 THEN DO WITH FRAME sel:
                    PAUSE 0.

                    IF LOOKUP(FRAME-FIELD,"lcBegTime,lcEndTime") > 0 THEN DO:
                       IF FRAME-FIELD = "lcBegTime" 
                       THEN i = INTEGER(INPUT FRAME sel lcBegTime) NO-ERROR.
                       ELSE i = INTEGER(INPUT FRAME sel lcEndTime) NO-ERROR.

                       IF ERROR-STATUS:ERROR THEN DO:
                          MESSAGE "Time cannot contain letters"
                          VIEW-AS ALERT-BOX.
                          NEXT.
                       END.
                    END.
                 END.   
                 APPLY LASTKEY.
              END. 

              IF ldtEndDate < ldtBegDate THEN DO:
                 MESSAGE "End date cannot be earlier than begin date"
                 VIEW-AS ALERT-BOX ERROR.
                 NEXT.
              END.

              /* check that times are sane */
              IF NOT fChkTime(INPUT-OUTPUT lcBegTime) THEN NEXT. 
              IF NOT fChkTime(INPUT-OUTPUT lcEndTime) THEN NEXT. 
              
              /* update actual time stamps */  
              MSOwner.TSBegin = fHMS2TS(ldtBegDate,lcBegTime).
              
              IF ldtEndDate >= 12/31/2050
              THEN MsOwner.TSEnd = 99999999.99999.
              ELSE MSOwner.TSEnd = fHMS2TS(ldtEndDate,lcEndTime).
           
              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOwner).
              
              LEAVE.
           END.

           RUN local-disp-row.     

           ufkey = TRUE.
           
        END.

        ELSE PAUSE MESSAGE "Press ENTER to continue".
        
        RELEASE MsOwner.

     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight NE "R" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " VIEW MSOwner " ufkey = TRUE ehto = 9. /*RUN Syst/ufkey.*/
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MSOwner.CLI.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOwner).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOwner).
       RUN local-disp-row.
       xrecid = recid(MSOwner).
       
       RELEASE MSOwner.
       
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSOwner) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSOwner) must-print = TRUE.
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
      FIND FIRST MSOwner WHERE recid(MSOwner) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FIRST MSOwner WHERE recid(MSOwner) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MSOwner
       WHERE MSOwner.MSSEQ = Msseq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MSOwner
       WHERE MSOwner.MSSEQ = Msseq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MSOwner
       WHERE MSOwner.MSSEQ = Msseq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MSOwner
       WHERE MSOwner.MSSEQ = Msseq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       MSOwner.CustNum 
       MsOwner.InvCust
       MsOwner.AgrCust
       lcCustName
       ldtBegDate
       ldtEndDate
       lcBegTime
       lcEndTime
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Customer WHERE 
        Customer.CustNum   = MSOwner.CustNum NO-LOCK NO-ERROR.

   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer).

   FIND CLIType where 
        CliType.Brand   = gcBrand  AND 
        CLIType.Clitype = MSOwner.Clitype NO-LOCK NO-ERROR.  

   fSplitTS(MsOwner.TsBeg,
            OUTPUT ldtBegDate,
            OUTPUT liBegTime).
            
   IF MsOwner.TsEnd >= 99999999 THEN ASSIGN
      ldtEndDate = 12/31/2054
      liEndTime  = 86399.
   ELSE DO:
      fSplitTS(MsOwner.TsEnd,
               OUTPUT ldtEndDate,
               OUTPUT liEndTime).
   END. 
         
   ASSIGN lcBegTime = REPLACE(STRING(liBegTime,"hh:mm:ss"),":","")
          lcEndTime = REPLACE(STRING(liEndTime,"hh:mm:ss"),":","").
 
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   ehto = 3. ufk = 0. RUN Syst/ufkey.p.
   ufkey = true.

   DEF VAR llUpdate    AS LOG  NO-UNDO. 
    
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      FIND BillTarg WHERE 
           BillTarg.CustNum    = MSOwner.CustNum AND
           BillTarg.BillTarget = MSOwner.BillTarget
      NO-LOCK NO-ERROR.

      IF MSOwner.TsEnd >= 99999999 
      THEN end-txt = "Current User".
      ELSE end-txt = "Previous User".

      ASSIGN lcAgrName = ""
             lcInvName = "".
      
      FIND bCustomer WHERE bCustomer.CustNum = MsOwner.AgrCust NO-LOCK.
      IF AVAILABLE bCustomer THEN
         lcAgrName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, 
                                      BUFFER bCustomer).

      FIND bCustomer WHERE bCustomer.CustNum = MsOwner.InvCust NO-LOCK.
      IF AVAILABLE bCustomer THEN
         lcInvName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, 
                                      BUFFER bCustomer).
      
              
      DISP 
      MSOwner.CLI
      MSOwner.MsSeq
      MsOwner.AgrCust lcAgrName
      MsOwner.InvCust lcInvName
      MSOwner.CustNum lcCustName
      Customer.Address
      Customer.ZipCode Customer.PostOffice 
      Customer.Country
      MSOwner.BillTarget
      BillTarg.RatePlan WHEN AVAIL BillTarg
      ldtBegDate
      lcBegTime
      ldtEndDate
      lcEndTime
      end-txt
      MSOwner.IMSI
      MSOwner.Clitype
      MSOwner.TariffBundle
      msowner.inportoper
      msowner.MandateID msowner.Mandatedate
      CLIType.CliName WHEN AVAIL CLIType
      MSOwner.CLIEvent

      WITH FRAME lis.
      message "Press ENTER !".
      PAUSE no-message.

      /* propably not used, so commented out */
/*
      /* don't show even menu to user */
      IF LOOKUP(KEYLABEL(LASTKEY),"1,F1") > 0 AND NOT llTerminated
         THEN DO:
      
          llUpdate = FALSE.
          
          IF lcPassword > "" THEN DO:
             lcAskPassWd = "".
             PAUSE 0.
             UPDATE lcAskPassWd 
                BLANK
                FORMAT "X(20)" 
                LABEL "Password"
                HELP "Password for changing address data"
                WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE ADDRESS "
                     SIDE-LABELS FRAME fPassword.
             
             llUpdate = (lcAskPassWd = lcPassword).
             
          END.
          ELSE llUpdate = TRUE.
          
          IF llUpdate THEN DO:
             FIND CURRENT MsOwner EXCLUSIVE-LOCK.
            
             UPDATE
             MsOwner.BillTarget
             lcCustName 
             Customer.Address
             Customer.ZipCode Customer.PostOffice 
             Customer.Country
             WITH FRAME lis
          EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "billtarget" THEN DO:
                   FIND FIRST billtarget WHERE 
                        billtarget.custnum = msowner.custnum AND
                        billtarget.billtarget = 
                           INPUT FRAME lis MSOwner.Billtarget
                        NO-LOCK NO-ERROR.
                   IF NOT AVAIL Billtarget THEN DO:
                      BELL.
                      MESSAGE "Unknown Billtarget !".
                      DISP Msowner.Billtarget.
                      NEXT.
                   END.
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
 
          END. 
          
      END.
*/          
      LEAVE.
   END.
END PROCEDURE.

