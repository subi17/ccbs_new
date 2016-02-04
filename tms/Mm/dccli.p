/* ----------------------------------------------------------------------
  MODULE .......: DCCLI
  TASK .........: Updates table DCCLI
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 19.02.02 tk eventlog
                  07.02.06/aam to TF  
                  17.03.06 jp  RELEASE DCCLI commented
                  17.01.07/aam create fees parameter to fPCActionRequest
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Func/cparam2.i}
{Rate/daycampaign.i}
{Func/timestamp.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {lib/eventlog.i}
        
    DEF VAR lhDCCLI AS HANDLE NO-UNDO.
    lhDCCLI = BUFFER DCCLI:HANDLE.
    RUN StarEventInitialize(lhDCCLI).
                    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhDCCLI).
    END.
END.

DEF INPUT PARAMETER  iiMsSeq AS INT           NO-UNDO.                
DEF INPUT PARAMETER  icEvent AS CHAR          NO-UNDO.

def /* new */ shared var siirto AS char.

DEF VAR lccli        LIKE MobSub.CLI           NO-UNDO.
DEF VAR liMsseq      AS INT                    NO-UNDO.
DEF VAR lcEvent      AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
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
DEF VAR iLoop        AS INT                    NO-UNDO.
DEF VAR ldEndStamp   AS DEC                    NO-UNDO.
DEF VAR lcAction     AS CHAR                   NO-UNDO. 
DEF VAR lcError      AS CHAR                   NO-UNDO.
DEF VAR lcPassword   AS CHAR                   NO-UNDO. 
DEF VAR lcTLPassword AS CHAR                   NO-UNDO. 
DEF VAR lcAskPassWd  AS CHAR                   NO-UNDO.
DEF VAR lcMemoTxt    AS CHAR                   NO-UNDO. 
DEF VAR lcDCEvent    AS CHAR                   NO-UNDO. 
DEF VAR llMemo       AS LOG                    NO-UNDO FORMAT "M/".

DEF BUFFER bDCCLI FOR DCCLI.

form
    DCCLI.MsSeq         COLUMN-LABEL "Subscr.ID"
    DCCLI.CLI           COLUMN-LABEL "MSISDN"
    DCCLI.DCEvent       COLUMN-LABEL "Contract"
    DCCLI.ValidFrom     COLUMN-LABEL "From"   FORMAT "99-99-9999"
    DCCLI.ValidTo       COLUMN-LABEL "To"     FORMAT "99-99-9999"
    DCCLI.TermDate                            FORMAT "99-99-9999"
    llMemo              COLUMN-LABEL "M"
WITH ROW FrmRow width 78 CENTERED overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " PERIODICAL CONTRACT ROWS "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    DCCLI.Msseq 
    DCCLI.CLI           /* label format */
    DCCLI.DCEvent        label "Per.Contract"
       DayCampaign.dcname   label "PC Name" FORMAT "X(30)"
    DCCLI.ContractDate   FORMAT "99-99-9999"
    DCCLI.ValidFrom
    DCCLI.ValidTo
    DCCLI.TermDate       FORMAT "99-99-9999"
WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

FORM
   lcMemoTxT VIEW-AS EDITOR SIZE 60 BY 5 
      HELP "Memo text"
   WITH OVERLAY ROW 10 CENTERED NO-LABELS TITLE " Mandatory Memo "
     FRAME fMemo.
 
form /* seek  DCCLI */
    lCCli
    HELP "Enter MSISDN "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN " 
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.
    
form 
     lcEvent
     HELP "Enter Event  "
     WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND EVENT "
     COLOR VALUE(cfc) NO-labels overlay FRAME f2.

 

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By cli   ,  By Name  ,By 3, By 4".

ASSIGN lcPassword   = fCParamC("MSAddressChg")    
       lcTLPassword = fCParamC("MSTimeLabelChg")
       lcDCEvent    = fCParamC("PerContractID").

RUN local-find-first.
IF AVAILABLE DCCLI THEN ASSIGN
   memory       = recid(DCCLI)
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

   IF must-add THEN DO:  /* Add a DCCLI  */ 
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           
           CLEAR FRAME lis NO-PAUSE.

           IF iiMsSeq ne 0 then
           find first mobsub where
                      mobsub.MsSeq = iiMsSeq no-lock no-error.
           if avail mobsub then ASSIGN
              lccli = mobsub.CLI iiMsSeq = mobsub.MsSeq.
           
           create DCCLI.

           ASSIGN
           DCCLI.CLI          = lccli
           DCCLI.msseq        = iiMsSeq
           DCCLI.Brand        = gcBrand
           DCCLI.DCEvent      = icEvent
           DCCLI.ContractDate = TODAY.

           IF DCCLI.DCEvent = "" THEN DCCLI.DCEvent = lcDCEvent.
           
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              DCCLI.CLI = "" THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDCCLI).
            
           iLoop = fGenerateCounters(DCCLI.msseq, 
                                     DCCLI.dcevent).
            
           MESSAGE iloop "counter items were created"
           VIEW-AS ALERT-BOX.
           
           ASSIGN
           memory = recid(DCCLI)
           xrecid = memory.
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DCCLI THEN LEAVE LOOP.

      NEXT LOOP.
   END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND DCCLI WHERE recid(DCCLI) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DCCLI THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(DCCLI).
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
        ufk[1]= 209  ufk[2]= 1045 ufk[3]= 2105 ufk[4]= 1048
        ufk[5]= 2240 ufk[6]= 1046 ufk[7]= 1047 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.

        IF iiMsSeq > 0 THEN ASSIGN
           ufk[1] = 0 
           ufk[2] = 0.
        
        IF icEvent > "" THEN ufk[2] = 0.
           
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row DCCLI.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DCCLI.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row DCCLI.DCEvent ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DCCLI.DCEvent WITH FRAME sel.
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
        FIND DCCLI WHERE recid(DCCLI) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE DCCLI THEN
              ASSIGN FIRSTrow = i memory = recid(DCCLI).
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
           IF NOT AVAILABLE DCCLI THEN DO:
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
                rtab[1] = recid(DCCLI)
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
           IF NOT AVAILABLE DCCLI THEN DO:
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
              rtab[FRAME-down] = recid(DCCLI).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DCCLI WHERE recid(DCCLI) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DCCLI THEN DO:
           memory = recid(DCCLI).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE DCCLI THEN memory = recid(DCCLI).
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
           FIND DCCLI WHERE recid(DCCLI) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET lCCli WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lCCli ENTERED THEN DO:
          FIND FIRST msowner WHERE 
                     msowner.CLI = lccli NO-LOCK NO-ERROR.
          IF avail msowner THEN 
          IF icEvent > "" THEN
            FIND FIRST DCCLI WHERE
                       DCCLI.msseq = Msowner.MsSeq AND
                       DCCLI.dcevent = icEvent NO-LOCK NO-ERROR.
          ELSE FIND FIRST DCCLI WHERE DCCLI.msseq = Msowner.MsSeq 
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE DCCLI THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DCCLI/DCCLI was found */
          ASSIGN order = 1 memory = recid(DCCLI) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET lCEvent WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lCEvent ENTERED THEN DO:
          FIND FIRST DCCLI WHERE DCCLI.DCEvent = lCEvent
          USE-INDEX DCEvent /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE DCCLI THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DCCLI/DCEvent was found */
          ASSIGN order = 2 memory = recid(DCCLI) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,F3") > 0 THEN DO:
       FIND FIRST DCCLI WHERE
            RECID(DCCLI) = rtab[FRAME-LINE]
       NO-LOCK NO-ERROR.

       RUN Mm/dccounter (DCCLI.msseq, 
                      DCCLI.dcevent,
                      DCCLI.ValidFrom,
                      DCCLI.ValidTo).

       ufkey = true.
       RUN Syst/ufkey.
       PAUSE 0.
     END.
     ELSE IF LOOKUP(nap,"5,F5") > 0 THEN DO:
         
         FIND FIRST DCCLI WHERE
              RECID(DCCLI) = rtab[FRAME-LINE]
         NO-LOCK NO-ERROR.

         RUN Mc/memo.p
              (INPUT 0,
              "mobsub",
              STRING(dccli.msseq),
              "MEMOS").
                             
         ufkey = true.
         RUN Syst/ufkey.
         PAUSE 0.
     END.
     
     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
     
        FIND FIRST DCCLI WHERE
            RECID(DCCLI) = rtab[FRAME-LINE]
        NO-LOCK NO-ERROR.

        ufkey = TRUE.
        
        IF lcTLPassword > "" THEN DO:
           lcAskPassWd = "".
           PAUSE 0.
           UPDATE lcAskPassWd 
              BLANK
              FORMAT "X(20)" 
              LABEL "Password"
              HELP "Password for special actions"
              WITH OVERLAY ROW 10 CENTERED TITLE " RESTRICTED ACCESS "
                   SIDE-LABELS FRAME fPassword.
             
           HIDE FRAME fPassword NO-PAUSE.
           IF lcAskPassWd NE lcTLPassword THEN NEXT.
        END.
             
        REPEAT:
   
           IF AVAILABLE DCCLI THEN RUN local-disp-row.
           
           ASSIGN
           ufkey = TRUE
           ufk   = 0
           ehto  = 0
           ufk[1] = 1049
           ufk[2] = 0
           ufk[3] = 0
           ufk[4] = 0
           ufk[5] = 5
           ufk[6] = 2100
           ufk[7] = 2102
           ufk[8] = 8.

           IF NOT AVAILABLE DCCLI THEN ASSIGN 
              ufk = 0
              ufk[5] = 5
              ufk[8] = 8.
              
           ELSE DO:  
              IF DCCLI.TermDate = ? OR DCCLI.ValidTo <= TODAY THEN ufk[2] = 0.
           
              IF DCCLI.ValidTo <= TODAY THEN ASSIGN 
                 ufk[6] = 0.
           END.
           
           RUN Syst/ufkey.   
        
           IF toimi = 8 THEN NEXT BROWSE.

           /* termination */
           IF toimi = 1 THEN DO:
              RUN Mm/dccliterm(DCCLI.MsSeq,
                            DCCLI.DCEvent,
                            DCCLI.PerContractId).
           END.

           ELSE IF toimi = 5 THEN DO:  /* add */
              RUN Mm/dccliadd (iiMsSeq).
              NEXT LOOP.
           END.

           ELSE IF toimi = 6 THEN DO:
              
              iLoop = fGenerateCounters(INPUT DCCLI.msseq, 
                                        INPUT DCCLI.dcevent).

              MESSAGE
              "Totally" iloop "new counter items were created"
              VIEW-AS ALERT-BOX.
        
           END.
   
           ELSE IF toimi = 7 THEN DO:
              Ok = FALSE.
              MESSAGE "Remove all counters starting from today ?"
              VIEW-AS ALERT-BOX QUESTION
              BUTTONS YES-NO
              SET Ok.

              IF Ok THEN DO:
                 iLoop = fRemoveCounters(INPUT DCCLI.msseq, 
                                         INPUT DCCLI.dcevent, 
                                         INPUT TODAY ).
                 MESSAGE
                 "Totally" iloop "counters removed" SKIP 
                 VIEW-AS ALERT-BOX.
              END.
           END.
              
        END.
     END.
           
     ELSE IF LOOKUP(nap,"6,f6,7,F7") > 0 THEN DO TRANSACTION:  

        {Syst/uright2.i}
        RUN local-find-this (false).

        ufkey = TRUE.
        IF LOOKUP(nap,"6,f6") > 0 
        THEN lcAction = "canc".
        ELSE lcAction = "term".

        RUN pTermRequest (lcAction).
        
        RUN local-find-this(false).    
        RUN local-disp-row.
            
     END. /* terminate/cancel */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       
       
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDCCLI).
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDCCLI).
       
       RUN local-disp-row.
       xrecid = recid(DCCLI).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(DCCLI) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(DCCLI) must-print = true.
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
      find DCCLI WHERE recid(DCCLI) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find DCCLI WHERE recid(DCCLI) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

      IF  Order       = 1        AND 
          iiMsSeq     > 0        AND 
          icEvent     > ""  THEN  
         FIND FIRST DCCLI WHERE
                    DCCLI.Brand   = gcBRand AND 
                    DCCLI.MSSEq   = iiMsSeq AND 
                    DCCLI.Dcevent = icEvent NO-LOCK NO-ERROR.
                                       
      ELSE IF Order = 1 AND iiMsSeq > 0 THEN
         FIND FIRST DCCLI WHERE
                    DCCLI.MSSEq = iiMsSeq  NO-LOCK NO-ERROR.
      ELSE IF  ORder = 1 AND icevent > "" THEN  FIND FIRST DCCLI WHERE
         DCCli.DCevent = icEvent  AND dccli.Brand = "1" NO-LOCK NO-ERROR.
      ELSE IF ORder = 1 THEN  FIND FIRST DCCLI NO-LOCK NO-ERROR.
      ELSE IF Order = 2  AND iiMsSeq > 0 THEN  
          FIND FIRST DCCLI WHERE 
                     DCCLI.MSSEq = iiMsSeq USE-INDEX DCEvent NO-LOCK NO-ERROR.
      ELSE IF Order = 2 AND dcevent > "" THEN
            FIND FIRST DCCLI WHERE 
                       DCCLI.Brand   = gcBrand AND
                       DCCLI.DCEvent = icEvent USE-INDEX DCEvent 
            NO-LOCK NO-ERROR.
      ELSE IF Order = 2 THEN  FIND FIRST DCCLI  USE-INDEX DCEvent.     


END PROCEDURE.

PROCEDURE local-find-LAST:
     IF  Order       = 1        AND 
          iiMsSeq     > 0        AND 
          icEvent     > ""  THEN  
         FIND LAST  DCCLI use-index DCEvent WHERE
                    DCCLI.Brand   = gcBRand AND 
                    DCCLI.MSSEq   = iiMsSeq AND 
                    DCCLI.Dcevent = icEvent NO-LOCK NO-ERROR.
      
      ELSE IF Order = 1 AND iiMsSeq > 0 THEN
         FIND LAST DCCLI WHERE
                    DCCLI.MSSEq = iiMsSeq  NO-LOCK NO-ERROR.
      ELSE IF  ORder = 1 AND icevent > "" THEN  FIND LAST DCCLI WHERE
       DCCli.DCevent = icEvent  AND dccli.Brand = "1"  NO-LOCK NO-ERROR.
            
      ELSE IF ORder = 1 THEN  FIND LAST DCCLI NO-LOCK NO-ERROR.
      ELSE IF Order = 2  AND iiMsSeq > 0 THEN  
          FIND LAST DCCLI WHERE 
                     DCCLI.MSSEq = iiMsSeq USE-INDEX DCEvent NO-LOCK NO-ERROR.
      ELSE IF Order = 2 AND dcevent > "" THEN
          FIND LAST  DCCLI WHERE
                     DCCLI.Brand   = gcBrand AND
                     DCCLI.DCEvent = icEvent USE-INDEX DCEvent
          NO-LOCK NO-ERROR.
      ELSE IF Order = 2 THEN
            FIND LAST DCCLI USE-INDEX DCEvent NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:

      IF  Order       = 1        AND 
          iiMsSeq     > 0        AND 
          icEvent     > ""  THEN  
         FIND NEXT  DCCLI use-index DCEvent WHERE
                    DCCLI.Brand   = gcBRand AND 
                    DCCLI.MSSEq   = iiMsSeq AND 
                    DCCLI.Dcevent = icEvent NO-LOCK NO-ERROR.
      
      ELSE IF Order = 1 AND iiMsSeq > 0 THEN
         FIND NEXT DCCLI WHERE
                    DCCLI.MSSEq = iiMsSeq  NO-LOCK NO-ERROR.
      ELSE IF  ORder = 1 AND icevent > "" THEN  FIND NEXT DCCLI WHERE
         DCCli.DCevent = icEvent AND dccli.Brand = "1"  NO-LOCK NO-ERROR.
      ELSE IF ORder = 1 THEN  FIND NEXT DCCLI NO-LOCK NO-ERROR.
      ELSE IF Order = 2  AND iiMsSeq > 0 THEN  
          FIND NEXT DCCLI WHERE 
                    DCCLI.MSSEq = iiMsSeq USE-INDEX DCEvent NO-LOCK NO-ERROR.
      ELSE IF Order = 2 AND dcevent > "" THEN
          FIND NEXT  DCCLI WHERE
                     DCCLI.Brand   = gcBrand AND
                     DCCLI.DCEvent = icEvent USE-INDEX DCEvent
          NO-LOCK NO-ERROR.
      ELSE IF Order = 2 THEN
             FIND NEXT DCCLI USE-INDEX DCEvent NO-LOCK NO-ERROR.
             
END PROCEDURE.

PROCEDURE local-find-PREV:
      
      IF  Order       = 1        AND 
          iiMsSeq     > 0        AND 
          icEvent     > ""  THEN  
         FIND PREV  DCCLI use-index DCEvent WHERE
                    DCCLI.Brand   = gcBRand AND 
                    DCCLI.MSSEq   = iiMsSeq AND 
                    DCCLI.Dcevent = icEvent NO-LOCK NO-ERROR.
      
      ELSE IF Order = 1 AND iiMsSeq > 0 THEN
         FIND PREV  DCCLI WHERE
                    DCCLI.MSSEq = iiMsSeq  NO-LOCK NO-ERROR.
      ELSE IF  ORder = 1 AND icevent > "" THEN  FIND Prev DCCLI WHERE
            DCCli.DCevent = icEvent  AND dccli.Brand = "1" NO-LOCK NO-ERROR.
            
      ELSE IF ORder = 1 THEN  FIND Prev DCCLI NO-LOCK NO-ERROR.
      ELSE IF Order = 2  AND iiMsSeq > 0 THEN  
         FIND PREV DCCLI WHERE 
                     DCCLI.MSSEq = iiMsSeq USE-INDEX DCEvent NO-LOCK NO-ERROR.
      ELSE IF Order = 2 AND dcevent > "" THEN
         FIND LAST  DCCLI WHERE
                    DCCLI.Brand   = gcBrand AND
                    DCCLI.DCEvent = icEvent USE-INDEX DCEvent
         NO-LOCK NO-ERROR.
      ELSE IF Order = 2 THEN 
         FIND Last DCCLI USE-INDEX DCEvent NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       DCCLI.MsSeq
       DCCLI.CLI 
       DCCLI.DCEvent

       DCCLI.ValidFrom
       DCCLI.ValidTo
       DCCLI.TermDate
       llmemo
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = gcBrand AND
              DayCampaign.DCEvent = DCCLI.DCEvent NO-LOCK NO-ERROR.
 
   FIND FIRST Memo WHERE  
              Memo.Brand     = gcBrand                AND 
              Memo.HostTable = "MobSub"               AND 
              Memo.KeyValue  = STRING(DCCLI.MsSeq)    AND 
              Memo.Memotitle Begins "Per.Contract"  NO-LOCK NO-ERROR. 

    if Avail Memo THEN llMemo = TRUE.
    ELSe               llMemo = FALSE.

END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP 
      DCCLI.msseq 
      DCCLI.cli
      DCCLI.DCEvent
      DCCLI.ContractDate
      DCCLI.ValidFrom
      DCCLI.ValidTo
      DCCLI.TermDate
      DayCampaign.DCName WHEN AVAIL DayCampaign
      WITH FRAME lis.

      IF new DCCLI and DCCLI.cli = "" THEN DO:
         update DCCLI.cli with frame lis.
         IF DCCLI.CLI = "" THEN RETURN.
         
         find first msowner where 
                    msowner.CLI = input DCCLI.cli NO-LOCK NO-ERROR.
         if avail msowner then DCCLI.msseq = msowner.MsSeq.
         disp DCCLI.msseq with frame lis. pause 0.
      END.

      IF NEW DCCLI THEN 
      UPDATE
          DCCLI.DCEvent WHEN icEvent = ""
          DCCLI.ContractDate
          DCCLI.ValidFrom
          DCCLI.ValidTo
      WITH FRAME lis EDITING:
             READKEY.
             IF FRAME-FIELD = "DCEvent" AND 
                LOOKUP(KEYLABEL(LASTKEY),"F9," + poisnap) = 0 THEN NEXT. 
                
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "DCEvent" THEN DO:
                   FIND DayCampaign WHERE 
                        DayCampaign.Brand   = gcBrand ANd
                        DayCampaign.DCEvent =
                   INPUT FRAME lis DCCLI.DCEvent NO-LOCK NO-ERROR.
                   IF NOT AVAIL DayCampaign THEN DO:
                      BELL.
                      MESSAGE "Unknown periodical contract" .
                      NEXT.
                   END.
                   DISP DayCampaign.dcname with frame lis.
                END.

                ELSE IF FRAME-FIELD = "ContractDate"  THEN DO:
                   IF INPUT DCCLI.ValidFrom = ? THEN DO:
                      DCCLI.ValidFrom = INPUT DCCLI.ContractDate.
                      DISPLAY DCCLI.ValidFrom WITH FRAME lis.
                   END.
                END.

                ELSE IF FRAME-FIELD = "ValidFrom"  THEN DO:
                   IF INPUT DCCLI.ValidTo = ? THEN DO:
                      IF MONTH(INPUT DCCLI.ValidFrom) = 2 AND
                         DAY(INPUT DCCLI.ValidFrom) > 28
                      THEN 
                      DCCLI.ValidTo = DATE(2,28, 
                                           YEAR(INPUT DCCLI.ValidFrom) + 1).
                      ELSE 
                      DCCLI.ValidTo = DATE(MONTH(INPUT DCCLI.ValidFrom),
                                           DAY(INPUT DCCLI.ValidFrom),
                                           YEAR(INPUT DCCLI.ValidFrom) + 1)
                                      - 1.
                      DISPLAY DCCLI.ValidTo WITH FRAME lis.
                   END.
                END.

                ELSE IF FRAME-FIELD = "ValidTo" THEN DO:
                   IF CAN-FIND(FIRST bDCCLI WHERE
                          bDCCLI.Brand     =  gcBrand               AND
                          bDCCLI.DCEvent   =  INPUT DCCLI.DCEvent   AND
                          bDCCLI.MsSeq     =  DCCLI.MsSeq           AND
                          bDCCLI.ValidTo   >= INPUT DCCLI.ValidFrom AND
                          bDCCLI.ValidFrom <= INPUT DCCLI.ValidTo   AND
                          RECID(bDCCLI)   NE  RECID(DCCLI))
                   THEN DO:
                      MESSAGE "Subscription already has a contract on given"
                              "period."
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END.
                END.
                
             END.

             APPLY LASTKEY.
             
      END. /* EDITING */
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.


