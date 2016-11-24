 /* ----------------------------------------------------------------------
  MODULE .......: TermMobsub
  TASK .........: UPDATEs table TermMobsub
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk  Event logging added
                  28.02.03 tk  tokens
                  08.02.06/aam periodical contract 
                  12.12.06/mvi new param to run msrequest (reqstat = ?)
                  20.03.07 kl  yoigo version
                  18.07.07 kl  termmobsubfind
                  31.10.07 jp  new parameter for msrequest

  Version ......: M15
  ---------------------------------------------------------------------- */

DEFINE INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
DEFINE INPUT PARAMETER icType   AS CHAR NO-UNDO.

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'Customer'}
{func.i}
{timestamp.i}
{msisdn.i}
{errors.i}
{fcustbal.i}
{eventval.i}
{cparam2.i}
{mobsub1.i}

{ffeecont.i}
{fsubser.i}
{fctserval.i}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTermMobsub AS HANDLE NO-UNDO.
   lhTermMobsub = BUFFER TermMobsub:HANDLE.
   RUN StarEventInitialize(lhTermMobsub).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhTermMobsub).
   END.

END.

DEF VAR TermMobsub        LIKE TermMobsub.CLI           NO-UNDO.
DEF VAR CustNum       LIKE Customer.Custnum       NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR orders        AS CHAR                   NO-UNDO.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 4.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.
DEF VAR llMemo        AS LOG                    NO-UNDO.
DEF VAR lcOutport     AS CHAR                   NO-UNDO.
DEF VAR lcCli         AS CHAR                   NO-UNDO FORMAT "X(12)" .
DEF VAR lcFirstname   LIKE Customer.FIRSTName   NO-UNDO.
DEF VAR lcLastName    LIKE Customer.Custname   NO-UNDO.
DEF VAR liCustNum     LIKe Customer.CustNum     NO-UNDO.
DEF VAR liAgrCustNum  LIKE Customer.CustNum     NO-UNDO.
DEF VAR lcPersonID    LIKE Customer.Orgid       No-UNDO.
DEF VAR liMSStatus    AS INT                    NO-UNDO FORMAT ">9".
DEF VAR liMsseq       LIKE termmobsub.msseq FORMAT ">>>>>>>>9".
DEF VAR def-sp-code   AS CHAR                   NO-UNDO.
DEF VAR liSaldotype   AS INT                    NO-UNDO.
DEF VAR killed        AS LOG                    NO-UNDO.
DEF VAR lcICC         LIKE SIM.ICc              NO-UNDO.
DEF VAR lcDCEvent     AS CHAR                   NO-UNDO. 
DEF VAR llMore        AS LOGICAL                NO-UNDO.
DEF BUFFER SearchCustomer FOR Customer.
DEF BUFFER UserCustomer   FOR Customer.
DEF BUFFER AgrCustomer    FOR Customer.
DEF BUFFER InvCustomer    FOR Customer.
DEF BUFFER SearchTermMobsub   FOR TermMobsub.
DEF BUFFER SearchSIM      FOR SIM.
DEF BUFFER SearchIMSI     FOR IMSI.
DEF BUFFER SearchMsowner  FOR MSOWNER.

DEF VAR lcSaldoFatime AS C  NO-UNDO.

ASSIGN lcSaldoFatime = fCParamC("SaldoAgreementAccount")
       lcDCEvent     = fCParamC("PerContractID").

{termmobsub.frm}

form
    TermMobsub.CLI         COLUMN-LABEL "MSISDN" FORMAT "X(10)"
    TermMobsub.MsSeq           COLUMN-LABEL "SubscrID" 
    TermMobsub.AgrCust     COLUMN-LABEL "AgrCust" 
    AgrCustomer.CustName   COLUMN-LABEL "Name"     FORMAT "X(20)" 
    AgrCustomer.orgid      COLUMN-LABEL "PerID/ComID"
    TermMobsub.MSStatus         FORMAT ">9"    COLUMN-LABEL "St" 
    llMemo                  FORMAT "*/"   COLUMN-LABEL "M"  
    KillMS.OutOp            FORMAT "x(7)"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  TERMINATED MOBILE SUBSCRIPTION  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.


form /* seek  TermMobsub */
    lcCli  
    HELP "Enter MSISDN  "
    WITH row 4  TITLE COLOR VALUE(ctc) " FIND Msisdn "
    COLOR VALUE(cfc) WIDTH 24  NO-LABELS OVERLAY FRAME f1.

form /* Customer :n nimella hakua varten */
  "LastName/Company:" lcLastName FORMAT "X(30)"
  HELP "Last name or company name" SKIP
  "FirstName ......:" lcFirstName
  FORMAT "X(20)"
  HELP "First name"
  with row 4 col 2 title color value(ctc) " FIND Name "
  COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.
                  
form /* seek  CustNum */
    liCustNum
    HELP "Enter Customer No "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer No"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.
                
form /* Customer :n nimella hakua varten */
  "LastName/Company:" lcLastName FORMAT "X(30)"
  HELP "Last name or company name" SKIP
  "FirstName ......:" lcFirstName
  FORMAT "X(20)"
  HELP "First name"
  with row 4 col 2 title color value(ctc) " FIND AGREEMENT NAME "
  COLOR value(cfc) NO-LABELS OVERLAY FRAME f4.
 
form /* seek  CustNum */
   lcPersonID
   HELP "Enter Person ID"
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Person ID"   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f5.

form /* seek  CustNum */
    liMSStatus
    HELP "Enter Subscription Status" 
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND StatusCode " 
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f6.
                
form
   lcICC
   HELP "Enter Person ICC" 
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) 
   "FIND ICC"  COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f7.

form /* seek Mobsub MsSeq */
    liMsSeq  
    HELP "Enter Subscription ID  "
    WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND Subscription ID "
    COLOR VALUE(cfc) WIDTH 24  NO-LABELS OVERLAY FRAME fMsSeq.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By MSISDN  ,  By SUBS ID ,  By CUSTNUM  ,  BY STATUS  , By 4".

run local-find-first.

IF AVAILABLE TermMobsub THEN ASSIGN
   Memory       = recid(TermMobsub)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "No Mobile Subscriptions available!"
    VIEW-aS ALERT-BOX.
    RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :  
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TermMobsub WHERE recid(TermMobsub) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TermMobsub THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TermMobsub).
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
         IF NOT llMore THEN
            ASSIGN
            ufk[1]= 209
            ufk[2]= 9018
            ufk[3]= 2902
            ufk[4]= 2903
            ufk[5]= 2214
            ufk[6]= 559
            ufk[7]= 555 /*NORE*/
            ufk[8]= 8
            ufk[9]= 1
            ehto  = 3
            ufkey = FALSE.
         ELSE ASSIGN   
            ufk[1] = 1740
            ufk[2] = 1740. /*under construction*/

         IF ictype  NE  "" THEN ASSIGN
            UFK[1] =  0
            UFK[2] =  0
            UFK[3] =  0
            UFK[4] =  0 
            UFK[5] =  0
            UFK[6] =  0
            UFK[7] =  0.
         
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TermMobsub.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TermMobsub.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TermMobsub.MsSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TermMobsub.MsSeq WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW TermMobsub.AgrCust ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TermMobsub.AgrCust WITH FRAME sel.
      END.
      IF order = 4 THEN DO:
        CHOOSE ROW TermMobsub.MSStatus ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TermMobsub.MSStatus WITH FRAME sel.
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
        FIND TermMobsub WHERE recid(TermMobsub) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TermMobsub THEN
              ASSIGN FIRSTrow = i Memory = recid(TermMobsub).
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
           IF NOT AVAILABLE TermMobsub THEN DO:
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
                rtab[1] = recid(TermMobsub)
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
           IF NOT AVAILABLE TermMobsub THEN DO:
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
              rtab[FRAME-DOWN] = recid(TermMobsub).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TermMobsub WHERE recid(TermMobsub) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TermMobsub THEN DO:
           Memory = recid(TermMobsub).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TermMobsub THEN Memory = recid(TermMobsub).
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
           FIND TermMobsub WHERE recid(TermMobsub) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND NOT llMore AND icType = "" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lccli WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcCli ENTERED THEN DO:
          FIND FIRST TermMobsub WHERE 
                     TermMobsub.CLI = lcCli NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TermMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TermMobsub/TermMobsub was found */
          ASSIGN order = 1 Memory = recid(TermMobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */
     
     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND NOT llMore AND icType = "" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME fMsSeq.
       SET liMsSeq WITH FRAME fMsSeq.
       HIDE FRAME fMsSeq NO-PAUSE.
       IF liMsSeq ENTERED THEN DO:
          FIND FIRST TermMobsub WHERE 
                     TermMobsub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TermMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some Mobsub/mobsub was found */
          ASSIGN order = 2 Memory = recid(TermMobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND NOT llMore AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       SET liCustNum WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF liCustNum ENTERED THEN DO:
          
          FIND FIRST TermMobsub WHERE 
                     TermMobsub.Brand   = "1"   AND 
                     TermMobsub.AgrCust = liCustNum NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TermMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TermMobsub/TermMobsub was found */
          ASSIGN order = 3 Memory = recid(TermMobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 AND NOT llMore AND 
       icType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f4.
       SET lcLastName lcFirstname WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       IF lcFirstName  > ""  OR lcLastName   > ""  THEN DO:
          run termmobsubfind.p("AGRNAME",lcLastname + "|" + lcFirstName).
       END.
     END. /* Search-4 */
    
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND NOT llMore AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f5.
       SET lcPersonid WITH FRAME f5.
       HIDE FRAME f5 NO-PAUSE.

       IF lcPersonID > "" THEN DO:
          run termmobsubfind.p("ID",lcPersonid).
       END.
     END. /* Search-5 */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND NOT llMore AND lcRight = "RW" AND 
       ictype = "" THEN DO: 
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f6.
       SET liMSStatus WITH FRAME f6.
       IF  liMSStatus  ne 0  THEN DO:
          FIND FIRST TermMobsub WHERE
                     TermMobsub.Brand     = "1"           AND
                     TermMobsub.MSStatus  = liMSStatus 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TermMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
                                                                                         /* some TermMobsub/TermMobsub was found */
          ASSIGN order = 4 Memory = recid(TermMobsub) must-print = TRUE.
          NEXT LOOP.
       ENd.
     END. 

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
        llMore = TRUE.
        ufkey = TRUE.
        NEXT LOOP.
     END.

     old F7 -> new F1
       iCType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f7.
       SET lcICC WITH FRAME f7.
       HIDE FRAME f7 NO-PAUSE.

       IF lcICC  > ""  THEN DO:
          FOR FIRST SearchSIM WHERE 
                    SearchSIM.ICC      = lcICC ,
              FIRST SearchTermMobsub WHERE 
                    SearchTermMobsub.Custnum = SearchSIM.CustNum  AND 
                    SearchTermMobsub.ICC     = lcICC NO-LOCK.
          END.

          IF NOT AVAILABLE SearchTermMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.

          IF Avail SearchTermMobsub THEN 
          FIND FIRST TermMobsub WHERE 
               RECID(TermMobsub) = RECID(SearchTermMobsub)
          NO-LOCK NO-ERROR.

          /* some TermMobsub/TermMobsub was found */
          ASSIGN order = 1 Memory = recid(TermMobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

 
     
     
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND lcRight = "RW" AND 
        ictype = "" THEN DO:
        
        RUN local-find-this (FALSE).

        RUN local-find-others(TRUE).
       
        
        RUN nnfmcu(OUTPUT liCustNum, OUTPUT liMsSeq).

        IF liCustNum NE ? AND liCustNum > 0 THEN DO:
           FIND  SearchCustomer WHERE 
                 SearchCustomer.CustNum = liCustNum NO-LOCK.
           IF MsSeq > 0 THEN 
              FIND SearchTermMobsub WHERE 
                   SearchTermMobsub.MsSeq = liMsSeq AND
                   SearchTermMobsub.brand = gcBrand NO-LOCK NO-ERROR.
           ELSE DO:
              FIND FIRST SearchTermMobsub WHERE 
                         SearchTermMobsub.Brand   = gcBrand AND
                         SearchTermMobsub.CustNum = liCustNum NO-LOCK NO-ERROR.

              IF NOT AVAIL SearchTermMobsub THEN DO:
                 MESSAGE 
                    "Customer No." string(liCustNum)                                                 "does NOT have ANY mobile subscriptions"
                 VIEW-AS ALERT-BOX TITLE " NO SUBSCRIPTIONS ".
              END.
           END.
           
           IF AVAIL SearchTermMobsub THEN DO:
              FIND   TermMobsub WHERE 
               RECID(TermMobsub) = RECID(SearchTermMobsub) NO-LOCK NO-ERROR.
               
              ASSIGN
                 memory = RECID(SearchTermMobsub)
                 must-print = TRUE.
              NEXT loop.
           END.
        END.
     END. 
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:

       RUN local-find-this(FALSE).

       RUN local-UPDATE-record(FALSE).  

       TermMobsubBROWSE:
       REPEAT :

          ASSIGN
             ufkey  = TRUE
             ufk    = 0  
             ehto   = 1  
             ufk[1] = 0
             ufk[2] = 788
             ufk[3] = 2244 
             ufk[4] = 1990
             ufk[5] = 927
             ufk[6] = 1992
             ufk[7] = 249 WHEN getTMSRight("VENDOR") = "RW"
             ufk[8] = 8  .
          
          run ufkey.   
          
          IF toimi = 8 THEN DO:
             HIDE FRAME lis NO-PAUSE.
             LEAVE.
          ENd.

          IF toimi = 2  THEN RUN persondata(TermMobsub.msseq).
          ELSE IF toimi = 3  THEN DO:
             RUN msrequest(-1,
                           ?, /* reqstat ? for all */
                           TermMobsub.MsSeq,
                           0,
                           0,
                           "").
             /* in case a request was run */
             RUN local-UPDATE-record(FALSE).
          END.      
          ELSE IF toimi = 4  THEN RUN termsubser(TermMobsub.MsSeq).
          ELSE IF Toimi = 5  THEN RUN memo(INPUT TermMobsub.CustNum,
                                           INPUT "TermMobsub",
                                           INPUT STRING(TermMobsub.MsSeq),
                                           INPUT "TermMobsub").

          ELSE IF toimi = 6 THEN DO:
       
             CALLBROWSE:
             REPEAT WITH FRAME lis ON ENDKEY UNDO, RETURN:

                ASSIGN
                   ufkey = TRUE
                   ufk   = 0
                   ehto  = 1
                   ufk[1] = 1992
                   ufk[2] = 844
                   ufk[3] = 562
                   ufk[4] = 2435
                   ufk[5] = 0
                   ufk[6]= 0
                   ufk[7]= 0.
                   ufk[8]= 8.
                run ufkey.   
        
                IF toimi = 8 THEN DO:
                   LEAVE CALLBROWSE.
                ENd.

                IF       toimi = 1  THEN RUN  msisdniv(TermMobsub.MsSeq).

                ELSE IF toimi = 2 AND avail TermMobsub  THEN
                   RUN  callstat.p(INPUT 0,TermMobsub.cli,"PRODUCT").
       
                ELSE IF toimi = 3 AND avail TermMobsub  THEN 
                   RUN  callstat.p(INPUT 0,TermMobsub.cli,"DATE").
       
                ELSE IF toimi = 4 AND avail TermMobsub  THEN
                   RUN  callstat.p(INPUT 0,TermMobsub.cli,"CCN").

                ELSE IF toimi = 6 THEN 
                   RUN persondata(TermMobsub.msseq).
        
             END.
          END.
          ELSE IF toimi = 7 AND ufk[7] > 0 AND avail TermMobsub  THEN DO:
             RUN mobsubdi.p(INPUT TermMobsub.MSSeq, OUTPUT killed).
          END.
          ELSE IF toimi = 6 THEN 
             RUN persondata(TermMobsub.msseq).
       END.
       ASSIGN  Memory = recid(TermMobsub) must-print = TRUE.
       NEXT LOOP.
    END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TermMobsub) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TermMobsub) must-print = TRUE.
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
      FIND TermMobsub WHERE recid(TermMobsub) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TermMobsub WHERE recid(TermMobsub) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND FIRST TermMobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN DO: {termmobfind.i FIRST } end.

       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND FIRST TermMobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN  {termmobfind.i FIRST }

       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND FIRST TermMobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {termmobfind.i FIRST }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND FIRST TermMobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {termmobfind.i FIRST }
       

END PROCEDURE.

PROCEDURE local-find-LAST:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND LAST  TermMobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {termmobfind.i LAST  }
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND LAST TermMobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN  {termmobfind.i LAST }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND LAST  TermMobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {termmobfind.i LAST  }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND LAST  TermMobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {termmobfind.i LAST  }
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND NEXT TermMobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {termmobfind.i NEXT }
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND NEXT TermMobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN  {termmobfind.i NEXT }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND NEXT TermMobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {termmobfind.i NEXT }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND NEXT TermMobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {termmobfind.i NEXT }
 
END PROCEDURE.

PROCEDURE local-find-PREV:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND PREV TermMobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {termmobfind.i PREV }
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND PREV TermMobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN  {termmobfind.i PREV }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND PREV TermMobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {termmobfind.i PREV }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND PREV TermMobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {termmobfind.i PREV }
 

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others(TRUE).
       
       CLEAR FRAME sel NO-PAUSE.

       PAUSE 0.
       DISPLAY 
          TermMobsub.CLI 
          TermMobsub.MsSeq
       TermMobsub.AgrCust
       DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER AgrCustomer) @
          AgrCustomer.Custname 
       AgrCustomer.OrgID
       KillMS.OutOp WHEN AVAIL KillMS 
       llmemo
       TermMobsub.msstatus 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   DEF input parameter llsel AS LOG NO-UNDO.
      
   IF CAN-FIND(FIRST memo WHERE 
                     Memo.Brand     = gcBrand                AND
                     Memo.HostTable = "TermMobsub"               AND
                     Memo.KeyValue  = STRING(TermMobsub.MsSeq)   AND
                    (Memo.MemoText NE "" OR 
                     Memo.MemoTitle NE ""))
   THEN ASSIGN llmemo  = TRUE.
   ELSE ASSIGN llmemo  = FALSE.

   FIND FIRST Usercustomer WHERE 
              UserCustomer.CustNum = TermMobsub.Custnum
   NO-LOCK NO-ERROR.

   FIND FIRST AgrCustomer WHERE
              AgrCustomer.CustNum = TermMobsub.AgrCust
   NO-LOCK NO-ERROR.

   FIND FIRST InvCustomer WHERE 
              InvCustomer.CustNum = TermMobsub.InvCust
   NO-LOCK NO-ERROR.
                 
   FIND FIRST MSISDN WHERE 
              MSISDN.CLI = TermMobsub.CLI
   NO-LOCK NO-ERROR.

   FIND FIRST KillMS WHERE 
              KillMS.MSSeq = TermMobsub.MSSeq
   NO-LOCK NO-ERROR.
   
   IF NOT llSel THEN DO:
   
      FIND FIRST BillTarget WHERE  
                 BillTarget.Billtarget = TermMobsub.BillTarget AND 
                 BillTArget.custnum    = TermMobsub.CustNum
      NO-LOCK NO-ERROR.
      
      IF AVAIL BillTarget THEN lcBillTarget = 
         STRING(TermMobsub.Billtarget) + " " + BillTarget.RatePlan.
      ELSE lcBillTarget = STRING(TermMobsub.Billtarget).
      
      FIND FIRST sim WHERE 
                 SIM.Brand  = gcBrand AND 
                 Sim.Icc = TermMobsub.ICC
      NO-LOCK NO-ERROR.
              
      IF Avail sim THEN
         FIND FIRST imsi WHERE 
                    imsi.icc = sim.icc
         NO-LOCK NO-ERROR.
   
      /* FAT */ 
      llFatime = can-find(FIRST fatime WHERE
                                Fatime.Brand = gcBrand AND 
                                fatime.cli    = TermMobsub.cli AND
                                FATime.MsSeq  = TermMobSub.MsSeq AND
                                fatime.invnum = 0).
      /* eGift */ 

      lleGift  =  can-find(FIRST FATime WHERE
                                 Fatime.Brand    = gcBrand AND 
                                 FATime.CustNum  = TermMobsub.CustNum AND 
                                 FATime.cli      = TermMobsub.CLI     AND
                                 FATime.MsSeq    = TermMobSub.MsSeq   AND
                                 Fatime.FTGRP    = "EGIFT"        AND 
                                 FATime.invnum  = 0).

      /* CallSpec */ 
          
       IF STRING(fCallSpecReport(TermMobsub.MsSeq)) > "" 
       THEN llCallSpec = TRUE.  
       ELSE llCallSpec = FALSE.

      /* SalesMan */ 

      FIND FIRST Salesman WHERE 
                 SalesMan.Brand    = gcBrand AND 
                 Salesman.Salesman = TermMobsub.Salesman
      NO-LOCK NO-ERROR.

      /* Status */ 
      lcStatus = STRING(TermMobsub.msstatus) + " " + 
                 entry(TermMobsub.MsStatus + 1, stnames).
      
      lcNumberInquiry = fNumberInqExpl(TermMobsub.MsSeq).  

      IF TermMobsub.ActivationTS > 0 THEN                          
         lcInportTime =    "Activated.....: "  + 
                           STRING(fTS2HMS(TermMobsub.activationTS)).
      
      ELSE IF AVAIL msisdn AND msisdn.portingDate ne ? then DO:
         lcINPortTime = "Inporting Time: " +
                        STRING(MSISDN.PortingTime,"99.99") + " on " +
                        String(msisdn.portingDate,"99-99-99").
      END.
      ELSE lcInPortTime = "".
                                                   
      lcNotifyNumber  = fNotifyNbrValue(TermMobsub.MsSeq).

      liSaldotype    = fCreditTypeValue(TermMobsub.MsSeq,
                                        OUTPUT liSaldoLimit) .

      /* saldolimit */ 
      lcSaldoType = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                                     "CreditType",
                                     "CreditType",
                                      STRING(liSaldoType)).
                                                    
      ldExtraLimit = DYNAMIC-FUNCTION("fChkSaldoAccount" in ghfunc1,
                                       INPUT TermMobsub.custnum,
                                       INPUT TermMobsub.cli, 
                                       INPUT year(today) * 100 + Month(today),
                                       INPUT lcSaldofatime).
      FIND FIRST Msowner WHERE 
                 Msowner.msseq = TermMobsub.MSseq
      NO-LOCK NO-ERROR.

      IF AVAIL msowner THEN
         lcTerMinated  = "TERMINATED....: " + fTS2HMS(msowner.tsend).
      ELSE
         lcTerMinated  = "TERMINATED....: N/A " . 

      IF      TermMobsub.servicechanges  = 0 THEN lcservicechanges = "Denied".
      ELSE IF TermMobsub.servicechanges  = 1 THEN lcservicechanges = "Allowed".
     
      /* SaldoServiceCounter, this not unbilledbalance */ 
      IF liSaldotype > 0 THEN 
         ldeSaldoSCounter   = fUnbilledBalance(TermMobsub.MsSeq,
                                               YEAR(TODAY) * 100 + MONTH(TODAY)).
      ELSE ldeSaldoSCounter = 0 .               
     
      lcNotifyNumber  = fNotifyNbrValue(TermMobsub.MsSeq)   .

      /* periodical contract */
      lcPerContr = "NO".
      FOR FIRST DCCLI NO-LOCK WHERE
                DCCLI.Brand      = gcBrand      AND
                DCCLI.DCEvent    = lcDCEvent    AND
                DCCLI.MsSeq      = TermMobsub.MsSeq AND
                DCCLI.ValidTo   >= TODAY        AND
                DCCLI.ValidFrom <= TODAY:
          
         IF DCCLI.TermDate NE ? THEN lcPerContr = "PPI".
         ELSE                        lcPerContr = "PPA".

      END.
      
   END.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record.

   DEF INPUT PARAMETER llUpdate AS LOG NO-UNDO.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      RUN local-find-others(FALSE).
      
      PAUSE 0.

      DISP 
         TermMobsub.MSSeq 
         TermMobsub.CliType 
         TermMobsub.AgrCust
         TermMobsub.CLI
         TermMobsub.CLI + " / " + TermMobsub.FixedNumber
            WHEN TermMobsub.FixedNumber NE ? @ TermMobsub.CLI
         lcBillTarget
         TermMobsub.Custnum
         TermMobsub.InvCust
         AgrCustomer.OrgID WHEN AVAIL AgrCustomer
         TermMobsub.ICC
         IMSI.PIN1 WHEN AVAIL IMSI
         IMSI.PIN2 WHEN AVAIL IMSI
         IMSI.PUK1 WHEN AVAIL IMSI
         IMSI.PUK2 WHEN AVAIL IMSI

         DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER AgrCustomer) 
            WHEN AVAIL AgrCustomer @ AgrCustomer.CustName 
         
         DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER InvCustomer)
            WHEN AVAIL InvCustomer @ InvCustomer.CustName 
         
         DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER UserCustomer)
            WHEN AVAIL UserCustomer @ UserCustomer.CustName
         
         llFatime
         llCallSpec
         llEgift
         TermMobsub.Salesman
         Salesman.SMName WHEN AVAIL salesman
         TermMobsub.servicechanges
         lcServiceChanges
         lcStatus 
         lcNumberInquiry
         llEgift
         lcPerContr
         TermMobsub.CreationDate
         TermMobsub.ActivationDAte
         lcinporttime
         lcTerminated
      WITH FRAME lis.

      COLOR DISP MESSAGE lcTerminated  with frame lis.

      PAUSE 0.
      
      LEAVE.

   END.

END PROCEDURE.

