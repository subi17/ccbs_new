 /* ----------------------------------------------------------------------
  MODULE .......: Mobsub
  TASK .........: UPDATEs table Mobsub
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk  Event logging added
                  28.02.03 tk  tokens
                  08.02.06/aam periodical contract 
                  06.03.06/aam skip egift and saldo payment in llFatime
                  04.04.06/aam check type of dccli
                  27.11.06/aam Reseller
                  05.12.06/mvi find agrcust with sur1,sur2,company(frame f2)
                  12.12.06/mvi new param to RUN Mm/msrequest.p (reqstat = ?)
                  06.03.07 kl  when avail on displays
                  21.03.07 kl  ppreqbr
                  31.10.07 jp  new parameter for msrequest

  Version ......: M15
  ---------------------------------------------------------------------- */

DEFINE INPUT PARAMETER iiCustNum AS INT  NO-UNDO INIT 0.
DEFINE INPUT PARAMETER icType    AS CHAR NO-UNDO.
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'OrdStat'}
{Mf/errors.i}
{Func/fcustbal.i}
{Syst/eventval.i}
{Func/cparam2.i}
{Mm/mobsub1.i}
{Func/matrix.i}
{Func/ffeecont.i}
{Func/fsubser.i}
{Func/fctserval.i}
{Func/barrfunc.i}
{Func/fdss.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
   lhMobsub = BUFFER Mobsub:HANDLE.
   RUN StarEventInitialize(lhMobsub).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMobsub).
   END.

END.
DEF NEW SHARED VAR siirto AS CHAR.  

DEF VAR Mobsub        LIKE Mobsub.CLI           NO-UNDO.
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
DEF VAR liMsseq       LIKE MobSub.MsSeq         NO-UNDO.
DEF VAR def-sp-code   AS CHAR                   NO-UNDO.
DEF VAR liSaldotype   AS INT                    NO-UNDO.
DEF VAR killed        AS LOG                    NO-UNDO.
DEF VAR lcICC         LIKE SIM.ICc              NO-UNDO.
DEF VAR lcFixedNumber LIKE MobSub.FixedNumber   NO-UNDO INIT "".
DEF VAR lcDCEvent     AS CHAR                   NO-UNDO. 
DEF VAR lcSurName1    AS CHAR                   NO-UNDO.
DEF VAR lcSurName2    AS CHAR                   NO-UNDO.
DEF VAR lcCompany     AS CHAR                   NO-UNDO.
DEF VAR lcLine        AS CHAR                   NO-UNDO FORMAT "x(75)".
/* BARRING */
DEF VAR lcBarrStat    AS CHAR                   NO-UNDO FORMAT "x(24)".
DEF VAR lcBarrMask    AS CHAR                  NO-UNDO FORMAT "x(24)".
DEF VAR lcBarrCmd     AS CHAR                   NO-UNDO.
DEF VAR lcPCLB        AS CHAR                   NO-UNDO.
DEF VAR lcDSSInfo     AS CHAR                   NO-UNDO.
DEF VAR lrCLBRec      AS RECID                  NO-UNDO.
DEF VAR lrOLBRec      AS RECID                  NO-UNDO.
DEF VAR lcMNP         AS CHARACTER NO-UNDO. 
DEF VAR liMultiSIMType AS INT NO-UNDO. 
DEF VAR llMore        AS LOGICAL                NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR           NO-UNDO.
DEF VAR lcAllowedDSS4SubsType AS CHAR           NO-UNDO. 

DEF BUFFER SearchCustomer FOR Customer.
DEF BUFFER UserCustomer   FOR Customer.
DEF BUFFER AgrCustomer    FOR Customer.
DEF BUFFER InvCustomer    FOR Customer.
DEF BUFFER SearchMobsub   FOR Mobsub.
DEF BUFFER SearchSIM      FOR SIM.
DEF BUFFER SearchIMSI     FOR IMSI.
DEF BUFFER SearchMsowner  FOR MSOWNER.

DEF VAR lcSaldoFatime AS C  NO-UNDO.
lcLine = FILL("-",75). /* mobsub.frm divider */

DEFINE TEMP-TABLE ttServPac
 FIELD SPac AS CHAR
 FIELD SCom AS CHAR
 FIELD SDes AS CHAR.



ASSIGN lcSaldoFatime = fCParamC("SaldoAgreementAccount")
       lcDCEvent     = fCParamC("PerContractID")
       lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
       lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").

FUNCTION fIsPermittedModule RETURNS LOGICAL  
 ( icCliType AS CHAR,
   icModule AS CHAR):

   DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
   
   IF fMatrixAnalyse("1",
                     "DENIED",
                     "SubsTypeFrom;Module",
                     icCliType + ";" + icModule,
                     OUTPUT ocResult) = ? THEN DO:
      RETURN TRUE.
   END.   
   ELSE DO:
      MESSAGE
         SUBST("This function is not allowed with clitype &1!",icCliType)
      VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
END FUNCTION. 

{Mm/mobsub.frm}

form
    Mobsub.CLI              COLUMN-LABEL "MSISDN" 
    Mobsub.MsSeq            COLUMN-LABEL "SubscrID" 
    Mobsub.AgrCust          COLUMN-LABEL "AgrCust" 
    AgrCustomer.CustName    COLUMN-LABEL "Name"     FORMAT "X(14)" 
    AgrCustomer.orgid       COLUMN-LABEL "PerID/ComID"
    UserCustomer.CustName   COLUMN-LABEL "User"     FORMAT "X(15)"  
    Mobsub.MSStatus         FORMAT ">9"    COLUMN-LABEL "St" 
    llMemo                  FORMAT "*/"    COLUMN-LABEL "M"  

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    "  MOBILE SUBSCRIPTION  "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.


form /* seek Mobsub CLI */
    lcCli  
    HELP "Enter MSISDN  "
    WITH ROW 4 COL 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Msisdn "
    COLOR VALUE(Syst.Var:cfc) WIDTH 24  NO-LABELS OVERLAY FRAME f1.

form /* seek Mobsub MsSeq */
    liMsSeq  
    HELP "Enter Subscription ID  "
    WITH ROW 4 COL 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Subscription ID "
    COLOR VALUE(Syst.Var:cfc) WIDTH 24  NO-LABELS OVERLAY FRAME fMsSeq.


form /* Customer :n nimella hakua varten */
  "SurName1" lcSurName1 FORMAT "x(30)"
  HELP "Customers 1st Surname" 
  SKIP
  "SurName2" lcSurName2 FORMAT "x(30)"
  HELP "Customers 2nd Surname"
  SKIP
  "Company:" lcCompany  FORMAT "X(30)"
  HELP "Company name" SKIP
  with row 4 col 2 title color value(Syst.Var:ctc) " FIND Name "
  COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.



form /* seek  CustNum */
    liCustNum
    HELP "Enter Customer No "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Customer No"
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f3.
                
form /* Customer :n nimella hakua varten */
  "LastName/Company:" lcLastName FORMAT "X(30)"
  HELP "Last name or company name" SKIP
  "FirstName ......:" lcFirstName
  FORMAT "X(20)"
  HELP "First name"
  with row 4 col 2 title color value(Syst.Var:ctc) " FIND AGREEMENT NAME "
  COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f4.
 


form /* seek  CustNum */
   lcPersonID
   HELP "Enter Person ID"
   WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Person ID"   COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f5.

form /* seek  CustNum */
    liMSStatus
    HELP "Enter Subscription Status" 
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND StatusCode " 
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f6.
                
form
   lcICC
   HELP "Enter Person ICC" 
   WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) 
   "FIND ICC"  COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f7.

form
   lcFixedNumber
   HELP "Enter Fixed Number" 
   WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) 
   "FIND FIXED NUMBER" COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME frSearchFixed.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By MSISDN  ,  By SUBS ID ,  By CUSTNUM  ,  BY STATUS  ".

run local-find-first.
IF AVAILABLE Mobsub THEN ASSIGN
   Memory       = recid(mobsub)
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
    END.

PrintPage:
   DO :  
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Mobsub WHERE recid(mobsub) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Mobsub THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(mobsub).
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
        Syst.Var:ufk[1]= 209  Syst.Var:ufk[2]= 9018 Syst.Var:ufk[3]= 2902  Syst.Var:ufk[4]= 2903
        Syst.Var:ufk[5]= 2214 Syst.Var:ufk[6]= 2901
        Syst.Var:ufk[7]= 555  Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        
        ELSE ASSIGN
        Syst.Var:ufk[1]= 559  Syst.Var:ufk[2]= 1740 Syst.Var:ufk[3]= 9852 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= 9861 /* SAPC-46 */
        Syst.Var:ufk[6]= 0
        Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.

        IF ictype  NE  "" THEN ASSIGN
         Syst.Var:ufk[1] =  0
         Syst.Var:ufk[2] =  0
         Syst.Var:ufk[3] =  0
         Syst.Var:ufk[4] =  0 
         Syst.Var:ufk[5] =  0
         Syst.Var:ufk[6] =  0
         Syst.Var:ufk[7] =  0.
         
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Mobsub.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Mobsub.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Mobsub.MsSeq  {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Mobsub.MsSeq WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW Mobsub.AgrCust {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Mobsub.AgrCust WITH FRAME sel.
      END.
      IF order = 4 THEN DO:
        CHOOSE ROW Mobsub.MSStatus {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Mobsub.MSStatus WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Mobsub WHERE recid(mobsub) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Mobsub THEN
              ASSIGN FIRSTrow = i Memory = recid(mobsub).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Mobsub THEN DO:
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
                rtab[1] = recid(mobsub)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE Mobsub THEN DO:
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
              rtab[FRAME-DOWN] = recid(mobsub).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Mobsub WHERE recid(mobsub) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Mobsub THEN DO:
           Memory = recid(mobsub).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Mobsub THEN Memory = recid(mobsub).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND Mobsub WHERE recid(mobsub) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND NOT llMore AND icType = "" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lccli WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcCli ENTERED THEN DO:
          FIND FIRST Mobsub WHERE 
                     Mobsub.CLI = lcCli NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Mobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some Mobsub/mobsub was found */
          ASSIGN order = 1 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */
     
     /* Search BY column 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND NOT llMore AND icType = "" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME fMsSeq.
       SET liMsSeq WITH FRAME fMsSeq.
       HIDE FRAME fMsSeq NO-PAUSE.
       IF liMsSeq ENTERED THEN DO:
          FIND FIRST Mobsub WHERE 
                     Mobsub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Mobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some Mobsub/mobsub was found */
          ASSIGN order = 2 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     
     ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND NOT llMore AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f3.
       SET liCustNum WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF liCustNum ENTERED THEN DO:
          
          FIND FIRST Mobsub WHERE 
                     Mobsub.Brand   = "1"   AND 
                     Mobsub.AgrCust = liCustNum NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Mobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some Mobsub/mobsub was found */
          ASSIGN order = 3 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 AND NOT llMore AND 
       icType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       
       ASSIGN
          lcSurname1 = ""
          lcSurname2 = ""
          lcCompany  = "".
       
       CLEAR FRAME f2.
       SET lcSurName1 lcSurName2 lcCompany WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       /* enable search only with the surnames or company */
       IF ( (lcSurName1  > "" OR lcSurName2  > "") AND lcCompany EQ "" ) OR 
          (lcSurname1 EQ "" AND lcSurname2 EQ "" AND lcCompany  > "") 
          THEN DO:
          
          RUN Mm/mobsubfind.p("AGRNAME",lcSurName1 + "|" + 
                                   lcSurname2 + "|" + 
                                   lcCompany).
       END.
       ELSE DO:
          BELL.
          MESSAGE 
            "Search enabled with only either Surnames or Company!".
          PAUSE 2 NO-MESSAGE.
          NEXT browse.
       END.
     END. /* Search-4 */
    
     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND NOT llMore AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f5.
       SET lcPersonid WITH FRAME f5.
       HIDE FRAME f5 NO-PAUSE.

       IF lcPersonID > "" THEN DO:
          RUN Mm/mobsubfind.p("ID",lcPersonid).
       END.
     END. /* Search-5 */
     
     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND NOT llMore AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       ASSIGN lcFirstName = ""
              lcSurName1  = ""
              lcSurName2  = ""
              lcCompany   = "".
                                                        
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       SET lcSurName1 lcSurName2 lcCompany WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       IF lcSurName1  > "" OR 
          lcSurName2  > "" OR 
          lcCompany   > "" THEN DO:
          RUN Mm/mobsubfind.p("USERNAME",lcSurName1 + "|" + 
                                    lcSurName2 + "|" + 
                                    lcCompany). 
       END.
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"7,f7") > 0 AND Syst.Var:ufk[7] > 0 THEN DO:
        llMore = TRUE.
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /********************/
     /* 2nd page options */
     /********************/
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND llMore AND lcRight = "RW" AND 
       ictype = "" THEN DO ON ENDKEY UNDO, NEXT LOOP: 
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f6.
       SET liMSStatus WITH FRAME f6.
       IF  liMSStatus  ne 0  THEN DO:
          FIND FIRST Mobsub WHERE
                     Mobsub.Brand     = "1"           AND
                     Mobsub.MSStatus  = liMSStatus 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Mobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
                                                                                         /* some Mobsub/mobsub was found */
          ASSIGN order = 4 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       ENd.
     END. 

     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND llMore AND 
       iCType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f7.
       SET lcICC WITH FRAME f7.
       HIDE FRAME f7 NO-PAUSE.

       IF lcICC  > ""  THEN DO:
          FOR FIRST SearchSIM WHERE 
                    SearchSIM.ICC      = lcICC ,
              FIRST SearchMobsub WHERE 
                    SearchMobsub.ICC     = lcICC NO-LOCK.
          END.

          IF NOT AVAILABLE SearchMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.

          IF Avail SearchMobsub THEN 
          FIND FIRST Mobsub WHERE 
               RECID(Mobsub) = RECID(SearchMobsub)
          NO-LOCK NO-ERROR.

          /* some Mobsub/mobsub was found */
          ASSIGN order = 1 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. 
     
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND lcRight = "RW" AND llMore AND 
        ictype = "" THEN DO:
        
        RUN local-find-this (FALSE).

        RUN local-find-others(TRUE).
       
        
        RUN Mm/nnfmcu.p(OUTPUT liCustNum, OUTPUT liMsSeq).

        IF liCustNum NE ? AND liCustNum > 0 THEN DO:
           FIND  SearchCustomer WHERE 
                 SearchCustomer.CustNum = liCustNum NO-LOCK.
           IF MsSeq > 0 THEN 
              FIND SearchMobsub WHERE 
                   SearchMobsub.MsSeq = liMsSeq AND
                   SearchMobsub.brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.
           ELSE DO:
              FIND FIRST SearchMobsub WHERE 
                         SearchMobsub.Brand   = Syst.Var:gcBrand AND
                         SearchMobsub.CustNum = liCustNum NO-LOCK NO-ERROR.

              IF NOT AVAIL SearchMobsub THEN DO:
                 MESSAGE 
                    "Customer No." string(liCustNum)                                                 "does NOT have ANY mobile subscriptions"
                 VIEW-AS ALERT-BOX TITLE " NO SUBSCRIPTIONS ".
              END.
           END.
           
           IF AVAIL SearchMobsub THEN DO:
              FIND   Mobsub WHERE 
               RECID(Mobsub) = RECID(SearchMobsub) NO-LOCK NO-ERROR.
               
              ASSIGN
                 memory = RECID(SearchMobsub)
                 must-print = TRUE.
              NEXT loop.
           END.
        END.
     END. 
     ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND llMore AND 
       iCType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME frSearchFixed.
       SET lcFixedNumber WITH FRAME frSearchFixed.
       HIDE FRAME frSearchFixed NO-PAUSE.

       IF lcFixedNumber  > ""  THEN DO:
          FOR FIRST SearchMobsub WHERE
                    SearchMobsub.Brand = Syst.Var:gcBrand AND
                    SearchMobsub.FixedNumber = lcFixedNumber NO-LOCK.
          END.

          IF NOT AVAILABLE SearchMobsub THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.

          IF Avail SearchMobsub THEN 
          FIND FIRST Mobsub WHERE 
               RECID(Mobsub) = RECID(SearchMobsub)
          NO-LOCK NO-ERROR.

          /* some Mobsub/mobsub was found */
          ASSIGN order = 1 Memory = recid(mobsub) must-print = TRUE.
          NEXT LOOP.
       END.
     END. 
     /* SAPC-46  SAPC Commands */     
     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND llMore AND 
       iCType = "" THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". 
       RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. 
       RUN Syst/ufkey.p. 
       ufkey = TRUE.

       RUN local-find-this(FALSE).
       IF AVAILABLE mobsub THEN 
          RUN Mc/ProCommandView.p(INPUT mobsub.msseq, 
                                  INPUT 0).  /* MsRequest.MsRequest */

       NEXT LOOP.
     END. 
     /* SAPC-46 end */      
     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:

       RUN local-find-this(FALSE).
       
       /* refresh mobsub status / barring status */
       lcBarrStat = MobSub.BarrCode.

       RUN local-UPDATE-record(FALSE).  

       MOBSUBBROWSE:
       REPEAT :

          ASSIGN
             ufkey = TRUE
             Syst.Var:ufk   = 0  
             Syst.Var:ehto  = 1  
             Syst.Var:ufk[1] = 7
             Syst.Var:ufk[2] = 788
             Syst.Var:ufk[3] = 2244 
             Syst.Var:ufk[4] = 1990 WHEN lcRight = "RW"
             Syst.Var:ufk[4] = 0 WHEN lcRight NE "RW"
             Syst.Var:ufk[5] =  927
             Syst.Var:ufk[6]=  1992
             Syst.Var:ufk[7]= 249
             Syst.Var:ufk[8]= 8  .
          RUN Syst/ufkey.p.   
          IF Syst.Var:toimi = 8 THEN DO:
             LEAVE.
          ENd.

          IF Syst.Var:toimi = 1 AND
             fIsPermittedModule(mobsub.clitype, "mobsub_update") THEN
             run local-update-record(TRUE).
          ELSE IF Syst.Var:toimi = 2  THEN RUN Mm/persondata.p(mobsub.msseq).
          ELSE IF Syst.Var:toimi = 3  THEN DO:
             RUN Mm/msrequest.p(-1,
                           ?, /* reqstat ? for all */
                           MobSub.MsSeq,
                           0,
                           0,
                           "").
              
             IF NOT AVAIL MobSub THEN DO:  
                MESSAGE 
                  "No Mobile Subscription available!"
                VIEW-AS ALERT-BOX.
                LEAVE.
             END.
             /* refresh mobsub status / barring status */
             lcBarrStat = MobSub.BarrCode.

             /* in case a request was run */
             RUN local-UPDATE-record(FALSE).
          END.      
          ELSE IF Syst.Var:toimi = 4  AND lcRight = "RW" AND 
            fIsPermittedModule(mobsub.clitype, "subser") THEN 
               RUN Mm/subser.p(Mobsub.MsSeq).
          ELSE IF Syst.Var:toimi = 5  THEN RUN Mc/memo.p(INPUT mobsub.CustNum,
                                           INPUT "Mobsub",
                                           INPUT STRING(MobSub.MsSeq),
                                           INPUT "Mobsub").

          ELSE IF Syst.Var:toimi = 6 
             THEN DO:
       
             CALLBROWSE:
             REPEAT WITH FRAME lis ON ENDKEY UNDO, RETURN:

                ASSIGN
                   ufkey  = TRUE
                   Syst.Var:ufk    = 0
                   Syst.Var:ehto   = 1
                   Syst.Var:ufk[1] = 1992
                   Syst.Var:ufk[2] = 844
                   Syst.Var:ufk[3] = 562
                   Syst.Var:ufk[4] = 2435
                   Syst.Var:ufk[5] = 0
                   Syst.Var:ufk[6] = 0
                   Syst.Var:ufk[7] = 1079.
                   Syst.Var:ufk[8] = 8.

                RUN Syst/ufkey.p.   
        
                IF Syst.Var:toimi = 8 THEN DO:
                   LEAVE CALLBROWSE.
                ENd.

                IF       Syst.Var:toimi = 1  THEN RUN Mm/msisdniv.p(Mobsub.MsSeq).

                ELSE IF Syst.Var:toimi = 2 AND avail mobsub  THEN
                   RUN Mm/callstat.p(INPUT 0,Mobsub.cli,"PRODUCT").
       
                ELSE IF Syst.Var:toimi = 3 AND avail mobsub  THEN 
                   RUN Mm/callstat.p(INPUT 0,Mobsub.cli,"DATE").
       
                ELSE IF Syst.Var:toimi = 4 AND avail mobsub  THEN
                   RUN Mm/callstat.p(INPUT 0,Mobsub.cli,"CCN").

                ELSE IF Syst.Var:toimi = 6 THEN 
                   RUN Mm/persondata.p(mobsub.msseq).
                   
                ELSE IF Syst.Var:toimi = 7 THEN
                   RUN Gwy/ppreqbr.p(MobSub.MsSeq).
        
             END.
          END.
          ELSE IF Syst.Var:toimi = 7 AND avail mobsub  THEN DO:
             RUN Mm/mobsubdi.p(INPUT Mobsub.MSSeq, OUTPUT killed).
             
             /* refresh mobsub status / barring status */
             lcBarrStat = MobSub.BarrCode.

              run local-update-record(FALSE).
          END.
       END.
       ASSIGN  Memory = recid(mobsub) must-print = TRUE.
       NEXT LOOP.
    END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(mobsub) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(mobsub) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN DO:
        IF llMore THEN DO:
           llMore = FALSE.
           ufkey = TRUE.
           NEXT LOOP.
        END.
        ELSE LEAVE LOOP.
     END.
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Mobsub WHERE recid(mobsub) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Mobsub WHERE recid(mobsub) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND FIRST Mobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN DO: {Mm/mobfind.i FIRST } end.
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND FIRST Mobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 2 THEN  {Mm/mobfind.i FIRST }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND FIRST Mobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {Mm/mobfind.i FIRST }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND FIRST Mobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {Mm/mobfind.i FIRST }
       

END PROCEDURE.

PROCEDURE local-find-LAST:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND LAST  Mobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {Mm/mobfind.i LAST  }
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND LAST Mobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 2 THEN  {Mm/mobfind.i LAST }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND LAST  Mobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {Mm/mobfind.i LAST  }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND LAST  Mobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {Mm/mobfind.i LAST  }
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND NEXT Mobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {Mm/mobfind.i NEXT }
 
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND NEXT Mobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 2 THEN  {Mm/mobfind.i NEXT }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND NEXT Mobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {Mm/mobfind.i NEXT }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND NEXT Mobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {Mm/mobfind.i NEXT }

END PROCEDURE.

PROCEDURE local-find-PREV:

       IF      order = 1  AND iiCustNum =  0 
       THEN FIND PREV Mobsub USE-INDEX cli     NO-LOCK NO-ERROR.
       
       ELSE IF order = 1  THEN  {Mm/mobfind.i PREV }
       
       ELSE IF order = 2 AND iiCustNum = 0 THEN 
       FIND PREV Mobsub USE-INDEX MSSeq     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 2 THEN  {Mm/mobfind.i PREV }
       
       ELSE IF order = 3 and iiCustNum =  0  
       THEN FIND PREV Mobsub USE-INDEX AgrCust NO-LOCK NO-ERROR.
       
       ELSE IF order = 3 THEN  {Mm/mobfind.i PREV }
       
       ELSE IF order = 4 AND iiCustNum = 0 THEN 
       FIND PREV Mobsub USE-INDEX MSSTatus     NO-LOCK NO-ERROR.
       
       ELSE IF ORDER = 4 THEN  {Mm/mobfind.i PREV }

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others(TRUE).
       
       CLEAR FRAME sel NO-PAUSE.

       PAUSE 0.
       DISPLAY 
          Mobsub.CLI
          MobSub.MsSeq
          Func.Common:mDispCustName(BUFFER UserCustomer)
             WHEN AVAIL UserCustomer @ UserCustomer.CustName
          Mobsub.AgrCust
          Func.Common:mDispCustName(BUFFER AgrCustomer)
             WHEN AVAIL AgrCustomer @ AgrCustomer.Custname 
          AgrCustomer.OrgID WHEN AVAIL AgrCustomer
          llmemo
          Mobsub.msstatus 
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   DEF input parameter llsel AS LOG NO-UNDO.

   DEF VAR liDSSMsSeq        AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit       AS DEC  NO-UNDO.
   DEF VAR lcBundleId        AS CHAR NO-UNDO.

   DEF BUFFER bDSSMobSub     FOR MobSub.
   DEF BUFFER bDSSTermMobSub FOR TermMobSub.
      
   IF CAN-FIND(FIRST memo WHERE 
                     Memo.Brand     = Syst.Var:gcBrand                AND
                     Memo.HostTable = "MobSub"               AND
                     Memo.KeyValue  = STRING(MobSub.MsSeq)   AND
                    (Memo.MemoText NE "" OR 
                     Memo.MemoTitle NE ""))
   THEN ASSIGN llmemo  = TRUE.
   ELSE ASSIGN llmemo  = FALSE.

   FIND FIRST Usercustomer WHERE 
              UserCustomer.CustNum = Mobsub.Custnum
   NO-LOCK NO-ERROR.

   FIND FIRST AgrCustomer WHERE
              AgrCustomer.CustNum = Mobsub.AgrCust
   NO-LOCK NO-ERROR.

   FIND FIRST InvCustomer WHERE 
              InvCustomer.CustNum = Mobsub.InvCust
   NO-LOCK NO-ERROR.
                 
   FIND FIRST MSISDN WHERE 
              MSISDN.CLI = Mobsub.CLI
   NO-LOCK NO-ERROR.
   IF NOT llSel THEN DO:
   
      FIND FIRST BillTarget WHERE  
                 BillTarget.Billtarget = Mobsub.BillTarget AND 
                 BillTArget.custnum    = Mobsub.CustNum
      NO-LOCK NO-ERROR.
      
      IF Mobsub.tariffbundle EQ "" THEN
         lcClitext = mobsub.clitype.
      ELSE DO:
         lcCliText = SUBSTRING(mobsub.clitype,1,8).
         IF LENGTH(lcCliText) < 8 THEN
            lcCliText = lcCliText + FILL(" ", 8 - LENGTH(lcCliText)).
         lcCliText = lcCliText + SUBSTRING(mobsub.tariffbundle,1,8).
      END.

      IF AVAIL BillTarget THEN
         lcBillTarget = STRING(Mobsub.Billtarget) + " " + BillTarget.RatePlan.
      ELSE 
         lcBillTarget = STRING(Mobsub.Billtarget).
      
      FIND FIRST sim WHERE 
                 SIM.Brand  = Syst.Var:gcBrand AND 
                 Sim.Icc = Mobsub.ICC
      NO-LOCK NO-ERROR.
              
      IF Avail sim THEN
         FIND FIRST imsi WHERE 
                    imsi.icc = sim.icc
         NO-LOCK NO-ERROR.
   
      
      /* SalesMan */ 

      FIND FIRST Salesman WHERE 
                 SalesMan.Brand    = Syst.Var:gcBrand AND 
                 Salesman.Salesman = Mobsub.Salesman NO-LOCK NO-ERROR.

      FIND FIRST Reseller WHERE 
                 Reseller.Brand    = Syst.Var:gcBrand AND 
                 Reseller.Reseller = Mobsub.Reseller NO-LOCK NO-ERROR.

      /* Status */ 
      lcStatus = STRING(Mobsub.msstatus) + " " + 
                 entry(Mobsub.MsStatus + 1, stnames).
      
      lcNumberInquiry = fNumberInqExpl(Mobsub.MsSeq).  

      IF Mobsub.ActivationTS > 0 THEN                          
         lcInportTime =    "Activated.....: "  + 
                           STRING(Func.Common:mTS2HMS(mobsub.activationTS)).
      
      ELSE IF AVAIL msisdn AND msisdn.portingDate ne ? then DO:
         lcINPortTime = "Inporting Time: " +
                        STRING(MSISDN.PortingTime,"99.99") + " on " +
                        String(msisdn.portingDate,"99-99-99").
      END.
      ELSE lcInPortTime = "".
                                                   
      lcNotifyNumber  = fNotifyNbrValue(Mobsub.MsSeq).

      liSaldotype    = fCreditTypeValue(Mobsub.MsSeq,
                                        OUTPUT liSaldoLimit) .

      /* saldolimit */ 
      lcSaldoType = Func.Common:mTMSCodeName("CreditType",
                                     "CreditType",
                                      STRING(liSaldoType)).
                                                    
      ldExtraLimit = Func.Common:mChkSaldoAccount(INPUT Mobsub.custnum,
                                       INPUT Mobsub.cli,
                                       INPUT year(today) * 100 + Month(today),
                                       INPUT lcSaldofatime).

      /* SaldoServiceCounter, this not unbilledbalance */ 
      IF liSaldotype > 0 THEN 
      ldeSaldoSCounter   = fUnbilledBalance(MobSub.MsSeq,
                                            YEAR(TODAY) * 100 + MONTH(TODAY)).
      ELSE ldeSaldoSCounter = 0 .               
     
      lcNotifyNumber  = fNotifyNbrValue(Mobsub.MsSeq)   .

            
      /* Get information about pending barrings */
      /*OnHoldiot*/
      ASSIGN
         lcPCLB   = ""
         lcDSSInfo = "".
/*
      IF fGetOnHoldBarrings(MobSub.MsSeq) > "" THEN
         lcPCLB = "Restored barrings exist". 
*/      
      lcBarrMask = fGetFinalMask(MobSub.MsSeq).
      
      lcMNP = Mnp.MNPOutGoing:mGetMNPOutOngoing(MobSub.CLI).
      IF lcMNP NE "" THEN lcMNP = "MNP: " + lcMNP.

      /* Display DSS related information */
      IF NOT MobSub.PayType AND
         fGetDSSMsSeqLimit(INPUT  MobSub.CustNum,
                           INPUT  Func.Common:mMakeTS(),
                           OUTPUT liDSSMsSeq,
                           OUTPUT ldeDSSLimit,
                           OUTPUT lcBundleId) THEN DO:
         FIND FIRST bDSSMobSub WHERE
                    bDSSMobSub.MsSeq = liDSSMsSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bDSSMobSub THEN DO:
            FIND FIRST bDSSTermMobSub WHERE
                       bDSSTermMobSub.MsSeq = liDSSMsSeq NO-LOCK NO-ERROR.
            IF AVAILABLE bDSSTermMobSub THEN
               lcDSSInfo = bDSSTermMobSub.CLI.
         END. /* IF NOT AVAILABLE bDSSMobSub THEN DO: */
         ELSE lcDSSInfo = bDSSMobSub.CLI.

         IF lcBundleId = {&DSS} THEN
            lcDSSInfo  = "DSS Active: " + lcDSSInfo.
         ELSE IF lcBundleId = {&DSS4} AND
            LOOKUP(MobSub.CLIType,lcAllowedDSS4SubsType) > 0 THEN
            lcDSSInfo  = "DSS4 Active: " + lcDSSInfo.
         ELSE IF lcBundleId = {&DSS2} AND
            LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 THEN
            lcDSSInfo  = "DSS2 Active: " + lcDSSInfo.
         ELSE lcDSSInfo = "".
      END. /* IF NOT MobSub.PayType AND */
   END.

   IF MobSub.MultiSimID > 0 THEN DO:

      liMultiSIMType = (IF Mobsub.MultiSimType = 1 THEN 2 ELSE 1).

      FIND FIRST SearchMobsub NO-LOCK WHERE
                 SearchMobsub.Brand = Syst.Var:gcBrand AND
                 SearchMobsub.MultiSimID = MobSub.MultiSimID AND
                 SearchMobsub.MultiSimType = liMultiSIMType NO-ERROR.

       lcMultiSim = "MultiSIM " + 
               (IF liMultiSIMType = 2 
                THEN "Sec.Sub: " ELSE  "Pri.Sub: ") + 
               (IF AVAIL SearchMobsub
                THEN STRING(SearchMobsub.MsSeq) ELSE "N/A").
   END.
   ELSE lcMultiSim = "".
   
   /* Provisioning type - SAPC-46 */
   IF NOT AVAIL userCustomer THEN 
      lcProvisioningType = "Unknown". 
   ELSE 
   DO:   
      IF usercustomer.accGrp = 1 THEN
         lcProvisioningType = "PL".
      ELSE IF usercustomer.accGrp = 2 THEN
         lcProvisioningType = "SAPC".
      ELSE
         lcProvisioningType = "Unknown".
   END. 
      
END PROCEDURE.

PROCEDURE local-UPDATE-record.

   DEF INPUT PARAMETER llUpdate AS LOG NO-UNDO.
      
   RUN local-find-others(FALSE).
   PAUSE 0.
   DISP
      Mobsub.MSSeq 
      lcCliText 
      Mobsub.AgrCust
      MobSub.CLI
      MobSub.CLI + " / " + Mobsub.FixedNumber
         WHEN (Mobsub.FixedNumber NE ? AND MobSub.CLI NE Mobsub.FixedNumber) 
            @ Mobsub.CLI
      lcBillTarget
      Mobsub.Custnum
      Mobsub.InvCust
      AgrCustomer.OrgID WHEN AVAIL AgrCustomer
      Mobsub.ICC
      IMSI.PIN1 WHEN AVAIL IMSI
      IMSI.PIN2 WHEN AVAIL IMSI
      IMSI.PUK1 WHEN AVAIL IMSI
      IMSI.PUK2 WHEN AVAIL IMSI
      
      Func.Common:mDispCustName(BUFFER AgrCustomer)
         WHEN AVAIL AgrCustomer @ AgrCustomer.CustName 
      
      Func.Common:mDispCustName(BUFFER InvCustomer)
         WHEN AVAIL InvCustomer @ InvCustomer.CustName 
      
      Func.Common:mDispCustName(BUFFER UserCustomer)
         WHEN AVAIL UserCustomer @ UserCustomer.CustName
      
      MObsub.Salesman
      MobSub.Reseller
      lcLine
      lcStatus 
      lcBarrMask @ lcBarrStat
      lcPCLB 
      lcDSSInfo
      lcNumberInquiry
      Mobsub.CreationDate
      Mobsub.ActivationDAte
      lcinporttime
      mobsub.paytype
      lcMNP
      lcMultiSim
      lcProvisioningType /* SAPC-46 */
   WITH FRAME lis.
     
   IF lcRight NE "RW" OR llupdate = FALSE  THEN RETURN.
      
   FIND CURRENT MobSub NO-LOCK NO-ERROR.
   
   UPDATE_LOOP:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
      Syst.Var:ehto = 9.
      RUN Syst/ufkey.p.
   
      DISP MobSub.Reseller
           MobSub.Salesman.

      PROMPT-FOR 
         Mobsub.Reseller
         Mobsub.Salesman
      WITH FRAME lis EDITING:
      
         READKEY.
            
         IF KEYLABEL(LASTKEY) = "F9" AND 
            LOOKUP(FRAME-FIELD,"Reseller,Salesman") > 0 
         THEN DO:

            IF FRAME-FIELD = "Reseller" THEN DO:
               
               RUN h-reseller.

               IF siirto ne ? THEN DO:
                  DISPLAY siirto @ Mobsub.Reseller.
               END.
               
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
            
            END.
            
            IF FRAME-FIELD = "Salesman" THEN DO:
               
               RUN Help/h-salesman.p.

               IF siirto ne ? THEN DO:
                 DISPLAY siirto @ MobSub.Salesman WITH FRAME lis.
               END.
               
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
            
            END.
          
         END.
        
        IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
           
            IF FRAME-FIELD = "Reseller" THEN DO:
             
               IF INPUT Mobsub.Reseller NE "" THEN DO:
               
                  FIND FIRST Reseller WHERE
                     Reseller.Brand = Syst.Var:gcBrand AND
                     Reseller.Reseller = INPUT Mobsub.Reseller 
                  NO-LOCK NO-ERROR.

                  IF NOT AVAIL Reseller THEN DO:
                     MESSAGE "Reseller not found"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

               END.

            END.
            
            IF FRAME-FIELD = "Salesman" THEN DO:
               
               IF LOOKUP(INPUT Mobsub.Salesman,"WEB,YOIGO,GIFT") = 0 THEN DO:
                  
                  FIND FIRST Salesman WHERE
                     Salesman.Brand = Syst.Var:gcBrand AND
                     Salesman.Salesman = INPUT Mobsub.Salesman
                  NO-LOCK NO-ERROR.

                  IF NOT AVAIL Salesman THEN DO:
                     MESSAGE "Salesman not found"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.  
                 
               END.

               NEXT-PROMPT Mobsub.Reseller.
               
            END.
         
         END.
         
         ON GO OF FRAME lis DO:
               
            FIND CURRENT MobSub NO-LOCK.
            
            IF CURRENT-CHANGED Mobsub THEN DO:
               
               MESSAGE {&MSG_RECORD_CHANGED}
               VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

            END. 
            ELSE DO: 
            
               FIND CURRENT MobSub EXCLUSIVE-LOCK.
               
               IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhMobsub ).
               
               ASSIGN
                  Mobsub.Reseller
                  Mobsub.Salesman.
               
               IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhMobsub ).
           
           END.

         END.
         
         APPLY LASTKEY.

      END. /* editing */
      
      LEAVE.

   END. /* repeat */
      
   FIND CURRENT MobSub NO-LOCK.
      
   DISP 
      Mobsub.Reseller
      Mobsub.Salesman WITH FRAME lis.

END PROCEDURE.
         
fCleanEventObjects().
