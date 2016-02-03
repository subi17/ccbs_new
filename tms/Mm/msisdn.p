 /* ----------------------------------------------------------------------
  MODULE .......: ttMSISDN.P
  TASK .........: Update/Browse ttMSISDN Numbers
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 10-06-99
  CHANGED ......: 23-06-99 jp ttMSISDN Classes
                  04-08-99 jp Search for status(F3)
                  13.08.99 pt Browse msowners (F4)
                  13.08.99 jp names of mobuser added
                  17.08.99 jp F6 fixed to mobsub.MUSeq
                  24.08.99 pt ttMSISDN.i
                  05.10.99 jp urights added
                  02.12.99 pt ttMSISDN.CustNum must NOT be changed
                  27.08.01 pt new fields for MNP
                  20.11.01 pt F1 search: m_pref can be overrun if desired
                  01.05.02 jp Brandcode
                  14.10.02 jr Removed BillLevel & BillTarget
                  06.11.02 jr Eventlog
                  10.03.03 tk tokens
                  17.03.03 jp fVALmino validationr rules changed
                  07.04.03 jp update custnum if sim status avail or reserved
                  14.04.03 tk OrderId to frame sel
                  25.08.03 jp change activation date for mobsub
                  01.10.03 jp ttMSISDN
                  26.01.04 jp change portingtime
                  13.12.04 jp send sms message
                  19.03.04 kl improved detail screen

  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ttMSISDN

DEF  INPUT PARAMETER   icCli     LIKE MSISDN.Cli     NO-UNDO.
DEF  INPUT PARAMETER   iiCustNum LIKE MSISDN.CustNum NO-UNDO.
DEF  INPUT PARAMETER   iiStatusCode LIKE MSISDN.StatusCode NO-UNDO.

{Syst/commali.i}
{Func/msisdn.i}
{Syst/eventval.i} 
{Func/timestamp.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Func/freplacesms.i}
{Syst/tmsconst.i}

DEF /* SHARED */ VAR siirto AS char.

DEF VAR CLI   like MSISDN.CLI   NO-UNDO.
DEF VAR CustNum  like MSISDN.CustNum  NO-UNDO.
DEF VAR StatusCode like MSISDN.StatusCode NO-UNDO.
Def VAR OrderId like MSISDN.orderid NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 4.
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
DEF VAR name         AS C   format "x(50)"     NO-UNDO.
DEF VAR name2        AS C   format "x(1)"      NO-UNDO  init " ".
DEF VAR hh           AS I                      NO-UNDO.
DEF VAR mm           AS I                      NO-UNDO.

DEF VAR def-ccode    AS C                      NO-UNDO.
DEF VAR def-mobpref  AS C                      NO-UNDO.
DEF VAR brandcode    AS C                      No-UNDO.
DEF VAR lcMSISDNType AS C                      NO-UNDO.
DEF VAR llFind       AS LOG                    NO-UNDO.
DEF VAR lcValidFrom  AS CHAR                   NO-UNDO FORMAT "X(16)".
DEF VAR lcValidTO    AS CHAR                   NO-UNDO FORMAT "X(16)".
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR lcPOS        AS CHAR                   NO-UNDO.
DEF VAR lcOrder      AS CHAR                   NO-UNDO.
DEF VAR ldeNow       AS DECIMAL NO-UNDO. 

m_pref = "".

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/create_eventlog.i}
   {lib/eventlog.i}

   DEFINE VARIABLE lhttMSISDN AS HANDLE NO-UNDO.
   lhttMSISDN = BUFFER MSISDN:HANDLE.
   RUN StarEventInitialize(lhttMSISDN).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhttMSISDN).
   END.
END.


DEF TEMP-TABLE ttMSISDN LIKE MSISDN.
DEF BUFFER bttMSISDN FOR ttMSISDN.

IF iiCustNum > 0 THEN 
FOR EACH MSISDN WHERE 
         MSISDN.Brand   = gcBrand AND 
         MSISDN.CustNum = iiCustNum NO-LOCK.
   
   CREATE ttMSISDN.
   BUFFER-COPY MSISDN TO ttMSISDN.      
END.         
ELSE IF icCli > "" THEN
FOR EACH MSISDN WHERE
         MSISDN.Brand = gcBrand AND 
         MSISDN.CLI = icCLI NO-LOCK.
         
   CREATE ttMSISDN.
   BUFFER-COPY MSISDN TO ttMSISDN.
END.
ELSE IF iiStatusCode > 0 THEN DO:
   disp " Please wait... " with frame frWait overlay row 7 centered.
   pause 0.
   
   ldeNow = fMakeTS().
   FOR EACH msisdn where
      msisdn.brand = gcBrand and
      msisdn.statuscode = iiStatusCode and
      msisdn.validto > ldeNow NO-LOCK use-index statuscode:
      CREATE ttMSISDN.
      BUFFER-COPY MSISDN TO ttMSISDN.
   END.
  
   HIDE FRAME frWait NO-PAUSE.
END.


form
    ttMSISDN.brand
    ttMSISDN.CLI      /* column-label format */
    ttMSISDN.CustNum     /* column-label format */
    Customer.CustName    format "x(12)"  column-label "CustName" 
    ttMSISDN.StatusCode    format "zz9"    column-label "Sta"
    msstat.StatusName    format "x(8)"
    ttMSISDN.OutOperator   format "x(4)"   column-label "MNP"
    ttMSISDN.ValidFrom     FORMAT "99999999"
    
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " MSISDN Numbers "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
  "MSISDN .....:" ttMSISDN.CLI  lcMSISDNType SKIP
  "Period .....:" lcValidFrom "-" lcvalidto  SKIP
  "Customer No.:" ttMSISDN.CustNum  lcCustName at 26 FORMAT "X(40)" SKIP
  "SubsSeq.....:" ttMSISDN.msseq             SKIP
  "Status .....:" ttMSISDN.StatusCode msstat.StatusName at 26 no-label SKIP
  "Action date.:" ttMSISDN.ActionDate        SKIP
  "Porting time:" ttMSISDN.PortingDate ttMSISDN.PortingTime       SKIP
  "To/From Op. :" ttMSISDN.OutOperator                             
    HELP "Operator name or abreviation"                           SKIP
  "Class ......:" ttMSISDN.MCCode  msclass.MCName at 26           SKIP
  "MNP.........:" ttMSISDN.MNP                                    SKIP
  "POS.........:" ttMSISDN.POS FORMAT "x(20)"
                  lcPOS no-label FORMAT "X(30)" SKIP
  "PayType ....:" ttMSISDN.PayType                                SKIP
  "OrderID.....:" lcOrder  FORMAT "X(20)"                         SKIP
WITH
   overlay row 4 centered NO-LABELS
   COLOR VALUE(cfc)
   title COLOR VALUE(ctc)
   ac-hdr WITH 
   Frame lis.

form /* seek ttMSISDN No.  by  CLI */
   "  Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(lcbrand = "*" OR 
      CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
   "MSISDN No.:" CLI format "x(11)"
      help "Enter ttMSISDN"
   WITH row 4 col 2 title COLOR VALUE(ctc) " FIND ttMSISDN NO."
   COLOR VALUE(cfc) no-labels overlay FRAME f1.

form /* seek ttMSISDN No.  by CustNum */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "CustomerNo:" CustNum
    help "Enter Customer"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CUST. NO. "
    COLOR VALUE(cfc) no-labels overlay FRAME f2.

form /* seek ttMSISDN No.  by StatusCode */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "StatusCode:" StatusCode 
    help "Enter Status"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND STATUS "
    COLOR VALUE(cfc) no-labels overlay FRAME f3.

form /* seek ttMSISDN No.  by StatusCode */
    "Brand Code:" lcBrand  HELP "Enter Brand"
     VALIDATE(lcbrand = "*" OR 
              CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Order Id..:" OrderId 
    help "Enter OrderId"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND ORDERID "
    COLOR VALUE(cfc) no-labels overlay FRAME f4.

FUNCTION fVALmino RETURNS LOGICAL(CLI AS CHAR).
   /* check the validity of new ttMSISDN No. */
   DEF VAR errmsg  AS C  NO-UNDO.
   DEF VAR numeric AS LO NO-UNDO.
   DEF VAR i       AS i  NO-UNDO.

   IF CLI NE "" THEN DO:
      IF NOT CLI BEGINS "0" THEN 
         errmsg = "MSISDN Number shall begin with '46!".
      ELSE DO:                    
         numeric = true.             
         DO i = 1 to LENGTH(CLI):
            IF INDEX("0123456789",SUBSTR(CLI,i,1)) = 0 THEN
            ASSIGN numeric = FALSE.
         END.
         IF NOT numeric THEN 
            errmsg = "Invalid characters in ttMSISDN number !".
      END.       

      IF errmsg NE "" THEN DO:
         MESSAGE errmsg
         VIEW-AS ALERT-BOX ERROR 
         TITLE "Invalid ttMSISDN Number !".
         RETURN FALSE.
      END.
      ELSE RETURN TRUE.
   END.
   ELSE RETURN TRUE.
END FUNCTION.



cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By ttMSISDN ,By CustNo ,By Status ,By OrderId".

FIND FIRST ttMSISDN NO-LOCK NO-ERROR.

IF AVAILABLE ttMSISDN THEN ASSIGN
   memory       = recid(ttMSISDN)
   must-print   = true
   must-add     = false.
ELSE DO:
      MESSAGE "No MSISDN numbers available !" VIEW-AS ALERT-BOX.
      RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       put screen row FrmRow + FrmDown + 3 col 35 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ttMSISDN  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, leave ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           FIND FIRST bttMSISDN NO-LOCK WHERE
                      bttMSISDN.Brand = gcBrand AND
                      bttMSISDN.CLI = bttMSISDN.CLI USE-INDEX CLI.
           
           FIND FIRST MobSub WHERE
                      MobSub.CLI = ttMSISDN.CLI NO-LOCK NO-ERROR.
           IF AVAIL MobSub THEN DO:
              MESSAGE "MSISDN is in use!" VIEW-AS ALERT-BOX ERROR.
              UNDO add-row, LEAVE add-row.
           END.

           CREATE ttMSISDN.
           BUFFER-COPY bttMSISDN EXCEPT ValidFrom TO ttMSISDN.
           ASSIGN
              ttMSISDN.ValidFrom = fMakeTS().

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
              UNDO add-row, LEAVE add-row.

           /* Do real update to database */
           FIND FIRST MSISDN EXCLUSIVE-LOCK WHERE
                      MSISDN.Brand = gcBrand AND
                      MSISDN.CLI = ttMSISDN.CLI USE-INDEX CLI NO-WAIT NO-ERROR.
           IF NOT AVAIL MSISDN THEN DO:
              MESSAGE "MSISDN not found or record is locked"
              VIEW-AS ALERT-BOX ERROR.
              UNDO add-row, LEAVE add-row.
           END.

           fMakeMsidnHistory(RECID(MSISDN)).
           ASSIGN
             MSISDN.StatusCode = ttMSISDN.StatusCode
             MSISDN.POS = ttMSISDN.POS.
   
           IF llDoEvent THEN fMakeCreateEvent((BUFFER MSISDN:HANDLE),
                                              "",
                                              katun,
                                              "").

           FIND CURRENT MSISDN NO-LOCK.

           ASSIGN
           memory = recid(ttMSISDN)
           xrecid = memory.
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      IF iiCustNum > 0 THEN
      FIND FIRST ttMSISDN WHERE
                 ttMSISDN.Brand   = lcBrand AND
                 ttMSISDN.CustNum = iiCustNum NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST ttMSISDN WHERE
                 ttMSISDN.Brand = lcBrand AND
                 ttMSISDN.Cli   = icCli  NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE ttMSISDN THEN leave LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND ttMSISDN WHERE recid(ttMSISDN) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttMSISDN THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(ttMSISDN).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN leave.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 816 ufk[2]= 0 ufk[3]= 0  
        ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= 1752 /* 238 */ ufk[7]= 0 /* 788 */ ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      CASE ORDER:
        WHEN 1 THEN
           choose row ttMSISDN.CLI   ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        when 2 THEN
           choose row ttMSISDN.CustNum  ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        WHEN 3 THEN
           choose row ttMSISDN.StatusCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.

      END.

      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND ttMSISDN WHERE recid(ttMSISDN) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-prev.
           IF AVAILABLE ttMSISDN THEN
              ASSIGN FIRSTrow = i memory = recid(ttMSISDN).
           ELSE leave.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* previous row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-prev.
           IF NOT AVAILABLE ttMSISDN THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttMSISDN)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttMSISDN THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(ttMSISDN).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttMSISDN WHERE recid(ttMSISDN) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE ttMSISDN THEN DO:
           memory = recid(ttMSISDN).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-prev.
              IF AVAILABLE ttMSISDN THEN memory = recid(ttMSISDN).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND ttMSISDN WHERE recid(ttMSISDN) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN repeat with frame sel.

       ASSIGN ufkey = TRUE ufk = 0 ehto = 1
       ufk[1]= 209  ufk[2]= 702 ufk[3]= 559 ufk[4] = 2211 
       ufk[8] = 8.
       run ufkey.

       if toimi = 8 then next browse.

       if toimi = 1 then do:

          cfc = "puyr". RUN ufcolor.
          ehto = 9. RUN ufkey. ufkey = true.
          clear frame f1.
          Disp lcBrand With FRAME f1.
          CLI = m_pref.             
          ok = FALSE.
          UPDATE lcBrand WHEN gcAllBrand = TRUE  
                 CLI WITH FRAME f1 EDITING:
             IF NOT ok and frame-field = "cli" THEN DO:
                ok = TRUE.
                DO i = 1 TO LENGTH(m_pref):
                   APPLY KEYCODE("cursor-right").
                END.   
             END.
             READKEY.
             APPLY LASTKEY.
          END.   

          HIDE FRAME f1 NO-PAUSE.
          IF CLI ENTERED THEN DO:
             if lcbrand ne "*" THEN 
             FIND FIRST ttMSISDN WHERE 
                        ttMSISDN.CLI >= CLI  AND
                        ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
             ELSE  FIND FIRST ttMSISDN  WHERE
                              ttMSISDN.CLI >= CLI NO-LOCK NO-ERROR.

             IF NOT  fRecFound(1) THEN NEXT Browse.
             NEXT LOOP.
          END.

        END. /* Search-1 */

        /* Search by col 2 */
        else if toimi = 2 then do:
          cfc = "puyr". RUN ufcolor.
          ehto = 9. RUN ufkey. ufkey = true.
          CLEAR FRAME f2.
          Disp lcBrand With FRAME f2.
          SET  lcBrand WHEN gcAllBrand = TRUE 
               CustNum WITH FRAME f2.
          HIDE FRAME f2 NO-PAUSE.
          IF CustNum ENTERED THEN DO:
             FIND FIRST ttMSISDN WHERE 
                        ttMSISDN.CustNum >= CustNum   AND 
                        ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.

             IF NOT  fRecFound(2) THEN NEXT Browse.

             NEXT LOOP.
          END.
        END. /* Search-2 */

        /* Search by col 3 */
        else if toimi = 3 then do:
          cfc = "puyr". RUN ufcolor.
          ehto = 9. RUN ufkey. ufkey = true.
          DISP lcBrand WITH FRAME f3.
          SET  lcBrand WHEN gcAllBrand = TRUE
               StatusCode WITH FRAME f3.
          HIDE FRAME f3 NO-PAUSE.
          IF StatusCode ENTERED THEN DO:
             FIND FIRST ttMSISDN USE-INDEX StatusCode WHERE 
                        ttMSISDN.StatusCode >= StatusCode   AND 
                        ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.

              IF NOT  fRecFound(3) THEN NEXT Browse.
             NEXT LOOP.
          END.
        END. /* Search-3 */

        /* Search by col 4 */
        else if toimi = 4 then do:
          cfc = "puyr". RUN ufcolor.
          ehto = 9. RUN ufkey. ufkey = true.
           Disp lcBrand With FRAME f4.
          SET   lcBrand WHEN gcAllBrand = TRUE  
               orderid WITH FRAME f4.
          HIDE FRAME f4 NO-PAUSE.
          IF orderid ENTERED THEN DO:
             IF lcBrand ne "*" THEN 
             FIND FIRST ttMSISDN use-index order WHERE 
                        ttMSISDN.orderid <= orderid  AND 
                        ttMSISDN.Brand = lcBrand 
             NO-LOCK NO-ERROR.
             ELSE 
             FIND FIRST ttMSISDN use-index order WHERE
                        ttMSISDN.orderid <= orderid 
             NO-LOCK NO-ERROR.           

             IF NOT  fRecFound(4) THEN NEXT Browse.

             NEXT LOOP.
          END.
        END. /* Search-4 */

     end.
/*
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN  DO:
        RUN local-find-this (false).

        FIND mobsub WHERE mobsub.CLI = ttMSISDN.CLI NO-LOCK NO-ERROR.

        IF NOT AVAIL mobsub THEN DO:
           MESSAGE
           "This ttMSISDN Number is not assigned" SKIP
           "to any Mobile Subscription"
           VIEW-AS ALERT-BOX ERROR
           TITLE " ttMSISDN IS NOT IN USE ".
           NEXT.
        END.

        ufkey = true.
        NEXT loop.
     END.



     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN 
CUST:     
     REPEAT: /* show customer data */
        RUN local-find-this (false).
        IF ttMSISDN.CustNum = 0 then do:
           message 
           "This MSISDN " ttMSISDN.CLI SKIP
           "is not attached to any customer !"
           view-as alert-box error.
           leave.
        end.
        pause 0.
        clear frame cust no-pause.
CU-DATA:
        repeat with frame cust:

           find Customer of ttMSISDN no-lock no-error.


CU-ACTION:
           repeat with frame cust:
              assign ufk = 0 ufk[8] = 8 ehto =  0.
              run ufkey.
              case toimi:
                 WHEN 8 THEN do:
                    hide frame cust no-pause.
                    LEAVE cu-data.
                 END.   
              end.
           end.     
        end.   
        ufkey = true. 
        LEAVE.
     end.      

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO: /* list known users of ttMSISDN */
       RUN local-find-this (false).
       RUN msowner2(ttMSISDN.cli).
       ufkey = TRUE.
       NEXT loop.
     END.
*/
     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i} 
        RUN local-find-this (false).
        must-add = true.
        NEXT LOOP.
     END.
      
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO: 
        RUN local-find-this (false).
        RUN eventsel.p("MSISDN",
                        "#BEGIN" + CHR(255) + gcBrand + CHR(255) +
                        STRING(ttMSISDN.CLI)).
        ufkey = true.
     END.   

/*
     ELSE IF LOOKUP(nap,"#") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-line.
       RUN local-find-this (false).

if ttMSISDN.StatusCode ne 1 then do:
   message "Status is" ttMSISDN.StatusCode "- do no deee it !".
   pause 2 no-message.
   next.
end.   

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ttMSISDN.CLI ttMSISDN.CustNum ttMSISDN.brand .

       RUN local-find-NEXT.
       IF AVAILABLE ttMSISDN THEN memory = recid(ttMSISDN).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-prev.
          IF AVAILABLE ttMSISDN THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(ttMSISDN).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ttMSISDN.CLI ttMSISDN.CustNum ttMSISDN.brand .
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhttMSISDN).
           DELETE ttMSISDN.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ttMSISDN
           WHERE ttMSISDN.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              leave LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */
*/
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:
       /* change */
       RUN local-find-this(FALSE).

       FIND mobsub WHERE mobsub.CLI = ttMSISDN.CLI NO-LOCK NO-ERROR.
       IF AVAIL mobsub THEN DO:
          RUN local-find-others.
          MESSAGE
             "This MSISDN" ttMSISDN.CLI "is currently assigned"    SKIP
             "into a mobile subscription that belongs to customer" SKIP
             Customer.CustNum  lcCustName 
          VIEW-AS ALERT-BOX TITLE " MSISDN is in use !".
       END.
       ASSIGN ac-hdr = " VIEW " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* If  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttMSISDN).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttMSISDN) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttMSISDN) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN leave LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo no-undo.

    if exlock then
      find ttMSISDN WHERE recid(ttMSISDN) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find ttMSISDN WHERE recid(ttMSISDN) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST ttMSISDN
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttMSISDN USE-INDEX CustNum
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST ttMSISDN USE-INDEX StatusCode
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST ttMSISDN USE-INDEX orderid
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttMSISDN
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttMSISDN USE-INDEX CustNum
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST ttMSISDN USE-INDEX StatusCode
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST ttMSISDN USE-INDEX orderid
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.     
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttMSISDN
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttMSISDN USE-INDEX CustNum
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT ttMSISDN USE-INDEX StatusCode
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT ttMSISDN USE-INDEX orderid
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev ttMSISDN
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev ttMSISDN USE-INDEX CustNum
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND prev ttMSISDN USE-INDEX StatusCode
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND prev ttMSISDN USE-INDEX orderid
       WHERE ttMSISDN.Brand = lcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME SEL no-pause. 
       DISPLAY 
       ttMSISDN.brand
       ttMSISDN.CLI
       ttMSISDN.StatusCode
       msstat.StatusName  when avail msstat
       "???"           when not avail msstat @ msstat.StatusName
       ttMSISDN.OutOperator
       ttMSISDN.ValidFrom
       ttMSISDN.CustNum
       Customer.CustName when avail Customer
       
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   
   find msclass where 
        MSClass.Brand = gcBrand AND 
        msclass.MCCode = ttMSISDN.MCCode no-lock no-error.
   
   find Customer of ttMSISDN no-lock no-error.
   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,BUFFER Customer).

   find msstat  of ttMSISDN no-lock no-error.
   
   IF ttMSISDN.CustNum ne 0 THEN

   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "MSISDN" AND
              TMSCodes.FieldName = "MSISDNtype" AND
              TMSCodes.CodeGroup = "MSISDN" AND
              TMSCodes.CodeValue = STRING(ttMSISDN.MSISDNtype)
   NO-LOCK NO-ERROR.

   IF AVAIL tmscodes THEN lcMSISDNType = TMSCodes.CodeName.
   ELSE                   lcMSISDNType = "".
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "MSISDN"    AND
              TMSCodes.FieldName = "POS" AND
              TMSCodes.CodeValue  = ttMSISDN.POS 
   NO-LOCK NO-ERROR.
   IF AVAIL tmscodes THEN lcPOS = TMSCodes.CodeName.
   ELSE                   lcPOS = "".
   
   FIND FIRST Order WHERE
              Order.OrderID = ttMSISDN.OrderId AND
              Order.Brand   = ttMSISDN.Brand
   NO-LOCK NO-ERROR.

   IF AVAIL Order THEN lcOrder = STRING(Order.OrderId).
   ELSE                lcOrder = "No order available".

END PROCEDURE.

PROCEDURE local-update-record:

   &SCOPED-DEFINE MSISDN_FUNC_RETURN " SEND RETURN NOTICE "
   &SCOPED-DEFINE MSISDN_FUNC_PROCESS " SHOW MNP PROCESS "
   &SCOPED-DEFINE MSISDN_FUNC_HISTORY " SHOW MSISDN HISTORY "
   
   DEF VAR llDispMenu   AS LOG  NO-UNDO.
   DEFINE VARIABLE lcMenuOptions AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcSelected AS CHARACTER NO-UNDO.  
   
   llDispMenu = NOT NEW ttMsisdn.

   FIND FIRST msowner WHERE
              msowner.CLI = ttMSISDN.CLI
   NO-LOCK NO-ERROR.

   UPDATE-LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP
         ttMSISDN.cli
         fTS2HMS(ttMSISDN.ValidFrom) @ lcValidFrom 
         fTS2HMS(ttMSISDN.Validto)   @ lcValidTo
         lcMSISDNType
         ttMSISDN.CustNum
         ttMSISDN.ActionDate
         ttMSISDN.PayType
         ttMSISDN.MSSeq
         ttMSISDN.POS
         lcPOS
         ttMSISDN.MNP
         lcOrder
         ttMSISDN.StatusCode
         ttMSISDN.McCode
         ttMSISDN.MNP
         lcCustName         when avail Customer
         msstat.StatusName  when avail msstat
         msclass.MCName     when avail msclass
      WITH FRAME lis.

      IF llDispMenu THEN DO:

         IF iiStatusCode NE {&MSISDN_ST_WAITING_RETURN} AND
            iiStatusCode NE {&MSISDN_ST_RETURN_NOTICE_SENT} AND
            iiStatusCode NE {&MSISDN_ST_RETURNED} THEN DO:
            MESSAGE "PRESS ENTER TO CONTINUE". PAUSE NO-MESSAGE.
            LEAVE. 
         END.
         
         ASSIGN
           ufk = 0
           ufk[7]= 1522
           ufk[8]= 8
           ufk[9]= 1
           ehto = 0.

         RUN ufkey.
      END.
      ELSE ASSIGN toimi = 1.
/*
      READKEY. 
      ASSIGN nap = keylabel(LASTKEY).
  */    
      IF toimi = 1 THEN DO TRANS:
         RUN pUpdate.
         IF NEW ttMsisdn THEN RETURN.
         llDispMenu = TRUE.
      END.
      
      IF toimi = 7 THEN DO:
         
         lcMenuOptions = "". 
         CASE iiStatusCode:
            WHEN {&MSISDN_ST_WAITING_RETURN} THEN 
               lcMenuOptions = {&MSISDN_FUNC_RETURN} + "|" +
                               {&MSISDN_FUNC_PROCESS}.
            WHEN {&MSISDN_ST_RETURN_NOTICE_SENT} OR WHEN {&MSISDN_ST_RETURNED} THEN 
               lcMenuOptions = {&MSISDN_FUNC_PROCESS}.
         END.

/*       lcMenuOptions = lcMenuOptions + "|" + {&MSISDN_FUNC_HISTORY}. */
         
         RUN selectbox(
            "MSISDN FUNCTION",
            lcMenuOptions,
            OUTPUT lcSelected).

         CASE lcSelected:
            
            WHEN {&MSISDN_FUNC_RETURN} THEN DO:
            
               ok = FALSE.
               MESSAGE 
                "Do you want to start number termination process (Y/N) ?"
               UPDATE ok.
               IF NOT ok THEN NEXT UPDATE-LOOP.
               
               RUN mnpnumbertermrequest.p(ttMSISDN.CLI,0).
               
               IF RETURN-VALUE BEGINS "ERROR:" THEN 
                  MESSAGE ENTRY(2,RETURN-VALUE,":") VIEW-AS ALERT-BOX ERROR.
               ELSE MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.

            END.      
            WHEN {&MSISDN_FUNC_PROCESS} THEN DO:
            
               RUN mnpclibr.p({&MNP_TYPE_TERMINATION}, 0, ttMSISDN.CLI).

            END.      
            WHEN {&MSISDN_FUNC_HISTORY} THEN DO:
            
               RUN msisdn.p(ttMSISDN.CLI, 0, 0).

            END.      
         END.

         NEXT UPDATE-LOOP.
      END.   
      
      IF toimi = 8 THEN DO:
         HIDE FRAME choices NO-PAUSE.
         HIDE MESSAGE.
         LEAVE UPDATE-LOOP.
      END.   

   END.

END PROCEDURE.

PROCEDURE pUpdate:

   DEF VAR lcReturn AS CHAR NO-UNDO. 
   DEF VAR llOk AS LOG NO-UNDO.

   FIND CURRENT ttMSISDN EXCLUSIVE-LOCK.
      
   ehto = 9.
   RUN ufkey.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      UPDATE
         ttMSISDN.StatusCode
         ttMSISDN.POS
      WITH FRAME lis EDITING:
 
         READKEY.
         
         IF KEYLABEL(LASTKEY) EQ "F2" THEN NEXT.
         
         IF KEYLABEL(LASTKEY) = "F9" THEN DO:

            CASE FRAME-FIELD:

               WHEN "POS" THEN DO:

                  RUN tmscodesbr.p(input "MSISDN",
                                 input "POS",
                                 input "SNSTest",
                                 input "Choose Stock",
                                 input "",
                                 OUTPUT lcReturn).

                  IF lcReturn NE "" THEN DO:
                     ttMSISDN.POS = lcReturn.
                  END.

                  DISP ttMSISDN.POS WITH FRAME lis.
                  ehto = 9. RUN ufkey.
                  NEXT.
               END.
            END.
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.
               
            IF FRAME-FIELD = "StatusCode" THEN DO:

               FIND MsStat WHERE
                    MsStat.StatusCode = INPUT ttMSISDN.StatusCode
               NO-LOCK NO-ERROR.

               IF NOT AVAIL MsStat OR MsStat.StatusCode EQ 3 THEN DO:
                  MESSAGE "Incorrect status code"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT ttMSISDN.StatusCode.
                  NEXT.
               END.
               ELSE DISP msstat.StatusName.

            END.
            ELSE IF FRAME-FIELD = "POS" THEN DO:

               FIND FIRST TMSCodes WHERE
                          TMSCodes.TableName = "MSISDN"    AND
                          TMSCodes.FieldName = "POS" AND
                          TMSCodes.CodeValue  = INPUT ttMSISDN.POS 
               NO-LOCK NO-ERROR.

               IF NOT AVAIL TMSCodes THEN DO:
                  MESSAGE "Unknown stock"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT ttMSISDN.POS.
                  NEXT.
               END.
               lcPOS = TMSCodes.CodeName.
               DISP lcPOS.
            END.
         END.
            
         APPLY LASTKEY.
      END.
   
      MESSAGE "Do you want to create new timestamp?"
         VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      TITLE " MSISDN " + ttMSISDN.CLI + " "
      SET llOk.

      IF NOT llOk THEN NEXT.

      LEAVE.
   END.

END PROCEDURE.
