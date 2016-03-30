/* ----------------------------------------------------------------------
  MODULE .......: SIM.P
  TASK .........: UPDATE SIM data
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 31-05-99
  CHANGED ......: 20.07.99 pt SimBatch
                  07.10.99 jp urights added
                  11.11.99 kl DELETE actions, seek FOR KooAa
                  14.10.02 jr Removed BillLevel
                  04.11.02 jr Eventlog 
                  10.03.03 tk tokens
                  27.03.03 jp no find rules
                  12.05.03 jp&ms update custnum
                  22.08.03 jp remove(f3) find sim art code 
                              add         find imsi no  
                  29.08.03 jp Brand handling
                  16.02.05 aam use Brand with MSISDN
                  03.08.05 mvi Added input parameter iiBatch
                               Changed browsing to 
                               All batches / Only specified batch
                  16.03.07 kl custnum from mobsub. F keys commented

  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Sim

{Syst/commali.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Sim'}

/** Call with input parameter 0 to browse all simbatches other 
 * values limit brosing to the specified batch.
 */

DEFINE INPUT PARAMETER iiBatch AS INTEGER  NO-UNDO.

DEF VAR llUseBatch AS LOGICAL INITIAL TRUE NO-UNDO.

IF iiBatch EQ 0 THEN llUseBatch = false.

gcallbrand = true.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR imsi     LIKE imsi.imsi     NO-UNDO.
DEF VAR ICC      LIKE SIM.ICC       NO-UNDO.
DEF VAR CustNum  LIKE SIM.CustNum   NO-UNDO.
DEF VAR SimArt   LIKE SimArt.SimArt NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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

DEF VAR sel-hdr      AS c                      NO-UNDO.
DEF VAR pr-custno    AS i                      NO-UNDO.
DEF VAR new_sim      AS LOG                    NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSim AS HANDLE NO-UNDO.
   lhSim = BUFFER Sim:HANDLE.
   RUN StarEventInitialize(lhSim).

   DEFINE VARIABLE lhIMSI AS HANDLE NO-UNDO.
   lhIMSI = BUFFER IMSI:HANDLE.
   RUN StarEventInitialize(lhIMSI).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSim).
   END.
END.

form
    SIM.ICC column-label "ICC" format "x(20)"                           
    MobSub.CLI column-label "Assigned to" format "x(9)"     
    SIM.Stock format "x(11)" column-label "Stock"
    SIM.SIMStat      format "z9" column-label "Status"
    SIMStat.SSName  format "x(26)" column-label "Status name"
WITH width 80 OVERLAY FrmDown DOWN ROW FrmRow
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)  
     " " + ynimi +
     " Sim Cards " + string(pvm,"99-99-99") + " "
     FRAME sel.

{Func/brand.i}
form
    SIM.ICC     label "Serial No ......."          SKIP
    SIM.SimBatch  label "Batch  No. ......"          SKIP
    SimBatch.DelDate
                label "Received ........"          skip(1)
    SIM.Stock format "x(12)" label "In Stock ........"
    Stock.StoName   no-label at 33 format "x(30)"  SKIP
    SIM.CustNum  label "Customer ........"    
    Customer.CustName no-label at 33  format "x(30)" SKIP
    SIM.SimArt label "Article Code ...."
    SimArt.SAName no-label at 33 format "x(30)"   SKIP
    SIM.ManCode label "Manufact ........"
    SimMan.ManName  no-label at 33  format "x(30)" SKIP
    SIM.ICC     label "ICC ............."          SKIP
    SIM.SIMStat label "Status .........."
    SIMStat.SSName no-label at 33  format "x(30)" skip(1)

 WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels     FRAME lis.

form /* seek SIM card  BY  ICC */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
    "Unknown brand")           SKIP

    "ICC Number:" icc
    help "Enter ID"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND ICC-ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek SIM card  BY CustNum */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
    "Unknown brand")      SKIP
    "CustNumber:" CustNum
    help "Enter Customer No."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek SIM card  BY SimArt */

    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
    "Unknown brand")           SKIP



    "Imsi Number:"     IMSI
    help "Enter IMSI No."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND IMSI "
    COLOR VALUE(cfc) 

    NO-LABELS OVERLAY FRAME f3.


DEFINE BUFFER ImsiCustomer FOR Customer.

form 
    IMSI.IMSI    COLON 20   SKIP
    IMSI.CustNum COLON 20  SKIP
    ImsiCustomer.CustName COLON 20 SKIP
    BillTarg.BillTarget COLON 20 SKIP
    IMSI.PIN1  COLON 20 SKIP
    IMSI.PIN2  COLON 20 SKIP
    IMSI.PUK1  COLON 20 SKIP
    IMSI.PUK2  COLON 20 
    WITH SIDE-LABELS 1 DOWN 
    TITLE COLOR VALUE(ctc) " IMSI number on SIM(ICC) " + ICC + " "
    OVERLAY ROW 2
    CENTERED FRAME imsi.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel. 

orders = "By ICC,By CustNo,By ArtCode, By 4".

FIND FIRST SIM  WHERE 
SIM.Brand = lcBrand  AND
(IF iiBatch EQ 0 THEN true ELSE sim.simbatch = iiBatch)
 NO-LOCK NO-ERROR.
IF AVAILABLE SIM THEN ASSIGN
   Memory       = recid(SIM)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No SIMs available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a SIM  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           PROMPT-FOR SIM.ICC
           VALIDATE
              (SIM.ICC = "" OR
              NOT CAN-FIND(SIM using  SIM.ICC),
              "SIM card " + string(INPUT SIM.ICC) +
              " already exists !").
           IF input SIM.ICC = "" THEN LEAVE ADD-ROW.
           CREATE SIM.
           ASSIGN
           SIM.Brand = lcBrand
           SIM.ICC   = INPUT FRAME lis SIM.ICC.

           ASSIGN pr-custno = ? .
           new_sim = TRUE.
           RUN local-UPDATE-record.

           if lookup(keyfunction(lastkey),"endkey,end-error") > 0 
           THEN UNDO, LEAVE.
           ASSIGN
           Memory = recid(SIM)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SIM WHERE 
      SIM.Brand = lcBrand AND
      (IF iiBatch EQ 0 THEN true ELSE sim.simbatch = iiBatch)
 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SIM THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND SIM WHERE recid(SIM) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SIM THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SIM).
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
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
      ufk = 0. ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      ufk = 0. 

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 206
        ufk[4]= 9808
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.          
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SIM.ICC {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SIM.ICC WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.
      
      nap = keylabel(LASTKEY).
      
      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND SIM WHERE recid(SIM) = Memory NO-LOCK.
        DO i = 1 TO fRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE SIM THEN
              ASSIGN FIRSTrow = i Memory = recid(SIM).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.
      
      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND SIM WHERE recid(SIM) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE SIM THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(SIM)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SIM WHERE recid(SIM) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE SIM THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(SIM).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SIM WHERE recid(SIM) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
           IF AVAILABLE SIM THEN DO:
           Memory = recid(SIM).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE SIM THEN Memory = recid(SIM).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
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
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND SIM WHERE recid(SIM) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ICC = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
      
       DISP lcBrand WITH FRAME F1. pause 0.
       UPDATE ICC WITH FRAME f1. 

       HIDE FRAME f1 NO-PAUSE.
       IF ICC <> "" THEN DO:
          If lcbrand ne "*" THEN DO:

            IF iiBatch = 0 THEN DO:
               FIND FIRST sim NO-LOCK WHERE
                          sim.brand = lcbrand AND
                          sim.icc   >= icc
                          NO-ERROR.
            END.
            ELSE DO:
               FIND FIRST sim NO-LOCK WHERE
                          sim.brand = lcBrand AND
                          sim.simbatch = iiBatch AND
                          sim.icc >= icc
                          NO-ERROR.
            END.
          END.               
          ELSE DO:
            FIND FIRST SIM  WHERE
                       SIM.ICC   >= ICC
            NO-LOCK NO-ERROR.                 
          END.
          
          IF NOT fRecFound(1) THEN NEXT .

          NEXT LOOP.
       END. /* icc <> "" */
     
     END. /* Search-1 */
     
     ELSE if LOOKUP(nap,"4,f4") > 0 THEN DO:
        FIND SIM where recid(SIM) = rtab[FRAME-LINE].
        ufkey = TRUE.
        FIND FIRST IMSI WHERE IMSI.ICC = SIM.ICC NO-LOCK NO-ERROR.
        IF AVAIL IMSI THEN
        DO:
            FIND ImsiCustomer WHERE ImsiCustomer.CustNum =  IMSI.CustNum NO-LOCK NO-ERROR. 

            FIND MobSub WHERE 
                 mobsub.brand = gcBrand AND
                 MobSub.IMSI = IMSI.IMSI NO-LOCK NO-ERROR. 
            FIND BillTarget WHERE
                 BillTarget.CustNum = ImsiCustomer.CustNum AND
                 BillTarget.BillTarget = Mobsub.BillTarget
                 NO-LOCK NO-ERROR.    
            PAUSE 0.
            DISPLAY IMSI.IMSI IMSI.CustNum IMSI.PIN1 IMSI.PIN2 IMSI.PUK1 IMSI.PUK2
              WITH FRAME imsi.
            PAUSE 0.
            IF AVAILABLE ImsiCustomer THEN
               DISPLAY ImsiCustomer.CustName WITH FRAME imsi.
            PAUSE 0.
            IF AVAILABLE BillTarg THEN
               DISPLAY BillTarg.BillTarget WITH FRAME imsi.          
            VIEW FRAME imsi.
            PAUSE 0.
            ehto = 1.
            ufk = 0.
            ufk[8] = 8.
            RUN Syst/ufkey.
            ufkey = TRUE.
        END.
        ELSE
           MESSAGE "No IMSI available!" VIEW-AS ALERT-BOX
              BUTTONS OK.
         
        HIDE FRAME imsi.

        NEXT LOOP.
     END.   
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION:
       
       {Syst/uright2.i}
       /* change */
       FIND SIM WHERE recid(SIM) = rtab[FRAME-line(sel)] NO-LOCK.
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       CLEAR FRAME  lis no-pause.
       DISPLAY SIM.ICC WITH FRAME lis.
       ASSIGN 
        pr-custno = SIM.CustNum
        new_sim   = FALSE.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
       RUN local-UPDATE-record.
       HIDE FRAME lis NO-PAUSE.

       IF LOOKUP(KEYFUNCTION(LASTKEY),"END-ERROR,ENDKEY") > 0 OR
       KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.

       /* IF customer number were changed ... */
       IF SIM.CustNum NE pr-custno THEN DO:

          FOR EACH IMSI WHERE
                   IMSI.ICC = SIM.ICC:

             ASSIGN
             IMSI.CustNum  = SIM.CustNum.
          END.
          MESSAGE 
          "New Customer No." SKIP
          "were UPDATEd onto IMSI and MSISDN"     SKIP
          "records also."
          VIEW-AS ALERT-BOX TITLE " CUSTOMER DATA CHANGED ".
       END.
       RUN local-disp-row.
       xrecid = recid(SIM).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SIM) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SIM) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SIM USE-INDEX simbatch  WHERE 
            SIM.Brand = lcBrand AND
            (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND FIRST SIM USE-INDEX SimBatch
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 3 THEN FIND FIRST SIM USE-INDEX SimBatch  
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SIM USE-INDEX simbatch
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SIM USE-INDEX SimBatch  
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 3 THEN FIND LAST SIM USE-INDEX Simbatch 
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SIM  USE-INDEX simbatch
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND NEXT SIM USE-INDEX CustNum 
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT SIM USE-INDEX SimBatch
       WHERE  
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev SIM    USE-INDEX simbatch
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND prev SIM USE-INDEX CustNum 
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

       ELSE IF order = 3 THEN FIND prev SIM USE-INDEX Simbatch  
       WHERE 
       SIM.Brand = lcBrand AND
       (IF iiBatch = 0 THEN true ELSE sim.simbatch = iiBatch)

       NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   FIND SIMStat where SIMStat.SIMStat = SIM.SIMStat no-lock no-error.

   DISPLAY
   SIM.ICC
   Mobsub.CLI WHEN AVAIL Mobsub
   "NotInUse" WHEN NOT AVAIL MobSub @ Mobsub.CLI
   SIM.Stock       
   SIM.SIMStat
   SIMStat.SSName when  AVAIL SIMStat
   "!! Unknown !!" when NOT AVAIL SIMStat @  SIMStat.SSName
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
       
   FIND SimBatch WHERE 
        SimBatch.Brand = gcBrand AND
        SimBatch.SimBatch = SIM.SimBatch NO-LOCK NO-ERROR.
   
   FIND FIRST MobSub WHERE MobSub.MsSeq = SIM.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN
      FIND FIRST Customer WHERE
         Customer.CustNum = MobSub.CustNum
      NO-LOCK NO-ERROR.
   ELSE RELEASE Customer.  
   
   FIND SIMStat where SIMStat.SIMStat = SIM.SIMStat no-lock no-error.
   FIND SimMan of SIM WHERE SimMan.Brand = gcBrand no-lock no-error.
   FIND SimArt WHERE 
        SimArt.Brand = gcBrand AND
        SimArt.SimArt = SIM.SimArt no-lock no-error.
   FIND Stock of SIM NO-LOCK NO-ERROR.

END PROCEDURE.


PROCEDURE local-UPDATE-record:

   RUN local-find-others.

   DISPLAY 
   SIM.SimBatch
   SimBatch.DelDate when AVAIL SimBatch  
   SIM.ManCode 
   SIM.SimArt
   SimArt.SAName   when AVAIL SimArt
   SimMan.ManName   when AVAIL SimMan
   Customer.CustName  when AVAIL Customer
   SIMStat.SSName  when AVAIL SimMan 
   SIM.Stock
   Stock.StoName    when AVAIL Stock
   SIM.CustNum
   Customer.CustNum WHEN AVAIL Customer @ SIM.CustNum
   SIM.SimStat
   WITH FRAME lis.

   IF lcRight = "RW" THEN DO:

REPEAT WITH FRAME LIS:

   DEFINE BUFFER bStoBal   FOR StoBal.

   PROMPT
       SIM.SimArt   WHEN NEW SIM
       SIM.ManCode  WHEN NEW SIM
       SIM.Stock WHEN SIM.SIMStat EQ {&SIM_SIMSTAT_AVAILABLE}
       SIM.SIMStat
   WITH FRAME lis EDITING:
      READKEY.

      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME LIS:
         PAUSE 0.
         IF FRAME-FIELD = "CustNum" THEN 
         DO:
            IF INPUT FRAME lis SIM.CustNum NE 0 THEN 
            DO:
               FIND Customer WHERE Customer.CustNum =
                                   INPUT FRAME lis SIM.CustNum 
               NO-LOCK NO-ERROR.
               IF NOT AVAIL Customer THEN 
               DO:
                  BELL.
                  MESSAGE "Unknown Customer !".
                  NEXT.
               END.
               DISP Customer.CustName.

            END.
            ELSE DISP "" @  Customer.CustName.
         END.

         ELSE IF FRAME-FIELD = "ManCode" THEN 
         DO:
            /* ASSIGN manufacture's code from SimMan record */
            FIND SimMan where 
                 SimMan.Brand = gcBrand AND
                 SimMan.Mancode = INPUT FRAME lis SIM.ManCode NO-LOCK NO-ERROR.
            IF NOT AVAIL SimMan THEN 
            DO:
               BELL.
               MESSAGE "Unknown Manufacturer Code !".
               NEXT.
            END.
            SIM.ManCode = SimMan.ManCode.
            DISP SIM.ManCode SimMan.ManName WITH FRAME lis.
         END.

         ELSE IF FRAME-FIELD = "SimArt" THEN 
         DO:
            FIND SimArt WHERE 
                 SimArt.Brand = gcBrand AND 
                 SimArt.SimArt =
                              INPUT FRAME lis SIM.SimArt 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL SimArt THEN 
            DO:
               BELL.
               MESSAGE "Unknown Article Code !".
               NEXT.
            END.

            DISP SimArt.SAName WITH FRAME lis.
         END.

         ELSE IF FRAME-FIELD = "SIMStat" THEN 
         DO:
            FIND SIMStat WHERE SIMStat.SIMStat =
                               INPUT FRAME lis SIM.SIMStat 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL SIMStat THEN 
            DO:
               BELL.
               MESSAGE "Unknown Status !".
               NEXT.
            END.
            DISP SIMStat.SSName WITH FRAME lis.
         END.
         
         ELSE IF FRAME-FIELD = "Stock" THEN 
         DO:
            FIND Stock WHERE
                 Stock.Brand = SIM.Brand AND
                 Stock.Stock = INPUT SIM.Stock NO-LOCK NO-ERROR.

            IF NOT AVAIL Stock THEN 
            DO:
               BELL.
               MESSAGE "Unknown Stock !".
               NEXT.
            END.
            DISP Stock.StoName WITH FRAME lis.

         END.
      END.
      APPLY LASTKEY.
   END. /* EDITING */
      
   FIND CURRENT SIM NO-LOCK.
   
   IF CURRENT-CHANGED SIM THEN DO:
      
      MESSAGE 
         "This record has been changed elsewhere while updating" 
      VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
         
      RETURN.

   END. 
   ELSE DO: 
   
      FIND CURRENT SIM EXCLUSIVE-LOCK.
   
      /* Adjustment the Stock Balance if stock is changed */
      IF SIM.Stock > "" AND SIM.Stock <> INPUT SIM.Stock THEN DO:
         FIND FIRST StoBal WHERE
                    StoBal.Brand  = SIM.Brand AND
                    StoBal.StoBal = SIM.Stock AND
                    StoBal.SimArt = SIM.SimArt
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE StoBal THEN DO:
            ASSIGN StoBal.Balance   = (StoBal.Balance - 1)
                   StoBal.DetBal[1] = (StoBal.DetBal[1] - 1).
            RELEASE Stobal.
         END.

         FIND FIRST bStoBal WHERE
                    bStoBal.Brand  = SIM.Brand AND
                    bStoBal.StoBal = INPUT SIM.Stock AND
                    bStoBal.SimArt = SIM.SimArt
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE bStoBal THEN DO:
            ASSIGN bStoBal.Balance   = (bStoBal.Balance + 1)
                   bStoBal.DetBal[1] = (bStoBal.DetBal[1] + 1).
            RELEASE bStoBal.
         END.
      END. /* IF SIM.Stock > "" AND SIM.Stock <> INPUT SIM.Stock THEN DO: */
      
      IF new_sim AND
      llDoEvent THEN RUN StarEventMakeCreateEvent(lhSIM).
      
      ASSIGN
          SIM.SimArt WHEN NEW SIM
          SIM.ManCode WHEN NEW SIM
          SIM.Stock WHEN SIM.SIMStat EQ {&SIM_SIMSTAT_AVAILABLE}
          SIM.SIMStat.
   
      IF NOT new_sim AND
      llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).

   END.

   FIND CURRENT SIM NO-LOCK.

   new_sim = FALSE.
   LEAVE.
END. /* repeat */
END.
ELSE PAUSE.


END PROCEDURE.

