/* ----------------------------------------------------------------------
  MODULE .......: EDRHistory_one_edr
  TASK .........: browse EDRHistory of one ticket
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'EDRHistory'}
{Func/timestamp.i}
{Func/callquery.i}
{Syst/eventval.i}

DEF INPUT PARAMETER icCLI     AS CHAR NO-UNDO.
DEF INPUT PARAMETER idaDateSt AS DATE NO-UNDO.
DEF INPUT PARAMETER iiTimeSt  AS INT  NO-UNDO.
DEF INPUT PARAMETER iiDtlSeq  AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 6.
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

DEF VAR lcTime       AS CHAR NO-UNDO.
DEF VAR lcUpdateTime AS CHAR NO-UNDO. 
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR lcBIName     AS CHAR NO-UNDO. 
DEF VAR lcGSMBnr     AS CHAR NO-UNDO. 
DEF VAR lcBDestName  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttHistory NO-UNDO
    LIKE EDRHistory
    FIELD Rated AS CHAR.
    
DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
    FIELD CDRTable AS CHAR.
    
       
FORM
    icCLI     COLON 10 LABEL "MSISDN" FORMAT "X(12)" SKIP
    idaDateSt COLON 10 LABEL "Time"  FORMAT "99-99-99" 
        lcTime NO-LABEL FORMAT "X(8)" 
    lcGSMBnr  COLON 10 LABEL "B-Number" FORMAT "X(20)"     
WITH ROW 1 WIDTH 80 SIDE-LABELS OVERLAY TITLE " EDR History " FRAME fHead.
    
FORM
    ttHistory.BillCode 
    ttHistory.BDest   FORMAT "X(16)"
    ttHistory.DCEvent FORMAT "X(16)"
    ttHistory.ErrorCode 
    ttHistory.Amount  FORMAT ">>>>9.999"
    ttHistory.Rated  FORMAT "X(8)" COLUMN-LABEL "Changed" 
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " Changes "
    FRAME sel.

form
    ttHistory.Brand       COLON 20
    ttHistory.InvCust     COLON 20 
       lcCustName NO-LABEL FORMAT "X(40)" SKIP
    ttHistory.MsSeq       COLON 20 
    ttHistory.CLI         COLON 20 SKIP(1)
    ttHistory.DateSt      COLON 20 
       lcTime NO-LABEL FORMAT "X(8)" SKIP
    ttHistory.DtlSeq      COLON 20 FORMAT ">>>>>>>>>>9"  SKIP(1)
    ttHistory.BillCode    COLON 20
       lcBIName NO-LABEL FORMAT "X(30)" SKIP
    ttHistory.BDest       COLON 20   
       lcBDestName NO-LABEL FORMAT "X(30)" SKIP
    ttHistory.DCEvent     COLON 20
    ttHistory.TariffNum   COLON 20
    ttHistory.Amount      COLON 20 FORMAT ">>>>9.9999"
    ttHistory.ErrorCode   COLON 20 SKIP(1)
    ttHistory.UpdateDate  COLON 20
      lcUpdateTime NO-LABEL FORMAT "X(8)" SKIP
    ttHistory.UpdateSource COLON 20  
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


RUN pInitHistory. 
   
lcTime = STRING(iiTimeSt,"hh:mm:ss").
PAUSE 0.
DISP icCLI idaDateSt lcTime lcGSMBNr WITH FRAME fHead.
cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE ttHistory THEN ASSIGN
   Memory       = recid(ttHistory)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No history available" VIEW-AS ALERT-BOX.
      RETURN.   
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
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
        FIND ttHistory WHERE recid(ttHistory) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttHistory THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttHistory).
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
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttHistory.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttHistory.BillCode WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND ttHistory WHERE recid(ttHistory) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttHistory THEN
              ASSIGN FIRSTrow = i Memory = recid(ttHistory).
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
           IF NOT AVAILABLE ttHistory THEN DO:
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
                rtab[1] = recid(ttHistory)
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
           IF NOT AVAILABLE ttHistory THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttHistory).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttHistory WHERE recid(ttHistory) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttHistory THEN DO:
           Memory = recid(ttHistory).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttHistory THEN Memory = recid(ttHistory).
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
           FIND ttHistory WHERE recid(ttHistory) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttHistory.InvCust.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttHistory).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttHistory) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttHistory) must-print = TRUE.
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
      FIND ttHistory WHERE recid(ttHistory) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttHistory WHERE recid(ttHistory) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:
      FIND FIRST ttHistory NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:
      FIND LAST ttHistory  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN DO:
      FIND NEXT ttHistory NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN DO:
      FIND PREV ttHistory NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttHistory.BillCode
       ttHistory.BDest
       ttHistory.DCEvent
       ttHistory.ErrorCode
       ttHistory.Amount
       ttHistory.Rated
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   lcTime = STRING(ttHistory.TimeStart,"hh:mm:ss").
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      IF ttHistory.UpdateTime = 0 THEN lcUpdateTime = "".
      ELSE lcUpdateTime = STRING(ttHistory.UpdateTime,"hh:mm:ss").
         
      FIND FIRST Customer WHERE Customer.CustNum = ttHistory.InvCust 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
          lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                        BUFFER Customer).
      ELSE lcCustName = "".
      
      FIND FIRST BillItem WHERE
          BillItem.Brand = gcBrand AND
          BillItem.BillCode = ttHistory.BillCode NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
      ELSE lcBIName = "".

      FIND FIRST BDest WHERE
          BDest.Brand = gcBrand AND
          BDest.BDest = ttHistory.BDest NO-LOCK NO-ERROR.
      IF AVAILABLE BDest THEN lcBDestName = BDest.BDName.
      ELSE lcBDestName = "".
       
      DISP 
           ttHistory.Brand
           ttHistory.InvCust
           lcCustName
           ttHistory.MsSeq
           ttHistory.CLI
           ttHistory.DateSt
           lcTime
           ttHistory.DtlSeq
           ttHistory.BillCode
           lcBIName
           ttHistory.BDest
           lcBDestName
           ttHistory.DCEvent
           ttHistory.TariffNum
           ttHistory.Amount
           ttHistory.ErrorCode
           ttHistory.UpdateDate
           lcUpdateTime
           ttHistory.UpdateSource
      WITH FRAME lis.

      ASSIGN 
         ehto = 0
         ufk  = 0
         ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

PROCEDURE pInitHistory:

   DEF VAR tthCDR AS HANDLE NO-UNDO.
   DEF VAR liErrorCodeOut AS INT  NO-UNDO. 
   
   EMPTY TEMP-TABLE ttHistory.

   FOR FIRST MobCDR NO-LOCK WHERE
             MobCDR.CLI = icCLI AND
             MobCDR.DateSt = idaDateSt AND
             MobCDR.TimeSt = iiTimeSt AND
             MobCDR.DtlSeq = iiDtlSeq:
      CREATE ttHistory.
      BUFFER-COPY MobCDR TO ttHistory.
      ASSIGN 
         ttHistory.Brand = gcBrand
         ttHistory.Rated = "Current"
         ttHistory.UpdateSource = "Current"
         lcGSMBnr = MobCDR.GSMBnr.
   END.

   /* check from old dbs if not in the latest */
   IF NOT CAN-FIND(FIRST ttHistory) THEN DO:
      tthCDR = TEMP-TABLE ttCall:HANDLE.
      EMPTY TEMP-TABLE ttCall.
     
      fMobCDRCollect(INPUT "post",
                     INPUT gcBrand,
                     INPUT katun,
                     INPUT idaDateSt,
                     INPUT idaDateSt,
                     INPUT 0,
                     INPUT "inv",
                     INPUT icCLI,
                     INPUT 0,
                     INPUT 0,
                     INPUT "",
                     INPUT "",
                     INPUT "",
                     INPUT 0,
                     INPUT-OUTPUT liErrorCodeOut,
                     INPUT-OUTPUT tthCDR).
      
      FOR FIRST ttCall WHERE
                ttCall.CLI = icCLI AND
                ttCall.DateSt = idaDateSt AND
                ttCall.TimeSt = iiTimeSt AND
                ttCall.DtlSeq = iiDtlSeq:
         CREATE ttHistory.
         BUFFER-COPY ttCall TO ttHistory.
         ASSIGN 
            ttHistory.Rated = "Current"
            ttHistory.UpdateSource = "Current"
            lcGSMBnr = ttCall.GSMBnr.
      END.

      EMPTY TEMP-TABLE ttCall.
      DELETE OBJECT tthCDR NO-ERROR.
   END.
   
   FOR EACH EDRHistory NO-LOCK WHERE
            EDRHistory.Brand  = gcBrand AND
            EDRHistory.CLI    = icCLI AND
            EDRHistory.DateSt = idaDateSt AND
            EDRHistory.TimeSt = iiTimeSt AND
            EDRHistory.DtlSeq = iiDtlSeq
   BY EDRHistory.UpdateDate DESC
   BY EDRHistory.UpdateTime DESC:
      CREATE ttHistory.
      BUFFER-COPY EDRHistory TO ttHistory.
      ttHistory.Rated = STRING(EDRHistory.UpdateDate,"99-99-99").
   END.
   
END PROCEDURE.

