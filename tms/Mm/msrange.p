/* ----------------------------------------------------------------------
  MODULE .......: MSRange.P
  TASK .........: UPDATE MSISDN number Ranges
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 15-06-99
  CHANGED ......: 24.06.99 pt ReserveDate & ExpireDate
                  05.10.99 jp urights added
                  06.11.02 jr Eventlog
                  10.03.03 tk tokens
                  11.09.03 jp Brand
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable msrange

{Syst/commali.i} 
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MSRange'}
{Func/msisdn.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CLIFrom  LIKE MSRange.CLIFrom  NO-UNDO.
DEF VAR CustNum LIKE MSRange.CustNum NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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
DEF VAR ava          AS INT                    NO-UNDO.
DEF VAR res          AS INT                    NO-UNDO.
DEF VAR tot          AS INT                    NO-UNDO.
DEF VAR stat-res     AS INT                    NO-UNDO.
DEF VAR stat-ava     AS INT                    NO-UNDO.
DEF VAR MSISDN1      AS c                      NO-UNDO.
DEF VAR MSISDN2      AS c                      NO-UNDO.
DEF VAR exdate       AS DA                     NO-UNDO.
DEF VAR ocPosCode    AS CHAR                   NO-UNDO.

DEF BUFFER xxMSISDN FOR MSISDN.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMSRange AS HANDLE NO-UNDO.
   lhMSRange = BUFFER MSRange:HANDLE.
   RUN StarEventInitialize(lhMSRange).

   DEFINE VARIABLE lhMSISDN AS HANDLE NO-UNDO.
   lhMSISDN = BUFFER MSISDN:HANDLE.
   RUN StarEventInitialize(lhMSISDN).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMSISDN).
   END.
END.

form
    MSRange.Brand 
    MSRange.CLIFrom      /* COLUMN-LABEL FORMAT */
    MSRange.CLITo
    MSRange.CustNum  
    Customer.CustName format "x(15)"
    MSRange.ReserveDate
    MSRange.ExpireDate
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " MSISDN number Ranges "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}


form
    MSRange.CLIFrom     /* LABEL FORMAT */
    MSRange.CLITo
    MSRange.CustNum    /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form
"  Now You can seek and reserve a range of MSISDN numbers " skip
"  for customer:" CustNum 
help "Customer to whom a MSISDN range shall be reserved "
  Customer.CustName 

WITH  OVERLAY ROW 2 WITH centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " Search And Reserve a MSISDN Range "
    NO-LABELS 
    FRAME f3.


form /* seek number Range  BY  CLIFrom */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "MsisdnFrom:" CLIFrom
    HELP "Enter MSISDN number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek number Range  BY CustNum */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "CustomerNo:" CustNum
    HELP "Enter Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By MSISDN  ,By Customer,By 3, By 4".

{Func/tmsparam.i MSStatusRes return}. stat-res = TMSParam.IntVal.
{Func/tmsparam.i MSStatusUnr return}. stat-ava = TMSParam.IntVal.

FIND FIRST MSRange
WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE MSRange THEN ASSIGN
   Memory       = recid(MSRange)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No MSISDN Ranges available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MSRange  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT  WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR 
           MSRange.CLIFrom
           MSRange.CLITo
           MSRange.CustNum
           MSRange.ExpireDate
           WITH FRAME LIS EDITING:
              READKEY.
              nap = keylabel(LASTKEY).
              IF lookup(nap,poisnap) >0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 if frame-field = "CLIFrom" THEN DO:
                    if INPUT FRAME lis MSRange.CLIFrom = "" THEN 
                    UNDO add-row, LEAVE add-row.

                    FIND MSISDN where 
                         MSISDN.CLI = INPUT FRAME lis MSRange.CLIFrom 
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL MSISDN THEN DO:
                       bell.  message "Unknown MSISDN No. !".
                       NEXT.
                    END.

                    /* make sure that no overlapping series */ 
                    FIND 
                    FIRST MSRange where
                          MSRange.CLIFrom <= INPUT FRAME lis MSRange.CLIFrom AND
                          MSRange.CLITo >= INPUT FRAME lis MSRange.CLIFrom 
                    NO-LOCK NO-ERROR.
                    IF AVAIL MSRange THEN DO:
                       MESSAGE
                       "This Range overlaps with other Range" SKIP
                       MSRange.CLIFrom "-" MSRange.CLITo      SKIP
                       "that belongs to Customer" MSRange.CustNum
                       VIEW-AS ALERT-BOX error.
                       NEXT.
                    END.


                 END. 

                 ELSE if frame-field = "CLITo" THEN DO:
                    if INPUT FRAME lis MSRange.CLITo = "" THEN  DO:
                       NEXT-PROMPT MSRange.CLIFrom.
                       NEXT.
                    END.   

                    FIND MSISDN where MSISDN.CLI =
                    INPUT FRAME lis MSRange.CLITo NO-LOCK NO-ERROR.
                    IF NOT AVAIL MSISDN THEN DO:
                       bell.  message "Unknown MSISDN No. !".
                       NEXT.
                    END.

                    /* make sure that no overlapping series */ 
                    FIND 
                    FIRST MSRange where
                          MSRange.CLIFrom <= INPUT FRAME lis MSRange.CLITo AND
                          MSRange.CLITo >= INPUT FRAME lis MSRange.CLITo 
                    NO-LOCK NO-ERROR.
                    IF AVAIL MSRange THEN DO:
                       MESSAGE
                       "This Range overlaps with other Range" SKIP
                       MSRange.CLIFrom "-" MSRange.CLITo      SKIP
                       "that belongs to Customer" MSRange.CustNum
                       VIEW-AS ALERT-BOX error.
                       NEXT.
                    END.
                 END.

                 ELSE if frame-field = "CustNum" THEN DO:
                    FIND Customer WHERE  Customer.CustNum = INPUT FRAME lis
                    MSRange.CustNum NO-LOCK NO-ERROR.
                    IF NOT AVAIL  Customer THEN DO:
                       BELL.
                       MESSAGE "Unknown Customer !".
                       NEXT.
                    END.
                    DISP Customer.CustName WITH FRAME lis.
                 END.   

                 IF INPUT FRAME lis MSRange.CLIFrom ne "" AND
                    INPUT FRAME lis MSRange.CLITo ne "" THEN DO:

                    IF INPUT FRAME lis MSRange.CLITo <
                    INPUT FRAME lis MSRange.CLIFrom THEN DO:

                       message "Invalid order of MSISDN Numbers"
                       VIEW-AS ALERT-BOX error.
                       NEXT-PROMPT MSRange.CLIFrom.
                       NEXT.
                    END.

                    /* are ALL numbers AVAILABLE ? */
                    ava = 0. res = 0. 
                    FOR
                    EACH MSISDN NO-LOCK WHERE
                         MSISDN.CLI >= INPUT FRAME lis MSRange.CLIFrom AND
                         MSISDN.CLI <= INPUT FRAME lis MSRange.CLITo:

                       IF MSISDN.CustNum > 0 THEN res = res + 1.
                                            ELSE ava = ava + 1.
                       tot = tot + 1.
                    END.

                    IF res > 0 THEN DO:
                       MESSAGE 
                       "Of totally" tot "MSISDN numbers" SKIP
                       res "numbers are currently reserved,"      SKIP
                       ava "numbers are available."     skip(1)
                       "number Range can  not be reserved" SKIP
                       "as definied - try with other limits"
                       VIEW-AS ALERT-BOX error.
                       NEXT-PROMPT MSRange.CLITo.
                       NEXT.
                    END.
                END.                
              END.                                 
              APPLY LASTKEY.
           END.  /* EDITING */  

           RUN local-create-MSRange(INPUT FRAME lis MSRange.CustNum,
                                    INPUT FRAME lis MSRange.CLIFrom,
                                    INPUT FRAME lis MSRange.CLITo,
                                    INPUT FRAME lis MSRange.ExpireDate).

           RUN local-UPDATE-record.  

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSRange).
           ASSIGN
           Memory = recid(MSRange)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSRange
      WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSRange THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSRange WHERE recid(MSRange) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSRange THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSRange).
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
        ufk[1]= 209 ufk[2]= 702 
        ufk[3]= (IF lcRight = "RW" THEN 243 ELSE 0)
        ufk[4]= 258 
        ufk[5]= (IF lcRight = "RW" THEN 257 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4   ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSRange.CLIFrom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSRange.CLIFrom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MSRange.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSRange.CustNum WITH FRAME sel.
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
        FIND MSRange WHERE recid(MSRange) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSRange THEN
              ASSIGN FIRSTrow = i Memory = recid(MSRange).
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
           IF NOT AVAILABLE MSRange THEN DO:
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
                rtab[1] = recid(MSRange)
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
           IF NOT AVAILABLE MSRange THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSRange).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSRange WHERE recid(MSRange) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSRange THEN DO:
           Memory = recid(MSRange).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSRange THEN Memory = recid(MSRange).
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
           FIND MSRange WHERE recid(MSRange) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       Disp lcBrand With FRAME f1.
       SET  lcBrand WHEN gcAllBrand = TRUE
            CLIFrom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CLIFrom ENTERED THEN DO:
          FIND FIRST MSRange WHERE 
                     MSRange.CLIFrom >= CLIFrom   AND 
                     msrange.Brand = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       Disp lcBrand With FRAME F2.
       SET  lcBrand WHEN gcAllBrand = TRUE 
            CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST MSRange USE-INDEX CustNum WHERE 
                     MSRange.CustNum >= CustNum  AND 
                     msrange.Brand = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:  
        RUN Mm/msisdnr(rtab[frame-line(sel)]).
        ufkey = TRUE.
        NEXT LOOP.
     END.   

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:  
        /* search & reserve */
         cfc = "lis". 
         RUN Syst/ufcolor.
         ehto = 9. 
         RUN Syst/ufkey. 
         ufkey = TRUE.




        RUN Mm/msrange1( INPUT stat-res,
                      INPUT Customer.CustNum,
                      OUTPUT ok,
                      OUTPUT MSISDN1,
                      OUTPUT MSISDN2,
                      OUTPUT exdate ,
                      OUTPUT ocPosCode).
        IF ok THEN 
        DO:              
           PAUSE 0.
           message "Creating the range and reservations ...".

           RUN local-create-MSRange(Customer.CustNum,
                                    MSISDN1,
                                    MSISDN2,
                                    exdate,
                                    ocPosCode ).
           PAUSE 0.
        END.
        ufkey      = TRUE.
        must-print = TRUE.
        FIND CURRENT MSRange.
/*        NEXT LOOP. */
        APPLY LASTKEY.
     END. /* F3 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND lcRight = "RW" THEN DO:  /* add */
         message "Don't use manual add until further !".     
         PAUSE 2 no-message.
         NEXT.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO :  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       MSRange.CLIFrom MSRange.CustNum .

       RUN local-find-NEXT.
       IF AVAILABLE MSRange THEN Memory = recid(MSRange).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MSRange THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MSRange).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       MSRange.CLIFrom MSRange.CustNum .
       
       IF ok THEN DO :
       /* Checking out IF there is reserved MSISDNs AND which are in
          limits of range */
         i = 0.
         FOR 
         EACH xxMSISDN NO-LOCK WHERE 
              xxMSISDN.Brand      = gcBrand         AND 
              xxMSISDN.CLI       >= MSRange.CLIFrom AND
              xxMSISDN.CLI       <= MSRange.CLITo   AND
              xxMSISDN.CustNum    = MSRange.CustNum AND 
              xxMSISDN.POS        = MSRANGE.Salesman AND 
              xxMSISDN.ValidTo   >= fmakeTS()        AND 
              xxMSISDN.StatusCode = 1                
              BREAK BY xxMSISDN.CLI:

              IF FIRST-OF(xxMSISDN.CLI) THEN
              DO:
              
              fMakeMsidnHistory(RECID(xxMSISDN)).

                ASSIGN
                MSISDN.StatusCode  = 0
                MSISDN.CustNum     = 0
                MSISDN.POS         = ""
                MSISDN.ActionDate  = Today.


                i = i + 1.   
              END.
put screen row 1 string(i).                
         END.

         IF i = 0 THEN DO:
            MESSAGE 
            "There were no unused numbers within this range -"  SKIP
            "No MSISDN Numbers were updated, but the range was deleted."
             view-as alert-box error title " Range was deleted. ".
         END. 
         ELSE 
            MESSAGE
            "Totally" i "MSISDN Numbers were freed"
            VIEW-AS ALERT-BOX INFORMATION.

         IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMSRange).

         DELETE MSRange.
           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST MSRange
           WHERE msrange.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */

     END. /* DELETE */

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSRange) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSRange) must-print = TRUE.
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
      FIND MSRange WHERE recid(MSRange) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MSRange WHERE recid(MSRange) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MSRange
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST MSRange USE-INDEX CustNum
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MSRange
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST MSRange USE-INDEX CustNum
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MSRange
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT MSRange USE-INDEX CustNum
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MSRange
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV MSRange USE-INDEX CustNum
       WHERE msrange.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DEFINE VARIABLE expDate AS DATE NO-UNDO. 
       expDate = MSRange.ExpireDate.
       IF expDate > Date(12,31,2049) THEN
       DO: 
          FIND CURRENT MSRange EXCLUSIVE-LOCK.
          ASSIGN MSRange.ExpireDate = Date(12,31,2049).
       END.
       DISPLAY 
       MSRange.Brand 
       MSRange.CLIFrom MSRange.CLITo
       MSRange.CustNum
       Customer.CustName WHEN AVAIL Customer
       MSRange.ReserveDate FORMAT "99-99-99"
       MSRange.ExpireDate FORMAT "99-99-99"
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND Customer of MSRange NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
         MSRange.CustNum
         Customer.CustName WHEN AVAIL Customer
         MSRange.CLIFrom
         MSRange.CLITo
         MSRange.ReserveDate
         MSRange.ExpireDate
      WITH FRAME lis.
      UPDATE
          MSRange.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

PROCEDURE LOCAL-CREATE-MSRange:

   DEF INPUT PARAMETER CustNum    AS i    NO-UNDO.
   DEF INPUT PARAMETER CLIFrom    AS c    NO-UNDO.
   DEF INPUT PARAMETER CLITo      AS c    NO-UNDO.
   DEF INPUT PARAMETER ExpireDate AS DA   FORMAT "99/99/99" NO-UNDO.
   DEF INPUT PARAMETER ocPosCode  AS CHAR NO-UNDO.

   CREATE MSRange.
   ASSIGN
      MSRange.Brand       = lcBrand 
      MSRange.CLIFrom     = CLIFrom
      MSRange.CLITo       = CLITo
      MSRange.CustNum     = CustNum
      MSRange.ReserveDate = pvm
      MSRange.ExpireDate  = ExpireDate
      MSRange.SalesMan    = ocPosCode
      Memory              = recid(MSRange).

   /* mark EACH MSISDN No. within this Range AS reserved */   
   tot = 0.

   FOR
   EACH xxMSISDN NO-LOCK WHERE
        xxMSISDN.CLI   >= MSRange.CLIFrom AND
        xxMSISDN.CLI   <= MSRange.CLITo   AND 
        xxMSISDN.Brand  = lcBrand         AND 
        xxMSISDN.StatusCode = 1          AND 
        xxMsisdn.pos    = ocPosCode:

        tot = tot + 1.

   END.    

   MESSAGE 
   "Totally" tot "MSISDN Numbers were "
   "reserved for customer" MSRange.CustNum
   VIEW-AS ALERT-BOX information.

END PROCEDURE.

