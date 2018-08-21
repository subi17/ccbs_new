/* ----------------------------------------------------------------------------
  MODULE .......: TARIFF.P
  FUNCTION .....: Update tariffs
  APPLICATION ..: TMS
  AUTHOR .......: PT
  CREATED ......: 21-01-96
  MODIFIED .....: VAR Currency FOR displaying Currency codes added
                  16.05.02/tk  event logging added
                  01.08.02 lp IF order = 1 THEN ... USE-INDEX PriceList
                              (not PriceList)
                  05.09.02/aam DiscPerc and Discount[1,2] removed
                  17.09.02/aam CustNum removed,
                               TZName added,
                               use this same routine for all callers
                  23.09.02/aam use CCN instead of BDest,
                               BillCode,
                               input parameter iiCCN
                  10.03.03 tk  tokens             
                  19.03.03/aam BDest and CustNum into use again
                  26.03.03/aam Min-price as default 
                  04.07.03 kl  CurrUnit from PriceList, irRowID parameter
                  09.09.03/aam brand
                  20.11.03/jp  def values for datatype and startfee when new
                  31.05.04/aam ServRid & MPMRid
                  02.12.04/aam 4 decimals to StartCharge
                  05.04.05/aam don't allow pricelist with CustNum > 0
  Version ......: M15
--------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Tariff

{Syst/commali.i}
{Syst/eventval.i}
{Func/tariffd.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Tariff'}
{Func/cparam2.i}
{Func/fcustpl.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTariff AS HANDLE NO-UNDO.
   lhTariff = BUFFER Tariff:HANDLE.
   RUN StarEventInitialize ( lhTariff ).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhTariff).
   END.

END.

DEF INPUT PARAMETER iiTType AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCCN   AS INT  NO-UNDO.
DEF INPUT PARAMETER icPlist AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiCust  AS INT  NO-UNDO. 
DEF INPUT PARAMETER icBDest AS CHAR NO-UNDO.
DEF INPUT PARAMETER irRowId AS INT  NO-UNDO.

DEF BUFFER xxtariff FOR Tariff.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcCSeek    LIKE Tariff.CCN          NO-UNDO.
DEF VAR plseek     LIKE PriceList.PriceList NO-UNDO.
DEF VAR liCustSeek LIKE Tariff.CustNum      NO-UNDO. 

DEF VAR firstline  AS INT                NO-UNDO.
DEF VAR order      AS INT                NO-UNDO.
DEF VAR ex-order   AS INT                NO-UNDO.
DEF VAR maxOrder   AS INT                NO-UNDO  init 2.
DEF VAR memory     AS RECID              NO-UNDO.
def var line       as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG                NO-UNDO.
DEF VAR must-add   AS LOG                NO-UNDO.
DEF VAR ufkey      AS LOG                NO-UNDO.
DEF VAR fr-header  AS CHAR               NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24    NO-UNDO.
DEF VAR i          AS INT                NO-UNDO.
DEF VAR j          AS INT                NO-UNDO.

DEF VAR lcCName    LIKE CCN.CCNName          NO-UNDO.
DEF VAR plname     LIKE PriceList.PLName     NO-UNDO.
DEF VAR xCCN       LIKE Tariff.CCN           NO-UNDO.
DEF VAR xPriceList LIKE PriceList.PriceList  NO-UNDO.

DEF VAR xrecid     AS RECID              NO-UNDO.
DEF VAR luku       AS DE                 NO-UNDO.
def var ok         as lo format "Yes/No" NO-UNDO.
DEF VAR dx         AS DE                 NO-UNDO.
def var Currency   as c  format "x(8)"   NO-UNDO EXTENT 6.
DEF VAR lcBDest    AS CHAR               NO-UNDO. 
DEF VAR llBDest    AS LOG                NO-UNDO.
DEF VAR lcTmp      AS CHAR               NO-UNDO.
DEF VAR lBrHdr     AS CHAR               NO-UNDO.
DEF VAR lcDataType AS CHAR               NO-UNDO.
DEF VAR llShowHistory AS LOG NO-UNDO INIT FALSE. 

lBrHdr = " TARIFF MAINTENANCE " + string(TODAY,"99-99-99") + " " .

form
   Tariff.Brand       column-label "Bra"      format "x(3)"
   Tariff.ValidFrom   column-label "From"     format "999999"
   Tariff.ValidTo     column-label "To"       format "999999"
   Tariff.PriceList   column-label "PL/BNr"   format "x(10)"
   Tariff.CCN         column-label "CCN"      format ">>>9"
   lcCName            column-label "Name"     format "x(9)"
   Tariff.BDest     column-label "BDest" format "X(12)"
   Tariff.Price[1]    column-label "Price1"   format ">>9.9999"
   Tariff.StartCharge[1]    column-label "StFee"   format ">>9.9999"
   Tariff.Discount[4] column-label "S"        format "F/S"
   lcBdest            column-label "B"        format "x"
WITH 
   width 80 OVERLAY scroll 1 15 DOWN ROW 1
   COLOR value(Syst.Var:cfc)
   title color value(Syst.Var:ctc) " " + Syst.Var:ynimi + lBrHdr
FRAME sel.


form /* ADD */
   "Rate ID.....:" Tariff.TariffNum FORMAT ">>>>>9"
      help "Tarif ID number" SKIP
   "CCN ........:" Tariff.CCN FORMAT ">>>9"
      help "Call case number"
      CCN.CCNName  format "x(30)" AT 32 SKIP
   "BDestination:" Tariff.BDest FORMAT "x(28)"
      help "B-Destination"
      BDest.BDName format "x(26)" AT 43 SKIP
   "Pricelist ..:" Tariff.PriceList FORMAT "X(16)" 
      help "Pricelist code"
      PriceList.PLName   format "x(22)" AT 32 SKIP
   "--------------------------------------------------------------------"
   SKIP
   "Valid during ..:" 
   Tariff.ValidFrom format "99-99-9999"
      validate(input Tariff.ValidFrom NE ?, 
               "Date is mandatory")
   "-" 
   Tariff.ValidTo   format "99-99-9999"
      validate(input Tariff.ValidTo >= input Tariff.ValidFrom AND
               input Tariff.ValidTo NE ?,
               "Check the dates; price would never become valid using these")
   "Minute/Second .:" AT 48 Tariff.RateType 
      VALIDATE(INPUT Tariff.RateType > 0,"Minimum is 1") 
   SKIP         
   "Price unit ....:" Tariff.DataType
      help "Rate data type" 
      lcDataType NO-LABEL FORMAT "X(20)"
   "Start / Min.Sec:" AT 48 Tariff.Discount[4] format "Fee/Sec"
      help "Starting fee or minimum charging seconds (Fee/Sec)"    
   SKIP
   "Billing Item ..:" Tariff.BillCode FORMAT "X(17)" 
      BillItem.BIName FORMAT "X(11)" NO-LABEL
   "Currency unit .:" AT 48 Tariff.CurrUnit
   SKIP
   "Reporting IDs .:" 
      Tariff.MPMRid  HELP "Call reporting ID" 
      Tariff.ServRid HELP "Service reporting ID" NO-LABEL 
   SKIP
   "Name     T  From - To      Price" AT 15
   "Starting fees" TO 67              
   SKIP

   " Time zone 1:"
   Tariff.TZName[1] FORMAT "X(8)"
   Tariff.DayType[1]
   Tariff.TZFrom[1] "-" Tariff.TZTo[1] Tariff.Price[1] currency[1]
   Tariff.StartCharge[1] AT 60 
     FORMAT "zzz9.9999" SPACE(1) SKIP

   " Time zone 2:"
   Tariff.TZName[2] FORMAT "X(8)"
   Tariff.DayType[2]
   Tariff.TZFrom[2] "-" Tariff.TZTo[2] Tariff.price[2] currency[2]
   Tariff.StartCharge[2] AT 60 
     FORMAT "zzz9.9999" SKIP

   " Time zone 3:"
   Tariff.TZName[3] FORMAT "X(8)"
   Tariff.DayType[3]
   Tariff.TZFrom[3] "-" Tariff.TZTo[3] Tariff.Price[3] currency[3]

   Tariff.StartCharge[3] AT 60 
      FORMAT "zzz9.9999" SKIP

   " Time zone 4:"
   Tariff.TZName[4] FORMAT "X(8)"
   Tariff.DayType[4]
   Tariff.TZFrom[4] "-" Tariff.TZTo[4] Tariff.Price[4] currency[4]
   Tariff.StartCharge[4] AT 60 
      FORMAT "zzz9.9999" SKIP

   " Time zone 5:"
   Tariff.TZName[5] FORMAT "X(8)"
   Tariff.DayType[5]
   Tariff.TZFrom[5] "-" Tariff.TZTo[5] Tariff.Price[5] currency[5]
   Tariff.StartCharge[5] AT 60 
      FORMAT "zzz9.9999" SKIP

   " Time zone 6:"
   Tariff.TZName[6] FORMAT "X(8)"
   Tariff.DayType[6]
   Tariff.TZFrom[6] "-" Tariff.TZTo[6] Tariff.Price[6] currency[6]
   Tariff.StartCharge[6] AT 60 
      FORMAT "zzz9.9999" SKIP

   " First Billable Sec " Tariff.FirstBillableSec  
      VALIDATE(INPUT Tariff.FirstBillableSec EQ 0 OR 
               (INPUT Tariff.DataType EQ 1 OR INPUT Tariff.DataType EQ 2),
               "Incompatible price unit")

   "OR Minimum sec:" AT 48 Tariff.MinSec
      help "Minimum charging seconds for calls"  
   SPACE(1)
   SKIP
WITH
   WITH ROW 1 centered COLOR value(Syst.Var:cfc) TITLE COLOR value(Syst.Var:ctc) 
   fr-header NO-LABEL OVERLAY
FRAME lis.

{Func/brand.i}

form /* haku PriceList:lla */
   "Brand:" lcBrand skip
   "Code :" plseek
   help "Give a pricelist code"
WITH 
   row 4 col 2 title color value(Syst.Var:ctc) " FIND Price LIST "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr1.

form /*  :n asno hakua varten */
   "Brand:" lcBrand skip
   "CCN .:" lcCSeek
   help "Give a CCN"
WITH 
   row 4 col 2 title color value(Syst.Var:ctc) " FIND CCN "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr2.

form /*  :n asno hakua varten */
   "Brand:" lcBrand skip
   "Cust :" liCustSeek
   help "Give customer number"
WITH 
   row 4 col 2 title color value(Syst.Var:ctc) " FIND Customer "
   COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr3.

IF irRowID NE 0 THEN DO WITH FRAME lis:

   FIND FIRST Tariff where 
              Tariff.TariffNum = irRowID
   NO-LOCK NO-ERROR.

   IF NOT AVAIL Tariff THEN
      MESSAGE "Tariff record does not exist !" VIEW-AS ALERT-BOX.

   ELSE DO:

      ASSIGN fr-header = " DETAILS ".

      CLEAR FRAME lis no-pause.

      RUN local-find-others.

      RUN pUpdate(FALSE,irRowID).

   END.

   RETURN.

END.

IF icBDest NE "" AND CAN-FIND(FIRST Tariff WHERE
                                    Tariff.Brand = lcBrand AND
                                    Tariff.CCN   = iiCCN   AND
                                    Tariff.BDest = icBDest) THEN ASSIGN
   llBDest = TRUE
   lBrHdr  = lBrHdr + "(BDest " + icBDest + ") ".
ELSE
   lBrHdr  = lBrHdr + "(CCN " + STRING(iiCCN) + ") ".

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.

ASSIGN
   xrecid    = ?
   delline   = 0
   ufkey     = TRUE
   order     = 1
   firstline = 0.

/* tariffs for particular ccn, pricelist or customer*/
IF iiCCN > 0  THEN ASSIGN 
   maxOrder = 2
   order    = 1
   icPlist  = ""
   iiCust   = 0. 
ELSE IF iiCust > 0 THEN ASSIGN 
   maxorder = 1
   order    = 2
   icPList  = "".
ELSE IF icPlist NE "" THEN ASSIGN 
   maxOrder = 1
   order    = 2.

RUN local-find-first.

IF AVAILABLE Tariff THEN ASSIGN 
   memory     = recid(Tariff)
   must-print = TRUE 
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No tariffs available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN 
      memory     = ? 
      must-print = FALSE 
      must-add   = FALSE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO ON ENDKEY UNDO, LEAVE: /* Tariff -ADD  */

      assign
         Syst.Var:cfc = "lis"
         ufkey     = true
         fr-header = " ADD "
         must-add  = FALSE.

      RUN Syst/ufcolor.p.

      CLEAR FRAME lis no-pause.
      Syst.Var:ehto = 9.
      RUN Syst/ufkey.p.
      PAUSE 0.

      RUN pUpdate(TRUE,0).

      /* set latest names as default */
      fSaveTZNames().

      ASSIGN
         memory = recid(Tariff)
         xrecid = memory.
      CLEAR FRAME lis no-pause.

      IF RETURN-VALUE NE "FALSE" AND llDoEvent THEN 
         RUN StarEventMakeCreateEvent( lhTariff ).

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN local-find-first.

      IF NOT AVAILABLE Tariff THEN LEAVE LOOP.
      NEXT LOOP.

   END.

   print-line:
   DO :
      IF must-print THEN DO:

         up FRAME-LINE - 1.

         FIND FIRST Tariff where 
              recid(Tariff) = memory
         NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:

            IF AVAILABLE Tariff THEN DO:

               RUN local-find-others.

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(Tariff).

               RUN local-find-next.

            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0 must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN Syst.Var:ufk = 0
         Syst.Var:ufk[1] = 886 Syst.Var:ufk[2]= 1163 Syst.Var:ufk[3]= 0 /* 714 */
         Syst.Var:ufk[4] = (IF llShowHistory THEN 1827 ELSE 1828) 
         Syst.Var:ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
         Syst.Var:ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)   
         Syst.Var:ufk[7] = 796 Syst.Var:ufk[8] = 8 Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 ufkey = FALSE.

         /* no sense in finding with ccn when ccn is already chosen */
         IF iiCCN > 0 THEN Syst.Var:ufk[2] = 0. 
         /* if pricelist or customer chosen then pricelist & customer 
            finds are both invalid*/
         ELSE IF icPlist NE "" OR iiCust > 0
         THEN ASSIGN Syst.Var:ufk[1] = 0
                     Syst.Var:ufk[3] = 0.

         {Syst/uright1.i '"5,6"'}

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Tariff.PriceList {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) Tariff.PriceList WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Tariff.CCN {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) Tariff.CCN WITH FRAME sel.
      END.
      
      Syst.Var:nap = KEYLABEL(LASTKEY).

      IF rtab[FRAME-LINE] = ? AND
         LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0
      THEN DO:
         BELL.
         message "Cursor is on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      if LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
         if maxOrder > 1 then do:
            if maxOrder = 2 and order = 1 then order = 3.
            ELSE if order + 1 > maxOrder then order = 1.
            ELSE order = order + 1. 
         end. 
      END.
      if LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
         if maxorder > 1 then do:
           if order - 1 < 1 then do:
              if maxOrder = 2 then order = 3.
              ELSE order = maxOrder.
           end.
           ELSE do:
              if maxOrder = 2 then order = 1.
              ELSE order = order - 1. 
           end. 
         end.
      END.                                              

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND Tariff where recid(Tariff) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE Tariff THEN
               ASSIGN firstline = i memory = recid(Tariff).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      /* previous line */
      if LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Tariff where recid(Tariff) = rtab[1] NO-LOCK.

            RUN local-find-prev.

            IF NOT AVAILABLE Tariff THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(Tariff)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      ELSE if LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Tariff where recid(Tariff) = rtab[FRAME-DOWN] NO-LOCK .

            RUN local-find-next.

            IF NOT AVAILABLE Tariff THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Tariff).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      ELSE if LOOKUP(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Tariff where recid(Tariff) = memory NO-LOCK NO-ERROR.

         RUN local-find-prev.

         IF AVAILABLE Tariff THEN DO:
            memory = recid(Tariff).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE Tariff THEN memory = recid(Tariff).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     ELSE if LOOKUP(Syst.Var:nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND Tariff where recid(Tariff) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku sarakk. 1 */
     ELSE if LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 THEN DO:  /* haku sar. 1 */
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        plseek = "".
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr1.
        UPDATE lcBrand WHEN Syst.Var:gcAllBrand AND iiCCN = 0
               plseek WITH FRAME hayr1.
        HIDE FRAME hayr1 no-pause.

        if plseek <> "" THEN DO:

           IF iiCCN > 0 THEN 
              FIND FIRST Tariff WHERE
                         Tariff.Brand     = lcBrand AND
                         Tariff.CCN       = iiCCN   AND
                         Tariff.PriceList = plseek USE-INDEX CCN
              NO-LOCK NO-ERROR.
           ELSE 
              FIND FIRST Tariff where
                         Tariff.Brand     = lcBrand AND
                         Tariff.PriceList = plseek
              NO-LOCK NO-ERROR.

           IF NOT AVAILABLE Tariff THEN DO:
              IF iiCCN > 0 THEN 
                 FIND FIRST Tariff WHERE
                            Tariff.Brand     = lcBrand AND
                            Tariff.CCN       = iiCCN   AND
                            Tariff.PriceList BEGINS plseek USE-INDEX CCN
                 NO-LOCK NO-ERROR.
              ELSE 
                 FIND FIRST Tariff where
                            Tariff.Brand     = lcBrand AND
                            Tariff.PriceList BEGINS plseek
                 NO-LOCK NO-ERROR.
           END.

           IF NOT AVAILABLE Tariff THEN DO:

              IF iiCCN > 0 THEN 
                 FIND FIRST Tariff WHERE
                            Tariff.Brand     = lcBrand AND
                            Tariff.CCN       = iiCCN  AND
                            Tariff.PriceList MATCHES "*" + plseek + "*"
                 USE-INDEX CCN NO-LOCK NO-ERROR.
              ELSE 
                 FIND FIRST Tariff where
                            Tariff.Brand     = lcBrand AND
                            Tariff.PriceList MATCHES "*" + plseek + "*"
                 NO-LOCK NO-ERROR.

           END.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.

        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     ELSE if Syst.Var:ufk[2] > 0 AND LOOKUP(Syst.Var:nap,"2,f2") > 0 THEN DO:  /* haku sar. 1 */
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        lcCSeek = 0.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN Syst.Var:gcAllBrand AND icPList = "" AND iiCust = 0
               lcCSeek WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.

        if lcCSeek <> 0 THEN DO:

           IF icPlist NE "" THEN 
              FIND FIRST Tariff WHERE 
                         Tariff.Brand     = lcBrand AND
                         Tariff.PriceList = icPlist AND
                         Tariff.CCN      >= lcCSeek USE-INDEX PriceList
              NO-LOCK NO-ERROR.
           ELSE IF iiCust > 0 THEN
              FIND FIRST Tariff WHERE 
                         Tariff.Brand     = lcBrand AND
                         Tariff.CustNum   = iiCust  AND
                         Tariff.CCN      >= lcCSeek USE-INDEX CustNum
              NO-LOCK NO-ERROR.
           ELSE 
              FIND FIRST Tariff where 
                         Tariff.Brand   = lcBrand AND
                         Tariff.CCN    >= lcCSeek
              NO-LOCK NO-ERROR.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     /* Haku sarakk. 3 */
     ELSE if Syst.Var:ufk[3] > 0 AND LOOKUP(Syst.Var:nap,"3,f3") > 0 THEN DO:  /* haku sar. 3 */
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        liCustSeek = 0.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr3.
        UPDATE lcBrand WHEN Syst.Var:gcAllBrand AND iiCCN = 0
               liCustSeek WITH FRAME hayr3.
        HIDE FRAME hayr3 no-pause.

        if liCustSeek <> 0 THEN DO:
           IF iiCCN > 0 THEN 
              FIND FIRST Tariff WHERE 
                         Tariff.Brand    = lcBrand  AND
                         Tariff.CCN      = iiCCN    AND
                         Tariff.CustNum >= liCustSeek USE-INDEX CCNCust
              NO-LOCK NO-ERROR.
           ELSE 
              FIND FIRST Tariff WHERE
                         Tariff.Brand    = lcBrand AND
                         Tariff.CustNum >= liCustSeek
              NO-LOCK NO-ERROR. 

           IF NOT fRecFound(3) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */
     
     ELSE IF Syst.Var:nap = "4" OR Syst.Var:nap = "f4" THEN DO:
        llShowHistory = NOT llShowHistory.
        CLEAR FRAME sel ALL no-pause.
        RUN local-find-first.
        ASSIGN
           memory = recid(Tariff)
           ufkey = true
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(Syst.Var:nap,"6,f6") > 0 AND lcRight = "RW" THEN DO TRANSAction:  
     /* removal */

        delline = FRAME-LINE.
        FIND Tariff where recid(Tariff) = rtab[FRAME-LINE] NO-LOCK.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(Syst.Var:ctc) 
           Tariff.ValidFrom
           Tariff.ValidTo
           Tariff.PriceList 
           Tariff.BDest
           Tariff.CCN
           Tariff.Price[1] Tariff.StartCharge[1]
           Tariff.Discount[4]
        WITH FRAME sel.

        RUN local-find-next.   
        IF AVAILABLE Tariff THEN memory = recid(Tariff).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND Tariff where recid(Tariff) = rtab[FRAME-LINE] NO-LOCK.
           /* AND THEN the previous one */
           RUN local-find-prev.
           IF AVAILABLE Tariff THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(Tariff).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND FIRST Tariff where 
             recid(Tariff) = rtab[FRAME-LINE]
        EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(Syst.Var:ccc) 
           Tariff.ValidFrom
           Tariff.ValidTo
           Tariff.PriceList
           Tariff.BDest
           Tariff.CCN
           Tariff.Price[1] Tariff.StartCharge[1]
           Tariff.Discount[4]
        WITH FRAME sel.

        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhTariff).

            DELETE Tariff.

            /* in the LAST record was deleted ? */
            RUN local-find-first.
            IF NOT AVAILABLE Tariff THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     ELSE if LOOKUP(Syst.Var:nap,"enter,return") > 0 AND lcRight = "RW"  THEN DO
     WITH FRAME lis TRANSACTION ON ENDKEY UNDO,leave:

        /* change */
        FIND FIRST Tariff where 
             recid(Tariff) = rtab[frame-line(sel)]
        EXCLUSIVE-LOCK.

        IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhTariff ).

        assign
           fr-header = " CHANGE "
           ufkey     = TRUE
           Syst.Var:ehto      = 9.

        CLEAR FRAME lis no-pause.

        RUN Syst/ufkey.p.

        ASSIGN
           plname = ""
           Syst.Var:cfc = "lis". 

        RUN Syst/ufcolor.p.

        RUN local-find-others.

        RUN pUpdate(FALSE,0).

        IF RETURN-VALUE NE "FALSE" AND llDoEvent THEN 
           RUN StarEventMakeModifyEvent ( lhTariff ).

        HIDE FRAME lis no-pause.

        RUN local-disp-row.

        xrecid = recid(Tariff).
     END.

     ELSE if LOOKUP(Syst.Var:nap,"7,f7") > 0 THEN DO WITH FRAME sel:  /* kr/min */
        FIND FIRST Tariff where 
             recid(Tariff) = rtab[FRAME-LINE]
        NO-LOCK NO-ERROR.
        ASSIGN Syst.Var:ufk = 0 ufkey = TRUE Syst.Var:ehto = 3. RUN Syst/ufkey.p.
        DISPLAY
           round(Tariff.Price[1] * 60 / 100,2) @ Tariff.Price[1]
           round(Tariff.StartCharge[1] * 60 / 100,2) @ Tariff.StartCharge[1].
        COLOR DISPLAY messages
           Tariff.Price[1]
           Tariff.StartCharge[1]
        WITH FRAME sel.
        message "Press ENTER !".
        PAUSE no-message.
        COLOR DISPLAY value(Syst.Var:cfc)
           Tariff.Price[1]
           Tariff.StartCharge[1]
        WITH FRAME sel.
        DISPLAY
           Tariff.Price[1]
           Tariff.StartCharge[1]
        WITH FRAME sel.
     END.

     ELSE if LOOKUP(Syst.Var:nap,"home,h") > 0 THEN DO:
        RUN local-find-first.
        ASSIGN
           memory     = recid(Tariff)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(Syst.Var:nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-last.
        ASSIGN
           memory     = recid(Tariff)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-FIRST:

   IF llBDest THEN DO:
      IF order = 1 THEN
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN   AND
                       Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN   AND
                       Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN AND
                       Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE   
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN   AND
                       Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN   AND
                       Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN   AND
                       Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCCN > 0 THEN DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand = lcBrand AND
                       Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.

   ELSE IF icPlist NE "" THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand     = lcBrand AND
                       Tariff.PriceList = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.PriceList  = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCust > 0 THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff WHERE
                       Tariff.Brand   = lcBrand AND
                       Tariff.CustNum = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType AND
                       Tariff.CustNum    = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END. 

   ELSE DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff USE-INDEX PriceList WHERE
                       Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND FIRST Tariff USE-INDEX CCN WHERE
                       Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND FIRST Tariff USE-INDEX CustNum WHERE
                       Tariff.Brand = lcBrand 
            NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST Tariff WHERE
                       Tariff.Brand      = lcBrand AND
                       Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF llBDest THEN DO:
      IF order = 1 THEN
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE   
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCCN > 0 THEN DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.

   ELSE IF icPlist NE "" THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand     = lcBrand AND
                      Tariff.PriceList = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.PriceList  = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCust > 0 THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff WHERE
                      Tariff.Brand   = lcBrand AND
                      Tariff.CustNum = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CustNum    = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END. 

   ELSE DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff USE-INDEX PriceList WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND NEXT Tariff USE-INDEX CCN WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND NEXT Tariff USE-INDEX CustNum WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
        ELSE
            FIND NEXT Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF llBDest THEN DO:
      IF order = 1 THEN
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE   
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCCN > 0 THEN DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.

   ELSE IF icPlist NE "" THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand     = lcBrand AND
                      Tariff.PriceList = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.PriceList  = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCust > 0 THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff WHERE
                      Tariff.Brand   = lcBrand AND
                      Tariff.CustNum = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CustNum    = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END. 

   ELSE DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff USE-INDEX PriceList WHERE
                      Tariff.Brand = lcBrand
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND PREV Tariff USE-INDEX CCN WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND PREV Tariff USE-INDEX CustNum WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
        ELSE
            FIND PREV Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF llBDest THEN DO:
      IF order = 1 THEN
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE   
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN   AND
                      Tariff.BDest = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN   AND
                      Tariff.BDest      = icBDest
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCCN > 0 THEN DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand = lcBrand AND
                      Tariff.CCN   = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CCN        = iiCCN
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCNCust NO-LOCK NO-ERROR.
   END.

   ELSE IF icPlist NE "" THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand     = lcBrand AND
                      Tariff.PriceList = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.PriceList  = icPlist
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
   END.

   ELSE IF iiCust > 0 THEN DO:
      IF order = 2 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff WHERE
                      Tariff.Brand   = lcBrand AND
                      Tariff.CustNum = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType AND
                      Tariff.CustNum    = iiCust
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END. 

   ELSE DO:
      IF order = 1 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff USE-INDEX PriceList WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX PriceList NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN
         IF iiTType = 0 THEN
            FIND LAST Tariff USE-INDEX CCN WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
         ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CCN NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN 
         IF iiTType = 0 THEN
            FIND LAST Tariff USE-INDEX CustNum WHERE
                      Tariff.Brand = lcBrand 
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            NO-LOCK NO-ERROR.
        ELSE
            FIND LAST Tariff WHERE
                      Tariff.Brand      = lcBrand AND
                      Tariff.TariffType = iiTType
                   AND (IF llShowHistory THEN TRUE
                       ELSE Tariff.ValidTo >= TODAY)
            USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.


PROCEDURE local-find-others:

   lcBDest = IF Tariff.BDest NE "" THEN "*" ELSE "". 

   FIND FIRST CCN WHERE
              CCN.Brand = Tariff.Brand AND
              CCN.CCN   = Tariff.CCN
   NO-LOCK NO-ERROR.

   IF AVAIL CCN THEN lcCName = CCN.CCNName.
   ELSE              lcCName = "!! BLANK !!".

END PROCEDURE.

PROCEDURE local-disp-row:

   DISPLAY 
      Tariff.Brand
      Tariff.ValidFrom
      Tariff.ValidTo
      Tariff.PriceList
      Tariff.CCN
      lcCName
      Tariff.BDest
      Tariff.Price[1]  when Tariff.Price[1] NE 0
      Tariff.StartCharge[1]  when Tariff.StartCharge[1] NE 0
      Tariff.Discount[4]
      "" when Tariff.Price[1] = 0 @ Tariff.Price[1]
      "" when Tariff.StartCharge[1] = 0 @ Tariff.StartCharge[1]
      lcBDest
   WITH FRAME sel.

   IF Tariff.PriceList = "" AND Tariff.BDest NE "" THEN
      DISPLAY Tariff.BDest @ Tariff.PriceList WITH FRAME sel.

END PROCEDURE. 


PROCEDURE pUpdate.

   DEF INPUT PARAMETER bNew AS LOG NO-UNDO.
   DEF INPUT PARAMETER iDtl AS REC NO-UNDO.

   IF bNew THEN DO:

      CREATE Tariff.

      ASSIGN
         Tariff.Brand       = lcBrand
         Tariff.CCN         = iiCCN
         Tariff.BDest       = ""
         Tariff.Pricelist   = icPlist
         Tariff.CustNum     = iiCust
         Tariff.DataType    = 1
         Tariff.discount[4] = TRUE.

      IF Tariff.CCN NE 0 THEN DO:
         FIND FIRST CCN WHERE
                    CCN.Brand = Tariff.Brand AND
                    CCN.CCN   = Tariff.CCN
         NO-LOCK NO-ERROR.
         DISPLAY
            Tariff.TariffNum
            Tariff.CCN
            CCN.CCNName
         WITH FRAME lis.
      END. 
      ELSE IF Tariff.PriceList NE "" THEN DO:
         FIND FIRST PriceList WHERE
                    PriceList.Brand     = Tariff.Brand AND
                    PriceList.PriceList = Tariff.Pricelist
         NO-LOCK NO-ERROR.
         Tariff.CurrUnit = PriceList.CurrUnit.

         DISPLAY
            Tariff.PriceList
            PriceList.PLName
            Tariff.CurrUnit
         WITH FRAME lis.
      END. 

      UPDATE
         Tariff.CCN       WHEN iiCCN = 0
         Tariff.BDest
         Tariff.PriceList WHEN (icPlist = "" AND iiCust = 0)
      WITH FRAME lis EDITING:

         READKEY.
         Syst.Var:nap = KEYLABEL(LASTKEY).

         IF Syst.Var:nap = "F4" THEN UNDO, RETURN "FALSE".

         IF LOOKUP(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "CCN" THEN DO:
               ASSIGN INPUT FRAME lis Tariff.CCN.
               IF TARIFF.CCN = 0 THEN UNDO, RETURN "FALSE".
               ELSE DO:
                  FIND FIRST CCN where 
                             CCN.Brand = Tariff.Brand AND
                             CCN.CCN   = Tariff.CCN
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL CCN THEN DO:
                     MESSAGE 
                        "Unknown CCN"
                     VIEW-AS ALERT-BOX.
                     NEXT-PROMPT Tariff.CCN.
                     NEXT.
                  END.
                  DISPLAY CCN.CCNName WITH FRAME lis.
               END.
            END.
            ELSE IF FRAME-FIELD = "PriceList" THEN DO:

               ASSIGN FRAME lis Tariff.PriceList.
               IF Tariff.PriceList = "" AND Tariff.CustNum = 0 THEN DO:
                  MESSAGE
                     "Rates must have a Price list code !"
                  VIEW-AS ALERT-BOX.
                  NEXT-PROMPT Tariff.PriceList.
                  NEXT.
               END. 
               ELSE IF Tariff.CustNum = 0 THEN DO:
                  /* check IF PriceList record exists */ 
                  FIND FIRST PriceList where 
                             PriceList.Brand     = Tariff.Brand AND
                             PriceList.PriceList = Tariff.PriceList
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL PriceList THEN DO:
                     message
                        "Unknown Price list"
                     VIEW-AS ALERT-BOX.
                     NEXT-PROMPT Tariff.PriceList.
                     NEXT.
                  END.
                  IF PriceList.Currency = "" OR
                     NOT CAN-FIND(Currency OF PriceList) THEN DO: 
                     MESSAGE
                        "Currency has not been defined for this pricelist."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT-PROMPT Tariff.PriceList.
                     NEXT.
                  END.

                  Tariff.CurrUnit = PriceList.CurrUnit.

                  DISP
                     PriceList.PLName
                     Tariff.CurrUnit
                  WITH FRAME lis.

               END.

            END.
            ELSE IF FRAME-FIELD = "Tariff.BDest" THEN DO:
               ASSIGN FRAME lis Tariff.BDest.
               IF Tariff.BDest = "" THEN
                  DISPLAY "" @ BDest.BDName WITH FRAME lis.
               ELSE DO:
                  FIND FIRST BDest where
                             BDest.Brand = Tariff.Brand AND
                             BDest.BDest = Tariff.BDest AND
                             BDest.ToDate >= Tariff.ValidFrom AND
                             BDest.FromDate <= Tariff.ValidTo
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL BDest THEN DO:  
                     MESSAGE
                        "Unknown B-Destination"
                     VIEW-AS ALERT-BOX.
                     NEXT-PROMPT Tariff.BDest.
                     NEXT.
                  END.
                  DISPLAY BDest.BDName WITH FRAME lis.
               END.
            END.

         END.

         APPLY LASTKEY.

      END.

      Tariff.ValidFrom = TODAY.
      DO WHILE MONTH(Tariff.ValidFrom) = MONTH(TODAY):
         Tariff.ValidFrom = Tariff.ValidFrom + 1.
      END.

      ASSIGN
         lcTmp             = fCParamC("DefCLStamp") 
         lcTmp             = SUBSTR(lcTmp,7,2) + 
                             SUBSTR(lcTmp,5,2) +
                             SUBSTR(lcTmp,1,4)
         Tariff.TariffNum  = next-value(Tariff)
         Tariff.TariffType = iiTType
         Tariff.ValidTo    = DATE(lcTmp).

      fGetTZNames(). 

   END.

   ELSE DO:

      IF Tariff.BDest NE "" THEN DO:
         FIND FIRST BDest WHERE
                    BDest.Brand = Tariff.Brand AND
                    BDest.BDest = Tariff.BDest AND
                    BDest.ToDate >= Tariff.ValidFrom AND
                    BDest.FromDate <= Tariff.ValidTo
         NO-LOCK NO-ERROR.
         IF AVAILABLE BDest THEN
            DISPLAY BDest.BDName WITH FRAME lis.
         ELSE
            DISPLAY "* UNKNOWN *" @ BDest.BDName WITH FRAME lis.
      END.
      ELSE DISPLAY "" @ BDest.BDName WITH FRAME lis.

      DISPLAY 
         Tariff.TariffNum
         lcCName          @ CCN.CCNName
         Tariff.CCN
         Tariff.BDest
         Tariff.PriceList
         Tariff.CurrUnit
         plname           @ PriceList.PLName
         Currency
      WITH FRAME lis.

   END.

   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Tariff"   AND
              TMSCodes.FieldName = "DataType" AND
              TMSCodes.CodeGroup = "Tariff"   AND
              TMSCodes.CodeValue = STRING(Tariff.DataType)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN DO:
      lcDataType = TMSCodes.CodeName.
      DISP lcDataType WITH FRAME lis.
   END.

   IF Tariff.PriceList NE "" THEN xPriceList = Tariff.Pricelist.
   ELSE                           xPriceList = fCustPriceList
                                                 (Tariff.CustNum,1, 
                                                  Tariff.ValidFrom).

   IF xPriceList NE "" THEN DO:

      FIND FIRST PriceList where                    
                 PriceList.Brand     = Tariff.Brand AND
                 PriceList.PriceList = xPriceList
      NO-LOCK NO-ERROR.

      IF AVAIL PriceList THEN DO:
         plname   = PriceList.PLName.
         FIND FIRST Currency where
                    Currency.Currency = PriceList.Currency
         NO-LOCK NO-ERROR.
         IF AVAILABLE Currency THEN 
            Currency = (IF Tariff.CurrUnit THEN Currency.Currency
                        ELSE                    Currency.SubName) +
                        "/" + lcDataType.
         ELSE Currency = "". 
         DISP Currency WITH FRAME lis.
      END.
      ELSE plname = "!! BLANK !!".

      IF Tariff.PriceList = "" THEN plname = "".

   END.

   FIND FIRST BillItem WHERE
              BillItem.Brand    = Tariff.Brand   AND
              BillItem.BillCode = Tariff.BillCode
   NO-LOCK NO-ERROR.

   IF AVAILABLE BillItem THEN
      DISPLAY BillItem.BIName WITH FRAME lis.
   ELSE
      DISPLAY "Unknown" @ BillItem.BIName WITH FRAME lis. 

   /* Show only details, called from a viewer program */
   IF iDtl NE 0 THEN DO:

      DISPLAY
         Tariff.ValidFrom
         Tariff.ValidTo
         Tariff.RateType
         Tariff.DataType
         Tariff.Discount[4]
         Tariff.BillCode
         Tariff.CurrUnit WHEN Tariff.CustNum NE 0
         Tariff.MPMRid
         Tariff.ServRid

         Tariff.TZName[1]
         Tariff.DayType[1]
         Tariff.TZFrom[1]
         Tariff.TZTo[1]
         Tariff.Price[1]
         Tariff.StartCharge[1]

         Tariff.TZName[2]
         Tariff.DayType[2]
         Tariff.TZFrom[2]
         Tariff.TZTo[2]
         Tariff.Price[2]
         Tariff.StartCharge[2]

         Tariff.TZName[3]
         Tariff.DayType[3]
         Tariff.TZFrom[3]
         Tariff.TZTo[3]
         Tariff.Price[3]
         Tariff.StartCharge[3]

         Tariff.TZName[4]
         Tariff.DayType[4]
         Tariff.TZFrom[4]
         Tariff.TZTo[4]
         Tariff.Price[4]
         Tariff.StartCharge[4]

         Tariff.TZName[5]
         Tariff.DayType[5]
         Tariff.TZFrom[5]
         Tariff.TZTo[5]
         Tariff.Price[5]
         Tariff.StartCharge[5]

         Tariff.TZName[6]
         Tariff.DayType[6]
         Tariff.TZFrom[6]
         Tariff.TZTo[6]
         Tariff.Price[6]
         Tariff.StartCharge[6]

         Tariff.FirstBillableSec
         Tariff.MinSec

      WITH FRAME lis.

      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Press ENTER to continue ...".
      PAUSE NO-MESSAGE.

      RETURN.

   END.

   UpdLoop:
   REPEAT ON ENDKEY UNDO UpdLoop, LEAVE UpdLoop:

      UPDATE
         Tariff.ValidFrom
         Tariff.ValidTo
         Tariff.RateType
         Tariff.DataType
         Tariff.Discount[4]
         Tariff.BillCode
         Tariff.CurrUnit WHEN Tariff.CustNum NE 0
         Tariff.MPMRid
         Tariff.ServRid
         
         Tariff.TZName[1]
         Tariff.DayType[1]
         Tariff.TZFrom[1]
         Tariff.TZTo[1]
         Tariff.Price[1]
         Tariff.StartCharge[1]

         Tariff.TZName[2]
         Tariff.DayType[2]
         Tariff.TZFrom[2]
         Tariff.TZTo[2]
         Tariff.Price[2]
         Tariff.StartCharge[2]

         Tariff.TZName[3]
         Tariff.DayType[3]
         Tariff.TZFrom[3]
         Tariff.TZTo[3]
         Tariff.Price[3]
         Tariff.StartCharge[3]

         Tariff.TZName[4]
         Tariff.DayType[4]
         Tariff.TZFrom[4]
         Tariff.TZTo[4]
         Tariff.Price[4]
         Tariff.StartCharge[4]

         Tariff.TZName[5]
         Tariff.DayType[5]
         Tariff.TZFrom[5]
         Tariff.TZTo[5]
         Tariff.Price[5]
         Tariff.StartCharge[5]

         Tariff.TZName[6]
         Tariff.DayType[6]
         Tariff.TZFrom[6]
         Tariff.TZTo[6]
         Tariff.Price[6]
         Tariff.StartCharge[6]

         Tariff.FirstBillableSec
         Tariff.MinSec

      WITH FRAME lis EDITING:

         /* disable EDITING unused fields */
         IF FRAME-FIELD =  "startFee"                     AND
            LOOKUP(string(frame-index),"1,2,3,4,5,6") > 0 AND
            INPUT Tariff.Discount[4] = FALSE THEN APPLY LASTKEY.
         ELSE IF FRAME-FIELD = "minsec" AND
                 INPUT Tariff.Discount[4] = TRUE THEN DO:
            MinSec = 0.
            DISPLAY MinSec WITH FRAME lis.
            APPLY LASTKEY.
         END. 

         IF FRAME-FIELD = "ValidTo" THEN DO:
            FIND FIRST xxtariff WHERE 
                       xxtariff.Brand     = Tariff.Brand     AND
                       xxtariff.CCN       = Tariff.CCN       AND 
                       xxtariff.BDest     = Tariff.BDest     AND 
                       xxtariff.CustNum   = Tariff.CustNum   AND 
                       xxtariff.PriceList = Tariff.PriceList AND 
                       xxtariff.ValidTo   > Tariff.ValidFrom AND
                 RECID(xxtariff) NE RECID(Tariff)
            NO-LOCK NO-ERROR.
            IF AVAIL xxtariff THEN DO: 
               MESSAGE
                  "An active rate was found." SKIP
                  "Do You want to close it when this" 
                  "rate comes active ?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
               IF ok THEN DO:
                  FIND CURRENT xxtariff EXCLUSIVE-LOCK.
                  ASSIGN xxtariff.ValidTo = Tariff.ValidFrom - 1. 
               END.
            END.
         END.

         READKEY.

         CASE KEYLABEL(LASTKEY):
             WHEN "F4" THEN UNDO, RETURN "FALSE".
             WHEN "F9" THEN DO:
                CASE FRAME-FIELD:
                   WHEN "DataType" THEN DO:
                      RUN Help/h-tmscodes.p
                            ("Tariff","DataType","Tariff", OUTPUT siirto).
                      IF INT(siirto) > 0 THEN DO:
                         Tariff.DataType = INT(siirto).
                         FIND FIRST TMSCodes WHERE
                                    TMSCodes.TableName = "Tariff"   AND
                                    TMSCodes.FieldName = "DataType" AND
                                    TMSCodes.CodeGroup = "Tariff"   AND
                                    TMSCodes.CodeValue = STRING(Tariff.DataType)
                         NO-LOCK NO-ERROR.
                         lcDataType = TMSCodes.CodeName.
                         DISP
                            Tariff.DataType
                            lcDataType
                         WITH FRAME lis.
                      END.

                      Syst.Var:ehto = 9.
                      RUN Syst/ufkey.p.
                      PAUSE 0.

                      NEXT-PROMPT Tariff.DataType.
                      NEXT.
                   END.
                END.
             END.
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),"home") > 0 AND
            index(frame-field,"hinta") > 0 THEN DO:
            ASSIGN
               luku        = round(decimal(frame-value) * 100 / 60,5)
               frame-value = string(luku,"zz9.99999").
            message "1/1 minute-rate converted into 1/100 second-rate".
            NEXT.
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "ValidFrom" THEN DO:
               IF INPUT Tariff.ValidFrom = ? THEN DO:
                  ok = FALSE.
                  MESSAGE 
                     "Date value can not be empty !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               IF Tariff.ValidFrom ENTERED THEN DO:
                  FIND FIRST xxtariff where
                             xxtariff.Brand     = Tariff.Brand     AND 
                             xxtariff.CCN       = CCN.CCN          AND
                             xxtariff.PriceList = Tariff.PriceList AND
                             xxtariff.CustNum   = Tariff.CustNum   AND
                             xxtariff.BDest     = Tariff.BDest     AND
                             xxtariff.ValidFrom = INPUT Tariff.ValidFrom
                  NO-LOCK NO-ERROR.
                  IF AVAIL xxtariff THEN DO:
                     MESSAGE
                        "There is already a Price for this combination of " SKIP
                        "CCN / pricelist / customer / BDest / valid from !"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               IF INPUT Tariff.ValidFrom < TODAY AND 
                  Tariff.ValidFrom ENTERED THEN DO:
                  MESSAGE "Date is HISTORY, accept anyway ?" UPDATE ok.
                  IF NOT ok THEN NEXT.
               END.
               ASSIGN Tariff.ValidFrom.
            END.

            ELSE IF FRAME-FIELD = "ValidTo" AND frame-index = 1 THEN DO:
               ASSIGN FRAME lis Tariff.TZTo[1].

               if Tariff.TZTo[1] = "0000" THEN DO:
                  MESSAGE
                     "1:st timezone can not end with 0 !"
                  VIEW-AS ALERT-BOX.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "DataType" THEN DO:

               Tariff.DataType = INPUT FRAME lis Tariff.DataType.

               FIND FIRST TMSCodes WHERE
                          TMSCodes.TableName = "Tariff"   AND
                          TMSCodes.FieldName = "DataType" AND
                          TMSCodes.CodeGroup = "Tariff"   AND
                          TMSCodes.CodeValue = STRING(Tariff.DataType)
               NO-LOCK NO-ERROR.
               IF NOT AVAIL TMSCodes THEN DO:
                  MESSAGE
                     "Unknown value !" SKIP
                     "Press F9 and choose correct value."
                  VIEW-AS ALERT-BOX.
                  NEXT.
               END.
               lcDataType = TMSCodes.CodeName.
               DISP
                  Tariff.DataType
                  lcDataType
               WITH FRAME lis.
            END.

            ELSE IF FRAME-FIELD = "BillCode" THEN DO:
               FIND FIRST BillItem WHERE 
                          BillItem.Brand    = Tariff.Brand AND
                          BillItem.BillCode = INPUT FRAME lis Tariff.BillCode
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE BillItem THEN DO:
                  MESSAGE
                     "Unknown billing item"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT Tariff.BillCode.
                  NEXT. 
               END.
               DISPLAY BillItem.BIName WITH FRAME lis. 
            END.

            ELSE IF FRAME-FIELD = "CurrUnit" THEN DO:
               ASSIGN Tariff.CurrUnit.
               IF Tariff.CustNum NE 0 THEN
                  FIND FIRST Currency WHERE
                             Currency.Currency = Customer.Currency
                  NO-LOCK NO-ERROR.
               ELSE 
                  FIND FIRST Currency WHERE
                             Currency.Currency = PriceList.Currency
                  NO-LOCK NO-ERROR.

               IF AVAILABLE Currency THEN 
                  Currency = (IF Tariff.CurrUnit THEN Currency.Currency
                              ELSE                    Currency.SubName) +
                                                      "/" + lcDataType.
               ELSE Currency = "". 

               DISP Currency WITH FRAME lis.

            END.

            /* CLEAR unused fields */
            ELSE IF FRAME-FIELD = "disc" AND frame-index = 4 THEN DO:
               ASSIGN FRAME lis Tariff.Discount[4].
               IF Tariff.Discount[4] = FALSE THEN DO:
                  DO i = 1 TO 6:
                     Tariff.StartCharge[i] = 0.00.
                     DISP Tariff.StartCharge[i] WITH FRAME lis.
                  END.
               END.
               ELSE IF Tariff.Discount[4] = TRUE THEN DO:
                  Tariff.MinSec = 0.00.
                  DISPLAY Tariff.MinSec WITH FRAME lis.
                  IF Tariff.CustNum NE 0 THEN DO:
                     FIND FIRST Customer where 
                                Customer.CustNum = Tariff.CustNum
                     NO-LOCK NO-ERROR.
                     IF AVAIL Customer AND NOT Customer.StartCharge THEN DO:
                        MESSAGE 
                           "Starting charges are barred on this customer !" SKIP
                           "Determining starting charges will have no Effect !"
                        VIEW-AS ALERT-BOX ERROR.
                    END.
                  END.
               END.
            END.

            ELSE IF FRAME-FIELD = "Price" THEN DO:
               IF INPUT Tariff.Price[FRAME-INDEX]   > 0 AND
                 (INPUT Tariff.DayType[FRAME-INDEX] = 0 OR
                  INPUT Tariff.DayType[FRAME-INDEX] > 3) THEN DO:
                  MESSAGE
                     "Day type must be 1 - 3"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT Tariff.DayType[FRAME-INDEX].
                  NEXT.
               END.
            END. 

            ELSE IF FRAME-FIELD = "TZFrom" THEN DO:
               IF LENGTH(INPUT Tariff.TZFrom[FRAME-INDEX]) LT 4 THEN DO:
                  MESSAGE
                     "Both hours and minutes must be given."
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT Tariff.TZFrom[FRAME-INDEX].
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "TZTo" THEN DO:
               IF LENGTH(INPUT Tariff.TZTo[FRAME-INDEX]) LT 4 THEN DO:
                  MESSAGE
                     "Both hours and minutes must be given."
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT Tariff.TZTo[FRAME-INDEX].
                  NEXT.
               END.
            END.
         END.

         APPLY LASTKEY.

      END. /* EDITING */

      ASSIGN
         llDayType = FALSE
         llDayFrom = FALSE
         llDayTo   = FALSE.

      /* is whole DAY determined */
      DO i = 1 TO 6:

         if Tariff.TZFrom[i] = "0000" AND Tariff.TZTo[i] = "0000" THEN DO:
            IF Tariff.Price[i] NE 0 THEN DO:
               MESSAGE
                  "Time zone is not defined for Price !"
               VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
            ELSE NEXT.
         END.

         /* any hours higer than 24:00 */
         if Tariff.TZFrom[i] > "2400" or Tariff.TZTo[i] > "2400" THEN DO:
            MESSAGE
               "Timezone can not have a higher value than 24 !"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.

         /* minutes over 59 */
         IF SUBSTRING(Tariff.TZFrom[i],3,2) > "59" OR 
            SUBSTRING(Tariff.TZTo[i],3,2)   > "59"  THEN DO:
            MESSAGE
               "Maximum value for minutes is 59"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.

         /* hours in right order */
         IF Tariff.TZFrom[i] > Tariff.TZTo[i] THEN DO:
            MESSAGE
               "Time limits must be in Size order !"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.

         if i = 1 AND Tariff.TZFrom[i] NE "0000" THEN DO:
            MESSAGE
               "Time limits must start from 0 !"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.
         ELSE IF i > 1 THEN DO:

            IF (Tariff.DayType[i] NE Tariff.DayType[i - 1]) AND 
                Tariff.TZFrom[i]  NE "0000"            AND
                Tariff.DayType[i] NE 2 THEN DO:
               MESSAGE
                  "Time limits must start from 0 !"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

            ELSE IF (Tariff.DayType[i] = Tariff.DayType[i - 1]) AND 
                     Tariff.TZFrom[i] NE Tariff.TZTo[i - 1] THEN DO:
               MESSAGE
                  "Time limits must not have gaps !"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

         END.

         /* DayType unvalid */
         IF (Tariff.DayType[i] < 1  OR
             Tariff.DayType[i] > 3) AND 
            (Tariff.TZFrom[i] NE "0000"  OR
             Tariff.TZTo[i]   NE "0000") THEN DO:
             MESSAGE
                "Price zone type must be 1 - 3"
             VIEW-AS ALERT-BOX.
             NEXT.
         END. 

         /* overlapping time zone definitions */
         DO j = 1 TO 6:

            /* check only within same type */
            IF i = j OR Tariff.DayType[i] NE Tariff.DayType[j] THEN NEXT. 

            IF (Tariff.TZFrom[j] <= Tariff.TZFrom[i] AND
                Tariff.TZTo[j]   >  Tariff.TZFrom[i]) OR
               (Tariff.TZFrom[j] <  Tariff.TZTo[i]   AND
                Tariff.TZTo[j]   >= Tariff.TZTo[i])  THEN DO:

               MESSAGE
                  "There are overlapping time zone definitions on lines" i 
                  "and" j       
               VIEW-AS ALERT-BOX.
               NEXT.

            END.

         END.

         /* both 00:00 and 24:00 must be found for types 1 and 3 */
         IF Tariff.DayType[i] = 1 OR Tariff.DayType[i] = 3 THEN DO:

            llDayType[Tariff.DayType[i]] = TRUE.

            IF Tariff.TZFrom[i] = "0000" THEN
               llDayFrom[Tariff.DayType[i]] = TRUE.

            IF Tariff.TZTo[i] = "2400" THEN 
               llDayTo[Tariff.DayType[i]] = TRUE.

         END.

      END.

      /* both 00:00 and 24:00 must be found for types 1 and 3 */
      DO i = 1 to 3:

         IF llDayType[i] = TRUE   AND
           (llDayFrom[i] = FALSE  OR
            llDayTo[i]   = FALSE) THEN DO:

            MESSAGE
               "Definitions for price zone" i "do not cover the whole"
               "day from 0:00 to 24:00"
            VIEW-AS ALERT-BOX.
            NEXT.

         END.

      END.

      DO i = 1 to 6: 
         IF Tariff.Price[i] > 0 THEN DO:
            i = -1.
            LEAVE.
         END. 
      END.

      IF i > 0 THEN DO:
         ok = FALSE.
         MESSAGE
            "All prices are zero. o you still want to save this tariff ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
         IF NOT ok THEN NEXT UpdLoop.
      END.

      MESSAGE
         "Do you accept this Price (Y/N) ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
      IF NOT ok THEN UNDO, NEXT UpdLoop.

      LEAVE UpdLoop.

   END.

   RETURN "". 

END PROCEDURE.

