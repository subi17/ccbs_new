/* ----------------------------------------------------------------------
  MODULE .......: OfferItem.p
  TASK .........: UPDATEs table OfferItem
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.01.09
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'OfferItem'}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/cparam2.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOfferItem AS HANDLE NO-UNDO.
   lhOfferItem = BUFFER OfferItem:HANDLE.
   RUN StarEventInitialize(lhOfferItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhOfferItem).
   END.

END.

DEF INPUT PARAMETER icOffer  AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilUpdate AS LOG  NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcType       AS CHAR NO-UNDO.
DEF VAR lcBegin      AS CHAR NO-UNDO.
DEF VAR lcEnd        AS CHAR NO-UNDO.
DEF VAR ldtFromDate  AS DATE NO-UNDO.
DEF VAR ldtToDate    AS DATE NO-UNDO. 
DEF VAR lcFrameField AS CHAR NO-UNDO. 
DEF VAR lcHelp       AS CHAR NO-UNDO.
DEF VAR lcItemName   AS CHAR NO-UNDO. 
DEF VAR llTopupVat   AS LOG  NO-UNDO.
DEF VAR ldDefFrom    AS DEC  NO-UNDO.
DEF VAR ldItemAmount AS DEC  NO-UNDO.
DEF VAR lcDiscount   AS CHAR NO-UNDO.
DEF VAR ldOfferFrom  AS DEC  NO-UNDO.
DEF VAR ldOfferTo    AS DEC  NO-UNDO.

FORM
    OfferItem.ItemType FORMAT "X(12)"
    OfferItem.ItemKey  FORMAT "X(20)"
    lcItemName         FORMAT "X(14)"       COLUMN-LABEL "Name"
    ldItemAmount       FORMAT "->>>>>>9.99" COLUMN-LABEL "Amount"
    ldtFromDate        FORMAT "99-99-99"    COLUMN-LABEL "From"
    ldtToDate          FORMAT "99-99-99"    COLUMN-LABEL "To"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " ITEMS FOR OFFER " + STRING(icOffer) + " "
    FRAME sel.

FORM
    OfferItem.Offer  COLON 20 FORMAT "X(16)"
       Offer.Description FORMAT "X(40)" NO-LABEL SKIP
    OfferItem.OfferItemID COLON 20 SKIP(1)
    OfferItem.ItemType COLON 20 FORMAT "x(256)" VIEW-AS FILL-IN SIZE 20 BY 1
       lcType COLON 41 FORMAT "X(30)" NO-LABEL SKIP
    OfferItem.ItemKey  COLON 20 FORMAT "x(256)" VIEW-AS FILL-IN SIZE 20 BY 1
       lcItemName COLON 41 NO-LABEL FORMAT "X(30)" SKIP
    OfferItem.Amount   COLON 20
       lcDiscount COLON 41 NO-LABEL FORMAT "X(30)" SKIP
    OfferItem.Periods  COLON 20 FORMAT ">>9" SKIP
    OfferItem.VatIncl  COLON 20 LABEL "Tax"
    OfferItem.BeginStamp COLON 20
       lcBegin FORMAT "X(20)" NO-LABEL SKIP
    OfferItem.EndStamp COLON 20
       lcEnd FORMAT "X(20)" NO-LABEL SKIP
    OfferItem.DispInUI COLON 20 SKIP
    OfferItem.DispOnInvoice COLON 20 
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fItemType RETURNS LOGIC
   (icItemType AS CHAR):

   IF icItemType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "OfferItem",
                                "ItemType",
                                icItemType).
   ELSE lcType = "".
                                
   DISP lcType WITH FRAME lis.
   
END FUNCTION.

FUNCTION fItemName RETURNS LOGIC
   (icItemType  AS CHAR,
    icItemValue AS CHAR):

   ASSIGN
      lcItemName = ""
      llTopupVat = FALSE
      lcDiscount = "".
   
   CASE icItemType:
   WHEN "BillItem" THEN DO:
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand AND
                 BillItem.BillCode = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcItemName = BillItem.BIName.
   END.
   WHEN "FATime" THEN DO:
      FIND FIRST FatGroup WHERE
                 FatGroup.Brand = gcBrand AND
                 FatGroup.FtGrp = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE FatGroup THEN lcItemName = FatGroup.FtgName.
   END.
   WHEN "DiscountPlan" THEN DO:
      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.Brand = gcBrand AND
                 DiscountPlan.DPRuleId = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE DiscountPlan THEN
         ASSIGN lcItemName = DiscountPlan.DPName
                lcDiscount = DiscountPlan.DPUnit.
   END.
   WHEN "Topup" THEN DO:
      FOR FIRST TopupScheme NO-LOCK WHERE
                TopupScheme.Brand = gcBrand AND
                TopupScheme.TopupScheme = icItemValue,
          FIRST TopupSchemeRow OF TopupScheme NO-LOCK WHERE
                TopupSchemeRow.EndStamp >= ldOfferFrom AND
                TopupSchemeRow.BeginStamp <= ldOfferTo:
         ASSIGN
            lcItemName = TopupScheme.Description
            llTopupVat = TopupScheme.VatIncl.
         IF TopupSchemeRow.DiscountAmount NE 0 THEN 
            lcDiscount = "(Recharge " + STRING(TopupSchemeRow.Amount) +
                         " - discount " + 
                         STRING(TopupSchemeRow.DiscountAmount) +
                         ")".
      END.                   
   END.                                            
   WHEN "PerContract" THEN DO:
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = gcBrand AND
                 DayCampaign.DCEvent = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE DayCampaign THEN lcItemName = DayCampaign.DCName.
   END.
   WHEN "ServicePackage" THEN DO:
       FIND ServPac WHERE
            ServPac.Brand   = gcBrand AND
            ServPac.ServPac = icItemValue NO-LOCK NO-ERROR.
       IF AVAILABLE ServPac THEN lcItemName = ServPac.SPName. 
   END.
   WHEN "BundleItem" THEN lcItemName = fCParamC(icItemValue).
   WHEN "OptionalBundleItem" THEN lcItemName = fCParamC(icItemValue).
   OTHERWISE lcItemName = "".
   END CASE.
   
END.

FUNCTION fPenaltyFeeAmount RETURNS DECIMAL
   (icPerContract AS CHAR):
   
   DEF VAR ldPenalty AS DEC NO-UNDO.
   
   ldPenalty = 0.
   
   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand   = gcBrand AND
             DayCampaign.DCEvent = icPerContract AND
             DayCampaign.TermFeeCalc > 0,
       FIRST FMItem NO-LOCK WHERE
             FMItem.Brand     = gcBrand AND
             FMItem.FeeModel  = DayCampaign.TermFeeModel AND
             FMItem.ToDate   >= TODAY AND
             FMItem.FromDate <= TODAY:
      ldPenalty = FMItem.Amount.
   END.

   RETURN ldPenalty.
   
END FUNCTION.

FUNCTION fTopUpAmount RETURNS DECIMAL
   (icTopupScheme AS CHAR):

   DEF VAR ldTopup AS DEC NO-UNDO.
   
   ldTopup = 0.
   
   FOR FIRST TopupScheme NO-LOCK WHERE
             TopupScheme.Brand = gcBrand AND
             TopupScheme.TopupScheme = icTopupScheme,
       FIRST TopupSchemeRow OF TopupScheme NO-LOCK WHERE
             TopupSchemeRow.EndStamp >= ldOfferFrom AND
             TopupSchemeRow.BeginStamp <= ldOfferTo:
             
      ldTopup = TopupSchemeRow.Amount - TopupSchemeRow.DiscountAmount.       
   END.
   
   RETURN ldTopup.
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST Offer WHERE 
           Offer.Brand = gcBrand AND
           Offer.Offer = icOffer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Offer THEN DO:
   MESSAGE "Offer not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

ASSIGN
   ldOfferFrom = fMake2DT(Offer.FromDate,0)
   ldOfferTo   = fMake2DT(Offer.ToDate,86399).

RUN local-Find-First.

IF AVAILABLE OfferItem THEN ASSIGN
   Memory       = recid(OfferItem)
   must-print   = TRUE
   must-add     = FALSE
   ldDefFrom    = fMake2DT(TODAY,0).
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No items available" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE
      ldDefFrom    = fMake2DT(Offer.FromDate,0).
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a OfferItem  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY icOffer @ OfferItem.Offer.

           i = 1. 
           FOR EACH OfferItem NO-LOCK BY OfferItem.OfferItemId DESC:
              i = OfferItem.OfferItemID + 1.
              LEAVE.
           END.
 
           CREATE OfferItem.
           ASSIGN 
              OfferItem.Brand   = gcBrand 
              OfferItem.OfferItemID = i
              OfferItem.Offer   = icOffer
              OfferItem.BeginStamp = ldDefFrom.
              OfferItem.EndStamp   = fMake2DT(12/31/2049,86399).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              OfferItem.ItemType = ""  
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOfferItem).

           ASSIGN
           Memory = recid(OfferItem)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE OfferItem THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND OfferItem WHERE recid(OfferItem) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OfferItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OfferItem).
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
        ufk   = 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7] = 1752
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0.
 
        ELSE IF NOT ilUpdate THEN ASSIGN
           ufk[5] = 0
           ufk[6] = 0.
          
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OfferItem.ItemType {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OfferItem.ItemType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,7,f7,8,f8") = 0 THEN DO:
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
        FIND OfferItem WHERE recid(OfferItem) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OfferItem THEN
              ASSIGN FIRSTrow = i Memory = recid(OfferItem).
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
           IF NOT AVAILABLE OfferItem THEN DO:
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
                rtab[1] = recid(OfferItem)
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
           IF NOT AVAILABLE OfferItem THEN DO:
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
              rtab[FRAME-DOWN] = recid(OfferItem).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OfferItem WHERE recid(OfferItem) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OfferItem THEN DO:
           Memory = recid(OfferItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OfferItem THEN Memory = recid(OfferItem).
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
           FIND OfferItem WHERE recid(OfferItem) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          OfferItem.ItemType
          OfferItem.ItemKey
          ldItemAmount.

       RUN local-find-NEXT.
       IF AVAILABLE OfferItem THEN Memory = recid(OfferItem).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE OfferItem THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(OfferItem).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          OfferItem.ItemType
          OfferItem.ItemKey
          ldItemAmount.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOfferItem).

           DELETE OfferItem.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE OfferItem THEN DO:
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
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOfferItem).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOfferItem).

       RUN local-disp-row.
       xrecid = recid(OfferItem).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OfferItem) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OfferItem) must-print = TRUE.
        NEXT LOOP.
     END.
         
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO: 
        RUN Mc/eventsel.p("offeritem", "#BEGIN" + chr(255) + gcBrand + chr(255) + icOffer).
        ufkey = TRUE.
        NEXT.
     END.   

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND OfferItem WHERE recid(OfferItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OfferItem WHERE recid(OfferItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST OfferItem WHERE 
      OfferItem.Brand = gcBrand AND
      OfferItem.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST OfferItem WHERE 
      OfferItem.Brand = gcBrand AND
      OfferItem.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT OfferItem WHERE 
      OfferItem.Brand = gcBrand AND
      OfferItem.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV OfferItem WHERE 
      OfferItem.Brand = gcBrand AND
      OfferItem.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      OfferItem.ItemType
      OfferItem.ItemKey
      lcItemName
      ldItemAmount
      ldtFromDate
      ldtToDate
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR liTime AS INT NO-UNDO.
   
   fSplitTS(OfferItem.BeginStamp,
             OUTPUT ldtFromDate,
             OUTPUT liTime).
   fSplitTS(OfferItem.EndStamp,
            OUTPUT ldtToDate,
            OUTPUT liTime).

   fItemName(OfferItem.ItemType,
             OfferItem.ItemKey).

   CASE OfferItem.ItemType:
   WHEN "PerContract" THEN DO:
      IF OfferItem.ItemKey BEGINS "PAYTERM" THEN
         ldItemAmount = OfferItem.Amount.
      ELSE
         ldItemAmount = fPenaltyFeeAmount(OfferItem.ItemKey).
   END.
   WHEN "Topup"       THEN ldItemAmount = fTopUpAmount(OfferItem.ItemKey).
   OTHERWISE ldItemAmount = OfferItem.Amount.
   END CASE. 
            
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llUpdateAmount AS LOG  NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      ASSIGN
         lcBegin = fTS2HMS(OfferItem.BeginStamp)
         lcEnd   = fTS2HMS(OfferItem.EndStamp).
     
      fItemType(OfferItem.ItemType).
         
      DISP 
         OfferItem.Offer        
         Offer.Description
         OfferItem.OfferItemID
         OfferItem.ItemType
         OfferItem.ItemKey        
         lcItemName
         ldItemAmount @ OfferItem.Amount
         lcDiscount
         OfferItem.Periods
         OfferItem.VatIncl
         OfferItem.BeginStamp
         lcBegin
         OfferItem.EndStamp
         lcEnd
         OfferItem.DispInUI
         OfferItem.DispOnInvoice
      WITH FRAME lis.

      llUpdateAmount = (LOOKUP(OfferItem.ItemType,"Topup,PerContract") = 0).
      
      IF NOT NEW OfferItem THEN DO:
      
         ASSIGN 
            llUpdateAmount = FALSE
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND ilUpdate
            ufk[6] = 1752
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.
      
         IF toimi = 6 THEN DO: 
            RUN Mc/eventsel.p("offeritem", "#BEGIN" + chr(255) + OfferItem.Brand + chr(255) + OfferItem.Offer
               + chr(255) + STRING(OfferItem.OfferItemId)).
            LEAVE.
         END.   
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT OfferItem EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE
            OfferItem.ItemType   WHEN NEW OfferItem
            OfferItem.ItemKey    WHEN NEW OfferItem
            OfferItem.Amount     WHEN llUpdateAmount
            OfferItem.VatIncl    WHEN llUpdateAmount
            OfferItem.BeginStamp WHEN NEW OfferItem
            OfferItem.EndStamp
            OfferItem.DispInUI
            OfferItem.DispOnInvoice
         WITH FRAME lis EDITING:
 
            READKEY.

            IF FRAME-FIELD = "Amount" THEN DO:
               IF INPUT OfferItem.ItemType = "topup" THEN DO:
                  NEXT-PROMPT OfferItem.BeginStamp.
                  NEXT.
               END.
            END.
            
            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"ItemType,ItemKey") > 0 
            THEN DO:

               IF FRAME-FIELD = "ItemType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "OfferItem", 
                                       "ItemType", 
                                       "Offer", 
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN 
                     DISP lcCode @ OfferItem.ItemType WITH FRAME lis.
               END.
             
               ELSE IF LOOKUP(FRAME-FIELD,"ItemKey") > 0 THEN DO:
                  
                  CASE INPUT FRAME lis OfferItem.ItemType:
                  WHEN "BillItem"    THEN lcHelp = "Help/nntuse.p".
                  WHEN "FATime"      THEN lcHelp = "Help/h-fatgroup.p".
                  WHEN "Topup"       THEN lcHelp = "Mm/topupscheme.p".
                  WHEN "PerContract" THEN lcHelp = "Help/h-daycamp.p".
                  WHEN "ServicePackage" THEN lcHelp = "Help/h-servpa.p".
                  WHEN "DiscountPlan" THEN lcHelp = "Help/h-discountplan.p".
                  OTHERWISE lcHelp = "".
                  END CASE.
                  
                  IF lcHelp > "" THEN DO:
                     ASSIGN 
                        gcHelpParam  = "offeritem"
                        siirto = ?.

                     RUN VALUE(lcHelp).
                     
                     gcHelpParam = "".

                     IF siirto NE "" AND siirto NE ? THEN 
                        DISPLAY siirto @ OfferItem.ItemKey WITH FRAME lis.
                  END.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "ItemType" THEN DO:
                  IF INPUT FRAME lis OfferItem.ItemType = "" THEN LEAVE.
                  fItemType(INPUT INPUT FRAME lis OfferItem.ItemType).
                  IF lcType = "" THEN DO:
                     MESSAGE "Unknown item type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "ItemKey" THEN DO:
                  fItemName(INPUT INPUT FRAME lis OfferItem.ItemType,
                            INPUT INPUT FRAME lis OfferItem.ItemKey).
                  IF lcItemName = "" THEN DO:
                     MESSAGE "Unknown item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  DISPLAY lcItemName WITH FRAME lis.

                  IF INPUT FRAME lis OfferItem.ItemType = "DiscountPlan" AND
                     AVAILABLE DiscountPlan THEN DO:
                     OfferItem.Periods = DiscountPlan.ValidPeriods.
                     DISPLAY lcDiscount OfferItem.Periods WITH FRAME lis.
                  END.
                  
                  IF INPUT FRAME lis OfferItem.ItemType = "Topup" THEN DO:
                     DISPLAY llTopupVat @ OfferItem.VatIncl WITH FRAME lis.
                     DISPLAY fTopUpAmount(INPUT INPUT OfferItem.ItemKey)
                         @ OfferItem.Amount WITH FRAME lis.
                  END.       
               END.

            END.
            
            APPLY LASTKEY.
         END.

         IF OfferItem.ItemType = "Topup" THEN
            OfferItem.VatIncl = llTopupVat.

         IF OfferItem.ItemType = "PerContract" THEN 
            OfferItem.Amount = 0.
            
         LEAVE UpdateField.
         
      END.
      
      IF NEW OfferItem THEN DO:
         ldDefFrom = OfferItem.BeginStamp.
         LEAVE.
      END.
   END.

END PROCEDURE.

