/* ----------------------------------------------------------------------
  MODULE .......: FMItem.P
  TASK .........: UPDATE Billing Event Items
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp  urights added
                  10-01-00 jpo Added BillType
                  28-12-01 jpo NEW FRAME FORMAT AND BillCycle FIELD
                  11.03.03 tk  tokens
                  24.06.03 aam FromDate, ToDate, InclAmt etc.,
                               eventlog
                  05.09.03 aam brand             
                  05.12.03 jp  Validation for fromdate and todate
                  30.05.07 as  Don't accept adding new billing item if same
                               active item allready exists.
                  18.05.09 rdv cui browser has been separated to beitem.i to
                               give more modularity
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable FMItem

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FMItem'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFMItem AS HANDLE NO-UNDO.
   lhFMItem = BUFFER FMItem:HANDLE.
   RUN StarEventInitialize(lhFMItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhFMItem).
   END.

END.


DEF INPUT PARAMETER    FeeModel LIKE FeeModel.FeeModel NO-UNDO.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR lcUnit       AS CHAR                   NO-UNDO. 
DEF VAR llServlimit  AS log                    NO-UNDO FORMAT "X/".
DEF VAR lcServicelname AS CHAR                 NO-UNDO format "X(25)".
DEF VAR lcToDate     AS CHAR                   NO-UNDO.
DEF VAR lcBroken     AS CHAR                   NO-UNDO.
DEF VAR lcFirstMonth AS CHAR                   NO-UNDO.
DEF BUFFER bBillItem FOR BillItem.
 



form
    FMItem.PriceList      format "x(10)"
    FMItem.BillCode       format "x(12)"  column-label "ProdC"
    BillItem.BIName       format "x(15)"
    FMItem.FromDate       
    FMItem.BillMethod
    FMItem.Interval      column-label "I"
    FMItem.BillCycle     column-label "BM"
    llservlimit          Column-label "SL" 
    FMItem.Amount        format "->,>>9.999"

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Items of B-Event " + 
        FeeModel.Brand + "/" + FeeModel.FeeModel + ": " + 
        FeeModel.FeeName + " "
    FRAME sel.

form
    FMItem.PriceList   FORMAT "X(16)" COLON 20                          
       PriceList.PLName NO-LABEL SKIP                              
    FMItem.BillCode     COLON 20 FORMAT "x(16)"                           
       BillItem.BIName  NO-LABEL SKIP                             
    FMItem.FromDate     COLON 20 LABEL "Valid" FORMAT "99-99-9999" 
       "-"  
       FMItem.ToDate NO-LABEL FORMAT "99-99-9999" SKIP
    FMItem.BillType     COLON 20 SKIP
    FMItem.BillMethod   COLON 20 SKIP
    FMItem.Interval     COLON 20 SKIP
    FMItem.BillCycle    COLON 20 SKIP
    FMItem.FFItemQty    COLON 20  FORMAT ">>>"  SKIP
    FMItem.FFEndDate    COLON 20 SKIP
    FMItem.Amount       COLON 20  FORMAT "->,>>9.999" SKIP
    FMItem.FirstMonthBR COLON 20 
       LABEL "First Fee"
       HELP "Fee for 1. month; 0=broken rental, 1=full month, 2=usage based"
       VALIDATE(INPUT FMItem.FirstMonthBR <= 2,
                "Valid values are 0-2")
       lcFirstMonth NO-LABEL FORMAT "X(20)" SKIP         
    FMItem.BrokenRental COLON 20 
       LABEL "Last Fee"
       HELP "Last month fee; 0=broken rental, 1=full month, 2=usage based"
       VALIDATE(INPUT FMItem.BrokenRental <= 2,
                "Valid values are 0-2")
       lcBroken NO-LABEL FORMAT "X(20)" SKIP
    FMItem.ServiceLimitGroup COLON 20
       lcServicelname    NO-LABEL       SKIP
          
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.



form /* seek Billing Event Item  BY  PriceList */
    PriceList
    HELP "Enter Price List Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND P-LIST "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billing Event Item  BY BillCode */
    BillCode
    HELP "Enter BillCode"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BillCode "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fBrokenRental RETURNS LOGIC
   (INPUT iiBrokenRental AS INT):
            
   IF iiBrokenRental > 2 THEN RETURN FALSE. 
   
   lcBroken = ENTRY(iiBrokenRental + 1,
                    "Broken Rental,Full Month,Usage Based").
   DISPLAY lcBroken WITH FRAME lis.
   
   RETURN TRUE.
   
END FUNCTION.

FUNCTION fFirstMonth RETURNS LOGIC
   (INPUT iiBrokenRental AS INT):
            
   IF iiBrokenRental > 2 THEN RETURN FALSE. 
   
   lcFirstMonth = ENTRY(iiBrokenRental + 1,
                        "Broken Rental,Full Month,Usage Based").
   DISPLAY lcFirstMonth WITH FRAME lis.
   
   RETURN TRUE.
   
END FUNCTION.

 {Func/brand.i} 

 /*-- cui browser in a include file  -----------------------------*/

 {Mc/beitem.i 'general'}

/* --------------------------------------------------------------- */

PROCEDURE local-disp-row:

       RUN local-find-others.

       CLEAR FRAME sel  NO-PAUSE.
       DISPLAY 
       FMItem.PriceList
       FMItem.BillCode
       BillItem.BIName      WHEN AVAIL BillItem
       FMItem.FromDate
       FMItem.BillMethod
       FMItem.Interval
       FMItem.BillCycle
       FMItem.Amount
       llservlimit

       WITH FRAME sel.
END PROCEDURE.


PROCEDURE local-update-record:

   DISPLAY
      FMItem.BillCode
      FMItem.PriceList 
      FMItem.FromDate
   WITH FRAME lis.

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      IF FMItem.InclBillCode > "" THEN 
         FIND bBillItem WHERE 
              bBillItem.Brand    = FMItem.Brand AND 
              bBillItem.BillCode = FMItem.BillCode NO-LOCK NO-ERROR.


      DISP
          PriceList.PLName   WHEN AVAIL PriceList
          BillItem.BIName    WHEN AVAIL BillItem
          FMItem.BillType
          FMItem.BillMethod
          FMItem.Interval
          FMItem.FFItemQty
          FMItem.FFEnddate
          FMItem.BillCycle
          FMItem.Amount
          lcServicelname
          FMItem.BrokenRental
          FMItem.FirstMonthBR
      WITH FRAME lis.

      fBrokenRental(FMItem.BrokenRental).
      fFirstMonth(FMItem.FirstMonthBR).
      
      IF lcRight = "RW" THEN DO:

         UPDATE
             FMItem.ToDate
             FMItem.BillType
             FMItem.BillMethod
             FMItem.Interval
             FMItem.BillCycle
             FMItem.FFItemQty
             FMItem.FFEnddate
             FMItem.Amount
             FMItem.FirstMonthBR
             FMItem.BrokenRental
             FMItem.ServiceLimitGroup

         WITH FRAME lis EDITING:
             READKEY.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:

                PAUSE 0.

                IF FRAME-FIELD = "Interval" THEN DO:
                   IF INPUT FRAME lis FMItem.BillMethod /* single */ THEN DO:
                      IF INPUT FRAME lis FMItem.Interval NE 0 THEN DO:
                         BELL.
                         MESSAGE 
                         "This is a SINGLE fee - no Interval allowed !".
                         NEXT.
                      END.   
                   END.
                   ELSE DO:  /* periodical */
                      IF INPUT FRAME lis FMItem.Interval = 0 THEN DO:
                         BELL.
                         MESSAGE 
                         "This is a FIXED fee - Interval must be given !".
                         NEXT.
                      END.   

                   END.
                END.

                ELSE IF FRAME-FIELD = "ToDate" THEN DO:
                   IF INPUT FMItem.ToDate = ? OR
                      INPUT FMItem.ToDate = "" THEN  DO:
                         NEXT-PROMPT FMItem.todate.
                         NEXT.
                   END.      
                END.


                ELSE IF FRAME-FIELD = "ServiceLimitGroup" THEN DO:
                   IF INPUT  servicelimitgroup  ne "" THEN DO:
                       FIND FIRST ServiceLimitGroup WHERE
                                  ServiceLimitGroup.Brand = gcBrand AND 
                                  ServiceLimitGroup.GroupCode = INPUT FRAME lis
                                  ServiceLimitGroup NO-LOCK NO-ERROR.
                       IF NOT AVAIL servicelimitgroup THEN DO:
                          BELL.
                          MESSAGE "Unknown ServiceLimit Group!".
                          NEXT-PROMPT ServiceLimitGroup. NEXT.
                       END.
                       DISP ServiceLimitGroup.Groupname @ lcServiceLName
                       WITH FRAME lis.
                   END.
                END.

                ELSE IF FRAME-FIELD = "BillType" THEN DO:
                   FIND BillType WHERE BillType.BillType =
                      INPUT FRAME lis FMItem.BillType NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillType THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Type !".
                      NEXT.
                   END.
                END.

                ELSE IF FRAME-FIELD = "BrokenRental" THEN DO:
                   IF NOT fBrokenRental(INPUT INPUT FMItem.BrokenRental)
                   THEN DO:
                      BELL.
                      MESSAGE "Invalid value".
                      NEXT.
                   END.
                END.

                ELSE IF FRAME-FIELD = "FirstMonthBR" THEN DO:
                   IF NOT fFirstMonth(INPUT INPUT FMItem.FirstMonthBR)
                   THEN DO:
                      BELL.
                      MESSAGE "Invalid value".
                      NEXT.
                   END.
                END.

                ELSE IF  FRAME-FIELD = "BillMethod" AND
                   INPUT FRAME lis FMItem.BillMethod = TRUE 
                THEN DISP 0 @ FMItem.Interval.

                ELSE IF 
                   (FRAME-FIELD = "FFEndDate" OR
                    FRAME-FIELD = "FFItemQty")           AND
                   INPUT FRAME lis FMItem.FFEndDate NE ? AND
                   INPUT FRAME lis FMItem.FFItemQty NE 0 
                THEN DO:
                   BELL.
                   MESSAGE 
                   "Choose either item quantity or end date, not both.".
                   NEXT.
                END.

             END.
             APPLY LASTKEY.
          END. /* EDITING */
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

PROCEDURE choose-row: 

 IF order = 1 THEN DO:
        CHOOSE ROW FMItem.PriceList {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FMItem.PriceList WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FMItem.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FMItem.BillCode WITH FRAME sel.
      END.

END PROCEDURE.


PROCEDURE highlight-row:

COLOR DISPLAY VALUE(ctc)
          FMItem.PriceList    
          FMItem.BillCode     
          BillItem.BIName     
          FMItem.BillMethod
          FMItem.Interval     
          FMItem.BillCycle    
          FMItem.Amount.

END PROCEDURE.

PROCEDURE local-add-record:

      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
      
      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.


           PROMPT-FOR 
              FMItem.PriceList
              FMItem.BillCode
              FMItem.FromDate
           EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
                 UNDO add-row, leave add-row.

              END.


              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF FRAME-FIELD = "PriceList" THEN DO:
                    IF INPUT FMItem.PriceList = "" THEN 
                    UNDO add-row, LEAVE add-row.

                    FIND PriceList WHERE 
                       PriceList.Brand = FeeModel.Brand AND
                       PriceList.PriceList = 
                       INPUT FMItem.PriceList
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL PriceList THEN DO:
                       BELL.
                       MESSAGE "Unknown Price List !".
                       NEXT.
                    END.
                    DISP PriceList.PLName.
                 END.   

                 ELSE IF FRAME-FIELD = "BillCode" THEN DO:
                    IF INPUT FMItem.BillCode = "" THEN DO:
                       NEXT-PROMPT FMItem.PriceList.
                       NEXT.
                    END.
                    FIND BillItem WHERE 
                       BillItem.Brand = FeeModel.Brand AND
                       BillItem.BillCode = 
                       INPUT FMItem.BillCode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL BillItem THEN DO:
                       BELL.
                       MESSAGE "Unknown BillCode !".
                       NEXT.
                    END.
                 END.   
                 IF FRAME-FIELD = "fromdate" THEN DO:
                    IF INPUT FMItem.FromDate = ? OR
                       INPUT FMItem.FromDate = "" THEN  DO:
                          NEXT-PROMPT FMItem.PriceList.
                          NEXT.
                    END.
                 END.   
              END.
              APPLY LASTKEY.
           END.   

           FIND FIRST FMItem WHERE
                      FMItem.Brand     = FeeModel.Brand          AND
                      FMItem.FeeModel  = FeeModel.FeeModel       AND
                      FMItem.PriceList = INPUT FMItem.PriceList  AND
                      FMItem.BillCode  = INPUT FMItem.BillCode   AND
                      FMItem.ToDate    > INPUT FMItem.FromDate
                      NO-LOCK NO-ERROR.
           IF AVAILABLE FMItem THEN DO: 
              BELL.
              MESSAGE
                 "There is already an item with" SKIP
                 "Price List" INPUT FMItem.PriceList  "AND"   SKIP
                 "product code" INPUT FMItem.BillCode "AND" SKIP
                 "it is valid to date"
                 STRING(FMItem.ToDate,"99/99/9999") "!" SKIP
                 SKIP
                 "Do you want to close it when this item comes active?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
              IF ok THEN DO:
                 FIND CURRENT FMItem EXCLUSIVE-LOCK.
                 ASSIGN FMItem.ToDate = INPUT FMItem.FromDate - 1. 
              END.
              ELSE DO:
                 UNDO add-row, NEXT add-row.                       
              END.
           END.

           CREATE FMItem.
           ASSIGN
           FMItem.Brand     = FeeModel.Brand
           FMItem.FeeModel  = FeeModel.FeeModel
           FMItem.PriceList = INPUT FRAME lis FMItem.PriceList
           FMItem.BillCode  = INPUT FRAME lis FMItem.BillCode
           FMItem.FromDate  = INPUT FRAME lis FMItem.FromDate.
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFMItem).

           ASSIGN
           memory = recid(FMItem)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.

END PROCEDURE.



