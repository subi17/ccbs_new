
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
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable FMItem

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FMItem'}
{Syst/tmsconst.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFMItem AS HANDLE NO-UNDO.
   lhFMItem = BUFFER FMItem:HANDLE.
   RUN StarEventInitialize(lhFMItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhFMItem).
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
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
 
 /* cc admin tool  mode forms ------------------------------------ */

DEFINE VARIABLE lcpaytype AS CHARACTER NO-UNDO.
DEFINE VARIABLE lipaytype AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcPriceListPostpaid AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPriceListPrepaid AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBIGroup AS CHARACTER NO-UNDO. 

FIND TMSParam WHERE TMSParam.Brand = "1" AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "FMItemPriceListPostpaid" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN lcPriceListPostpaid = TMSParam.CharVal.

FIND TMSParam WHERE TMSParam.Brand = "1" AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "FMItemPriceListPrepaid" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN lcPriceListPrepaid = TMSParam.CharVal.

FIND TMSParam WHERE TMSParam.Brand = "1" AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "BIGroup" NO-LOCK NO-ERROR.
 IF AVAIL TMSParam THEN lcBIGroup = TMSParam.CharVal.


{Func/dialog.i}
 /* create records in ttable  for bill items */
FOR EACH BillItem WHERE LOOKUP(BillItem.BIGroup , lcBIGroup ) > 0 AND
                        BillItem.Brand = "1" NO-LOCK:
       CREATE ttable. 
       ASSIGN ttable.ValueId = BillItem.BillCode
              ttable.Description = BillItem.BIName.
END.



form

    FMItem.BillCode       format "x(12)"  column-label "ProdC"
    BillItem.BIName       format "x(15)"
    FMItem.FromDate
    FMItem.ToDate      
    lcpaytype             format "x(15)" column-label "Valid for "
    FMItem.Amount        format "->,>>9.99"

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Items of B-Event " + 
        FeeModel.Brand + "/" + FeeModel.FeeModel + ": " + 
        FeeModel.FeeName + " "
    FRAME sel.

form
    
    lipaytype   FORMAT ">" LABEL "Applied to "
       HELP "1=Postpaid 2=Prepaid" lcpaytype NO-LABEL FORMAT "X(20)" SKIP                                                 
    FMItem.BillCode  FORMAT "x(16)" 
    BillItem.BIName  NO-LABEL SKIP

    FMItem.FromDate LABEL "Valid " FORMAT "99-99-9999" 
       "-"  
    FMItem.ToDate NO-LABEL FORMAT "99-99-9999" SKIP

    FMItem.Amount  FORMAT "->,>>9.99" SKIP
         
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.



form /* seek Billing Event Item  BY BillCode */
    BillCode
    HELP "Enter BillCode"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BillCode "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



FUNCTION fFetchPriceList RETURN LOGICAL ():
    IF FMItem.PriceList = lcPriceListPostpaid THEN 
       ASSIGN lipaytype = 1 
              lcpaytype = "Postpaid".
    ELSE IF FMItem.PriceList = lcPriceListPrepaid THEN 
       ASSIGN lipaytype = 2
              lcpaytype = "Prepaid".
    RETURN TRUE.
END FUNCTION.

 {Func/brand.i} 

 /*call cui browser -----------------------------*/

{Mc/beitem.i 'cc'}

/* ---------------------------------------------- */

PROCEDURE local-disp-row:

       RUN local-find-others.

       fFetchPriceList().

       CLEAR FRAME sel  NO-PAUSE.
       DISPLAY 
       FMItem.BillCode
       BillItem.BIName      WHEN AVAIL BillItem
       FMItem.FromDate
       FMItem.ToDate
       lcpaytype
       FMItem.Amount
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE choose-row:

 IF order = 1 THEN DO:
        CHOOSE ROW FMItem.BillCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FMItem.BillCode WITH FRAME sel.
      END.
END PROCEDURE.


PROCEDURE highlight-row:

 COLOR DISPLAY VALUE(ctc)
       FMItem.BillCode
       BillItem.BIName      WHEN AVAIL BillItem
       FMItem.FromDate
       FMItem.ToDate
       lcpaytype
       FMItem.Amount.

END PROCEDURE.

PROCEDURE local-update-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      IF FMItem.InclBillCode > "" THEN 
         FIND bBillItem WHERE 
              bBillItem.Brand    = FMItem.Brand AND 
              bBillItem.BillCode = FMItem.BillCode NO-LOCK NO-ERROR.

      fFetchPriceList().

      DISP
          lipaytype 
          lcpaytype
          FMItem.BillCode
          BillItem.BIName    WHEN AVAIL BillItem
          FMItem.FromDate 
          FMItem.ToDate
          FMItem.Amount
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:

         UPDATE 
             FMItem.ToDate
             FMItem.Amount 
             WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:

                PAUSE 0.
               
               IF FRAME-FIELD = "ToDate" THEN DO:
                   IF INPUT FMItem.ToDate = ? OR
                      INPUT FMItem.ToDate = "" THEN  DO:
                         NEXT-PROMPT FMItem.todate.
                         NEXT.
                   END.      
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      END.
      ELSE PAUSE.
      LEAVE.
   END.


END PROCEDURE.

PROCEDURE local-add-record: 
      
        
        /*only one FMItem per FeeModel */
        FIND FIRST FMItem OF FeeModel NO-LOCK NO-ERROR.
        
        IF AVAIL FMItem THEN DO:
           MESSAGE "Forbidden to add more than one billing item" VIEW-AS ALERT-BOX. 
           ASSIGN
             must-add   = FALSE
             must-print = TRUE
             ufkey      = TRUE.
            RETURN.
        END.
        
        DEFINE VARIABLE lcPriceList AS CHARACTER NO-UNDO.
          /* dialog to fetch the charge/compensation billing item */
       
          DEFINE VARIABLE lctitle AS CHARACTER INITIAL "Select Billing Item " NO-UNDO.
          DEFINE VARIABLE lrecid AS RECID NO-UNDO.
          DEFINE VARIABLE loutValueId AS CHARACTER NO-UNDO. 


          RUN Help/h-dialog.p (INPUT TABLE ttable BY-REFERENCE ,
                          INPUT lctitle,
                          OUTPUT lrecid,
                          OUTPUT loutValueId).
          
          FIND ttable WHERE RECID(ttable) = lrecid NO-LOCK NO-ERROR.

          IF NOT AVAIL ttable THEN DO:
             ASSIGN
                must-add   = FALSE
                must-print = TRUE
                ufkey      = TRUE.
             RETURN.
          END.
          
       FIND BillItem WHERE 
            BillItem.Brand = gcBrand AND 
            BillItem.BillCode = ttable.ValueId NO-LOCK NO-ERROR.

      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
      
      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           ASSIGN 
            lipaytype = 1
            lcpaytype = "Postpaid".

           DISPLAY BillItem.BIName WITH FRAME lis.

            PROMPT-FOR 
              lipaytype
              FMItem.FromDate
           WITH FRAME lis EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
                 UNDO add-row, leave add-row.

              END.


              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF FRAME-FIELD = "lipaytype" THEN DO: 
                    IF (INPUT lipaytype) > 2 OR (INPUT lipaytype) < 1
                   THEN DO:
                      BELL.
                      MESSAGE "Invalid value".
                      NEXT.
                   END.
                   
                   IF INPUT lipaytype = 1 THEN DO:
                       IF BillItem.BIGroup = {&BITEM_GRP_COMPENSATION} THEN DO:
                          BELL.
                          MESSAGE "Postpaid Compensation are not allowed".
                          NEXT.
                       END.
                       lcpaytype = "Postpaid".
                       lcPriceList = lcPriceListPostpaid.
                    END.
                    ELSE IF INPUT lipaytype = 2 THEN DO:
                      lcpaytype = "Prepaid".
                      lcPriceList = lcPriceListPrepaid.
                    END.
                  
                   DISP lcpaytype WITH FRAME lis.
                 END.   

                  
                 IF FRAME-FIELD = "fromdate" THEN DO:
                    IF INPUT FMItem.FromDate = ? OR
                       INPUT FMItem.FromDate = "" THEN  DO:
                          NEXT.
                    END.
                 END.
                 
              END.
              APPLY LASTKEY.
           END.   

           lipaytype  = INPUT FRAME lis lipaytype.

           FIND FIRST FMItem WHERE
                      FMItem.Brand     = FeeModel.Brand          AND
                      FMItem.FeeModel  = FeeModel.FeeModel       AND
                      FMItem.PriceList = lcPricelist             AND
                      FMItem.BillCode  = BillItem.BillCode   AND
                      FMItem.ToDate    > INPUT FMItem.FromDate
                      NO-LOCK NO-ERROR.
           IF AVAILABLE FMItem THEN DO: 
              BELL.
              MESSAGE
                 "There is already an item " SKIP
                 "for" lcpaytype "and"   SKIP
                 "product code" BillItem.BillCode "AND" SKIP
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
           FMItem.PriceList = lcPriceList
           FMItem.BillCode  = BillItem.BillCode
           FMItem.FromDate  = INPUT FRAME lis FMItem.FromDate
           FMItem.BillMethod = TRUE. 
           FMItem.BillType  = "CC".
           

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFMItem).

           ASSIGN
           memory = recid(FMItem)
           xrecid = memory.
           LEAVE.
        END.
        LEAVE.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.

END PROCEDURE.


