/* -----------------------------------------------
  MODULE .......: reqstatmenu.p
  FUNCTION .....: Show available status for request
  APPLICATION ..: TMS
  AUTHOR .......: JT
  CHANGED.......: 31.10.07 jp  new parameter for msrequest
                  05.03.08 jt  replaced index with lookup (caused noob errors)
  
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */
{Syst/commali.i}

DEFINE INPUT PARAMETER iiType  AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttStatMenu
 FIELD FrameIndex  AS INTEGER
 FIELD StatVal     AS INTEGER
 FIELD StatusText  AS CHARACTER
 INDEX FrameIndex FrameIndex.

DEF VAR menuc     AS C  EXTENT 15 FORMAT "X(54)"   NO-UNDO.
DEF VAR llCalc    AS LOG                           NO-UNDO INIT TRUE.
DEF VAR liQty     AS INT                           NO-UNDO. 
DEF VAR lcName    AS CHAR                          NO-UNDO. 
DEF VAR liInvType AS INT                           NO-UNDO.
DEF VAR liLoop    AS INTEGER NO-UNDO.
DEF VAR liStatCount AS INTEGER NO-UNDO INIT 0.
DEF VAR lcStatText AS CHARACTER EXTENT 15.
DEF VAR lcRowText  AS CHARACTER NO-UNDO.
DEF VAR liFill     AS INTEGER NO-UNDO.
DEF VAR lcAmount   AS CHARACTER NO-UNDO INIT "Qty: Not calculated". 
DEF VAR lcRoll     AS CHARACTER NO-UNDO INIT "/ - \\ |". /* anim frames */
DEF VAR liAnim     AS INTEGER   NO-UNDO INIT 1.
DEF VAR liSlow     AS INTEGER   NO-UNDO.
DEF VAR lcSpace    AS CHAR      NO-UNDO.
FORM
    "Processing:" lcRoll FORMAT "x(2)"

    WITH ROW 6 OVERLAY 1 DOWN
    COLOR VALUE(cfc)
    CENTERED NO-LABEL
FRAME Calc.


RUN pInitMenu.

FOR EACH ttStatMenu NO-LOCK:
   liFill = 34 - LENGTH(ttStatMenu.StatusText).  

   IF ttStatMenu.StatVal <> 2 THEN
      lcRowText = ttStatMenu.StatusText + FILL(" ",liFill) + lcAmount.
   ELSE
      lcRowText = ttStatMenu.StatusText + FILL(" ",liFill) + "Qty: Not allowed".
   lcStatText[ttStatMenu.FrameIndex] = lcRowText.
END.



STATS:
DO WHILE TRUE: 
   ASSIGN ufk = 0 ufk[4] = 6902 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
   
   DISPLAY lcStatText[1]  @ menuc[1] SKIP
           lcStatText[2]  @ menuc[2] SKIP   
           lcStatText[3]  @ menuc[3] SKIP
           lcStatText[4]  @ menuc[4] SKIP
           lcStatText[5]  @ menuc[5] SKIP
           lcStatText[6]  @ menuc[6] SKIP
           lcStatText[7]  @ menuc[7] SKIP
           lcStatText[8]  @ menuc[8] SKIP
           lcStatText[9]  @ menuc[9] SKIP
           lcStatText[10] @ menuc[10] SKIP
           lcStatText[11] @ menuc[11] SKIP
           lcStatText[12] @ menuc[12] SKIP

   WITH OVERLAY FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE "Available type statuses"  CENTERED ROW 2 WITH 1 COL.
   HIDE FRAME choices.

   
     
   IF LOOKUP(KEYLABEL(LASTKEY),"4,F4") > 0  THEN DO:
      /* Exclude "Done" status from counting */
      FIND ttStatMenu WHERE 
           ttStatMenu.FrameIndex = FRAME-INDEX
      NO-LOCK NO-ERROR.
      IF AVAIL ttStatMenu AND ttStatMenu.StatVal <> 2 THEN DO:
         liStatCount = 0.
         ETIME(Yes).
         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.ReqType   = iiType AND
                  MsRequest.ReqStatus = ttStatMenu.StatVal AND
                  MsRequest.Brand     = "1":
            liStatCount = liStatCount + 1.
            IF liStatCount MOD 100 = 0 THEN liSlow = liSlow + 1.
            IF liSlow > 25 THEN ASSIGN liAnim = liAnim + 1
                                       liSlow = 0.
               
            IF liAnim > 4 THEN liAnim = 1.
            DISP ENTRY(liAnim,lcRoll," ") WITH FRAME Calc.
            PAUSE 0.
         END.
         lcAmount = "Qty: " + STRING(liStatCount).
         liFill = 34 - LENGTH(ttStatMenu.StatusText).
         lcRowText = ttStatMenu.StatusText + FILL(" ",liFill) + lcAmount.
         lcStatText[ttStatMenu.FrameIndex] = lcRowText.
      END.
      HIDE FRAM Calc.
   END.

   
   IF LOOKUP(KEYLABEL(LASTKEY),"RETURN")  > 0  THEN DO:
      FIND ttStatMenu WHERE 
           ttStatMenu.FrameIndex = FRAME-INDEX
      NO-LOCK NO-ERROR.
      RUN Mm/msrequest(iiType,ttStatMenu.StatVal,0,0,0,"").
      NEXT.
   
   END.
   
   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN RETURN.
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.

PROCEDURE pInitMenu.
   
   DEF VAR liRowCount AS INTEGER NO-UNDO INIT 0.
   DEFINE VARIABLE lcStats AS CHARACTER NO-UNDO.

   FOR FIRST TMSCodes NO-LOCK WHERE
            TMSCodes.TableName  = "requem"    AND
            TMSCodes.CodeGroup  = "SubsLMenu" AND
            iiType = INT(ENTRY(1,TMSCodes.FieldName,",")) USE-INDEX TableNAme:
      
      lcStats = SUBSTRING(TMSCodes.FieldName,
                         INDEX(tmscodes.fieldname,",") + 1,
                         LENGTH(TMSCodes.FieldName)).
   END.

   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "msrequest" AND
            TMSCodes.FieldName = "ReqStatus":
      
      IF LOOKUP(TMSCodes.CodeValue,lcStats,",") > 0 THEN DO: 
         
         IF LENGTH(TMSCodes.CodeValue) > 1 THEN lcSpace = " ".
         ELSE lcSpace = "  ".
         liRowCount = liRowCount + 1.
         
         CREATE ttStatMenu.
        
         ASSIGN ttStatMenu.FrameIndex = liRowCount
                ttStatMenu.StatVal    = INT(TMSCodes.CodeValue)
                ttStatMenu.StatusText = "  " + 
                                        TMSCodes.CodeValue + 
                                        lcSpace + 
                                        TMSCodes.CodeName  + 
                                        "  ".
      
              
      END.
   END.
END.

