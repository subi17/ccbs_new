/* -----------------------------------------------
  MODULE .......: reqstatmaint.p
  FUNCTION .....: Maintain request statuses and functions 
  APPLICATION ..: TMS
  AUTHOR .......: JT
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */
 
{Syst/commali.i}

DEFINE INPUT PARAMETER iiType  AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttStatFunc
 FIELD RowId     AS INTEGER
 FIELD FuncVal   AS INTEGER
 FIELD FuncText  AS CHARACTER
 INDEX Order FuncVal RowId.


DEFINE TEMP-TABLE ttStatMenu
 FIELD RowInd      AS INTEGER
 FIELD StatVal     AS INTEGER
 FIELD StatusText  AS CHARACTER
 INDEX StatVal StatVal.

DEF VAR menuc     AS C  EXTENT 17 FORMAT "X(54)"   NO-UNDO.
DEF VAR llCalc    AS LOG                           NO-UNDO INIT TRUE.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR liQty     AS INT                           NO-UNDO. 
DEF VAR lcName    AS CHAR                          NO-UNDO. 
DEF VAR liInvType AS INT                           NO-UNDO.
DEF VAR liLoop    AS INTEGER NO-UNDO.
DEF VAR liStatCount AS INTEGER NO-UNDO INIT 0.
DEF VAR lcStatText AS CHARACTER EXTENT 17.
DEF VAR lcRowText  AS CHARACTER NO-UNDO.
DEF VAR liFill     AS INTEGER NO-UNDO.
DEF VAR lcSelect   AS CHARACTER NO-UNDO. 
DEF VAR lcSpace    AS CHAR      NO-UNDO.
DEF VAR liRowCount AS INTEGER NO-UNDO INIT 0.
DEF VAR liCount    AS INTEGER NO-UNDO.
DEF VAR lcKey      AS CHARACTER NO-UNDO.
DEF VAR lcReturn   AS CHARACTER NO-UNDO.
DEF VAR ReqStat   AS INTEGER   NO-UNDO.

FORM "New status:" ReqStat FORMAT "z9"

 WITH  OVERLAY ROW 4 centered
 COLOR VALUE(cfc)
 TITLE COLOR VALUE(ctc) " Add status "
 NO-LABELS
 FRAME upd.

RUN pInitMenuText.


/*************************** Browsing part **********************************/
DO WHILE TRUE: 
   
   ASSIGN ufk    = 0
          ufk[1] = 0
          ufk[2] = 0
          
          ufk[5] = 11
          ufk[6] = 6970
          ufk[7] = 4
          ufk[8] = 8
          ehto   = 3.

      RUN ufkey.
   
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
           lcStatText[13] @ menuc[13] SKIP
           lcStatText[14] @ menuc[14] SKIP
           lcStatText[15] @ menuc[15] SKIP
           lcStatText[16] @ menuc[16] SKIP
           lcStatText[17] @ menuc[17] SKIP
   WITH OVERLAY FRAME choices NO-LABELS.
   
   CHOOSE FIELD menuc NO-ERROR AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " Select available statuses for request type: "  
   CENTERED ROW 1 WITH 1 COL.
   lcKey = KEYLABEL(LASTKEY).
   
   IF LOOKUP(lcKey,"DELETE,F7,7") > 0 THEN DO:
      IF FRAME-INDEX > liRowCount THEN DO:
         MESSAGE "Empty row, nothing to delete!" VIEW-AS ALERT-BOX.
         NEXT.
      END.
      FIND ttStatMenu WHERE
           ttStatMenu.RowInd = FRAME-INDEX
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttStatMenu THEN DO:
         MESSAGE "No statuses for request type:" iiType SKIP
                 "Add status to edit its functions!" 
         VIEW-AS ALERT-BOX.
         NEXT.
      END.
      FIND MsReqStatFunc WHERE 
           MsReqStatFunc.ReqType = iiType AND
           MsReqStatFunc.ReqStat = ttStatMenu.StatVal AND
           LENGTH(MsReqStatFunc.FuncGroup) < 3
      EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAILABLE MsReqStatFunc THEN DO:
         MESSAGE "Remove functions from status before deleting!"
         VIEW-AS ALERT-BOX.
         NEXT.
      END.
      
      ASSIGN MsReqStatFunc.FuncGroup = "".
      RUN pInitMenuText.
   
   END.
   
   IF LOOKUP(lcKey,"CURSOR-RIGHT,RETURN,5,F5") > 0 THEN DO:
      IF FRAME-INDEX > liRowCount THEN DO:
         MESSAGE "Empty row, nothing to edit!" VIEW-AS ALERT-BOX.
         NEXT.
      END.

      /* Get Status from temp table, Type from INPUT PARAMETER */
      FIND ttStatMenu WHERE
           ttStatMenu.RowInd = FRAME-INDEX
      NO-LOCK NO-ERROR.
      
      /* Get RECID using Type AND Status, use RECID to store
         selection from reqfuncitembr.p */

      FIND MsReqStatFunc WHERE
           MsReqStatFunc.ReqType   = iiType AND
           MsReqStatFunc.ReqStatus = ttStatMenu.StatVal
      NO-LOCK NO-ERROR.

      
      /* Open browser for request function items, send
         MsRequest Type and MsRequest Status to store
         values with browser */
      
      RUN msreqfuncitem(MsReqStatFunc.FuncGroup,
                        OUTPUT lcReturn).
      FIND CURRENT MsReqStatFunc EXCLUSIVE-LOCK.
      ASSIGN MsReqStatFunc.FuncGroup = lcReturn.
      RUN pInitMenuText.
      
   END.
   

   IF LOOKUP(lcKey,"6,F6") > 0 THEN DO:
      /* Ask for status to be updated / added, 0 is that msrequest
         has this status available */
      UPDATE ReqStat WITH FRAME upd.
      
      FIND FIRST TMSCodes WHERE
                 TMSCodes.TableName = "MsRequest" AND
                 TMSCodes.FieldName = "ReqStatus" AND
                 TMSCodes.CodeValue = STRING(ReqStat)
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSCodes THEN DO:
         MESSAGE "Status does not exist!" VIEW-AS ALERT-BOX.
         NEXT.
      END.
      FIND MsReqStatFunc WHERE
           MsReqStatFunc.ReqType   = iiType AND
           MsReqStatFunc.ReqStatus = INPUT FRAME upd ReqStat
      EXCLUSIVE-LOCK NO-ERROR.
      
      
      IF NOT AVAILABLE MsReqStatFunc THEN DO:
         CREATE MsReqStatFunc.
         ASSIGN MsReqStatFunc.ReqType = iiType 
                MsReqStatFunc.ReqStat = INPUT FRAME upd ReqStat
                MsReqStatFunc.FuncGroup = "0".
      END.
      ELSE IF AVAILABLE MsReqStatFunc THEN DO:
         IF MsReqStatFunc.FuncGroup = "" THEN DO:
            ASSIGN MsReqStatFunc.FuncGroup = "0".
         END.
         ELSE DO:                                      
            IF MsReqStatFunc.FuncGroup > "" THEN DO:
               MESSAGE "Status already exists with this request type!"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.
         END.
      END.
      HIDE FRAM upd.
      RUN pInitMenuText.
   END.
   
   IF LOOKUP(lcKey,"8,F8") > 0 THEN DO:
      CLEAR FRAME choises.
      LEAVE.
   END.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.

PROCEDURE pInitMenuText.

/* Reset menu */
FOR EACH ttStatMenu EXCLUSIVE-LOCK:
   DELETE ttStatMenu.
END.
DO liLoop = 1 TO 17:
   lcStatText[liLoop] = "".
END.

DEF VAR lcGrp AS CHAR NO-UNDO.

FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "msrequest" AND
         TMSCodes.FieldName BEGINS "ReqStatus":
   FIND FIRST MsReqStatFunc NO-LOCK WHERE
              MsReqStatFunc.ReqType = iiType AND
              MsReqStatFunc.ReqStatus = INT(TMSCodes.CodeValue) AND
              MsReqStatFunc.FuncGroup > ""
   NO-ERROR.
   IF NOT AVAILABLE MsReqStatFunc THEN NEXT.
   IF LENGTH(TMSCodes.CodeValue) > 1 THEN lcSpace = " ".
   ELSE lcSpace = "  ".
      liRowCount = liRowCount + 1.
      CREATE ttStatMenu.

      ASSIGN ttStatMenu.StatVal    = INT(TMSCodes.CodeValue)
             ttStatMenu.StatusText = "  " +
                                     TMSCodes.CodeValue +
                                     lcSpace +
                                     TMSCodes.CodeName  +
                                     "  ".
END.

liRowCount = 0.

FOR EACH ttStatMenu EXCLUSIVE-LOCK:
    FIND MsReqStatFunc WHERE
     MsReqStatFunc.ReqType = iiType AND
     MsReqStatFunc.ReqStat = ttStatMenu.StatVal
    NO-LOCK NO-ERROR.

    IF AVAIL MsReqStatFunc THEN lcGrp = SUBSTRING(MsReqStatFunc.FuncGroup,3).
     ELSE lcGrp = "". 
    ASSIGN liRowCount = liRowCount + 1
          ttStatMenu.RowInd = liRowCount
          liFill = 40 - LENGTH(ttStatMenu.StatusText)
          lcRowText = ttStatMenu.StatusText + FILL(" ",liFill) + lcGrp
          lcStatText[ttStatMenu.RowInd] = lcRowText.
          
END.
END.
