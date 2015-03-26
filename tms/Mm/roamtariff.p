/* -------------------------------------------------------------------------
  MODULE .......: ROAMTARIFF.P
  FUNCTION .....: Creates and updates roaming tariffs.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 26-06-07
  MODIFIED .....: 
  Version ......: xfera 
--------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RoamTariff
{commali.i}
{eventval.i}
{lib/tokenlib.i}

{lib/tokenchk.i 'RoamTariff'}
&SCOPED-DEFINE BLOCKS_TOTAL 6

DEF BUFFER xxtariff FOR RoamTariff.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRoamTariff AS HANDLE NO-UNDO.
   lhRoamTariff = BUFFER RoamTariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff ).
   
   ON F12 ANYWHERE DO:
      RUN eventview2(lhRoamTariff).
   END.
   
   DEFINE VARIABLE lhRoamTariff2 AS HANDLE NO-UNDO.
   lhRoamTariff2 = BUFFER xxtariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff2 ).
   
   DEFINE VARIABLE lhRTItem AS HANDLE NO-UNDO.
   lhRTItem = BUFFER RTItem:HANDLE.
   RUN StarEventInitialize ( lhRTItem ).

   DEFINE VARIABLE lhRoamBDest AS HANDLE NO-UNDO.
   lhRoamBDest = BUFFER RoamBDest:HANDLE.
   RUN StarEventInitialize ( lhRoamBDest ).

END.

DEF INPUT PARAM iiTariffNum LIKE RoamTariff.TariffNum NO-UNDO.
DEF INPUT PARAM icPriceList LIKE RoamTariff.PriceList.
DEF INPUT PARAM iiRateType LIKE RoamTariff.RateType.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR must-print AS LOG                NO-UNDO.
DEF VAR must-add   AS LOG                NO-UNDO.
DEF VAR ufkey      AS LOG                NO-UNDO.
DEF VAR lcLisHeader  AS CHAR               NO-UNDO.
DEF VAR i          AS INT                NO-UNDO.
DEF VAR j          AS INT                NO-UNDO.
def var ok         as lo format "Yes/No" NO-UNDO.

DEF VAR lcRateType AS CHAR               NO-UNDO.
DEF VAR lcPeakPrice    LIKE RoamBDest.Tariff NO-UNDO EXTENT 3 INIT 0.
DEF VAR lcPeakFrom     LIKE RoamBDest.TZFrom NO-UNDO EXTENT 3 INIT "0000".
DEF VAR lcPeakTo       LIKE RoamBDest.TZTo   NO-UNDO EXTENT 3 INIT "0000".

DEF VAR lcStepSize     LIKE RTItem.StepSize NO-UNDO EXTENT {&BLOCKS_TOTAL}.
DEF VAR lcSizeLimit    LIKE RTItem.SizeLimit NO-UNDO EXTENT {&BLOCKS_TOTAL}.
DEF VAR lcUnit         LIKE RTItem.Unit NO-UNDO EXTENT {&BLOCKS_TOTAL}.
DEF VAR lcStepNum      LIKE RTItem.StepNum NO-UNDO EXTENT {&BLOCKS_TOTAL}.
DEF VAR lcTariff AS DECIMAL FORMAT ">>9.9999999" NO-UNDO EXTENT {&BLOCKS_TOTAL}. 
DEF VAR lcUsedBlocks   AS INTEGER FORMAT "9" NO-UNDO init {&BLOCKS_TOTAL}.
DEF VAR lcBlockInfo    AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lcLisTitle AS CHAR NO-UNDO.
lcLisTitle = "ROAMING TARIFF MAINTENANCE " + string(pvm,"99-99-99") + " " .
lcBlockInfo = "  BSize  Size  Unit Price".
DEF VAR lcTariffLabel      AS CHAR FORMAT "x(16)" INIT "Tariff.........:".
DEF VAR lcServiceLabel     AS CHAR FORMAT "x(16)" INIT "Service........:".
DEF VAR lcRoamingTypeLabel AS CHAR FORMAT "x(16)" INIT "Roaming type...:".
DEF VAR lcPeakFromLabel1   AS CHAR FORMAT "x(16)".
DEF VAR lcPeakFromLabel2   AS CHAR FORMAT "x(16)".
DEF VAR lcPeakFromLabel3   AS CHAR FORMAT "x(16)".
DEF VAR lcValidFromLabel   AS CHAR FORMAT "x(16)" INIT "Valid from.....:".
DEF VAR llNewTariff AS LOG NO-UNDO. 

DEF TEMP-TABLE ttRec NO-UNDO
   FIELD rid AS RECID
   FIELD num AS INT.

FORM /* Add and modify roamtariff */
   RoamTariff.PriceList COLON 16 FORMAT "X(16)"
   HELP "Roaming operator PLMN, group or DEFAULT"
   SKIP
    
   RoamTariff.RateType COLON 16 
   VALIDATE(RoamTariff.RateType NE 0, "Cannot be 0")
   HELP "Roaming tariff type (f9)"
   lcRateType FORMAT "X(20)" NO-LABEL
   SKIP
   
   "--------------------------------------------------------------------"
   SKIP
    
   lcValidFromLabel no-label RoamTariff.ValidFrom format "99-99-9999" no-label
      validate(RoamTariff.ValidFrom NE ? AND
         RoamTariff.ValidFrom > TODAY,
         "Cannot be empty or earlier than tomorrow")
   RoamTariff.ValidTo format "99-99-9999" NO-LABEL
      validate(RoamTariff.ValidTo NE ? AND
         RoamTariff.ValidTo >= TODAY,
         "Cannot be earlier than today")
   SKIP 
    
   lcTariffLabel NO-LABEL RoamTariff.Tariff NO-LABEL
   help "Tariff"
   lcBlockInfo AT 41 NO-LABEL
   SKIP 
   
   lcServiceLabel NO-LABEL RoamTariff.Service NO-LABEL
   VALIDATE(RoamTariff.Service NE "", "Cannot be empty") 
   lcStepNum[1] AT 40 no-label
   lcSizeLimit[1] no-label
   lcStepSize[1] no-label
   VALIDATE(RoamTariff.RateType=5 
         OR(RoamTariff.RateType=3 AND lcStepSize[1] > 0)
         OR(RoamTariff.RateType=8 AND lcStepSize[1] > 0)
         OR(RoamTariff.RateType=9 AND lcStepSize[1] > 0)
         OR(RoamTariff.RateType=10 AND lcStepSize[1] > 0),
            "Cannot be 0")
   
   lcUnit[1] no-label
   lcTariff[1] no-label
   SKIP
   lcRoamingTypeLabel NO-LABEL RoamTariff.RoamingType NO-LABEL
   VALIDATE(RoamTariff.RoamingType = 1 
      OR RoamTariff.RoamingType = 2, "Unknown roaming type")
   HELP "1 = originating, 2 = terminating" 
   lcStepNum[2] AT 40 no-label
   lcSizeLimit[2] no-label
   lcStepSize[2] no-label
   VALIDATE(RoamTariff.RateType=5 
         OR(RoamTariff.RateType=3 AND lcStepSize[2] > 0)
         OR(RoamTariff.RateType=8 AND lcStepSize[2] > 0)
         OR(RoamTariff.RateType=9 AND lcStepSize[2] > 0)
         OR(RoamTariff.RateType=10 AND lcStepSize[2] > 0),
            "Cannot be 0")
   lcUnit[2] no-label
   lcTariff[2] no-label
   SKIP
   
   lcStepNum[3] AT 40 no-label 
   lcSizeLimit[3] no-label
   lcStepSize[3] no-label
   lcUnit[3] no-label
   lcTariff[3] no-label
   SKIP
   
   lcStepNum[4] AT 40 no-label
   lcSizeLimit[4] no-label
   lcStepSize[4] no-label
   lcUnit[4] no-label
   lcTariff[4] no-label
   SKIP
   
   lcPeakFromLabel1 no-label lcPeakFrom[1] no-label 
   lcPeakTo[1] no-label
   lcPeakPrice[1] no-label
   
   lcStepNum[5] AT 40 no-label
   lcSizeLimit[5] no-label
   lcStepSize[5] no-label
   lcUnit[5] no-label
   lcTariff[5] no-label
   
   SKIP 
   
   lcPeakFromLabel2 no-label lcPeakFrom[2] no-label 
   lcPeakTo[2] no-label 
   lcPeakPrice[2] no-label
   
   lcStepNum[6] AT 40 no-label
   lcSizeLimit[6] no-label
   lcStepSize[6] no-label
   lcUnit[6] no-label
   lcTariff[6] no-label

   SKIP 

   lcPeakFromLabel3 no-label lcPeakFrom[3] no-label 
   lcPeakTo[3] no-label
   lcPeakPrice[3] no-label

WITH
   SIZE 80 BY 19 ROW 1 centered COLOR value(cfc)
   TITLE COLOR value(ctc) lcLisHeader
   SIDE-LABELS 
FRAME lis.

RoamTariff.RateType:label      in frame lis = "RateType.......".
RoamTAriff.PriceList:label     in frame lis = "Operator.......".
RoamTariff.ValidTo:label       in frame lis = "".

ASSIGN
   ufkey = TRUE
   ufk  = 0
   ehto = 9.
RUN ufkey.p.

FUNCTION fGetRateTypeText RETURNS CHAR 
   (INPUT piRateType AS INT):
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "RoamTariff"   AND
              TMSCodes.FieldName = "RateType" AND
              TMSCodes.CodeGroup = "Roaming"   AND
              TMSCodes.CodeValue = STRING(piRateType) 
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN RETURN TMSCodes.CodeName.
   ELSE RETURN ?.
END.

FUNCTION fToBytes RETURNS INT
   (INPUT piSize AS INT,
   INPUT pcUnit AS CHAR):
   CASE pcUnit:
      WHEN "B" THEN.
      WHEN "kB" THEN
        piSize = piSize * 1024.
      WHEN "MB" THEN 
        piSize = piSize * 1048576.
   END.
   RETURN piSize.
END.

initLoop:
   REPEAT WITH FRAME lis ON ENDKEY UNDO initLoop, RETRY initLoop:
   
   /* Add PLMN and RateType separately from others */
   IF iiTariffNum = 0 THEN DO:
      CREATE RoamTariff.
      ASSIGN
         lcLisHeader = "ADD TARIFF".

      IF icPriceList NE "" THEN DO:
         RoamTariff.PriceList = icPriceList.
      END.

      IF iiRateType NE 0 THEN DO:
         RoamTariff.RateType = iiRateType.
         lcRateType = fGetRateTypeText(iiRateType).
         DISP lcRateType WITH FRAME lis.
      END.

      UPDATE
         RoamTariff.PriceList
         RoamTariff.RateType
      WITH FRAME lis EDITING:
         
         READKEY.
         nap = KEYLABEL(LASTKEY).
                  
         CASE nap:
            WHEN "F2" THEN NEXT.
            WHEN "F4" THEN UNDO, RETURN "FALSE".
            WHEN "F9" THEN DO:
               CASE FRAME-FIELD:
                  WHEN "RateType" THEN DO:
                     RUN h-tmscodes
                        ("RoamTariff","RateType","Roaming", OUTPUT siirto).
                     IF INT(siirto) > 0 THEN DO:
                        RoamTariff.RateType = INT(siirto).
                        FIND FIRST TMSCodes WHERE
                           TMSCodes.TableName = "RoamTariff"   AND
                           TMSCodes.FieldName = "RateType" AND
                           TMSCodes.CodeGroup = "Roaming"   AND
                           TMSCodes.CodeValue = STRING(RoamTariff.RateType)
                        NO-LOCK NO-ERROR.
                        IF AVAIL TMSCodes THEN DO:
                           lcRateType = TMSCodes.CodeName.
                           DISP lcRateType WITH FRAME lis.
                        END.
                        DISPLAY RoamTariff.RateType WITH FRAME lis.
                     END.
                     ehto = 9.
                     RUN ufkey.
                     NEXT.
                  END.
                  WHEN "PriceList" THEN DO:
                     NEXT. 
                  END.
               END.
            END.
         END.

         IF LOOKUP(nap,poisnap) > 0  THEN DO:
            
            IF FRAME-FIELD = "RateType" THEN DO:
               
               ASSIGN RoamTariff.RateType.
               lcRateType = fGetRateTypeText(RoamTariff.RateType).
               IF lcRateType NE ? THEN DO: 
                  DISP lcRateType WITH FRAME lis.
                  IF (RoamTariff.RateType EQ 7) THEN DO:
                     RoamTariff.PriceList = "DEFAULT".
                     DISP RoamTariff.PriceList WITH FRAME lis.
                  END. 
               END.
               ELSE DO: 
                   MESSAGE 
                     "Unknown ratetype"
                   VIEW-AS ALERT-BOX.
                   lcRateType = "".
                   DISP lcRateType WITH FRAME lis.
                   NEXT-PROMPT RoamTariff.RateType.
                   NEXT.
               END.
            END.

            IF FRAME-FIELD = "PriceList" THEN DO:
               FIND FIRST RoamOper where 
                          RoamOper.PLMN = INPUT RoamTariff.PriceList
               NO-LOCK NO-ERROR.
               IF AVAIL RoamOper THEN DO:
                  RoamTariff.PriceList = RoamOper.PLMN.
               END.
               ELSE DO:
                  FIND FIRST RoamGroup WHERE 
                     RoamGroup.RoamGroup = INPUT RoamTariff.PriceList
                     NO-LOCK NO-ERROR. 
                  IF AVAIL RoamGroup THEN DO:
                    RoamTariff.PriceList = RoamGroup.RoamGroup.
                  END.
                  ELSE DO:
                     IF INPUT RoamTariff.PriceList = "DEFAULT" THEN DO:
                        RoamTariff.PriceList = "DEFAULT".
                     END.
                     ELSE DO:
                        MESSAGE 
                        "Unknown PLMN code"
                        VIEW-AS ALERT-BOX.
                        NEXT-PROMPT RoamTariff.PriceList.
                        NEXT.
                     END.
                  END.
               END.      
               DISPLAY RoamTariff.PriceList WITH FRAME lis.
            END.
         END. 
         
         APPLY LASTKEY.
      END.

      /* Set default values */
      ASSIGN
         RoamTariff.ValidFrom = TODAY + 1
         RoamTariff.ValidTo   = TODAY + 1
         RoamTariff.RoamingType = (IF RoamTariff.RateType = 10 THEN 2 ELSE 1).
   END.
 
   ELSE IF iiTariffNum > 0 THEN DO:
      
      FIND FIRST RoamTariff WHERE RoamTariff.TariffNum = iiTariffNum
      NO-LOCK.

      IF RoamTariff.ValidTo >= TODAY THEN DO:
         FIND FIRST RoamTariff WHERE RoamTariff.TariffNum = iiTariffNum
         EXCLUSIVE-LOCK.
         lcLisHeader = "MODIFY TARIFF".
      END.
      ELSE DO:
         lcLisHeader = "VIEW TARIFF".
      END.
      lcRateType = fGetRateTypeText(RoamTariff.RateType).
      DISP RoamTariff.PriceList RoamTariff.RateType lcRateType WITH FRAME lis.
   END.

   RUN pUpdateRoamTariff.
   IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
   UNDO initLoop, LEAVE initLoop.
   ELSE LEAVE.
END.

PROCEDURE pUpdateRoamTariff:

/* Default units */

CASE RoamTariff.RateType:
   WHEN 3 OR WHEN 8 OR WHEN 9 OR WHEN 10 THEN ASSIGN
      lcUnit[1] = "sec"
      lcUnit[2] = "sec".
   WHEN 5 THEN DO:
      DO i = 1 TO {&BLOCKS_TOTAL}:
         lcUnit[i] = "kB".
      END.
   END.
END CASE.

/* Collects roamtariff related data when modifying */
IF iiTariffNum > 0 THEN DO:
   IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhRoamTariff ).
   
   CASE RoamTariff.RateType:
   
      WHEN 4 THEN DO:
         DISPLAY RoamTariff.RoamingType WITH FRAME lis.
      END.

      WHEN 3 OR WHEN 5 OR WHEN 8 OR WHEN 9 OR WHEN 10 THEN DO:
         FOR EACH RTItem WHERE RTItem.TariffNum = iiTariffNum NO-LOCK:
            lcSizeLimit[RTItem.StepNum] = RTItem.SizeLimit.
            lcStepSize[RTItem.StepNum]  = RTItem.StepSize.
            lcTariff[RTItem.StepNum]    = RTItem.Tariff.
            lcUnit[RTItem.StepNum]      = RTItem.Unit.
         END.
      END.

      WHEN 7 THEN DO:
         i = 1.
         FOR EACH RoamBDest WHERE RoamBDest.TariffNum = iiTariffNum NO-LOCK:
            IF i <= 3 THEN DO:
               lcPeakPrice[i] = RoamBDest.Tariff.
               lcPeakFrom[i] = RoamBDest.TZFrom.
               lcPeakTo[i] = RoamBDest.TZTo.
               CREATE ttRec.
               ASSIGN 
                  rid = RECID(RoamBDest)
                  num = i.
               i = i + 1.
            END.   
         END.
      END.

   END.
END.

IF iiTariffNum = 0 THEN ASSIGN RoamTariff.ValidTo = DATE(12,31,9999). 

/* Set form labels */      
CASE RoamTariff.RateType:

   WHEN 1 THEN DO:    
      ASSIGN 
         lcServiceLabel = "Service........:"
         lcTariffLabel =  "Discount %.....:".
      DISP lcValidFromLabel lcTariffLabel lcServiceLabel WITH FRAME lis.
   END.
   
   WHEN 2 THEN DO:
      ASSIGN 
         lcTariffLabel      = "Conn. charge...:"
         lcValidFromLabel   = "Valid from.....:".
      DISPLAY lcValidFromLabel lcTariffLabel WITH FRAME lis.
   END.

   WHEN 3 OR WHEN 10 THEN DO:
      ASSIGN
         lcBlockInfo   = "         Size  Unit Price"
         lcTariffLabel = "Conn. charge...:".
      DISPLAY lcValidFromLabel lcBlockInfo lcTariffLabel WITH FRAME lis.
   END.

   WHEN 4 THEN DO:
      ASSIGN
         lcTariffLabel      = "Message price..:"
         lcRoamingTypeLabel = "Roaming type...:"
         lcValidFromLabel   = "Valid from.....:".
      DISP lcValidFromLabel lcTariffLabel lcRoamingTypeLabel WITH FRAME lis.
   END.
   
   WHEN 5 THEN DO:  
      lcValidFromLabel = "Valid from.....:".
      DISPLAY lcValidFromLabel lcBlockInfo WITH FRAME lis.
   END.

   WHEN 7 THEN DO:
      ASSIGN
         lcTariffLabel    = "Conn. charge...:"
         lcPeakFromLabel1 = "Time zone 1....:"
         lcPeakFromLabel2 = "Time zone 2....:"
         lcPeakFromLabel3 = "Time zone 3....:".
      DISP lcValidFromLabel lcTariffLabel lcPeakFromLabel1
         lcPeakFromLabel2 lcPeakFromLabel3 WITH FRAME lis.    
   END.

   WHEN 8 THEN DO:
      ASSIGN
         lcTariffLabel  = "Conn. charge...:"
         lcServiceLabel = "Zone...........:".
         lcBlockInfo = "         Size  Unit Price".
      DISP lcValidFromLabel lcTariffLabel 
         lcServiceLabel lcBlockInfo WITH FRAME lis.
   END.
   
   WHEN 9 THEN DO:
      ASSIGN
         lcTariffLabel  = "Conn. charge...:"
         lcServiceLabel = "Country Code...:".
         lcBlockInfo = "         Size  Unit Price".
      DISP lcValidFromLabel lcTariffLabel 
         lcServiceLabel lcBlockInfo WITH FRAME lis.
   END.

END.

llNewTariff = (iiTariffNum EQ 0 OR RoamTariff.ValidFrom > TODAY).

IF iiTariffNum > 0 THEN DO:

      DISPLAY
         RoamTariff.ValidFrom  
         RoamTariff.ValidTo
         RoamTariff.Tariff    WHEN RoamTariff.RateType NE 5   
         
         RoamTariff.RoamingType WHEN RoamTariff.RateType EQ 4
                                AND iiTariffNum = 0
         RoamTariff.Service WHEN LOOKUP(STRING(RoamTariff.RateType),"1,8,9") > 0
         
         /* value added numbers fields */
         
         lcPeakFrom[1]        WHEN RoamTariff.RateType EQ 7
         lcPeakTo[1]          WHEN RoamTariff.RateType EQ 7 
         lcPeakPrice[1]       WHEN RoamTariff.RateType EQ 7
         
         lcPeakFrom[2]        WHEN RoamTariff.RateType EQ 7
         lcPeakTo[2]          WHEN RoamTariff.RateType EQ 7 
         lcPeakPrice[2]       WHEN RoamTariff.RateType EQ 7
         
         lcPeakFrom[3]        WHEN RoamTariff.RateType EQ 7
         lcPeakTo[3]          WHEN RoamTariff.RateType EQ 7 
         lcPeakPrice[3]       WHEN RoamTariff.RateType EQ 7
         
         /* gprs fields */
         
         lcStepSize[1] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         lcUnit[1] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0
         lcTariff[1] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 

         lcStepSize[2] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         lcUnit[2] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0
         lcTariff[2] WHEN LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         
         lcsizelimit       when roamtariff.ratetype eq 5
         lcstepsize        when roamtariff.ratetype eq 5 
         lcunit            when roamtariff.ratetype eq 5
         lctariff          when roamtariff.ratetype eq 5
         
      WITH FRAME lis.

      IF roamtariff.validto < today then do:
         HIDE MESSAGE NO-PAUSE.
         MESSAGE "Press ENTER to continue ...".
         PAUSE NO-MESSAGE.
      
         RETURN.
      END.

   END.

UpdLoop:

   REPEAT WITH FRAME lis ON ENDKEY UNDO UpdLoop, RETRY UpdLoop:
      
      UPDATE
         RoamTariff.ValidFrom WHEN llNewTariff 
         RoamTariff.ValidTo
         RoamTariff.Tariff    WHEN llNewTariff AND RoamTariff.RateType NE 5   
         
         RoamTariff.RoamingType WHEN llNewTariff AND RoamTariff.RateType EQ 4
                                AND iiTariffNum = 0
         RoamTariff.Service WHEN LOOKUP(STRING(RoamTariff.RateType),"1,8,9") > 0
         
         /* value added numbers fields */
         
         lcPeakFrom[1]        WHEN llNewTariff AND RoamTariff.RateType EQ 7
         lcPeakTo[1]          WHEN llNewTariff AND RoamTariff.RateType EQ 7 
         lcPeakPrice[1]       WHEN llNewTariff AND RoamTariff.RateType EQ 7
         
         lcPeakFrom[2]        WHEN llNewTariff AND RoamTariff.RateType EQ 7
         lcPeakTo[2]          WHEN llNewTariff AND RoamTariff.RateType EQ 7 
         lcPeakPrice[2]       WHEN llNewTariff AND RoamTariff.RateType EQ 7
         
         lcPeakFrom[3]        WHEN llNewTariff AND RoamTariff.RateType EQ 7
         lcPeakTo[3]          WHEN llNewTariff AND RoamTariff.RateType EQ 7 
         lcPeakPrice[3]       WHEN llNewTariff AND RoamTariff.RateType EQ 7
         
         lcStepSize[1] WHEN llNewTariff AND 
                            LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         lcUnit[1] WHEN llNewTariff AND
                        LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0
         lcTariff[1] WHEN llNewTariff AND
                          LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 

         lcStepSize[2] WHEN llNewTariff AND
                            LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         lcUnit[2] WHEN llNewTariff AND
                        LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0
         lcTariff[2] WHEN llNewTariff AND
                          LOOKUP(STRING(RoamTariff.RateType),"3,8,9,10") > 0 
         
         lcsizelimit       when llNewTariff AND roamtariff.ratetype eq 5
         lcstepsize        when llNewTariff AND roamtariff.ratetype eq 5 
         lcunit            when llNewTariff AND roamtariff.ratetype eq 5
         lctariff          when llNewTariff AND roamtariff.ratetype eq 5
      WITH FRAME lis EDITING:
         

         READKEY.
         nap = KEYLABEL(LASTKEY).

         HIDE MESSAGE no-pause.
         CASE KEYLABEL(LASTKEY):
            WHEN "F2" THEN NEXT.
            WHEN "F4" THEN UNDO, RETURN "FALSE".
            WHEN "F9" THEN DO:
               CASE FRAME-FIELD:

                  WHEN "Service" THEN DO:
                     IF RoamTariff.RateType EQ 1 THEN DO:
                        RUN h-tmscodes
                           ("RoamTariff","Service","Roaming", OUTPUT siirto).
                        IF siirto NE "" THEN RoamTariff.Service = siirto.
                        DISPLAY RoamTariff.Service WITH FRAME lis.
                     END.
                     ehto = 9.
                     RUN ufkey.
                     NEXT.
                  END.

               END.
            END.
         END.
         
         /* Displays gprs step and block size in different units */
         IF RoamTariff.RateType EQ 5 AND
            (FRAME-FIELD = "lcStepSize" OR FRAME-FIELD = "lcSizeLimit") THEN DO:
            ASSIGN lcUnit[FRAME-INDEX].
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) <= 0 THEN DO:
               APPLY LASTKEY.
               
               DEF VAR lcTempSize AS INT.
               
               IF FRAME-FIELD = "lcStepSize" THEN
                  ASSIGN
                     lcStepSize[FRAME-INDEX]
                     lcTempSize = lcStepSize[FRAME-INDEX].
              
               IF FRAME-FIELD = "lcSizeLimit" THEN
                  ASSIGN 
                     lcSizeLimit[FRAME-INDEX]            
                     lcTempSize = lcSizeLimit[FRAME-INDEX].
               
               CASE STRING(lcUnit[FRAME-INDEX]):

                  WHEN "kB" THEN
                     MESSAGE STRING(lcTempSize * 1024) + " B " +
                         STRING(TRUNCATE(lcTempSize / 1024, 2)) + " MB".
               
                  WHEN "MB" THEN
                     MESSAGE STRING(lcTempSize * 1048576) + " B " +
                         STRING(lcTempSize * 1024) + " kB".

                  WHEN "B" THEN
                     MESSAGE STRING(TRUNCATE(lcTempSize / 1024, 2)) 
                     + " kB, "
                     + STRING(TRUNCATE(lcTempSize / 1048576, 2)) 
                     + " MB".  
               END.
               NEXT.
             END.  
         END.

         /* handle field change */
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0
            THEN DO:
            IF FRAME-FIELD = "lcStepSize" THEN DO:
               ASSIGN lcStepSize[FRAME-INDEX].
            END.

            IF FRAME-FIELD = "Service" THEN DO:
               IF RoamTariff.RateType = 1 THEN DO:               
                  FIND FIRST TMSCodes WHERE
                             TMSCodes.TableName = "RoamTariff"   AND
                             TMSCodes.FieldName = "Service" AND
                             TMSCodes.CodeGroup = "Roaming"   AND
                             TMSCodes.CodeValue = 
                                STRING(INPUT RoamTariff.Service)
                  NO-LOCK NO-ERROR.

                  IF NOT AVAIL TMSCodes THEN DO:
                      MESSAGE 
                        "Unknown service"
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT RoamTariff.Service.
                      NEXT.
                  END.
               END.
            END.

            IF FRAME-FIELD = "Tariff" THEN DO:
               IF RoamTariff.RateType = 1 AND INPUT RoamTariff.Tariff > 100
                  THEN DO:
                  MESSAGE "Discount cannot be greater than 100%".
                  NEXT-PROMPT RoamTariff.Tariff.
                  NEXT.
               END.
               ASSIGN RoamTariff.Tariff.
            END.

            IF FRAME-FIELD = "lcUnit" THEN DO:
               ASSIGN FRAME lis lcUnit[FRAME-INDEX].
            END.

         END.
         APPLY LASTKEY.
 
      END. /* EDITING */
 
      /* Validate blocks create blocks */
      CASE RoamTariff.RateType:
     
      WHEN 3 OR WHEN 10 THEN DO:
         DO i = 1 TO 2:
            IF (LOOKUP(lcUnit[i],"MIN,SEC") = 0) THEN DO:
               MESSAGE "Error in block " + STRING(i) +
                  ": Unit must be SEC or MIN" VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         END.
      END.

      WHEN 5 THEN DO:
      
         IF lcStepSize[1] = 0 THEN DO:
            MESSAGE "You have not defined first block!" VIEW-AS ALERT-BOX.
            NEXT UpdLoop.
         END.
         
         /* Check that no data in undefined blocks */
         DO i = 2 TO {&BLOCKS_TOTAL}:
            IF lcStepSize[i] = 0 AND
              (lcSizeLimit[i] <> 0 OR
                  lcTariff[i] <> 0) THEN DO:
               MESSAGE "No step size defined in block " + STRING(i)
                  VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         END.
         
         /* Count used blocks */ 
         DO i = 2 TO {&BLOCKS_TOTAL}:
            IF lcStepSize[i] = 0 THEN DO:
               lcUsedBlocks = i - 1.
               LEAVE.
            END.
            IF i = {&BLOCKS_TOTAL} THEN lcUsedBlocks = {&BLOCKS_TOTAL}.
         END.
         
         /* Check that no used blocks after first empty block 
           (last blocks can have only stepsize defined) */
         DO j = (i + 1) TO {&BLOCKS_TOTAL}:
            IF lcStepSize[j] <> 0 THEN DO:
               MESSAGE "Gaps between blocks are not allowed!" VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         END.
         
         DO i = 1 TO lcUsedBlocks:
            
            IF (lcSizeLimit[i] = 0 AND i <> lcUsedBlocks) THEN DO:
               MESSAGE "Only last block can have block size 0!"
                  VIEW-AS ALERT-BOX.
               NEXT UpdLoop.   
            END.
            
            IF lcSizeLimit[i] <> 0 AND lcSizeLimit[i] < lcStepSize[i] 
               THEN DO:
               MESSAGE "Error in block " + STRING(i) +
                  ": Step size cannot be greater than block size!"
                  VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
                     
            IF (LOOKUP(lcUnit[i],"B,kB,MB") = 0) THEN DO:
               MESSAGE "Error in block " + STRING(i) +
               ": Unit must be B, kB or MB" VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.

            IF i > 1 AND lcSizeLimit[i] <> 0 THEN DO:

               IF fToBytes(lcSizeLimit[i - 1],lcUnit[i - 1]) 
                >= fToBytes(lcSizeLimit[i],lcUnit[i]) THEN DO:
                  MESSAGE "Error in block " + STRING(i) +
                  ": Block size cannot have smaller value than previous block!"
                  VIEW-AS ALERT-BOX.
                  NEXT UpdLoop.
               END.
               
               IF fToBytes(lcSizeLimit[i],lcUnit[i]) - 
                  fToBytes(lcSizeLimit[i - 1],lcUnit[i - 1]) <
                  fToBytes(lcStepSize[i], lcUnit[i]) THEN DO:
                  MESSAGE "Error in block " + STRING(i) +
                     ": Step size value is too large!" 
                  VIEW-AS ALERT-BOX.
                  NEXT UpdLoop.
               END.    

            END.

         END.
         
         IF (lcSizeLimit[lcUsedBlocks] > 0) THEN DO:
               MESSAGE "Error in block " + STRING(lcUsedBlocks) +
                  ": Block size for last block must be 0!"
                  VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
         END.
      
      END.
      
      /* TimeZone validation begins */
      WHEN 7 THEN DO:
       
         DO i = 1 TO 3:
     
            /* any hours higher than 24 */
            IF SUBSTRING(lcPeakFrom[i],1,2) > "24" OR
               SUBSTRING(lcPeakTo[i],1,2) > "24" THEN DO:
               MESSAGE
                  "Maximum value for hours is 24 !"
               VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.

            /* minutes over 59 */
            IF SUBSTRING(lcPeakFrom[i],3,2) > "59" OR 
               SUBSTRING(lcPeakTo[i],3,2)   > "59"  THEN DO:
               MESSAGE
                  "Maximum value for minutes is 59 !"
               VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
            
            /* hours in right order */
            IF lcPeakFrom[i] > lcPeakTo[i] THEN DO:
               MESSAGE
                  "Time zone end time must be greater than start time !"
               VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         
            /* 1st time zone must start from 00:00 */
            IF i = 1 AND lcPeakFrom[i] NE "0000" THEN DO:
               MESSAGE
                  "Time zones must start from 00:00 !"
               VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
            /* no gaps between time zones */
            ELSE IF i > 1 AND lcPeakTo[i - 1] NE "2400" 
               AND lcPeakFrom[i] NE lcPeakTo[i - 1] THEN DO:
               MESSAGE
                 "Time zones cannot have gaps between them !"
                  VIEW-AS ALERT-BOX.
                  NEXT UpdLoop.
            END.
            
         END.      
         
         /* 24:00 must be found */
         ok = false.
         DO i = 1 to 3:
            
            IF lcPeakTo[i] EQ "2400" THEN DO:
               IF i < 3 THEN
                  IF lcPeakTo[i + 1] NE "0000" THEN DO:
                     MESSAGE "Time zones cannot have gaps between them !"
                        VIEW-AS ALERT-BOX.
                     NEXT UpdLoop.
               END.
               ok = true.
               LEAVE.
            END.

         END.
         IF NOT ok THEN DO:
            MESSAGE
               "Definitions for price zones do not cover the whole"
               "day from 0:00 to 24:00 !"
            VIEW-AS ALERT-BOX.
            NEXT.
         END.
         
      END. /* END TimeZone validation */
      
      WHEN 8 THEN DO:
         IF LOOKUP(RoamTariff.Service,"1,2,3,4,5") = 0 THEN DO:
             MESSAGE
                "Unknown zone number"
             VIEW-AS ALERT-BOX.
             NEXT UpdLoop.
         END.
         DO i = 1 TO 2:
            IF (LOOKUP(lcUnit[i],"MIN,SEC") = 0) THEN DO:
               MESSAGE "Error in block " + STRING(i) +
                  ": Unit must be min or sec" VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         END.
      END.      
      
      WHEN 9 THEN DO:
         
         FIND FIRST RoamCountry WHERE
            RoamCountry.Prefix = RoamTariff.Service NO-LOCK NO-ERROR.
         
         IF NOT AVAIL RoamCountry THEN DO:
             MESSAGE
                "Unknown country code"
             VIEW-AS ALERT-BOX.
             NEXT UpdLoop.
         END.
         DO i = 1 TO 2:
            IF (LOOKUP(lcUnit[i],"MIN,SEC") = 0) THEN DO:
               MESSAGE "Error in block " + STRING(i) +
                  ": Unit must be min or sec" VIEW-AS ALERT-BOX.
               NEXT UpdLoop.
            END.
         END.
      END.      
      
      END. /* END CASE */
      
      IF RoamTariff.ValidTo < RoamTariff.ValidFrom THEN DO: 
         MESSAGE "End date cannot be empty or smaller than begin date"
            VIEW-AS ALERT-BOX.
         NEXT UpdLoop.
      END.      
      
      /* tariff begin and end date cannot exist inside another tariff */
  
      FIND FIRST xxtariff WHERE 
                 xxtariff.PriceList = RoamTariff.PriceList AND 
                 xxtariff.ValidTo   > RoamTariff.ValidTo AND
                 xxtariff.ValidFrom < RoamTariff.ValidFrom AND
                 xxtariff.RateType  = RoamTariff.RateType AND
                 xxtariff.RoamingType = RoamTariff.RoamingType AND
                 xxtariff.Service = RoamTariff.Service AND
           RECID(xxtariff) NE RECID(RoamTariff)
      NO-LOCK NO-ERROR.
      IF AVAIL xxtariff THEN DO:
         MESSAGE
            "Tariff valid dates cannot exist inside another tariff."
         VIEW-AS ALERT-BOX.
         NEXT UpdLoop.
      END.
      
      /* tariffs between begin and end date must be deleted */
      
      FIND FIRST xxtariff WHERE 
                 xxtariff.PriceList = RoamTariff.PriceList AND 
                 xxtariff.ValidFrom >= RoamTariff.ValidFrom AND
                 xxtariff.ValidTo <= RoamTariff.ValidTo AND
                 xxtariff.RateType = RoamTariff.RateType  AND
                 xxtariff.RoamingType = RoamTariff.RoamingType AND
                 xxtariff.Service = RoamTariff.Service AND
           RECID(xxtariff) NE RECID(RoamTariff) NO-LOCK NO-ERROR.

       IF AVAIL xxtariff THEN DO:      
         
         MESSAGE
            "RoamTariff: " + STRING(RoamTAriff.ValidFrom) + " " +
               STRING(RoamTAriff.ValidTo) SKIP
            "xxTariff: " + STRING(xxtariff.ValidFrom) + " " +
               STRING(xxtariff.ValidTo) SKIP
            "Tariffs were found inside begin and end dates." SKIP
            "Do you want to delete those tariffs?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
         IF ok THEN DO:
            FOR EACH xxtariff WHERE 
                     xxtariff.PriceList = RoamTariff.PriceList AND 
                     xxtariff.ValidFrom >= RoamTariff.ValidFrom AND
                     xxtariff.ValidTo <= RoamTariff.ValidTo AND
                     xxtariff.RateType = RoamTariff.RateType AND
                     xxtariff.RoamingType = RoamTariff.RoamingType AND
                     xxtariff.Service = RoamTariff.Service AND
               RECID(xxtariff) NE RECID(RoamTariff) EXCLUSIVE-LOCK.
               
               IF llDoEvent THEN RUN StarEventMakeDeleteEvent ( lhRoamTariff2 ).
               
               FOR EACH RTItem WHERE RTItem.TariffNum = xxtariff.TariffNum
                  EXCLUSIVE-LOCK:
                  IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRTItem).
                  DELETE RTItem.            
               END.

               FOR EACH RoamBDest WHERE RoamBDest.TariffNum = xxtariff.TariffNum
                  EXCLUSIVE-LOCK:
                  IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRoamBDest).
                  DELETE RoamBDest.            
               END.
               
               DELETE xxtariff.
            END.   
         END.
         ELSE DO:
            MESSAGE "Tariff cannot be accepted!" VIEW-AS ALERT-BOX.
            UNDO, NEXT UpdLoop.
         END.   
      END.

      /* next tariff begin date must be later than end date */
      FIND FIRST xxtariff WHERE 
                 xxtariff.PriceList = RoamTariff.PriceList AND 
                 xxtariff.ValidTo   > RoamTariff.ValidTo AND
                 xxtariff.ValidFrom > RoamTariff.ValidFrom AND
                 xxtariff.ValidFrom <= RoamTariff.ValidTo AND
                 xxtariff.RateType  = RoamTariff.RateType  AND
                 xxtariff.RoamingType = RoamTariff.RoamingType AND
                 xxtariff.Service = RoamTariff.Service AND
           RECID(xxtariff) NE RECID(RoamTariff)
      NO-LOCK NO-ERROR.

      IF AVAIL xxtariff THEN DO: 
         MESSAGE
            "A future tariff begin date overlaps end date." SKIP
            "Do you want to move future tariff begin date." SKIP
            "to start after end date?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
         IF ok THEN DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhRoamTariff2 ).
            FIND CURRENT xxtariff EXCLUSIVE-LOCK.
            ASSIGN
               xxtariff.ValidFrom = RoamTariff.ValidTo + 1. 
               IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhRoamTariff2 ).
         END.
         ELSE DO:
            UNDO, NEXT UpdLoop.
         END.   
      END.

      /* previous tariff must be closed */
      FIND FIRST xxtariff WHERE 
                 xxtariff.PriceList = RoamTariff.PriceList AND 
                 xxtariff.ValidTo   >= RoamTariff.ValidFrom AND
                 xxtariff.ValidFrom <= RoamTariff.ValidFrom AND
                 xxtariff.RateType  = RoamTariff.RateType  AND
                 xxtariff.RoamingType = RoamTariff.RoamingType AND
                 xxtariff.Service = RoamTariff.Service AND
           RECID(xxtariff) NE RECID(RoamTariff)
      NO-LOCK NO-ERROR.

      IF AVAIL xxtariff THEN DO: 
         MESSAGE
            "An active tariff was found." SKIP
            "Do you want to create new tariff and close previous one?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
         IF ok THEN DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhRoamTariff2 ).
            FIND CURRENT xxtariff EXCLUSIVE-LOCK.
            ASSIGN
               xxtariff.ValidTo = RoamTariff.ValidFrom - 1. 
            IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhRoamTariff2 ).
         END.
         ELSE DO:
            UNDO, NEXT UpdLoop.
         END.   
      END.
      ELSE DO:
         MESSAGE
            "Do you accept this Price (Y/N) ?"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
         IF NOT ok THEN UNDO, NEXT UpdLoop.      
      END. 
      
      IF iiTariffNum = 0 THEN DO:
         RoamTariff.TariffNum = NEXT-VALUE(RoamTariff). 
      END.
      
      CASE RoamTariff.RateType:

         WHEN 3 OR WHEN 8 OR WHEN 9 OR WHEN 10 THEN DO:
            DO i = 1 TO 2:
               
               IF iiTariffNum > 0 THEN DO: 
                  FIND FIRST RTitem WHERE RTItem.TariffNum = iiTariffNum AND
                      RTItem.StepNum = i EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL RTItem AND llDoEvent 
                     THEN RUN StarEventSetOldBuffer ( lhRTItem ).
               END.
               ELSE CREATE RTItem.

               ASSIGN
                  RTItem.StepSize  = lcStepSize[i]
                  RTItem.Tariff    = lcTariff[i]
                  RTItem.Unit      = lcUnit[i]
                  RTItem.StepNum   = i
                  RTItem.TariffNum = RoamTariff.TariffNum.
                 
               IF llDoEvent THEN
                  IF iiTariffNum > 0 THEN 
                     RUN StarEventMakeModifyEvent ( lhRTItem ).
                  ELSE 
                     RUN StarEventMakeCreateEvent( lhRTItem ).
            END.
         END.

         WHEN 5 THEN DO:    
                        
            IF (iiTariffNum > 0) THEN DO:
               DO i = lcUsedBlocks + 1 TO {&BLOCKS_TOTAL}:
                  FIND RTItem WHERE RTItem.TariffNum = RoamTariff.TariffNum AND
                  RTItem.StepNum = i EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL RTItem THEN DO:
                     IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRTItem).
                     DELETE RTItem.
                  END.   
               END.
            END.
            
            DO i=1 TO lcUsedBlocks:
               
               FIND RTItem WHERE RTItem.TariffNum = RoamTariff.TariffNum AND
                  RTItem.StepNum = i EXCLUSIVE-LOCK NO-ERROR.
               
               IF AVAIL RTItem AND llDoEvent THEN
                  RUN StarEventSetOldBuffer ( lhRTItem ).
               ELSE CREATE RTItem.
               
               ASSIGN
                  RTItem.StepSize  = lcStepSize[i]
                  RTITem.SizeLimit = lcSizeLimit[i]
                  RTItem.Tariff    = lcTariff[i]
                  RTItem.Unit      = lcUnit[i]
                  RTItem.StepNum   = i
                  RTItem.TariffNum = RoamTariff.TariffNum.
                
               IF llDoEvent THEN
                  IF iiTariffNum > 0 THEN 
                     RUN StarEventMakeModifyEvent ( lhRTItem ).
                  ELSE   
                     RUN StarEventMakeCreateEvent( lhRTItem ).
            END.
            
         END.

         WHEN 7 THEN DO:
           
            DEF VAR llNew AS LOG.
            DO i = 1 TO 3:
               
               IF lcPeakTo[i] NE "0000" THEN DO:
                  llNew = TRUE. 
                     
                  IF iiTariffNum > 0 THEN 
                     FIND ttRec WHERE ttRec.num = i NO-LOCK NO-ERROR.
                     
                  IF iiTariffNum > 0 AND AVAIL ttRec THEN DO: 
                     FIND RoamBDest WHERE RECID(RoamBDest) = ttRec.rid
                        EXCLUSIVE-LOCK NO-ERROR.
                     IF AVAIL RoamBDest THEN DO:
                        llNew = FALSE. 
                        IF llDoEvent THEN
                           RUN StarEventSetOldBuffer ( lhRoamBDest ).
                     END.      
                  END.
                  ELSE DO:
                     CREATE RoamBDest.
                  END.      
                  
                  ASSIGN
                     RoamBDest.Tariff = lcPeakPrice[i]
                     RoamBDest.TariffNum = RoamTariff.TariffNum 
                     RoamBDest.TZFrom = lcPeakFrom[i]
                     RoamBDest.TZTo = lcPeakTo[i].
                 
                  IF llDoEvent THEN
                     IF iiTariffNum > 0 AND llNew = FALSE THEN 
                        RUN StarEventMakeModifyEvent ( lhRoamBDest ).
                     ELSE   
                        RUN StarEventMakeCreateEvent( lhRoamBDest ).
               END.
               
               ELSE IF iiTariffNum >0 THEN DO:
                  FIND ttRec WHERE ttRec.num = i NO-LOCK NO-ERROR.
                  IF AVAILABLE ttRec THEN DO:
                     RUN StarEventMakeDeleteEvent( lhRoamBDest ).
                     DELETE RoamBDest.
                  END.   
               END.
               
            END. 
         END.   
      END.
      LEAVE UpdLoop.
   END.

   IF llDoEvent THEN
      IF iiTariffNum > 0 THEN
         RUN StarEventMakeModifyEvent ( lhRoamTariff ).
      ELSE   
         RUN StarEventMakeCreateEvent( lhRoamTAriff ).
   
   RETURN STRING(RoamTariff.TariffNum).
END.
