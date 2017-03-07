/* feeamtchg.p          01.12.04/aam
*/

{Syst/commali.i}
{Syst/eventval.i}
{Func/cparam2.i}
{Func/nncoit2.i}

DEF VAR lcCLIType   AS CHAR NO-UNDO.
DEF VAR ldOldAmt    AS DEC  NO-UNDO.
DEF VAR ldNewAmt    AS DEC  NO-UNDO.
DEF VAR liFromPer   AS INT  NO-UNDO.
DEF VAR ldtPerDate  AS DATE NO-UNDO.
DEF VAR ldtFromDate AS DATE NO-UNDO.
DEF VAR lcEventDir  AS CHAR NO-UNDO.
DEF VAR llOk        AS LOG  NO-UNDO. 
DEF VAR liMobSub    AS INT  NO-UNDO.
DEF VAR liFee       AS INT  NO-UNDO. 
DEF VAR lcBillCode  AS CHAR NO-UNDO INIT "KKMAKSU,ALEKKMAKSU". 
DEF VAR liConcerns  AS INT  NO-UNDO.
DEF VAR liSign      AS INT  NO-UNDO. 
DEF VAR liQty       AS INT  NO-UNDO. 
DEF VAR llNewFee    AS LOG  NO-UNDO. 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   lhFixedFee = BUFFER FixedFee:HANDLE.
   RUN StarEventInitialize(lhFixedFee).

END.

DEF STREAM sLog.

lcEventDir = fCparamC("EventlogDir").

FORM liMobSub LABEL "CLIs" 
WITH 1 DOWN OVERLAY ROW 12 CENTERED SIDE-LABELS FRAME fMobQty.

FORM liFee LABEL "Fees" 
WITH 1 DOWN OVERLAY ROW 15 CENTERED SIDE-LABELS FRAME fFeeQty.

 
ehto = 9.
RUN Syst/ufkey.p.

REPEAT ON ENDKEY UNDO, RETURN:                            
   PAUSE 0.
   UPDATE lcCLIType   
             FORMAT "X(15)" 
             LABEL "CLIType"   
             HELP  "CLIType" SKIP
          lcBillCode 
             FORMAT "X(30)"
             LABEL "Billing Items"
             HELP "Fees with these billing items"
             SKIP
          ldOldAmt    
             FORMAT "->>>>>>9.999" 
             LABEL  "Old Amount" 
             HELP   "Amount that is replaced" SKIP
          ldNewAmt    
             FORMAT "->>>>>>9.999" 
             LABEL  "New Amount"
             HELP   "New amount" SKIP
          ldtFromDate
             FORMAT "99-99-99"    
             LABEL  "New Fees From" 
             HELP   "New fees that have been created after this date" 
             VALIDATE(INPUT ldtFromDate NE ?,
                      "Date is mandatory") SKIP
          ldtPerDate   
             FORMAT "99-99-99"    
             LABEL  "Old Fees From" 
             HELP   "Unbilled items for old fees from this date onwards" 
             VALIDATE(INPUT ldtPerDate NE ?,
                      "Date is mandatory") SKIP
          WITH OVERLAY ROW 3 CENTERED TITLE " Fee Amount Change " 
              1 COL FRAME fAmtChg.
            
   IF lcCLIType = "" THEN DO:
      HIDE FRAME fAmtChg NO-PAUSE.
      RETURN.
   END.

   FIND CLIType WHERE 
        CLIType.Brand   = gcBrand AND
        CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CLIType THEN DO:
       MESSAGE "Unknown CLI type"
       VIEW-AS ALERT-BOX.
       NEXT.
   END.
 
   llOk = TRUE.
   
   MESSAGE "Start changing amounts for fees ?" 
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE  " START " 
   SET llOk.
   
   IF llOk THEN LEAVE.
   
END.

ehto = 5.
RUN Syst/ufkey.p.

OUTPUT STREAM sLog TO VALUE(lcEventDir + "/feeamtchg_" +
                            STRING(YEAR(TODAY),"9999") + 
                            STRING(MONTH(TODAY),"99")  +
                            STRING(DAY(TODAY),"99")    +
                            ".log") APPEND.

PUT STREAM sLog UNFORMATTED
   lcCLIType ";" lcBillCode ";" ldOldAmt ";"
   ldNewAmt ";" ldtFromDate ";" ldtPerDate SKIP.

liFromPer = YEAR(ldtPerDate) * 10000 +
            MONTH(ldtPerDate) * 100  +
            DAY(ldtPerDate).

PAUSE 0.
VIEW FRAME fFeeQty.
PAUSE 0.
VIEW FRAME fMobQty.

FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand   = gcBrand AND
         MobSub.CLIType = lcCLIType:

   liMobSub = liMobSub + 1.
            
   PAUSE 0.
   DISP liMobSub WITH FRAME fMobQty.
   
   FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
            FixedFee.Brand     = gcBrand              AND 
            FixedFee.HostTable = "MobSub"             AND
            FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
            LOOKUP(FixedFee.BillCode,lcBillCode) > 0  AND
            /* change also credit fees */
            ABS(FixedFee.Amt)  = ldOldAmt:
            

      /* nothing to change */
      IF NOT CAN-FIND(FIRST FFItem OF FixedFee WHERE FFItem.Billed = FALSE)
      THEN NEXT.
      
      liFee = liFee + 1.
      PAUSE 0.
      DISP liFee WITH FRAME fFeeQty.
      
      llNewFee = FALSE.
      /* new fee */
      IF NOT CAN-FIND(FIRST FFItem OF FixedFee WHERE FFItem.Billed = TRUE) AND
         FixedFee.BegDate >= ldtFromDate
      THEN DO:
         llNewFee = TRUE.
         FOR EACH FFItem OF FixedFee:
            DELETE FFItem.
         END.
      END.
      
      ASSIGN liConcerns = liFromPer 
             liSign     = IF FixedFee.Amt < 0 THEN -1 ELSE 1.
      
      IF FixedFee.BegDate >= ldtFromDate THEN       
      liConcerns = YEAR(ldtFromDate) * 10000 +
                   MONTH(ldtFromDate) * 100  +
                   DAY(ldtFromDate). 
               
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).
      
      PUT STREAM sLog UNFORMATTED
          FixedFee.CustNum   CHR(9)
          FixedFee.BillCode  CHR(9)
          FixedFee.KeyValue  CHR(9)
          MobSub.CLI         CHR(9)
          liConcerns         CHR(9)
          FixedFee.Amt       CHR(9)
          ldNewAmt * liSign  SKIP.
          
      FixedFee.Amt = ldNewAmt * liSign.
          
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).
 
      /* create all items from scratch -> broken month gets noted */
      IF llNewFee THEN DO:
         liQty = fMakeContract(FixedFee.FFNum,0).
      END.    
      
      /* update old, unbilled items */  
      ELSE     
      FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK WHERE
               FFItem.Concerns[1] >= liConcerns AND
               FFItem.Billed       = FALSE      AND
               ABS(FFItem.Amt)     = ldOldAmt:
         FFItem.Amt = FixedFee.Amt.      
      END.
         
   END.
   
END.    

OUTPUT STREAM sLog CLOSE.

HIDE FRAME fFeeQty NO-PAUSE.
HIDE FRAME fMobQty NO-PAUSE.


MESSAGE "Fees updated"
VIEW-AS ALERT-BOX
TITLE " DONE ".

                            



