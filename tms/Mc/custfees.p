/* ----------------------------------------------------------------------
  MODULE .......: custfees.p
  TASK .........: Set contract fees from Billing Events FOR a customer
  APPLICATION ..: nn
  AUTHOR .......: JP
  CREATED ......: 13.02.2002 JP
  CHANGED ......: Ask Billing target
                  03.03.03/aam parameters for fcheck-pl,
                               BillTarget before feemodel,
                               check everything once more when F5 is pressed
                  12.09.03/aam brand                               
                  13.01.04/aam more parameters for fMakeSetFees
                  14.04.04/aam create contract (fFeeContract)
                  01.12.05/aam pass "?" as price to fMakeSetFees
                  06.04.06/aam pass always true to fMakeSetFees as last param.
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{nncoit2.i}
{fcustpl.i}
{timestamp.i}
{fmakeservlimit.i}
{setfees.i}
{eventval.i}
{ffeecont.i}

DEF  INPUT PARAMETER   asnro LIKE Customer.CustNum NO-UNDO.

DEF new  shared VAR siirto AS CHAR.
DEF VAR lcFeeModel  LIKE FeeModel.FeeModel  NO-UNDO FORMAT "X(16)".
DEF VAR BegPer   AS I                 NO-UNDO.
DEF VAR InterAct AS LO                NO-UNDO init TRUE.

DEF VAR Period   AS I  NO-UNDO.
DEF VAR ConPer   AS I  NO-UNDO.
DEF VAR codate   AS DA  NO-UNDO.
DEF VAR rc       AS I  NO-UNDO.
DEF VAR yy       AS I  NO-UNDO.
DEF VAR mm       AS I  NO-UNDO.
DEF VAR ok       AS LO NO-UNDO.
DEF VAR ask-data AS LO NO-UNDO.
DEF VAR UserName AS C  NO-UNDO.
DEF VAR itype    AS LO NO-UNDO.

DEF VAR liBillTarget LIKE BillTarget.BillTarget no-undo.
DEF VAR lcPriceList  LIKE pricelist.pricelist   no-undo. 
DEF VAR lcContract   AS CHAR                    NO-UNDO. 

FORM
SKIP(1)
" NOTE:  This program creates a set of Billable fees for "     SKIP
"        a customer number "   Customer.CustNum
          Customer.CustName FORMAT "x(24)"                     SKIP 
"        according to a pre-defined BILLING EVENT"             SKIP(1)
"        Billing Target ........:" liBillTarget                  SKIP(1) 
         lcFeeModel AT 9 FORMAT "X(16)" 
HELP "Code of a Billing Event  (empty: RETURN)"         
         SPACE(0) ":" FeeName                                  SKIP(1)
"        Contract begins .... ..:" codate FORMAT "99-99-99"    
HELP "FIRST Date   (YYYYMMDD) what this Event conserned"       SKIP



"        (F1) CHANGE another Billing Event    "            SKIP
"        (F4) CHECK the contents of the Event "            SKIP
"        (F5) CREATE the fees                 "            SKIP
"        (F8) SKIP the creation and return    "            SKIP
WITH
   CENTERED OVERLAY ROW 1 NO-LABELS
   TITLE " BILLING Event ACTIVATION "
   FRAME info.

FIND Customer  WHERE Customer.CustNum = asnro NO-LOCK NO-ERROR.


/******************************************
* IF interactive mode: show the intention *
* AND ask FOR confirmation                *
******************************************/
ASSIGN
codate = TODAY.


IF InterAct THEN DO WITH FRAME info:
   /* calculate Period that this fee concerns (NEXT MONTH) */
   ConPer = fNextP(Period).

   PAUSE 0.
   IF lcFeeModel ne "" THEN 
      FIND FeeModel  WHERE 
           FeeModel.Brand    = gcBrand AND
           FeeModel.FeeModel = lcFeeModel NO-LOCK NO-ERROR.
   ELSE ask-data = TRUE.

   DISP 
      FeeModel.FeeName    WHEN AVAIL FeeModel
      Customer.CustName
      Customer.CustNum
   WITH FRAME info.
   COLOR DISP MESSAGES 
      lcFeeModel 
      FeeModel.FeeName 
      Customer.CustName
      /* Period */
   WITH FRAME info.

Action:
   REPEAT WITH FRAME info:

      IF ask-data THEN DO ON ENDKEY UNDO, RETRY:
         EHTO = 9. RUN ufkey. 

         UPDATE 
           liBillTarget 
           lcFeeModel
           codate

         WITH FRAME info EDITING:
            READKEY.
            IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 
            THEN undo action, leave action.

            IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "liBillTarget" 
            THEN DO:
                  RUN h-billtarg (INPUT asnro).
                  IF siirto NE "" AND siirto NE ? THEN 
                     DISPLAY INTEGER(siirto) ;& liBillTarget.
                  ehto = 9.
                  RUN ufkey.
                  NEXT. 
            END.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME info:
               PAUSE 0. /* clears lowest line on screen */

               IF FRAME-FIELD = "lcFeeModel" THEN DO:

                  IF INPUT lcFeeModel = "" THEN DO:
                     HIDE FRAME info NO-PAUSE.
                     UNDO, RETURN.
                  END.   
                  FIND FeeModel  WHERE 
                       FeeModel.Brand    = gcBrand AND
                       FeeModel.FeeModel = INPUT FRAME info lcFeeModel 
                       NO-LOCK NO-ERROR.
                  IF NOT AVAIL FeeModel THEN DO:
                     BELL.
                     MESSAGE "UNKNOWN BILLING EVENT".
                     NEXT.
                  END.
                  DISP FeeModel.FeeName WITH FRAME info.

                  lcPriceList = fFeeModelPriceList(BillTarget.CustNum,
                                                   BillTarget.BillTarget,
                                                   FeeModel.FeeModel,
                                                   INPUT INPUT Codate).
                  IF lcPriceList = "" THEN DO:
                     MESSAGE
                     "PriceList Cannot be Found!" SKIP
                     VIEW-AS ALERT-BOX .
                     NEXT-PROMPT liBillTarget. next.
                  END.
                  IF Fcheck-PL(FeeModel.FeeModel,
                               lcPriceList) = FALSE THEN.
               END.

               ELSE IF FRAME-FIELD = "liBillTarget" THEN DO:
                   find first BillTarget WHERE
                              BillTarget.CustNum = asnro AND
                              BillTarget.BillTarget = INPUT liBillTarget
                   NO-LOCK NO-ERROR.           
                   IF NOT AVAIL BillTarget 
                   THEN DO:
                      BELL.
                      MESSAGE 
                      "Unknown Billing Target '" input liBillTarget "'"
                      VIEW-AS ALERT-BOX.    
                      NEXT-PROMPT liBillTarget. NEXT.
                   END. 

               END.
            END.
            APPLY LASTKEY.
         END. /* EDITING */

         ask-data = FALSE.

      END.

      ASSIGN
      ufk = 0 ufk[1] = 7 ufk[4] = 294 ufk[5] = 15 ufk[8] = 8 ehto = 0.
      if lcFeeModel = "" THEN ufk[5] = 0.
      RUN ufkey.


      IF toimi = 1 THEN DO:
        ask-data = TRUE.
        NEXT Action.
      END.

      IF toimi = 4 THEN DO:
         /* show items (contents) of this Billing Event */
         RUN beitempl(lcFeeModel,lcPriceList).
         NEXT.
      END.

      ELSE IF TOIMI = 5 THEN DO:

         find first BillTarget WHERE
                    BillTarget.CustNum    = asnro AND
                    BillTarget.BillTarget = liBillTarget
         NO-LOCK NO-ERROR.           
         IF NOT AVAIL BillTarget THEN DO:
            BELL.
            MESSAGE 
            "Unknown Billing Target '" liBillTarget "'"
            VIEW-AS ALERT-BOX.    
            NEXT Action.
         END. 

         lcPriceList = fFeeModelPriceList(BillTarget.CustNum,
                                          BillTarget.BillTarget,
                                          lcFeeModel,
                                          Codate).
         IF lcPriceList = "" THEN DO:
            MESSAGE
            "PriceList Cannot be Found!" SKIP
            VIEW-AS ALERT-BOX .
            NEXT Action.
         END.

         IF Fcheck-pl(lcFeeModel,
                      lcPriceList) = FALSE THEN NEXT Action.

         LEAVE Action. /* i.e. DO the job ... */
      END.   

      ELSE IF TOIMI = 8 THEN DO:
         HIDE FRAME info.
         UNDO, RETURN.
      END.
   END. /* Action */
   HIDE FRAME info.
END.  /* interactive mode */         


FIND FIRST FMItem WHERE
           FMItem.Brand     = gcBrand AND 
           FMItem.FeeModel  = lcFeeModel  AND
           FMItem.PriceList = lcPriceList NO-LOCK NO-ERROR.
IF AVAIL FMItem THEN DO:
   ASSIGN
   begper      = INT(YEAR(codate) * 100 + MONTH(codate))
   itype       = FMItem.BillMethod.

   IF interact THEN DO:
      IF CAN-FIND (FIRST SingleFee where
                         SingleFee.Brand     = gcBrand                  AND
                         SingleFee.CustNum   = Customer.CustNum         AND
                         SingleFee.CalcObj   = " "                      AND
                         SingleFee.BillPeriod = begper                  AND
                         SingleFee.BillCode    = FMItem.BillCode        AND
                         SingleFee.Amt   = FMItem.Amount)  OR
         CAN-FIND (FIRST FixedFee WHERE
                         FixedFee.Brand     = gcBrand                   AND
                         FixedFee.CustNum   = Customer.CustNum          AND
                         FixedFee.CalcObj   = ""                        AND
                         FixedFee.BillCode = FMItem.BillCode            AND
                         FixedFee.InUse   = TRUE                        AND
                         FixedFee.BegPeriod = begper                    AND
                         FixedFee.EndPeriod = 999999                    AND
                         FixedFee.Amt   = FMItem.Amount   )
      THEN DO:

         MESSAGE 
         "Billable item(s) already exist? Do you want continue?"
         VIEW-AS ALERT-BOX  BUTTON YES-NO  TITLE " VERIFY Event " UPDATE ok.
         IF ok = FALSE THEN NEXT.
      END.
   END.

   FIND PriceList WHERE
        PriceList.Brand     = gcBrand AND
        PriceList.PriceList = lcPriceList NO-LOCK.

   /* contract */
   lcContract = fFeeContract(gcBrand,
                             Customer.CustNum,
                             "",  /* take salesman from user */
                             CoDate,
                             "Customer fee creation").
 
   fMakeSetfees(lcFeeModel,
                Customer.CustNum,
                0,
                liBillTarget,
                " ",  /* a-index */
                " ",  /* BIName */
                BegPer,
                CoDate,    /* TO FixedFee.BegDate */
                ?,
                lcContract,
                katun,
                "",
                0,
                "",
                "").

   IF InterAct  THEN DO:
      MESSAGE 
      "Billing event " lcFeeModel " has been activated and "  SKIP
      "created fees can be browsed from customer's info (f7)" SKIP    
      VIEW-aS ALERT-BOX .
   END.
END.

