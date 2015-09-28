/* ----------------------------------------------------------------------
  MODULE .......: setfees.p
  TASK .........: Set contract fees from Billing Events FOR a customer
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 05.10.1999 pt
  CHANGED ......: 03.11.1999 pt also an empty FeeModel can be received
                  14.11.1999 pt check that Event contains items FOR this
                                Price List
                  04.01.2000 pt Show ConPer WITH FRAME info, FUNCTION fNextP
                  11.01.2000 jp FMItem.BillType = SingleFee.BillType
                  15.05.2002 jp more parameters
                  25.09.2002 jp ask date, not period
                  26.09.2002 jp New pricelist search!
                  11.10.2002 jr BillLevel:t korvattu BillTarget:lla 
                  03.03.2003 aam parameters for fcheck-pl,
                                 pricelist with fFeeModelPriceList
                  13.03.2003 tk tokens               
                  17.03.2003 jp  pricelist with fFeeModelPriceList NOT interact
                  16.05.2003 tk  eventlog
                  26.05.2003 tk  don't ask data automatically
                  06.06.2003 tk  codate = mobsub.activationdate if not int.act,
                                 removed _ from memos
                  06.06.2003 tk  Single and contract fee: memo[2] commented out
                  24.06.2003 aam FMItem.FromDate, ToDate, FFItemQty, FFEndDate
                  16.07.2003 tk  check if fee has already been created
                  10.09.2003 jp  Brand
                  09.10.2003 jp contract
                  18.11.2003 jp  empty memo field
                  15.01.2004 aam VatIncl
                  14.04.2004 aam CustPP -> iiContract
                  06.05.2004 aam set period after update of codate
                  03.09.2004 jp  servicelimitgroup handling
                  31.12.2004 aam ldActStamp for fMakeServLimit
                  14.12.2005 aam username from customer, not msowner
                  26.01.2006 aam fees to invoice customer
                  20.03.2006/aam take invoice customer primarily from subscr.
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{nncoit2.i}
{fcustpl.i}
{timestamp.i}
{fmakeservlimit.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'FixedFee'}
{eventval.i}
{fcustdata.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.

   lhFixedFee = BUFFER FixedFee:HANDLE.
   lhSingleFee = BUFFER SingleFee:HANDLE.

   RUN StarEventInitialize(lhFixedFee).
   RUN StarEventInitialize(lhSingleFee).

END.

DEF input parameter FeeModel   LIKE FeeModel.FeeModel  NO-UNDO.
DEF input parameter MsSeq      LIKE MobSub.MsSeq       NO-UNDO.
DEF input parameter BegPer     AS I                    NO-UNDO.
DEF input parameter HostTable  AS C                    NO-UNDO.
DEF input parameter KeyValue   AS C                    NO-UNDO.
DEF input parameter iiContract AS I                    NO-UNDO.
DEF input parameter InterAct   AS LO                   NO-UNDO.

DEF VAR Period      AS I  NO-UNDO.
DEF VAR ConPer      AS I  NO-UNDO.
DEF VAR codate      AS DA NO-UNDO init today.
DEF VAR endDate     AS DA NO-UNDO.
DEF VAR ContrType   AS I  NO-UNDO INIT 1.
DEF VAR rc          AS I  NO-UNDO.
DEF VAR yy          AS I  NO-UNDO.
DEF VAR mm          AS I  NO-UNDO.
DEF VAR ask-data    AS LO NO-UNDO.
DEF VAR UserName    AS C  NO-UNDO.
DEF VAR condate     AS DA NO-UNDO.
DEF VAR ok          AS LOG NO-UNDO.

DEF VAR lcPriceList  AS CHAR NO-UNDO.
DEF VAR liEndPeriod  AS INT  NO-UNDO. 
DEF VAR lcCode       AS CHAR  NO-UNDO.
DEF VAR lcFrameField AS CHAR  NO-UNDO.
DEF VAR lcContrType  AS CHAR  NO-UNDO.
DEF VAR lcCTypeLst   AS CHAR  NO-UNDO.
DEF VAR i            AS INT   NO-UNDO.
DEF VAR ldActStamp   AS DEC   NO-UNDO. 
DEF VAR liFeeCust    AS INT   NO-UNDO.
DEF VAR lcError      AS CHAR  NO-UNDO.

FORM
SKIP(1)
" NOTE:  You should now create a set of Billable fees for "    SKIP
"        a mobile subscriber"                                  SKIP
"       " MobSub.CLI SPACE(0) ":" UserName FORMAT "x(24)"    SKIP 
"        according to a pre-defined BILLING Event"             SKIP(1)
         FeeModel AT 9 FORMAT "X(16)" 
HELP "Code of a Billing Event  (empty: RETURN)"         
         SPACE(0) ":" FeeName FORMAT "X(20)"                  SKIP(1)

"        Contract begins ......:" codate FORMAT "99-99-99"     
HELP "First date when this event shall be billed"              SKIP


"        Contract ID ..........:" Contract.contract 

SKIP

"        You can now:"                                         SKIP
"           - CHANGE another Billing Event    (F1)"            SKIP
"           - CHECK the contents of the Event (F4)"            SKIP
"           - CREATE the fees                 (F5)"            SKIP
"           - SKIP the creation and return    (F8)"    
WITH
   CENTERED OVERLAY ROW 1 NO-LABELS
   TITLE " BILLING Event ACTIVATION "
   FRAME info.

FIND MobSub  WHERE MobSub.MsSeq = MsSeq NO-LOCK NO-ERROR.

IF NOT AVAIL mobsub THEN DO:
   BELL.
   MESSAGE 
   "Unknown Mobile Subscription sequence no. '" string(msseq) "'"
   VIEW-AS ALERT-BOX.
END.

liFeeCust = MobSub.InvCust.

FIND FIRST msowner WHERE 
           msowner.brand = gcBrand AND 
           msowner.CLI   = mobsub.Cli no-lock no-error.

IF iiContract > 0 
THEN FIND Contract WHERE 
          Contract.Brand    = gcBrand AND
          Contract.Contract = STRING(iiContract) NO-LOCK NO-ERROR.

/* use default if not given */          
IF iiContract = 0 OR NOT AVAILABLE Contract 
THEN FIND Contract  WHERE 
          Contract.Brand    = gcBrand AND 
          Contract.Contract = msowner.contract NO-LOCK NO-ERROR.

IF avail contract THEN ASSIGN
   contrtype = Contract.ContrType
   enddate   = contract.ToDate.

FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
username = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,     
                            BUFFER Customer).
IF liFeeCust = 0 THEN liFeeCust = Customer.InvCust.                            

FIND BillTarg WHERE 
     BillTarget.CustNum      = Mobsub.CustNum AND
     BillTarget.BillTarget   = Mobsub.BillTarget   NO-LOCK NO-ERROR.             
IF NOT AVAIL BillTarget AND 
   MsSeq > 0            AND 
   InterAct 
THEN DO:
   MESSAGE 
   "Unknown Billing Target " Mobsub.Billtarget 
   VIEW-aS ALERT-BOX.
   RETURN.
   
END.   
/* Invoicing Period */
IF BegPer = 0 THEN
   Period = YEAR(MobSub.ActivationDate) * 100 + MONTH(MobSub.ActivationDate).
ELSE
   Period = BegPer.
/******************************************
* IF interactive mode: show the intention *
* AND ask FOR confirmation                *
******************************************/

IF InterAct THEN DO WITH FRAME info:
   /* calculate Period that this fee concerns (NEXT MONTH) */
      period = year(codate) * 100 + Month(codate).
      ConPer  = fNextP(Period).

   
   PAUSE 0.
   IF FeeModel ne "" THEN DO:
      FIND FeeModel  WHERE 
           FeeModel.Brand    = gcBrand  AND 
           FeeModel.FeeModel = FeeModel NO-LOCK NO-ERROR.

      /* PriceList is stored on PricePlan record */
      lcPriceList =  fFeeModelPriceList(INPUT Billtarget.custnum,
                                        INPUT BillTarget.billTarget,
                                        INPUT FeeModel.FeeModel,
                                        INPUT today).


   END.

   DISP 
      FeeModel            WHEN AVAIL FeeModel
      FeeModel.FeeName    WHEN AVAIL FeeModel
      username
      MobSub.CLI
      codate
      Contract.Contract   WHEN AVAIL contract
   WITH FRAME info.
   COLOR DISP MESSAGES 
      FeeModel 
      FeeModel.FeeName 
      username
      MobSub.CLI
   WITH FRAME info.

ACTION:
   REPEAT WITH FRAME info:

      IF ask-data THEN DO ON ENDKEY UNDO, RETRY:
         EHTO = 9. RUN ufkey. 
         UPDATE 
           FeeModel
           codate  
           WITH FRAME info EDITING:
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME info:
               PAUSE 0. /* clears lowest line on screen */

               IF FRAME-FIELD = "FeeModel" THEN DO:

                  IF INPUT FeeModel = "" THEN DO:
                     HIDE FRAME info NO-PAUSE.
                     UNDO, RETURN.
                  END.   
                  FIND FeeModel  WHERE 
                       FeeModel.Brand    = gcBrand   AND 
                       FeeModel.FeeModel = 
                  INPUT FRAME info FeeModel NO-LOCK NO-ERROR.
                  IF NOT AVAIL FeeModel THEN DO:
                     BELL.
                     MESSAGE "UNKNOWN BILLING Event".
                     NEXT.
                  END.
                  DISP FeeModel.FeeName WITH FRAME info.

                  /* PriceList is stored on PricePlan record */
                  lcPriceList =  fFeeModelPriceList(INPUT Billtarget.custnum,
                                                    INPUT BillTarget.billTarget,
                                                    INPUT FeeModel.FeeModel,
                                                    INPUT today).

                  IF Fcheck-PL(FeeModel.FeeModel,
                               lcPriceList) = FALSE THEN.
               END.

            END.
            APPLY LASTKEY.
         END. /* EDITING */

         ask-data = FALSE.
         period = year(codate) * 100 + Month(codate).

      END.

      ASSIGN
      ufk = 0 ufk[1] = 7 ufk[4] = 294 
      ufk[5] = (IF lcRight = "RW" THEN 15 ELSE 0)
      ufk[8] = 8 ehto = 0.
      if FeeModel = "" THEN ufk[5] = 0.
      RUN ufkey.


      IF toimi = 1 THEN DO:
        ask-data = TRUE.
        NEXT Action.
      END.

      IF toimi = 4 THEN DO:
         /* show items (contents) of this Billing Event */
         run beitempl(FeeModel,lcPriceList).
         NEXT.
      END.
      ELSE IF TOIMI = 5 AND lcRight = "RW" THEN DO:
         IF Fcheck-pl(FeeModel.FeeModel,
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
ELSE DO:
   lcPriceList =  fFeeModelPriceList(INPUT Billtarget.custnum,
                                           BillTarget.billTarget,
                                           FeeModel,
                                           today).
   codate = MobSub.ActivationDate.


END.
/* DO the job : */


/******************************************************
* Search THRU whole billing Event BillCode package of  *
* this Price list code                                *
******************************************************/

IF CoDate = ? THEN CoDate = TODAY.

IF CoDate <= TODAY 
THEN ldActStamp = 0.
ELSE ldActStamp = fMake2DT(CoDate,1).

ok = TRUE.

FOR                             
EACH FMItem NO-LOCK  WHERE
     FMItem.Brand     = gcBrand     AND 
     FMItem.FeeModel  = FeeModel    AND
     FMItem.PriceList = lcPriceList AND
     FMItem.FromDate <= CoDate      AND
     FMItem.ToDate   >= CoDate,

     BillItem no-lock WHERE
     BillItem.BRand    = gcBrand   AND 
     BillItem.BillCode = FMItem.BillCode,
FIRST PriceList NO-LOCK WHERE
      PriceList.Brand     = gcBrand AND
      PriceList.PriceList = lcPriceList:

     IF FMItem.BillType NE "NF" THEN /* Not no fee */
        IF FMItem.BillMethod = TRUE /* a SINGLE FEE, not No Fee */ THEN DO:
           /* calculate Period that this fee concerns (NEXT MONTH) */
           ConPer = fNextP(Period).

           IF InterAct THEN DO:

              IF CAN-FIND(FIRST SingleFee WHERE 
                                SingleFee.CustNum    = liFeeCust AND
                                SingleFee.BillCode   = FMItem.BillCode  AND
                                SingleFee.KeyValue   = keyvalue         AND
                                SingleFee.HostTable  = HostTable)

              THEN DO:
                 MESSAGE
                    "Fees may have already been created" SKIP
                    "for " FMItem.BillCode "!" SKIP
                    "Do you want to create more fees ?"
                 VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.
              END.
              ELSE ok = TRUE.
           END.

           IF NOT ok THEN NEXT.                           


           /* make a OBI record (a single payment) */
           CREATE SingleFee.
           ASSIGN
           SingleFee.FMItemId    = NEXT-VALUE(bi-seq).                             
           ASSIGN
           SingleFee.Brand       = gcBrand 
           SingleFee.CustNum     = liFeeCust  /* customer number         */
           SingleFee.BillTarget  = MobSub.BillTarget /* Billing Target        */
           SingleFee.CalcObj     = "MsSeq" +       /* MobSub id               */
                                   STRING(MsSeq)   
           SingleFee.BillCode    = FMItem.BillCode    /* BillCode Code        */
           SingleFee.BillPeriod  = Period           /* billing Period      */
           SingleFee.Concerns[1] = ConPer           /* Period concerned       */
           
           SingleFee.Amt         = FMItem.Amount  /* Payment                 */
           SingleFee.BillType    = FMItem.BillType
           Singlefee.ServiceLimitGroup = FMItem.ServiceLimitGroup
           SingleFee.FeeModel    = FMItem.FeeModel
           SingleFee.HostTable   = hosttable
           SingleFee.KeyValue    = keyvalue
           SingleFee.VatIncl     = PriceList.InclVat
           SingleFee.CustPP      = 0
           SingleFee.Contract    = IF AVAILABLE Contract
                                   THEN Contract.Contract
                                   ELSE "".

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSingleFee).

        END.  /* single fee */

        ELSE DO: /* A PERIODICAL FEE * */
           /* We have TO make a contract fee + Billable contract items */
           ConPer = fNextP(Period).
           if codate = ? then 
           codate = date(int(substring(STRING(ConPer),5,2)),
                         1,int(substring(STRING(ConPer),1,4))). 

           IF FMItem.FFEndDate NE ? AND FMItem.FFItemQty = 0
           THEN liEndPeriod = YEAR(FMItem.FFEndDate) * 100 + 
                              MONTH(FMItem.FFEndDate).
           ELSE liEndPeriod = 999999.

           IF InterAct THEN DO:

              IF CAN-FIND(FIRST FixedFee WHERE 
                                FixedFee.CustNum    = liFeeCust AND
                                FixedFee.BillCode   = FMItem.BillCode AND
                                FixedFee.KeyValue   = keyvalue        AND
                                FixedFee.HostTable  = HostTable       AND
                                FixedFee.EndPeriod  = 999999)
              THEN DO:
                 MESSAGE
                    "Fees may have already been created" SKIP
                    "for " FMItem.BillCode "!" SKIP
                    "Do you want to create more fees ?"
                 VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.
              END.
              ELSE ok = TRUE.
           END.
           
           IF NOT ok THEN NEXT.                           

           CREATE FixedFee.

           ASSIGN      
           FixedFee.Brand      = gcBrand 
           FixedFee.FFNum      = NEXT-VALUE(contract) /* sequence FOR contract   */
           FixedFee.BegPeriod  = Period           /* beginning Period        */
           FixedFee.CustNum    = liFeeCust        /* customer no.            */
           FixedFee.CalcObj    = "MsSeq" +       /* MobSub id               */
                                 STRING(MsSeq)
           FixedFee.BillCode   = FMItem.BillCode    /* BillCode code         */
           FixedFee.Amt        = FMItem.Amount  /* Billable amount         */
           FixedFee.BillMethod = FMitem.BillCycle 
           FixedFee.Interval   = FMItem.Interval   /* Billing Interval MONTHS */
           FixedFee.EndPeriod  = (IF AVAIL contract AND 
                                           contract.contrType = 2 AND 
                                           contract.todate ne ? THEN 
                                     YEAR(contract.todate) * 100 +  
                                     MONTH(contract.todate)       
                                 ELSE  999999)   /* until further           */
           FixedFee.BegDate    = codate
           FixedFee.ServiceLimitGroup = FMItem.ServiceLimitGroup
           FixedFee.KeyValue   = keyvalue
           FixedFee.HostTable  = HostTable     
           FixedFee.FeeModel   = FMItem.FeeModel
           FixedFee.VatIncl    = PriceList.InclVat
           FixedFee.CustPP     = 0
           
           FixedFee.InclAmt      = FMItem.InclAmt
           FixedFee.InclUnit     = FMItem.InclUnit
           FixedFee.InclBillCode = FMItem.InclBillCode
           FixedFee.Cli          = mobsub.cli
           FixedFee.Contract     = contract.contract WHEN avail Contract.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFixedFee).

           /* NEXT we generate Billable items */         
           rc = fMakeContract (FixedFee.FFNum,
                               FMItem.FFItemQty).

           IF InterAct THEN DO:
              MESSAGE 
              "Totally" rc "individual CFee items created" 
              "for customer" MobSub.CustNum "of BillCode" FixedFee.BillCode
              VIEW-AS ALERT-BOX
              TITLE "PERIODICAL FEE CREATED".

              /* THEN we SHOW always ALL Billable items ... */          
           END.
           
           IF FMItem.ServiceLimitGroup ne "" THEN DO:
              fMakeServLimit(INPUT  FMItem.ServiceLimitGroup,
                                    msseq,
                                    (IF FMItem.ServiceLimitGroup BEGINS {&DSS}
                                     THEN Customer.Custnum ELSE ?),
                                    ldActStamp,
                                    ?,
                             OUTPUT lcError).
           
              IF interact then 
              MESSAGE
              "Service Limit Group "  FMItem.ServiceLimitGroup SKIP
              "created" 
              VIEW-AS ALERT-BOX.
           END.
           
           
        END.   
     /* END IF FMItem.BillType NE "NF" THEN */ 
END. /* FOR EACH FMItem */


