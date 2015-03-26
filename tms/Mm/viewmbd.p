/* ----------------------------------------------------------------------
  MODULE .......: viewmbd.p
  TASK .........: View Billing Data of one Mobile CDR on screen
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 01.09.99 pt
  CHANGED ......: 03.10.99 pt TITLE
                  03.11.99 jp (f4) view rate record                  
                  08.11.99 jp NEW FIELD: ttCall.InvNum
                  16.11.99 pt TITLE
                  30.11.99 jp NOT rated, f4 hidden / pp-code
                  09.09.02 jp kr-label removed
                  14.10.02 jr Removed BillLevel
                  21.03.03 jp xbsub.i
                  25.03.03 jp xbsub.i -> func.i
                  25.05.03 jp show b-number (f6)
                  14.07.03 jp b-pref added for frame lis
                  10.09.03 jp Brand
  Version ......: M15
 ---------------------------------------------------------------------- */

{commali.i}
{rate_roamzone.i}
{mobcdr_bdest.i}

DEF TEMP-TABLE  ttCall NO-UNDO LIKE Mobcdr
   FIELD CDRTable AS CHAR.  

DEFINE INPUT PARAMETER  TABLE FOR ttCall.  
DEFINE INPUT PARAMETER  idtDate  AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER  iiTime   AS INT   NO-UNDO.
DEFINE INPUT PARAMETER  icCli    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER  iidtlseq AS INT   NO-UNDO.


DEF VAR UserName     AS C   NO-UNDO FORMAT "x(30)".
DEF VAR wdy          AS C   NO-UNDO.
DEF VAR wd           AS C   NO-UNDO.
DEF VAR Time-S       AS C   NO-UNDO FORMAT "x(8)".
DEF VAR Time-E       AS C   NO-UNDO FORMAT "x(8)".
DEF VAR cdurat       AS C   NO-UNDO FORMAT "x(8)".
DEF VAR lcInvnum     AS C   NO-UNDO FORMAT "x(20)" .
DEF VAR i            AS I   NO-UNDO. 
DEF VAR ErrCode      AS C   NO-UNDO.
DEF VAR DiscGroup    AS C   NO-UNDO.
DEF VAR vds          AS C   NO-UNDO.
DEF BUFFER invcust FOR Customer.
DEF VAR Billed       AS LO  NO-UNDO.
DEF VAR rateplan     AS C   NO-UNDO.
DEF VAR reasonC      AS C   NO-UNDO.
DEF VAR RCName       AS C   NO-UNDO.
DEF VAR reasonT      AS C   NO-UNDO.
DEF VAR siirto       AS C   NO-UNDO.
DEF VAR bsub         AS C   NO-UNDO.
DEF VAR lcCMT        AS C   NO-UNDO FORMAT "X(35)" .
DEF VAR lcErrName    AS C   NO-UNDO FORMAT "x(25)" .
DEF VAR lcCDRType    AS C   NO-UNDO.
DEF VAR lcDataAmt    AS C   NO-UNDO.
DEF VAR ppCharge     AS CH  NO-UNDO.
DEF VAR lcBDName     AS CHAR NO-UNDO.

DEF BUFFER bccn for ccn.

wdy = "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
vds = "NO V-DISC,PENDING,CALCULATED,(3),(4),(5),(6),(7),(8),BARRED,(10),(11),,".

FIND   FIRST  ttCall WHERE
              ttcall.datest = idtDate AND
              ttcall.timest = iiTime  AND
              ttCall.Cli    = icCli   AND
              ttcall.Dtlseq = iiDtlseq NO-LOCK NO-ERROR.

IF       ttCall.EventType = "MSO" THEN lcCMT = "Mobile Originating ".
ELSE IF  ttCall.EventType = "MST" THEN lcCMT = "Mobile Terminating ".
ELSE IF  ttCall.EventType = "CFO" THEN lcCmt = "Call Forward"       .
ELSE IF  ttCall.EventType = "SSP" THEN lcCmt = "Supplementary Service" .
ELSE IF  ttCall.EventType = "OMM" THEN lcCmt = "Originating SMS".
ELSE IF  ttCall.spocmt = 72 OR 
         ttCall.spocmt = 73 THEN lcCmt = "Terminating VAS".
ELSE IF  ttCall.EventType = "TMM" THEN lcCmt = "Terminating SMS - non billable ".
ELSE IF  ttCall.EventType = "60"  THEN lcCMT = "GPRS" .
ELSE IF  ttCall.EventType = "70"  THEN lcCMT = "Roaming GPRS" .
ELSE                             lcCmt = "".

FIND Customer WHERE 
     Customer.CustNum    = ttCall.CustNum         NO-LOCK NO-ERROR. 

FIND invcust WHERE 
     invcust.CustNum     = ttCall.InvCust         NO-LOCK NO-ERROR. 

FIND MobSub WHERE 
     MobSub.cli          = ttCall.cli              NO-LOCK NO-ERROR.

IF ttCall.ppFlag = 0 THEN DO:
   
   FIND InvSeq where InvSeq.InvSeq = ttCall.InvSeq no-lock no-error.
   
   IF AVAIL InvSeq AND InvSeq.Invnum > 0 
   THEN lcInvnum =  "" + STRING(invseq.invnum).
   ELSE lcInvnum = "NOT INVOICED".
END.
ELSE DO:
   
   FIND ppInvSeq where ppInvSeq.InvSeq = ttCall.InvSeq no-lock no-error.
   
   IF AVAIL ppInvSeq AND ppInvSeq.Invnum > 0 
   THEN lcInvnum = "" + STRING(ppinvseq.invnum).
   ELSE lcInvnum = "NOT INVOICED".
END.

FIND BillTarg WHERE 
     BillTarg.CustNum    = ttCall.CustNum AND
     BillTarg.BillTarget = ttCall.BillTarget  NO-LOCK NO-ERROR.

lcBDName = fMobCDRBDestName(ttCall.SpoCMT,
                            ttCall.BDest,
                            ttCall.BType,
                            ttCall.DateSt).
                         
FIND BillItem WHERE
     BillItem.Brand          = gcBrand      AND 
     BillItem.BillCode       = ttCall.BillCode        NO-LOCK NO-ERROR.

FIND FIRST CCN WHERE
           CCN.Brand     = gcBrand          AND 
           CCN.CCN       = ttCall.rateCCN           NO-LOCK NO-ERROR.    
           
FIND FIRST bCCN WHERE
           bCCN.Brand     = gcBrand          AND 
           bCCN.CCN       = ttCall.CCN           NO-LOCK NO-ERROR.    

ASSIGN
   Time-S      = string(ttCall.TimeStart, "hh:mm:ss")
   Time-E      = string(ttCall.Timest + ttCall.BillDur, "hh:mm:ss")   
   cdurat      = string(ttCall.BillDur,   "hh:mm:ss").

IF DiscGroup = "" THEN  DiscGroup = "<NONE>".

IF ttCall.ErrorCode NE 0 THEN 
    ErrCode = " (ERROR " +  STRING(ttCall.ErrorCode) + ") ".  
ELSE ErrCode = " ".

/* Note: Prepaid calls always in KB else postpaid calls in bytes */
IF ttCall.ppFlag = 1 THEN
   ASSIGN lcCDRType = "Prepaid"
          lcDataAmt = STRING((ttCall.Datain + ttCall.DataOut) / 1024).
ELSE
   ASSIGN lcCDRType = "Postpaid"
          lcDataAmt = STRING((ttCall.Datain + ttCall.DataOut) / 1024 / 1024).

IF ttCall.PPFlag > 0 THEN 
   RUN cdr_detail_value.p("PrepCDR",
                          ttCall.DateSt,
                          ttCall.DtlSeq,
                          "Balance after",
                          OUTPUT ppCharge).

FORM
 ttCall.CLI          LABEL "A-sub Number .."
 lcCDRType           LABEL "CDR type ..."       AT 51                   SKIP  
   
 ttCall.GsmBnr   LABEL "B-sub Number .."  ttCall.bpref  NO-LABEL
 ttCall.DateSt       LABEL "Date........"       AT 51 format "99-99-9999"
  wd format "X(3)" NO-LABEL                                             SKIP
   
 "B-sub Operator :" ttCall.BDest NO-LABEL FORMAT "X(8)" 
 lcBDName NO-LABEL   FORMAT "x(14)"  
 Time-S          LABEL "Started ...." AT 51                             SKIP
 
 ttCall.ccn  FORMAT ">>9"  label "Reporting CCN ." bCCN.CCNName No-label
     format "X(29)"
 cdurat label "Duration ..."  AT 51                                     SKIP
 ttCall.rateCCN  FORMAT ">>9"    label "RateCCN/Type..." CCN.CCNName
                 format "x(23)" NO-LABEL          
 Time-E    label "Ended ......" AT 51                                SKIP
 
 BillTarget.RatePlan  format "x(13)" label "RatePlan ......"            
 lcDataAmt label "Data(Mb)...:" AT 51 SKIP

 ttCall.CustNum   label "User .........." format ">>>>>>>>9"
   Customer.CustName AT 29 NO-LABEL format "x(48)"                  skip
 
 ttCall.InvCust   label "Inv.Customer..." format ">>>>>>>>9" 
    invcust.CustName AT 29 format "x(48)"  NO-LABEL                 SKIP

  lcInvnum label "Invoice number."   
  lcErrName no-label  at 51 format "x(28)"                          SKIP
WITH OVERLAY TITLE " BILLING DATA OF  A " +
" '" + ttCall.EventType + "' MOBILE CALL "  + ErrCode
ROW 1 WIDTH 80 side-labels  FRAME cdr.
               


FORM
   ttCall.BillCode label "Billing Item..." format "x(15)"  
   BillItem.BIName      no-label format "x(42)"                    SKIP
 
   ttCall.Charge  label  "PP Charge ....."                         SKIP
   ppCharge       label  "Balance after.."                         SKIP(1)

WITH
   OVERLAY  
   ROW 11 
   WIDTH 80
   side-labels 
   FRAME Prepcdr.


FORM
   ttCall.BillCode    label "Billing Item..." format "x(15)"  
   BillItem.BIName      no-label format "x(42)"                    SKIP
   ttCall.startcharge FORMAT ">>>>9.999" label "Starting fee..."   SKIP   
   ttCall.MpmAmt      FORMAT ">>>>9.999" label "Unit charge...."   SKIP
   ttCall.Charge      FORMAT ">>>>9.999" label "3rd party ....."   SKIP   
"------------------------------------------------------------------------------"                                                                  SKIP 
   ttCall.Amount FORMAT ">>>>9.999"     
                      label "TOTAL ........."                      SKIP        WITH
   OVERLAY  
   ROW 11 WIDTH 80 side-label 
FRAME Postcdr.

FORM
   ttCall.BillCode    label "Billing Item..." format "x(15)"  
   BillItem.BIName      no-label format "x(42)"                    SKIP
   ttCall.startcharge FORMAT ">>>>9.999" label "Starting fee..."   SKIP   
   ttCall.MpmAmt      FORMAT ">>>>9.999" label "Unit charge...."   SKIP
"------------------------------------------------------------------------------"                                                                  SKIP 
   ttCall.Amount FORMAT ">>>>9.999"     
                      label "TOTAL ........."                      SKIP(1)    
WITH OVERLAY  ROW 11 WIDTH 80 side-label 
FRAME Roamcdr.

FORMAT 
"Enter the text/code for browsing the detailded information of the call." 

 SKIP(1)

"NOTICE that your user information, date and time of browsing, "   SKIP 
"A-Number and B-Number are stored to EventLog Database."           SKIP(1)   
"Reason Codes:"                                                    SKIP
" 1 - TroubleShooting "                                            SKIP
" 2 - Authority requests"                                          SKIP
" 3 - Invoicing "                                                  SKIP
" 4 - Other reason   "                                             SKIP(1)      

"Reason Code" ReasonC format "9"  Tmscode.codename FORMAT "X(35)"  SKIP
"Reason Text" ReasonT format "x(60)"                     

WITH CENTERED ROW 2 COLOR VALUE(cfc) TITLE 
"SHOW B-NUMBER" OVERLAY side-label no-label FRAME reason .



PAUSE 0.

DISP  
 ttCall.DateSt  ENTRY(Weekday(ttCall.DateSt),wdy) WHEN ttCall.DateSt NE ? @ wd Time-S
 ttCall.CustNum Customer.CustName   WHEN     AVAIL Customer
             "!! UNKNOWN !!"   WHEN NOT AVAIL Customer @ Customer.CustName  cdurat
 ttCall.InvCust  invcust.CustName  WHEN     AVAIL invcust
             "!! UNKNOWN !!"   WHEN NOT AVAIL invcust @ invcust.CustName  Time-E
 ttCall.CLI

 DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
          ttCall.gsmbnr,
          ttCall.custnum,
          ttCall.bdest,
          ttCall.BType,
          ttCall.bpref,
          true) @ ttCall.GsmBnr 

 ttCall.BDest   
 ttCall.rateCCN CCN.CCNName        WHEN     AVAIL ccn 
 ttCall.CCN bCCN.CCNName        WHEN     AVAIL bccn 
 lcBDName
 BillTarget.RatePlan WHEN AVAIL Billtarget
 ttCall.bpref
 lcInvNum
 lcCDRType
 lcDataAmt
WITH  FRAME cdr .

IF LOOKUP(STRING(ttCall.spo,"9999"),"0003,0004,0007,0053,0093") > 0 AND 
   ttCall.ppFlag = 0 THEN DO:

   PAUSE 0.
   DISP
   ttCall.BillCode  BillItem.BIName    WHEN     AVAIL BillItem
   "!! UNKNOWN !!"   WHEN NOT AVAIL BillItem @ BillItem.BIName
   ttCall.StartCharge
   ttCall.Amount - ttCall.StartCharge @ ttCall.mpmamt
   ttCall.Amount
   WITH FRAME RoamCDR.
END.
ELSE IF ttCall.PPFlag = 1 THEN DO:
   
   PAUSE 0.
   DISP
   ttCall.BillCode  BillItem.BIName    WHEN     AVAIL BillItem
                     "!! UNKNOWN !!"   WHEN NOT AVAIL BillItem @ BillItem.BIName
   ppCharge
   ttCall.Charge 
   WITH FRAME PrepCDR.                      

END.
ELSE DO:

   PAUSE 0.
   DISP
   ttCall.BillCode  BillItem.BIName    WHEN     AVAIL BillItem
                     "!! UNKNOWN !!"   WHEN NOT AVAIL BillItem @ BillItem.BIName
   ttCall.StartCharge
   ttCall.Charge
   ttCall.Amount - ttCall.StartCharge @ ttCall.mpmamt 
   ttCall.Amount
   WITH FRAME PostCDR.
   
END.

PAUSE 0.
                                                 
IF ttCall.ErrorCode > 0 THEN DO:
   FIND MobError WHERE          
        MobError.MobError = ttCall.ErrorCode NO-LOCK NO-ERROR.
   lcErrName = "ERROR:" +    
               IF AVAILABLE MobError THEN MobError.MeName 
               ELSE STRING(ttCall.ErrorCode).
END.               
ELSE lcErrName = "".

IF lcErrName ne "" THEN DO:
   COLOR DISPLAY  message lcErrName  WITH FRAME cdr.    
   disp lcErrName with frame cdr. PAUSE 0.
END.           
PAUSE 0.

Action:                    
repeat WITH FRAME cdr:    
   ASSIGN 
   ufk = 0 
   ufk[2] = 9790
   ufk[4] = 1112  
   ufk[6] = 1115 
   ufk[5] = 2421
   ufk[7] = 1772
   ufk[8] = 8 
   ehto = 0.

   RUN ufkey.

   IF toimi = 2 THEN DO:
      RUN edrhistory_one_edr.p(ttCall.CLI,
                               ttCall.DateSt,
                               ttCall.TimeSt,
                               ttCall.DtlSeq).
   END.
   
   ELSE IF toimi = 4 THEN DO:
       /* View rate record */
       RUN nnhitt2(ttCall.TariffNum).
   END.

   ELSE if toimi = 7 THEN DO:
         
         bsub = DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
                  ttCall.Gsmbnr,
                  ttCall.custnum,
                  ttCall.bdest,
                  ttCall.BType,
                  ttCall.bpref,
                  TRUE).

      RUN   local-Show-record.

      IF reasonc > "0" THEN DO TRANS:

         hide frame reason no-pause.

         CREATE eventlog.
         ASSIGN
         eventlog.eventdate  = TODAY
         eventlog.eventtime  = STRING(TIME,"HH:MM:SS")
         eventlog.usercode   = katun
         eventlog.action     = 'Check'.
         ASSIGN
         eventlog.KEY        =  reasonT + chr(255) + ttCall.cli + chr(255) +
                                STRING(ttCall.datest,"99-99-9999") + chr(255) + 
                                string(ttcall.TimeStart, "hh:mm:ss")
         eventlog.tablename      = "ttCall"
         eventlog.ModifiedFields = "ttCall.gsmbnr"
         eventlog.FieldFormats   = "Char"
         eventlog.DataValues     =  ttCall.gsmbnr. 

         bsub = ttCall.gsmbnr.

        disp bsub @ ttCall.gsmbnr WITH FRAME cdr.  PAUSE 0.
        ufk[5] =  0.
        run ufkey.p.
      END.   
   end.
   
   ELSE IF toimi = 6 THEN DO:
      RUN viewmcdr2.p(INPUT ttCall.Datest, ttCall.Dtlseq,
                    IF ttCall.CDRTable > ""
                    THEN ttCall.CDRTable
                    ELSE "MobCdr").
   END.

   ELSE IF toimi = 5 THEN DO:
      RUN viewtable.p((BUFFER ttcall:HANDLE)).
   END.

   ELSE IF toimi = 8 THEN LEAVE Action.
END.

HIDE FRAME cdr     NO-PAUSE.
HIDE FRAME PrepCDR NO-PAUSE.

PROCEDURE local-Show-record:
   REPEAT ON ENDKEY UNDO, LEAVE:

      DISP
      WITH FRAME reason.
      UPDATE
          ReasonC
          reasonT
      WITH FRAME reason
      EDITING:
             READKEY.
             IF FRAME-FIELD = "ReasonC" AND keylabel(lastkey) = "F9" 
             THEN DO:
                RUN h-tmscodes(INPUT "FixCDR",  /* TableName*/
                                     "ReasonCode", /* FieldName */
                                     "ReasonCode", /* GroupCode */
                               OUTPUT siirto).
                 ASSIGN ReasonC = siirto.
                 find first TMSCodes WHERE
                              TMSCodes.TableName = "FixCDR"       AND
                              TMSCodes.FieldName = "ReasonCode"   AND
                              TMSCodes.CodeGroup = "ReasonCode"   AND
                              TMSCodes.CodeValue =  reasonC
                 no-lock no-error.
                 IF avail tmscodes then do:
                    disp reasonc tmscodes.codename with frame reason. 
                     reasont = tmscodes.codename.
                    IF INPUT  FRAME Reason  reasonC NE "4" THEN LEAVE.
                 ENd.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME reason:
                PAUSE 0.
                IF FRAME-FIELD = "ReasonC" THEN DO:
                   find first TMSCodes WHERE
                              TMSCodes.TableName = "FixCDR"  AND
                              TMSCodes.FieldName = "ReasonCode" AND
                              TMSCodes.CodeGroup = "ReasonCode"   AND
                              TMSCodes.CodeValue = INPUT  FRAME Reason  reasonC
                   no-lock no-error.
                   if not available TMSCodes then do:
                      MESSAGE 
                         "Unknown Reason  Code" 
                         input  FRAME reason reasonC       
                         VIEW-AS ALERT-BOX.
                      NEXT-PROMPT reasonC. NEXT.
                   END.
                   disp tmscodes.codename with frame reason.
                   reasont = tmscodes.codename.
                   IF INPUT  FRAME Reason  reasonC NE "4" THEN 
                   LEAVE.
                END.
                IF FRAME-FIELD = "ReasonT" THEN DO:
                    IF INPUT ReaSonT = "" THEN DO:
                       BELL.
                       MESSAGE 
                       "Must be some reason?".
                       NEXT-PROMPT reasont. NEXT.
                    END.
                    Assign ReasonT.

                ENd.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.


