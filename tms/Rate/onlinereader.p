/* ----------------------------------------------------------------------
  MODULE .......: Onlinereader
  TASK .........: read Mobile CDR files into Billing Database
  APPLICATION ..: 
  AUTHOR .......: 
  CREATED ......: 10.07.2007
  CHANGED ......: 14.12.2006 nagios changes
                  11.01.07/aam fInvSeq to mobcdr_rate.i
  VERSION ......: YOIGO
------------------------------------------------------------------------ */
   
{commali.i}
{func.i}
{email.i}
{cparam2.i}
{cdrvar.i}
{fcustcnt.i}  
{chkbal2.i}
{mobol_tt.i}
{fmakeservice.i}
{timestamp.i}
{fsubser.i}
{fservlimit.i}
{onlinevar.i}
{daycampaign.i}
{ficora.i}
{heartbeat.i}
{cdrstream_counter.i}
{error_codes.i}
{tmsconst.i}
{rating_ttcall.i}
{rate_roamzone.i}
{onlinereader_oldcdr.i}
{rating_double_check.i}
{premiumnumber.i}
{create_eventlog.i}
{cdr_online.i}

def INPUT PARAMETER    pvmlog      as lo   no-undo FORMAT "Yes/No" init true.
def INPUT PARAMETER    ticfile     as char FORMAT "x(30)" no-undo.
def INPUT PARAMETER    bDispErrors as lo   no-undo.
DEF INPUT PARAMETER    iiPort      AS INT  NO-UNDO.

SESSION:SYSTEM-ALERT-BOXES = TRUE. 
SESSION:NUMERIC-FORMAT     = "European".

DEF STREAM callrec.
DEF STREAM errors.
DEF STREAM cdrfiles.
def stream alarms.     /* monthly call alarm + cdrp error stream */

/* TEMP-TABLES */

DEF TEMP-TABLE ttDetail NO-UNDO LIKE mcdrdtl.McdrDtl2.


def new shared var callrec  as c  no-undo FORMAT "x(360)".
DEF VAR cdrfile     AS C  NO-UNDO.
DEF VAR amt2        AS I  NO-UNDO.
DEF VAR amt3        AS I  NO-UNDO.
DEF VAR ok          AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR bleave      AS LO  NO-UNDO.
def var c_error1    as C   no-undo.
def var errfile     as char no-undo.
DEF VAR  bol        AS LOG NO-UNDO.

DEF VAR lcSep       AS C  NO-UNDO INIT "|".
DEF VAR slseq              AS I  NO-UNDO.
DEF VAR CallTimeStamp      AS DE NO-UNDO.
def var errdir             as char no-undo.
def var bOLMsg             as lo  no-undo.
DEF VAR CreditType         AS I    NO-UNDO.
DEF VAR liunkcust          AS INT  NO-UNDO.
DEF VAR lcNotifyNumber     AS CHAR NO-UNDO.

DEF VAR b_PNP              AS LO NO-UNDO.
DEF VAR b_dest             AS C  NO-UNDO.
DEF VAR b_ccn              AS I  NO-UNDO.
DEF VAR b_prodcode         AS C  NO-UNDO.
DEF VAR b_dg-code          AS C  NO-UNDO.
DEF VAR b_foc              AS LO NO-UNDO.
DEF VAR b_pref             AS C  NO-UNDO.
DEF VAR b_asubtype         AS I  NO-UNDO.

DEF VAR count      AS I    NO-UNDO.
DEF VAR TotValue   AS DE   NO-UNDO.
DEF VAR old_price  AS DE   NO-UNDO.
DEF VAR new_price  AS DE   NO-UNDO.
DEF VAR lInvSeq     AS I   NO-UNDO.
DEF VAR lcSetVersion  AS CHAR  NO-UNDO.
DEF VAR ldtTMSTime    AS DATETIME NO-UNDO. 

{tariff_tt.i}

{mobcdr_rate.i}
{detailseq.i}
 
DEF VAR b_btluok          AS C       NO-UNDO.
DEF VAR r_dest            AS C       NO-UNDO.
DEF VAR xConfDir          AS C       NO-UNDO.
DEF VAR errorcode         AS I       NO-UNDO.
DEF VAR MobsubLimit       AS INT     NO-UNDO.
/* vas */
DEF VAR lcBillCode        AS CHAR    NO-UNDO. 
DEF VAR ldtRefill         AS DATE    NO-UNDO.

DEF VAR lcreader  AS CHAR NO-UNDO.
DEF VAR liPause   AS INT  NO-UNDO.
DEF VAR liNagios  AS INT  NO-UNDO.
DEF VAR ldtDate   AS DATE NO-UNDO.
DEF VAR lcTime    AS CHAR NO-UNDO.
DEF VAR liStream  AS INT  NO-UNDO.
DEF VAR lcReturn  AS CHAR NO-UNDO.
DEF VAR lhttCall  AS HANDLE NO-UNDO.

DEF VAR liTempDialType AS INT NO-UNDO.

{tmsparam.i RepConfDir        return}. xConfDir        = tmsparam.CharVal.
{tmsparam.i ErrCDouble        return}. errorcode       = tmsparam.IntVal.
{tmsparam.i UnknownCustomer   RETURN}. liunkcust       = tmsparam.IntVal.


CASE iiPort:
WHEN 2210 THEN ASSIGN
   liStream = 1
   lcReader = "MO:Postpaid".
   
WHEN 2211 THEN ASSIGN
   liStream = 2
   lcReader = "MO:Postpaid2".

WHEN 2212 THEN ASSIGN
   liStream = 3
   lcReader = "MO:Postpaid3".

WHEN 2213 THEN ASSIGN
   liStream = 4
   lcReader = "MO:Postpaid4".

WHEN 2214 THEN ASSIGN
   liStream = 5
   lcReader = "MO:Postpaid5".

WHEN 2215 THEN ASSIGN
   liStream = 11
   lcReader = "MO:Postpaid6".
   
WHEN 2216 THEN ASSIGN
   liStream = 12
   lcReader = "MO:Postpaid7".

WHEN 2217 THEN ASSIGN
   liStream = 13
   lcReader = "MO:Postpaid8".

WHEN 2218 THEN ASSIGN
   liStream = 14           
   lcReader = "MO:Postpaid9".

WHEN 2219 THEN ASSIGN
   liStream = 15
   lcReader = "MO:Postpaid10".

WHEN 2220 THEN ASSIGN
   liStream = 6
   lcReader = "MO2:Prepaid".
   
WHEN 2221 THEN ASSIGN
   liStream = 7
   lcReader = "MO2:Prepaid2".

WHEN 2222 THEN ASSIGN
   liStream = 8
   lcReader = "MO2:Prepaid3".

WHEN 2223 THEN ASSIGN
   liStream = 9
   lcReader = "MO2:Prepaid4".
   
WHEN 2224 THEN ASSIGN
   liStream = 10
   lcReader = "MO2:Prepaid5".

WHEN 2250 THEN ASSIGN
   liStream = 30
   lcReader = "MO3:VAS-CGY".

WHEN 2270 THEN ASSIGN
   liStream = 40
   lcReader = "MO5:RoamFraud".

OTHERWISE ASSIGN 
   liStream = 99
   lcreader = "MO4:Temporarily_online".
END CASE.

form
   cdrfile         column-label "Source"    FORMAT "x(25)"
   ldtDate         column-label "Date"      FORMAT "9999-99-99"
   lcTime           column-label "Time"      FORMAT "x(8)"
   amt2            column-label "Amt/File"  FORMAT "zzzzzzz9"
   amt3            column-label "Total Amt" FORMAT "zzzzzzzzz9"
WITH ROW 2 CENTERED WIDTH 70 TITLE " LAST SAVED CALL (ONLINE STARTED " +
STRING(TODAY,"99-99-99") + " " + STRING(time,"HH:MM:SS") + ")" FRAME clog.

VIEW FRAME clog . pause 0.
DISP " PORT " + STRING(iiport) + " WAITING..." @ cdrfile WITH FRAME cLOG.

FORM
   c_error1 AT 2  NO-LABEL FORMAT "x(75)" 
with 
   row 7 scroll 1 11 down centered width 78 
   title "   M E S S A G E S   &   E R R O R S   " frame err.

disp " " @ c_error1 with frame err.


FUNCTION fBCopy RETURNS LOGICAL.
   
   DEF VAR lcTemp     AS CHAR No-UNDO.
   DEF VAR lcCDR      AS CHAR No-UNDO.
   DEF VAR liLoop     AS INT  No-UNDO.
   DEF VAR llErrorCDR AS LOG  NO-UNDO.
   DEF VAR llSaved    AS LOG  NO-UNDO.
   
   if amt3 mod 1 = 0 THEN DO:
      DISP
         "PORT:" + STRING(iiPort) + " MSC:" + lcmscid  @ cdrfile 
         ttcall.Datest @ ldtdate
         STRING(ttCall.Timest,"hh:mm:ss") @ lcTime   
         amt2  
         amt3           
      WITH FRAME CLOG.
      PAUSE 0 . 
   END.

   llErrorCDR = FALSE.
   IF ttCall.PPFlag = 0 AND ttCall.ErrorCode > 0 THEN DO:
      IF CAN-FIND(FIRST ttCDRError WHERE 
                        ttCDRError.CDRError = ttCall.ErrorCode)
      THEN llErrorCDR = TRUE. 
   END.
 
   IF ttCall.mscid = "NRTRDE" THEN DO:
      CREATE FraudCdr.
      BUFFER-COPY ttCall TO FraudCdr.
   END.
   ELSE DO:
   
      /* if this is an old cdr that came in late then check if it should be 
         saved to old db, otherwise or if this fails then save to current db */
      llSaved = FALSE.
      IF ttCall.DateSt < TODAY THEN DO:
         llSaved = fSaveCDR2OldDb(IF llErrorCDR THEN "ErrorCDR"
                                  ELSE IF ttCall.PPFlag = 0 THEN "MobCDR"
                                       ELSE "PrepCDR",
                                  lhttCall).
      END.

      IF NOT llSaved THEN DO:
         IF llErrorCDR THEN DO:
            CREATE roamcdr.ErrorCDR.
            BUFFER-COPY ttCall TO ErrorCDR.
            RELEASE ErrorCDR.
         END.   
         ELSE IF ttCall.PPFlag = 0 THEN DO:
            CREATE mcdr.Mobcdr.   
            BUFFER-COPY TTCall TO Mobcdr.
         END.    
         ELSE DO:
            CREATE prepcdr.PrepCdr.
            BUFFER-COPY ttCall TO PrePcdr.
         END.
      END.
   END.
   
   DO liLoop = 1 TO NUM-ENTRIES(callrec,"|"):
      
      ASSIGN
      lcTemp = ENTRY(liLoop,callrec,"|")
      lcTemp = RIGHT-TRIM(lcTemp)
      lcTemp = LEFT-TRIM(lcTemp).
                                    
      IF liLoop = 1 THEN lcCDR = lcTemp.
      ELSE               lcCDR = lcCDR + "|" + lcTemp.
   END.
   
   IF ttCall.MSCID = "NRTRDE" THEN DO:
      CREATE FraudDtl.
      ASSIGN 
         FraudDtl.Datest  = ttCall.Datest 
         FraudDtl.DtlSeq  = ttCall.DtlSeq
         FraudDtl.Version = lcVersion
         FraudDtl.Detail  = lcCDR.
   END.
   ELSE DO:
   
      llSaved = FALSE.
      IF ttCall.DateSt < TODAY THEN DO:
         llSaved = fSaveDetail2OldDb(IF llErrorCDR THEN "ErrorCDR"
                                     ELSE "McdrDtl2",
                                     ttCall.DateSt,
                                     ttCall.DtlSeq,
                                     lcVersion,
                                     lcCDR).
      END.

      IF NOT llSaved THEN DO:
         IF llErrorCDR THEN DO:
            CREATE roamcdr.ErrorDtl.
            ASSIGN 
               ErrorDtl.Datest  = ttCall.Datest 
               ErrorDtl.DtlSeq  = ttCall.DtlSeq
               ErrorDtl.Version = lcVersion
               ErrorDtl.Detail  = lcCDR.
         END.
         ELSE DO:
            CREATE mcdrdtl.McdrDtl2.
            ASSIGN 
               McdrDtl2.Datest  = ttCall.Datest 
               McdrDtl2.dtlseq  = ttCall.DtlSeq
               McdrDtl2.Version = lcVersion
               McdrDtl2.Detail  = lcCDR.
         END.
      END.
   END.
   
   IF ttCall.ErrorCode = 0 THEN DO:
      
      CREATE TMQUEUE.
      BUFFER-COPY ttCall TO TMQUEUE.
         
      ASSIGN
         TmQueue.Qty     = 1
         TmQueue.EventID = ttCall.DtlSeq
         TmQueue.Source  = ttCall.MSCID
         TmQueue.PayType = 1 + INT(ttCall.PPFlag > 0)
         TMQueue.ReportingID = ttCall.ServRid + "," + ttCall.MPMRid
         TMQueue.ExtraAmount = ttCall.MPMAmt.
      IF ttCall.PPFlag > 0 THEN DO:
         IF LOOKUP(lcVersion,"0104,0105,0106") > 0 THEN 
            TMQueue.PPBalance = DECIMAL(ENTRY(74,lcCDR,"|")) NO-ERROR.
         TmQueue.Amount = ttCall.Charge.
      END.
         
      RELEASE TMQueue.
   END.
   
   /* handling time for kpi */
   fUpdateKPICounter(ttCall.MSCID,
                     liStream,
                     ldtTMSTime,
                     lcMediatorTime).
                     
   delete TTCall.
   RELEASE mcdr.mobcdr.
   RELEASE prepcdr.prepCDR.
   RELEASE FraudCdr.

   RETURN ERROR-STATUS:ERROR.
END.

FUNCTION fCleanTime RETURNS CHARACTER
  (INPUT tme AS CHAR, INPUT del AS CHAR).
   DEF VAR hit AS i NO-UNDO.
   LOOP:
   REPEAT:
      hit = index(tme,del).
      if hit > 0 then substr(tme,hit,1) = "".
      ELSE LEAVE LOOP.
   END.
   RETURN tme.
END.

/* errorlog must be openend now though the day of 1. cdr is not yet known
   thus name is only "err0000.log". */

assign
   errfile    = "pre-err.log"
   bOL        = (ticfile = "Mobile_OnLine")
   lhttCall   = BUFFER ttCall:HANDLE.

if NOT bOL then input stream cdrfiles from value(ticfile).
else do:
   /* OnLine started message */
   assign
      callrec = "@OnLine started:"
      callrec = callrec + fill(" ",20 - length(callrec))
      callrec = callrec + "Date: " + string(today,"99-99-99") +
                          "Time: " + string(time,"hh:mm:ss")
      substr(callrec,43,8) = "NOTICE:".
end.      


ehto = 3. run ufkey.
   
   /* QUIT menutext */
   run ufxkey(8,3).
   
   If NOT bOL then
      disp
         "Reading CDR file:" @ c_error1
      with frame err.

   
{ticketfunc.i}
{rating_package.i}

fFillTT().
ldtRefill = Today.
   
MOBCDR:   
DO WHILE TRUE  WITH FRAME CLOG:
  
   VIEW frame err.
   PAUSE 0.

   IF ldtRefill NE TODAY THEN DO:
      lcReturn = "RESET".
      LEAVE MobCDR.
   END.
 
   ETIME(YES).
   
   liNagios = fKeepAlive(lcReader).
      
   liPause = 60.
   
   if bOL then callrec = readOnline (iiPort , lipause).
   else do:
      import stream cdrfiles UNFORMATTED callrec  no-error.

      if error-status:error then do:
          message "File reading is finished !" view-as alert-box
          title " CDRs from file ".
          leave MOBCDR.
       end.
   end.

   IF ETIME >= liPause * 1000 AND callrec = "" THEN NEXT Mobcdr.

   if bOL AND callrec = "" then do:
      readkey pause 0.
      ok = false.
      
      case lastkey:
        when keycode("F8") OR when keycode("8") then
           message "ARE YOU SURE YOU WANT TO STOP READING THE CDRs (Y/N) ?"
              update bLeave.
        otherwise next Mobcdr.
      end.

      if bLeave then leave MOBCDR.
   end.
   else do:
      readkey pause 0.
      ok = false.
      case lastkey:
         when keycode("F8") OR when keycode("8") then do:
            message "ARE YOU SURE YOU WANT TO STOP READING THE CDRs (Y/N) ?"
              update ok.
           if ok then leave MOBcdr.
        end.
      end.
   end.

   If NOT bOL then
      disp
         "Reading CDR file:" @ c_error1
      with frame err.

   /* OnLine error messages */
   if bOLMsg then do:
      /* if error diplaying is allowed */
      if bDispErrors then do:
         disp 
            substr(callrec,1,75)  @ c_error1 
         with frame err. 
         down with frame err.
         if frame-down = 10 then scroll up.
         pause 0.
         disp 
            substr(callrec,1,75)  @ c_error1
         with frame err.
         down with frame err.
         if frame-down = 10 then scroll up.
         pause 0.             
         output stream alarms to value(errdir + errfile) append.
         put stream alarms unFORMATted substr(callrec,2) chr(10).
         output stream alarms close.
      end. /* bDispErrors */
   end.

   IF LOOKUP(TRIM(Entry(1,callrec,lcSep)),'"ESPXF') = 0 AND 
      LOOKUP(TRIM(Entry(1,callrec,lcSep)),'ESPXF') = 0 
   THEN DO:

      /** fNagios(lcreader). **/
       
      DISP
         STRING(time,"hh:mm:ss") + " " + 
         substr(callrec,1,65)  @ c_error1
         with frame err.
      down with frame err.
      PAUSE 0.                             
      NEXT.
   END.

DO TRANS: 

   ASSIGN  
      lcVersion    = TRIM(ENTRY( 5,callrec,lcSep))
      lcRecordType = TRIM(ENTRY( 4,callrec,lcSep))
      lcSetVersion = lcVersion + lcRecordType.
 
   CASE lcSetVersion:
   WHEN "0101MM" OR 
   WHEN "0101YC" OR
   WHEN "0102GE" THEN lcEvent = TRIM(ENTRY(9,callrec,lcSep)).
   WHEN "0102YC" OR 
   WHEN "0101YF" OR
   WHEN "0103MM" OR
   WHEN "0104GE" OR 
   WHEN "0104MM" OR
   WHEN "0105MM" OR
   WHEN "0106MM" THEN lcEvent = TRIM(ENTRY(11,callrec,lcSep)).
   OTHERWISE DO:
      MESSAGE 
         "Unknown SNS-CDR format (cdr-ticket no:"  ENTRY( 8,callrec,lcSep) 
      VIEW-AS ALERT-BOX.
      NEXT.
   END.
   END CASE. 
   
   IF lcEvent = "HEAD" THEN DO:
      amt2 = 0 .
      NEXT.
   END.
                                 
   ELSE IF lcEvent = "TRAI" THEN DO:
      NEXT MobCDR.
   END.
                                                      
   /* it was a real CALL record, NOT a start OR END record */
   ELSE DO : 
      {set_to_empty.i}   
      CASE lcSetVersion:
      WHEN "0101MM" THEN DO:  {set0101mm.i}   END.
      WHEN "0101YC" THEN DO:  {set0101yc.i}   END.
      WHEN "0101YF" THEN DO:  {set0101yf.i}   END.
      WHEN "0102GE" THEN DO:  {set0102gen.i}  END.
      WHEN "0102YC" THEN DO:  {set0102yc.i}   END.
      WHEN "0103MM" THEN DO:  {set0103mm.i}   END.
      WHEN "0104GE" THEN DO:  {set0104ge.i}   END.
      WHEN "0104MM" OR WHEN
           "0105MM" THEN DO:  {set0104mm.i}   END.
      WHEN "0106MM" THEN DO:  {set0106mm.i}   END.
      END CASE.

      {onlinesave.i}  

      ASSIGN ttcall.rateccn = ttcall.spocmt.

      IF LOOKUP(STRING(ttCall.spocmt),"7,33,96,106") > 0 THEN
         ASSIGN
         ttCall.CLI      = lcDestAddress
         ttCall.gsmbnr   = lcOrigAddress
         ttCall.Roaming  = 1.
      
      ELSE IF ttCall.spocmt = 78 THEN 
         ASSIGN ttCall.CLI      = lcOrigAddress
                ttCall.gsmbnr   = lcSubsInfo.
      ELSE IF ttCall.spocmt = 72 OR 
              ttCall.spocmt = 73 THEN DO: 
         
         ttCall.gsmbnr = lcSubsInfo.
      END.   

      ELSE IF ttCall.spocmt = {&GB_CCN} THEN DO:   
         ttCall.gsmbnr = {&GB_B_NBR}.
      END.    
      
      ELSE IF ttCall.spocmt = 71 OR
              ttCall.spocmt = 74 THEN DO:
         
      END.
      
      /* MMS service manipulating */
      ELSE IF ttCall.spocmt = 94 OR 
              ttCall.spocmt = 95 THEN DO:
      END.        

      IF LOOKUP(string(ttCall.spocmt,"999"),"000") > 0 THEN DO:
         ASSIGN
            ttCall.errorcode = {&CDR_ERROR_NON_BILLABLE_CALL_FORWARDING} 
            ttCall.Invseq    = 0 .
         fBCopy().
         NEXT mobcdr.
      ENd.

      /* UPDATE record counter */
      ASSIGN
         ttCall.ReadDate  = TODAY
         ttCall.ReadTime  = TIME
         ttCall.ReadInTS  = fMake2Dt(ttCall.ReadDate, ttCall.ReadTime)
         ldtTMSTime       = NOW
         TTCall.custNum   = liunkcust
         amt2 = amt2 + 1
         amt3 = amt3 + 1.

      /**** ANALYSE  **********/

      IF lcStartDate NE "" THEN ASSIGN
         ttCall.Datest     = DATE(INT(SUBSTR(lcStartDate,5,2)),  /* MONTH */
                                  INT(SUBSTR(lcStartDate,7,2)),  /* DAY   */
                                  INT(SUBSTR(lcStartDate,1,4))). /* YEAR  */

      IF lcStartTime  NE "" THEN  ASSIGN
          /* Convert time fields into INTEGER cormat (no. of seconds) */
         TTCall.TimeStart  =  3600 * INT(SUBSTR(lcStartTime,1,2)) +
                                60 * INT(SUBSTR(lcStartTime,3,2)) +
                                     INT(SUBSTR(lcStartTime,5,2)).

      ttCall.dtlseq = fStreamSequence(INPUT ttCall.datest, liStream).

      oiErrorCode = fRawTicketCheck().

      IF oiErrorCode = 0 THEN 
         oiErrorCode = fCallCaseCheck(ttCall.SpoCMT,ttCall.DateSt).
       
      IF oiErrorCode = 0 THEN 
         fTicketCheck(INPUT "EVENT",   
                      STRING(ttCall.EventType), 
                      OUTPUT oiErrorCode).

      IF oiErrorCode = 0 THEN 
         fTicketCheck(INPUT "btype",   
                      STRING(ttCall.bType),     
                      OUTPUT oiErrorCode).
                              
      IF oiErrorCode = 0 THEN 
         fTicketCheck(INPUT "GSMBNR",  
                      STRING(ttCall.gsmbnr),   
                      OUTPUT oiERrorCode).
                                     
      ASSIGN 
         CallTimeStamp = YEAR(ttCall.Datest) * 10000 + 
                         Month(ttCall.DateST) * 100 + 
                         DAY(ttCall.Datest) + (ttCall.TimeStart / 100000).

      IF oiErrorCode = 0 AND LOOKUP(STRING(ttCall.spocmt),"7,33") > 0 THEN DO:
         IF lcMSRN = "" AND ttCall.MSCID NE "NRTRDE" THEN oiErrorCode = 9012.
      END.
      
      IF oiErrorCode = 0 THEN DO:
         IF ttCall.MSCID NE "NRTRDE" AND
            (LOOKUP(STRING(ttCall.Spocmt),"3,4,7,17,32") = 0 OR
            (LOOKUP(STRING(ttCall.SpoCMT),"3,7") > 0 AND 
             ttCall.MSCID = "PRE" AND ttCall.PPFlag = 1))
         THEN DO:
            IF lcSourceName NE "FIXED" THEN
               fTicketCheck(INPUT "MSOWNER", 
                            STRING(ttCall.CLI),
                            OUTPUT oiERrorCode).               
            ELSE
             fTicketCheck(INPUT "MSOWNER_FIXED",
                            STRING(ttCall.CLI),
                            OUTPUT oiERrorCode). 
         END.
         ELSE DO: 
            IF  LOOKUP(STRING(ttCall.Spocmt),"3,4,32") > 0 THEN 
               fTicketCheck(INPUT "IMSI", 
                            STRING(ttCall.IMSI),
                            OUTPUT oiErrorCode).
            ELSE fTicketCheck(INPUT "IMSI", 
                              STRING(ttCall.IMSI2),
                              OUTPUT oiErrorCode).
         END.                     
      END.

      /* NRTRDE stream does not contain ppflag, so force it */
      IF AVAIL MSOwner AND 
         ttCall.MSCID = "NRTRDE" THEN ttCall.PPFlag = INT(MSOWNER.PayType).

      IF oiErrorCode   = 0  AND 
         ttCall.SpoCMT = 51 AND 
         ttCall.GsmBnr = "" THEN DO:
     
         ASSIGN oiErrorCode = {&CDR_ERROR_UNKNOWN_GSM_B_NUMBER}.
      END.

      /* gprs without any data */
      IF oiErrorCode = 0 AND  
         LOOKUP(STRING(ttCall.SpoCMT),"90,91,92,93") > 0 AND 
         ttCall.DataIn + ttCall.DataOut = 0 THEN DO:
         /* prepaid */
         IF ttCall.PPFlag > 0 THEN DO:
            IF ttCall.Charge < 0.01 THEN 
               oiErrorCode = {&CDR_ERROR_NATIONAL_GPRS_ZERO_DATA}.
         END.   
         /* postpaid */
         ELSE DO:
            IF ttCall.SpoCMT = 93 THEN 
               oiErrorCode = {&CDR_ERROR_NATIONAL_GPRS_ZERO_DATA}.
            ELSE oiErrorCode = {&CDR_ERROR_ROAMING_GPRS_ZERO_DATA}.
         END.   
      END.   

      /* roaming voice mail forwarding */
      IF oiErrorCode = 0 THEN DO:
       
         /* - mark VM b-number 633 to CC 32 (mediator marks only 632) */
         IF LOOKUP(STRING(ttCall.SpoCMT),"3,4") > 0 AND
            ttCall.DateSt >= 7/1/10 AND
            LOOKUP(ttCall.GsmBnr,"633,633633633") > 0 AND
            lcForwardIndicator = "1" THEN 
               ttCall.SpoCMT = 32.
      
         IF ttCall.SpoCMT = 32 THEN oiErrorCode = 9011.
         ELSE IF ttCall.SpoCMT = 33 THEN oiErrorCode = 9010.
      END.

      IF oiErrorCode = 0 AND
         msowner.CLIType EQ "CONTM2" AND
         ttCall.EventType EQ "CALL" THEN 
         oiErrorCode = {&CDR_ERROR_NON_BILLABLE_CONTM2_CALL}.
      
      /* Anonymous numbers */
      IF liBtype = 22 THEN oiErrorcode = {&CDR_ERROR_NON_INVOICEABLE_CALL}.
                              
      IF oiErrorCode > 0 THEN DO:
         ASSIGN
         ttCall.ErrorCode = oiErrorCode
         ttCall.InvSeq    = 0.
                                                                     
         fBCopy().
         NEXT mobcdr.
      END.
                                                        
      IF ttCall.ppFlag ne INT(Msowner.PayType) THEN DO:
         /* emm cannot always determine correct type for subscription and
            returns code 6, 'unknown' */
         IF ttCall.ppFlag = 0 AND LOOKUP(ttCall.SubsType,"6, 6") > 0 THEN
            ttCall.ppFlag = 1.
         ELSE DO:
            ttCall.ErrorCode = {&CDR_ERROR_WRONG_PAY_TYPE}.
            fBCopy().
            NEXT.
         END.
      END.

      IF llSaldoReminder AND ttCall.ppFlag = 0 AND
         CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq = MsOwner.MsSeq) THEN DO:
         
         ASSIGN CreditType      = fCreditTypeValue(MsOwner.MsSeq,
                                                   OUTPUT MobSubLimit).
         IF MobSubLimit NE 0 THEN
         lcNotifyNumber = fNotifyNbrValue(MsOwner.MsSeq).
      END.   
      ELSE ASSIGN
           MobsubLimit       = 0
           CreditType        = 0
           lcNotifyNumber    = "".
         
         /************************************
         * A-Sub was now recognized.  UPDATE *
         * associated customer data fields   *
         ************************************/

      ASSIGN
         TTCall.custNum    = msowner.CustNum            
         TTCall.BillTarget = msowner.BillTarget    /* Invoicing Target   */
         TTCall.MSSeq      = msowner.MSSeq   /* Sequence FOR a Subscrip   */
         TTCall.imsi       = MSOwner.IMSI    /* save imsi number */
         TTCall.CLIType    = msowner.CLIType /* MobSub connection type  */
         lcratebrand       = msowner.Brand.

      /***************************************
      * Invoicing Definitions from Billtarget:  *
      *                                      *
      * Note that there can be a different   *
      * Invoicing Customer whose CustNo is   *
      * stored in A-Sub Customer's Invoicing *
      * Target Record                        *
      ***************************************/

      /*******************************
      * CALLING PART IS NOW ANALYSED *
      *                              *
      * NEXT ANALYSE CALLED PART     *
      * -> destination               *
      * -> product code              *
      * -> discount group            *
      * -> CCN                       *
      *******************************/

      ASSIGN
         ttCall.CustNum    = MsOwner.CustNum
         ttCall.InvCust    = MsOwner.InvCust
         ttCall.AgrCust    = MsOwner.AgrCust
         ttCall.billTarget = MsOwner.BillTarget.
      
      DO:     

         fanalbsub( INPUT  ttCall.CustNum,
                       INPUT  ttCall.GsmBnr,
                       INPUT  ttCall.Btype,
                       OUTPUT b_dest,
                       OUTPUT r_dest,
                       OUTPUT b_ccn,
                       OUTPUT b_dg-code,
                       OUTPUT b_foc,
                       OUTPUT b_PNP,
                       OUTPUT b_prodcode,
                       OUTPUT b_asubtype,
                       OUTPUT oiErrorcode).

         IF b_dest = "" OR b_dest = ? OR oiErrorCode NE 0 THEN DO:
            ASSIGN
               TTCall.ErrorCode = oiErrorCode WHEN oiErrorCode NE 0 
               TTCall.ErrorCode = IF ttCall.SpoCMT = 200
                                  THEN {&CDR_ERROR_UNKNOWN_VOIP_B_DESTINATION}
                                  ELSE {&CDR_ERROR_UNKNOWN_B_DESTINATION}
                                  WHEN oiErrorCode EQ 0.
            fBCopy().
            NEXT MobCDR.
         END.

         IF fIsDoubleCall("online",?) THEN DO:
            fBCopy().
            NEXT MobCDR.
         END.
         
         /************************************
             * B-sub was identified: NEXT        *
         * UPDATE B-sub inFORMATion on TTCall  *
         ************************************/
 
         ASSIGN
            TTCall.BDest     = b_dest       /* Classified Destination   */
            TTCall.ccn       = b_ccn        /* Consecutive Country No.   */
            TTCall.BillCode  = b_prodcode   /* ONLY WHEN PNP */
            ttCall.dialtype  = b_asubtype.

            /* SOME SPECIAL CASES */

            /* Data call get always same product code */
      END.

      ASSIGN 
         lidialtype = ttCall.Dialtype
         liCCN      = ttCall.RateCCN.

      IF ttCall.Spocmt = 66 OR
         ttCall.Spocmt = 1066 THEN DO:
        
           liccn = 0.
           IF ttCall.Spocmt = 66 THEN liTempDialType = 4.
           ELSE liTempDialType = 1.
           FOR FIRST BDest NO-LOCK WHERE
                     BDest.Brand  = gcBrand AND
                     BDest.Bdest  = ttCall.BDest AND
                     BDest.DestType = ttCall.BType AND
                     BDest.Class  = 2 AND
                     BDest.ToDate >= ttCall.DateSt AND
                     BDest.FromDate <= ttCall.Datest,
               FIRST RateCCN NO-LOCK WHERE
                     RateCCN.BDestID  = Bdest.BDestID AND
                     RateCCN.DialType = liTempDialType /* ttCall.DialType */ :

               ASSIGN
               liCCN      = RateCCN.CCN
               liDialType = 4.
           END.
           
           IF liCCN = 0 THEN 
              liCCN = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
              
           IF liCCN = 0 THEN liCCN = ttCall.SpoCmt.   
      END.

      IF liCCN = 0 OR liCCN = 999 OR ttCall.BType = 2 THEN 
          liCCN = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).

      IF (liCCN = 69  OR liCCN = 1069) AND ttCall.BillDur > 11 THEN DO:
         IF CAN-FIND(FIRST ttDuration WHERE 
                           ttDuration.CallCase = "61" AND
                           ttDuration.BDest    = ttCall.BDest AND
                           ttDuration.FromDate <= ttCall.DateSt) THEN 
            IF liCCN EQ 69 THEN liCCN = 61.
            ELSE liCCN = 1061.
      END.

      /* YDR-1853 */
      IF ttCall.Spocmt = 63 AND ttCall.BillDur <= 20 THEN 
         ASSIGN lidialtype = 20
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
      ELSE IF ttCall.Spocmt = 63  AND ttCall.BillDur > 20 THEN 
         ASSIGN lidialtype = 21
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
      ELSE IF ttCall.Spocmt = 1063 AND ttCall.BillDur <= 20 THEN
         ASSIGN lidialtype = 23
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
      ELSE IF ttCall.Spocmt = 1063  AND ttCall.BillDur > 20 THEN
         ASSIGN lidialtype = 24
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).          

      ASSIGN
         c_time      = ttCall.timestart
         x-time      = ttCall.timestart + ttCall.billdur
         c_day       = ttCall.datest
         c_dur       = ttCall.billdur
         asub-cust   = ttCall.CustNum
         rate-cust   = MsOwner.CustNum
         rate-plcode = ""
         asubtype    = lidialtype
         libilltarget = ttCall.billtarget
         c_bppref     = "MOB"
         bsubs        = b_Dest
         ttCall.RateCCN = liCCN.
 
      /* GPRS as Data amount based (bytes) */
      IF lidialtype = 7 THEN c_dur = ttCall.DataIN + ttCall.DataOut.

      /* duration may be only partly billable */
      ELSE IF CAN-FIND(FIRST ttDuration WHERE
                        ttDuration.CallCase = STRING(ttCall.RateCCN)) THEN DO:
         c_dur = fBillableDuration(STRING(ttCall.RateCCN),
                                   ttCall.BDest,
                                   ttCall.DateSt,
                                   ttCall.BillDur).
      END.
      
      fTariff().

      IF rc = 0 THEN  DO:

         /* final check if still billable */
         ttCall.ErrorCode = fFinalBillableCheck().
         IF ttCall.ErrorCode > 0 THEN DO:
            ttCall.InvSeq = 0.
            fBCopy().
            NEXT MobCDR.
         END.

          /* data, voice and other packages */
         fPackageCalculation().
                  
         IF ttCall.ErrorCode > 0 THEN DO:
            ttCall.InvSeq = 0.
            fBCopy().
            NEXT MobCDR.
         END.
          
         /* PRERATED POSTPAID TICKETS */ 
         IF TTCall.PPFlag = 0 THEN DO:
            IF LOOKUP(STRING(ttCall.SpoCMT),"72,73,78," + 
                       STRING({&GB_CCN})) > 0 THEN 
               bPrice = ttCall.ccharge.
         END.
         
         ASSIGN               
               ttCall.GrossAmt    = bprice 
               ttCall.startCharge = base
               ttCall.refprice    = bprice
               ttCall.VatIncl     = llVatIncl
               ttCall.DiscType    = DiscType
               ttCall.Fdisc       = DiscVal
               ttCall.DiscFP      = discpr
               ttCall.CurrUnit    = llCurrUnit
               ttCall.TotDisc     = ttCall.FDisc
               ttCall.Amount      = bprice - ttCall.TotDisc
               ttCall.ErrorCode   = 0
               TotValue           = TotValue + ttCall.GrossAmt
               ttCall.TariffNum   = tttariff.TariffNum
               ttCall.ServRid     = lcServRid
               ttCall.MPMRid      = lcMPMRid.
            
      END.
      ELSE IF rc = 9 THEN DO:  /* RC NE 0 */
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_NO_BILLING_TARGET_FOUND}
            ttCall.InvSeq    = 0 .
      END.      /* RC */
      ELSE IF rc = 8 THEN DO:  /* RC NE 0 */
         ASSIGN
           ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}
           ttCall.InvSeq    = 0 .
      END.      /* RC */
      ELSE IF rc = 7 THEN DO:  /* RC NE 0 */
        ASSIGN
           ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PREF_FOUND}
           ttCall.InvSeq    = 0 .
      END.      /* RC */
      ELSE IF rc = 5 THEN DO:
        ASSIGN
           ttCall.ErrorCode = {&CDR_ERROR_NO_TARIFF_FOUND}
           ttCall.InvSeq    = 0 .
      END.

      /* Update PremiumNumber Operator information only for VOICE */
      IF ttCall.MSCID <> "CCGW" THEN DO:
         FIND FIRST BillItem WHERE
                    BillItem.Brand    = gcBrand AND
                    BillItem.BillCode = ttCall.BillCode NO-LOCK NO-ERROR.
         IF AVAILABLE BillItem AND BillItem.BIGroup = "6" THEN
            ttCall.ServiceName = fGetPremiumServiceName(ttCall.GsmBnr,
                                                        ttCall.DateSt).
      END. /* IF ttCall.MSCID <> "CCGW" THEN DO: */
      ELSE IF ttCall.spocmt EQ {&GB_CCN} THEN DO:
         ttCall.ServiceName = "Google".  /* YPR-3890 */
      END.

      IF ttCall.mscid EQ "NRTRDE" THEN ttCall.InvSeq = 0.
      ELSE IF ttCall.ErrorCode = 0 THEN 
      ASSIGN
         ttCall.invseq = fInvSeq(ttCall.AgrCust,
                                 ttCall.InvCust,
                                 ttCall.MsSeq,
                                 ttCall.DateSt,
                                 ttCall.PPFlag). 

      IF ttCall.invseq ne 0 then DO:

         fcallcount(input ttCall.MSSEQ,
                    YEAR(ttCall.Datest) * 100 + MONTH(ttCall.Datest),
                    ttCall.Amount).
               
         IF MobSubLimit  ne 0 THEN DO:
              
            fChkMobsubLimit(INPUT ttCall.InvCust,
                            INPUT ttCall.cli,
                            INPUT ttCall.MSSeq,
                            INPUT lcNotifyNumber,
                            INPUT ttCall.datest,
                            INPUT DECIMAL(mobsubLimit),
                            INPUT credittype).
         END.
      END.   
         
      IF (ttCall.Addbpref = "151" OR 
          ttCall.AddbPref = "152") AND
         ttCall.BType ne 1 THEN DO:
         ttCall.billCode  = "151".
      END.

      fbcopy().
      NEXT mobcdr.
      
   END. /* analyse call line */
   
   PAUSE 0.

 END. /* DOTRANS */
END. /* mobcdr */

HIDE FRAME CLOG NO-PAUSE.
HIDE FRAME err NO-PAUSE.

RETURN lcReturn.


