/**
 * Get CDR details for a subscription between given dates.
 *
 * @input       msseq;int;mandatory;mobsub sequence number
                startdate;date;mandatory;all cdrs from this date
                enddate;date;mandatory;all cdrs up to this date
                search_mode;string;mandatory;NORMAL or DETAILED
                username;string;optional;username of caller
                reasoncode;int;optional;reason why query is done
                reason;string;optional;reason why query is done
 * @output      mobile;struct;Mobile line CDRs
                fixed_line;struct;Fixed line CDRs
 * @mobile      cdr_datastruct;struct;contains needed CDR information as specified above ruby stub implementation
 * @fixed_line  cdr_datastruct;struct;contains needed CDR information as specified above ruby stub implementation
 */
{Syst/commpaa.i}
Syst.Var:katun = "Newton RPC".
Syst.Var:gcBrand = "1".

{Func/cparam2.i}
{Func/fdestcountry.i}
{Func/callquery.i}
{Func/istc.i}
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/transname.i}
{Rate/rate_roamzone.i}
{Syst/tmsconst.i}

&SCOPED-DEFINE NORMAL "NORMAL" 
&SCOPED-DEFINE DETAILED "DETAILED" 

/* Input parameters */
DEFINE VARIABLE piMsSeq       AS INTEGER   NO-UNDO.
DEFINE VARIABLE pdStartDate   AS DATE      NO-UNDO.
DEFINE VARIABLE pdEndDate     AS DATE      NO-UNDO.
DEFINE VARIABLE pcSearchMode  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE piReasonCode  AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcReason      AS CHARACTER NO-UNDO.
DEFINE VARIABLE top_array     AS CHARACTER NO-UNDO. 

/* Output parameters */
DEFINE VARIABLE resp_struct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE resp_rows     AS CHARACTER NO-UNDO.
DEFINE VARIABLE resp_row      AS CHARACTER NO-UNDO.

/* Local variables */
DEFINE VARIABLE lcBrand     AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE liAllow     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNonCombinedData AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcCLIType     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcCLITypeName AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaFirstDay   AS DATE      NO-UNDO.
DEFINE VARIABLE ldaLastDay    AS DATE      NO-UNDO.

DEFINE VARIABLE tthCDR         AS HANDLE  NO-UNDO.
DEFINE VARIABLE liErrorCodeOut AS INTEGER NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR.

DEF TEMP-TABLE ttData NO-UNDO
   FIELD BIName AS CHAR
   FIELD DataAmt  AS DEC
   FIELD DataSum  AS DEC
   INDEX BIName BIName.

/* Check Input parameters */
top_array = validate_request(param_toplevel_id, "int,dateTime,dateTime,string,string,[int],[string]").
IF top_array EQ ? THEN RETURN.

ASSIGN
   piMsSeq        = get_pos_int(param_toplevel_id, "0")
   pdStartDate    = get_date(param_toplevel_id, "1")
   pdEndDate      = get_date(param_toplevel_id, "2")
   pcSearchMode   = get_string(param_toplevel_id, "3")
   pcUserName     = get_string(param_toplevel_id, "4").

tthCDR = TEMP-TABLE ttCall:HANDLE.

/* limit the date YOT-247 */
DEFINE VARIABLE ldLimitDate AS DATE NO-UNDO. 
IF (day(today) = 29 AND month(today) = 2 ) THEN
  ldLimitDate = date(month(today),day(today) - 1,year(today) - 1).
ELSE
  ldLimitDate = date(month(today),day(today),year(today) - 1).
/* -------------------- */

IF NUM-ENTRIES(top_array) >= 6 THEN
   piReasonCode   = get_pos_int(param_toplevel_id, "5").
IF NUM-ENTRIES(top_array) >= 7 THEN
   pcReason       = get_string(param_toplevel_id, "6").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

IF LOOKUP(pcSearchMode,({&NORMAL} + "," + {&DETAILED})) = 0 
   THEN pcSearchMode = {&NORMAL}.

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

FIND FIRST CliType WHERE
           CliType.CliType = MobSub.CliType
NO-LOCK NO-ERROR.

IF NOT AVAILABLE CliType THEN
   RETURN appl_err(SUBST("CliType &1 not found", MobSub.CliType)).
   
FIND FIRST RatePlan WHERE
   RatePlan.Brand    = "1" AND
   RatePlan.RatePlan = CliType.PricePlan NO-LOCK NO-ERROR.

IF NOT AVAILABLE RatePlan THEN
   RETURN appl_err(SUBST("RatePlan &1 not found", CliType.PricePlan)).


IF pcUserName EQ "miyoigo" THEN DO:
   
   liAllow = fCParamI("CDR_Details").
   IF liAllow = 0 THEN RETURN.
                    
END. /* IF pcUserName EQ "miyoigo" THEN DO: */

lcNonCombinedData = fCParamC("NON_COMBINED_DATA_ROWS").

ASSIGN ldaFirstDay = DATE(MONTH(pdStartDate),1,YEAR(pdStartDate))
       ldaLastDay  = Func.Common:mLastDayOfMonth(pdEndDate).

fGetMsOwnerTempTable(MobSub.Custnum,ldaFirstDay,ldaLastDay,
                     TRUE,MobSub.PayType).


FUNCTION fRepText RETURNS CHARACTER
  (INPUT piType AS INTEGER,
   INPUT piLang AS INTEGER,
   INPUT pcKey  AS CHARACTER,
   INPUT pdtDate AS DATE):

   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.
   
   lcReturn = fTranslationName(lcBrand,piType,pcKey,piLang,pdtDate).
   IF lcReturn = ? OR lcReturn = "" THEN lcReturn = "-".
   
   RETURN lcReturn.

END FUNCTION.

/* This function makes header lines for mobile and fixed numbers */
FUNCTION fMakeHeader RETURNS LOGICAL
  (INPUT pcStructId AS CHARACTER):

/* HEADERS FIRST ROW */
/* "headers" => 
  #              Number A      Number B    Date         Time     Type    Destination  Tariff      Minutes/KB/Pcs   Price
  "headers" => ["Número A",   "Número B", "Fecha",     "Hora",  "Tipo",  "Destino",   "Tarifa",   "Mins/KB/Cant.", "Importe"],
  "rows" => [   ["040654321", "040123456", "1.1.2001", "12:21", "Voice", "Operator2", "Contrato", "1:20",          "1,10"],
                ["040654321", "040123456", "2.1.2001", "15:22", "Voice", "Operator2", "Contrato", "3:21",          "2,20"],
                ["040654321", "040123456", "3.1.2001", "22:51", "Voice", "Operator2", "Contrato", "5:13",          "1,40"],
                ["040654321", "040123456", "4.1.2001", "12:54", "Voice", "Operator1", "Contrato", "7:12",          "4,20"],
                ["040654321", "040123456", "5.1.2001", "17:21", "Voice", "Operator1", "Contrato", "0:32",          "0,80"] ]
*/

/* PostPaid headers */
IF pcSearchMode = {&NORMAL} THEN DO:
   
   add_string(pcStructId, "", from_utf8("Número A")).          /* A number */
   add_string(pcStructId, "", from_utf8("Número B")).          /* B number */
   add_string(pcStructId, "", "Fecha").
   add_string(pcStructId, "", "Hora").
   add_string(pcStructId, "", "Tipo").
   add_string(pcStructId, "", "Destino").
   add_string(pcStructId, "", "Tarifa").
   add_string(pcStructId, "", "Mins/MB/Cant.").
   add_string(pcStructId, "", "Importe").

   /* PrePaid headers */
   IF MobSub.PayType EQ TRUE AND pcUserName NE "miyoigo" 
      THEN add_string(pcStructId, "", "Saldo").

END.

ELSE IF pcSearchMode = {&DETAILED} THEN DO:
   
   add_string(pcStructId, "", from_utf8("Número A")).          /* A number */
   add_string(pcStructId, "", from_utf8("Número B")).          /* B number */
   add_string(pcStructId, "", "Informe CCN").        /* Report CCN */
   add_string(pcStructId, "", from_utf8("Tarificación CCN")).  /* Rate CCN */
   add_string(pcStructId, "", "Plan de tarifas").    /* Rate Plan */
   add_string(pcStructId, "", "Tipo de CDR").        /* CDR Type */
   add_string(pcStructId, "", "Fecha").              /* Date */
   add_string(pcStructId, "", "Inicio").             /* Started */
   add_string(pcStructId, "", "Fin").                /* Ended */
   add_string(pcStructId, "", from_utf8("Duración")).          /* Duration */
   add_string(pcStructId, "", "Datos (MB)").         /* Data (Mb) */ 
   add_string(pcStructId, "", from_utf8("Facturar ítem")).     /* Bill Item */
   add_string(pcStructId, "", "Tarifa inicial").     /* Starting fee */
   add_string(pcStructId, "", "Cobro por unidad").   /* Unit charge */
   add_string(pcStructId, "", "Cobro total").        /* Total charge */
   add_string(pcStructId, "", from_utf8("Nº Factura")).        /* Invoice number */
   
END.
RETURN TRUE.

END FUNCTION.

FUNCTION fResponseRow RETURNS LOGICAL
   (ihCDR AS HANDLE,
    ilLastCDROfDay AS LOG):
   
   DEFINE VARIABLE ldaDateSt AS DATE NO-UNDO. 
   DEFINE VARIABLE lcBillCode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcBIName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCCNName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDetails AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ppCharge AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcRateCCNName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEnded AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCDRType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE llDataRowCDR AS LOGICAL NO-UNDO. 

   /* Variables for DETAILED SEARCH */
   DEFINE VARIABLE ldeStartCharge AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldeAmount AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE lcInvNum AS CHARACTER NO-UNDO. 

   ASSIGN
      ldaDateSt = ihCDR::DateSt
      lcBillCode = ihCDR::BillCode
      lcBIName  = fRepText(1,1,lcBillCode,ldaDateSt)
      lcRateCCNName = fRepText(3,1,ihCDR::RateCCN,ldaDateSt)
      lcDetails = ""
      ldeAmount = (IF MobSub.Paytype = FALSE 
                   THEN ihCDR::Amount ELSE ihCDR::Charge).

   /* country name for roaming */
   IF LOOKUP(STRING(ihCDR::SpoCMT),{&ROAMING_CALLCASE}) > 0 THEN
      lcCCNName = fDestCountryName(Syst.Var:gcBrand,
                                  1,
                                  ihCDR::SpoCMT,
                                  ihCDR::DateSt,
                                  ihCDR::DtlSeq,
                                  ihCDR::GsmBnr,
                                  ihCDR::BType,
                                  ihCDR::MSCID,
                                  ihCDR::ServiceName).
   /* ServiceName for Premium Number */
   ELSE DO:
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = Syst.Var:gcBrand AND
                 BillItem.BillCode = ttCall.BillCode NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem AND BillItem.BIGroup = "6" THEN
         lcCCNName = ttCall.ServiceName.
   END. /* ELSE DO: */

   IF lcCCNName = "" THEN lcCCNName = fRepText(3,1,ihCDR::CCN,ldaDateSt).
  
   /* DATA VALUES */
   IF pcSearchMode = {&NORMAL} THEN DO:
   
      CASE STRING(ihCDR::EventType):
         
         WHEN "CALL" THEN lcDetails = STRING(ihCDR::BillDur,"HH:MM:SS").
         
         WHEN "GPRS" THEN DO:
            
            /* assumed that all GPRS events belong to group 3 (internet) */
            IF LOOKUP(lcBillCode,lcNonCombinedData) = 0 THEN DO:
               
               FIND FIRST ttData WHERE ttData.BIName = lcBiName NO-ERROR.
               IF NOT AVAILABLE ttData THEN DO:
                  CREATE ttData.
                  ttData.BIName = lcBIName.
               END.
               ASSIGN
                  ttData.DataAmt = ttData.DataAmt + 
                                   ihCDR::DataIn + ihCDR::DataOut
                  ttData.DataSum = ttData.DataSum + ldeAmount.
               
               IF ilLastCDROfDay = FALSE THEN RETURN TRUE.
               llDataRowCDR = TRUE.
            END. 
            ELSE DO:
               /* Prepaid cdrs have value as kBytes */
               IF MobSub.PayType = TRUE THEN
               lcDetails = STRING(INT(
                         (ihCDR::DataIn + ihCDR::DataOut) / 1024 + 0.49999)).
               ELSE
               lcDetails = STRING(INT(
                         (ihCDR::DataIn + ihCDR::DataOut) / 1024 / 1024 + 0.49999)).
            END.
         
         END. /* WHEN "GPRS" THEN DO: */

         WHEN "SMS" THEN lcDetails = "1".
      END.
      
      IF ilLastCDROfDay THEN DO:
         /* Saldo Amount for prepaid subscription */
         IF Mobsub.PayType = TRUE AND pcUserName NE "miyoigo" THEN
            RUN Mm/cdr_detail_value.p("PrepCDR",
                                   ldaDatest,
                                   ihCDR::DtlSeq,
                                   "Balance after",
                                   OUTPUT ppCharge).

         FOR EACH ttData NO-LOCK:
            resp_row = add_array(resp_rows, "").
            add_string(resp_row, "","").
            add_string(resp_row, "","").
            add_string(resp_row, "", STRING(ldaDateSt, "99-99-99")).
            add_string(resp_row, "","").
            add_string(resp_row, "", ttData.BIName).
            add_string(resp_row, "", "").
            add_string(resp_row, "", lcCLITypeName).

            /* Prepaid cdrs have value as kBytes */
            IF MobSub.PayType = TRUE THEN
            add_string(resp_row, "", STRING(INT((ttData.DataAmt / 1024) + 
                                     0.49999))).
            ELSE
            add_string(resp_row, "", STRING(INT((ttData.DataAmt / 1024 / 1024) + 
                                     0.49999))).
            add_string(resp_row, "", TRIM(STRING(ttData.DataSum,"zzz9.999"))).

            /* decimal string must have only 2 decimals */
            IF ppCharge > "" THEN
            add_string(resp_row, "", (IF index(ppCharge,",") > 0 then
                       substring(ppCharge,1,index(ppCharge,",") + 2)
                       else ppCharge)).
         END.

         EMPTY TEMP-TABLE ttData.
         
      END.
      
      IF llDataRowCDR THEN RETURN TRUE. 
      
      resp_row = add_array(resp_rows, "").
      add_string(resp_row, "", ihCDR::CLI).     /* A number */
      add_string(resp_row, "", ihCDR::GsmBnr).  /* B number */
      add_string(resp_row, "", STRING(ldaDateSt, "99-99-99")).
      add_string(resp_row, "", STRING(ihCDR::TimeStart, "HH:MM:SS")).
      add_string(resp_row, "", lcBIName).
      add_string(resp_row, "", lcCCNName).
      add_string(resp_row, "", lcCLITypeName).
      add_string(resp_row, "", lcDetails).
      
      add_string(resp_row, "", TRIM(STRING(ldeAmount,"zzz9.999"))).
      
      IF Mobsub.PayType = TRUE AND pcUserName NE "miyoigo" THEN DO:
         
         RUN Mm/cdr_detail_value.p("PrepCDR",
                                ldaDatest,
                                ihCDR::DtlSeq,
                                "Balance after",
                                OUTPUT ppCharge).

         /* decimal string must have only 2 decimals */
         add_string(resp_row, "", 
            IF index(ppCharge,",") > 0 then
            substring(ppCharge,1,index(ppCharge,",") + 2) else ppCharge ).
      END.
   
   END.
      
   ELSE IF pcSearchMode = {&DETAILED} THEN DO:
   
      resp_row = add_array(resp_rows, "").
      
      lcEnded = STRING(ihCDR::Timestart + ihCDR::BillDur, "hh:mm:ss").

      IF ihCDR:BUFFER-FIELD("ppFlag"):BUFFER-VALUE = 1 THEN DO:
         lcCDRType = "Prepaid".
         FIND ppInvSeq where ppInvSeq.InvSeq = ihCDR::InvSeq no-lock no-error.
         IF AVAIL ppInvSeq AND ppInvSeq.Invnum > 0 
         THEN lcInvnum = "" + STRING(ppinvseq.invnum).
      END.
      ELSE DO:
         lcCDRType = "Postpaid".
         FIND InvSeq WHERE InvSeq.InvSeq = ihCDR::InvSeq NO-LOCK NO-ERROR.
         IF AVAIL InvSeq AND InvSeq.Invnum > 0 
         THEN lcInvnum =  "" + STRING(invseq.invnum).
      END.     
 
      add_string(resp_row, "", ihCDR::CLI).     /* A number */
      add_string(resp_row, "", ihCDR::GsmBnr).  /* B number */
      add_string(resp_row, "", lcCCNName).      /* Report CCN */
      add_string(resp_row, "", lcRateCCNName).  /* Rate CCN */ 
      add_string(resp_row, "", lcCLITypeName).  /* CLI Type Name */
      add_string(resp_row, "", lcCDRType).      /* CDR Type */
      add_string(resp_row, "", STRING(ldaDateSt, "99-99-99")). /* Date */
      
      /* Started */
      add_string(resp_row, "", STRING(ihCDR::TimeStart, "HH:MM:SS")).
      add_string(resp_row, "", lcEnded). /* Ended */
      
      /*Duration*/
      add_string(resp_row, "", STRING(ihCDR::BillDur,"HH:MM:SS")).

      /* Data (Mb) */
      /* Prepaid cdrs have value as kBytes */
      IF MobSub.PayType = TRUE THEN
      add_string(resp_row, "", 
            STRING((ihCDR::DataIn + ihCDR::DataOut) / 1024)).
      ELSE
      add_string(resp_row, "", 
            STRING((ihCDR::DataIn + ihCDR::DataOut) / 1024 / 1024)).
      
      add_string(resp_row, "", lcBIName).       /* Bill Item */
      
      IF MobSub.PayType = FALSE THEN DO:     
         
         ldeStartCharge = ihCDR::StartCharge.
         /* Starting fee */
         add_string(resp_row, "", TRIM(STRING(ldeStartCharge,"zzz9.999"))). 
         /* Unit Charge */
         add_string(resp_row,"", TRIM(STRING(ldeAmount - ldeStartCharge, "zzz9.999"))).
      END.
      ELSE DO:
         add_string(resp_row, "", "").
         add_string(resp_row, "", "").
      END.
      
      add_string(resp_row, "", TRIM(STRING(ldeAmount,"zzz9.999"))).
      
      /* Invoice number */ 
      add_string(resp_row, "", lcInvnum).
   END.

END FUNCTION. 

/* Query TMS for CDRs */

/* Collect CDRs from given number and make rows */
FUNCTION fCollectCdrs RETURNS LOGICAL
   (INPUT pcCLI    AS CHARACTER):

EMPTY TEMP-TABLE ttCall.
   
fMobCDRCollect(INPUT TRIM(STRING(MobSub.PayType,"pre/post")),
               INPUT Syst.Var:gcBrand,
               INPUT Syst.Var:katun,
               INPUT pdStartDate,
               INPUT pdEndDate,
               INPUT 0,
               INPUT "",
               INPUT pcCLI,
               INPUT 0,
               INPUT 0,
               INPUT "",
               INPUT "",
               INPUT "",
               INPUT 0,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR).

FOR EACH ttCall WHERE 
         ttCall.ErrorCode = 0,
   FIRST ttMsOwner WHERE
         ttMsOwner.CustNum   = MobSub.CustNum AND
         ttMsOwner.MsSeq     = MobSub.MsSeq   AND
         ttMsOwner.FromDate <= ttCall.DateSt  AND
         ttMsOwner.ToDate   >= ttCall.DateSt NO-LOCK
   BREAK BY ttCall.DateSt
         BY ttCall.TimeSt:

   IF FIRST-OF(ttCall.DateST) AND
      lcCLIType <> (ttMsOwner.CLIType + ttMsOwner.TariffBundle) THEN DO:
      IF ttMsOwner.TariffBundle > "" THEN
         lcCLITypeName = fRepText(9,1,ttMsOwner.TariffBundle,TODAY).
      ELSE
         lcCLITypeName = fRepText(9,1,ttMsOwner.CLIType,TODAY).
   END.

   IF ttCall.DateST < ldLimitDate THEN NEXT. /* YOT-247 */

   /* exclude billed calls for MiYoigo */
   IF pcUserName = "miyoigo" AND MobSub.PayType = FALSE THEN DO:
      FIND FIRST InvSeq NO-LOCK WHERE
          InvSeq.InvSeq = ttCall.invSeq NO-ERROR.
      IF AVAIL InvSeq AND InvSeq.Billed = TRUE THEN NEXT.
   END.  

   IF LAST-OF(ttCall.DateST) THEN DO:
      fResponseRow(BUFFER ttCall:HANDLE,TRUE).
      lcCLIType = ttMsOwner.CLIType + ttMsOwner.TariffBundle.
   END.
   ELSE fResponseRow(BUFFER ttCall:HANDLE,FALSE).   
END.
   RETURN TRUE.
END FUNCTION. 

resp_struct = add_struct(response_toplevel_id, "").

/* Collect CDR data and put them into responce rows */
resp_row = add_array(resp_struct, "headers").
fMakeHeader(resp_row).
resp_rows = add_array(resp_struct, "rows").
fCollectCdrs(MobSub.Cli).

/* If there is fixed line number also then put that data into fixed_line struct 
   when convergent is not partial terminated. */
IF MobSub.FixedNumber <> ? AND
   MobSub.FixedNumber <> MobSub.Cli THEN DO:
   fCollectCdrs(MobSub.FixedNumber).
END.

/* Create CallScanner record from XML-RPC query */
DO TRANS:
   CREATE CallScanner.
   ASSIGN
      CallScanner.TMSTime     = Func.Common:mMakeTS()
      CallScanner.UserCode    = pcUserName 
      CallScanner.SystemID    = "XFERA_WEB" 
      CallScanner.EventType   = "CLI"
      CallScanner.ReasonCode  = STRING(piReasonCode) + " " + pcReason
      CallScanner.Level       = ""
      CallScanner.Target      = MobSub.Cli
      CallScanner.StartTime   = STRING(pdStartDate,"99/99/99") + " " +
                                STRING("00:00:00")
                                            
      CallScanner.AccessType  = "r"
      CallScanner.EndTime     = STRING(pdEndDate,"99/99/99") + " " +
                                STRING("23:59:59")
                                            
      CallScanner.SearchRule  = "cli = " + STRING(MobSub.CLI)   +    " AND " +
                                "DateST >= " + STRING(pdStartDate) + " AND " +
                                "DateST <= " + STRING(pdEndDate) + ";" 
                                NO-ERROR.
END.

FINALLY:
   EMPTY TEMP-TABLE ttCall.
   EMPTY TEMP-TABLE ttData.
   EMPTY TEMP-TABLE ttMsOwner.
      IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
END.
/* EOF newton_get_cdr_details.p */
