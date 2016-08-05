/* ----------------------------------------------------------------------
  MODULE .......: publish_invoice.p
  TASK .........: Publish invoices to Newton AND HPD
  APPLICATION ..: tms
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 06.08.2015
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/email.i}
{Syst/host.i}

DEFINE INPUT PARAMETER iiMsRequest AS INT NO-UNDO.

DEF VAR ldaDateFrom    AS DATE NO-UNDO. 
DEF VAR liCount        AS INT  NO-UNDO. 
DEF VAR liDumped       AS INT  NO-UNDO. 
DEF VAR lcToday        AS CHAR NO-UNDO.
DEF VAR lcAddrConfDir  AS CHAR NO-UNDO. 
DEF VAR lcContent      AS CHAR NO-UNDO. 
DEF VAR llgError       AS LOG  NO-UNDO.
DEF VAR lcLogFile      AS CHAR NO-UNDO. 
DEF VAR lcContLogDir   AS CHAR NO-UNDO. 

DEFINE STREAM strout.

   FIND MsRequest WHERE
        MsRequest.Brand     = gcBrand     AND
        MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.   
  
   IF NOT AVAIL MsRequest OR
                MsRequest.ReqType NE ({&REQTYPE_PUBLISH_INVOICE}) THEN
      RETURN "ERROR".

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
 
   ASSIGN liCount        = 0
          lcContent      = ""
          llgError       = NO
          liDumped       = 0
          ldaDateFrom    = ?
          lcLogFile      = ""
          lcAddrConfDir  = ""
          lcContLogDir   = ""
          lcToday        = ""
          ldaDateFrom    = DATE(MONTH(TODAY),1,YEAR(TODAY))
          lcAddrConfDir  = fCParamC("RepConfDir")
          lcContLogDir   = fCParam("PublishInvoice","ContentLogDir")
          lcToday        = STRING(YEAR(TODAY),"9999") + 
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99") 
          lcLogFile      = lcContLogDir + "PIWebDisplay_" + lcToday + STRING(TIME) + ".log".

   OUTPUT STREAM strout TO VALUE(lcLogFile) APPEND.

   IF lcAddrConfDir > "" THEN 
      lcAddrConfDir = lcAddrConfDir + "publishinvoice.email".

   IF ldaDateFrom EQ ? THEN DO:
      fReqStatus(3,"Invalid Invoice Date").   
      LEAVE.   
   END.

   /* Publish invoices to Newton */
   RUN Inv/invoice_webdisp(ldaDateFrom,
                       1,     /* inv.type */
                       "",
                       0,
                       99999999,
                       "",
                       "ZZZZZZZZZZZZ",
                       YES,
                       OUTPUT liCount).
   
   PUT STREAM strout UNFORMATTED 
      "Display permit was set to " + STRING(liCount) + " Invoices." SKIP.

   OUTPUT STREAM strout CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcLogFile,"").

   IF llgError THEN LEAVE.

   lcLogFile = lcContLogDir + "PIHPDdump_" + lcToday + STRING(TIME) + ".log".

   OUTPUT STREAM strout TO VALUE(lcLogFile) APPEND.

   FIND FIRST DumpFile NO-LOCK WHERE 
              DumpFile.Brand    EQ gcBrand AND
              DumpFile.DumpName EQ {&DUMP_INVOICE_PUPU} NO-ERROR.
 
   IF AVAIL DumpFile THEN DO:
      RUN Syst/dumpfile_run.p(DumpFile.DumpID,  /* Dump ID */
                       "Full",
                       "",
                       fIsThisReplica(),
                       OUTPUT liDumped). 
   
      IF liDumped >= 0 THEN
         PUT STREAM strout UNFORMATTED
            "Published total Invoice full dump event count : " + STRING(liDumped) SKIP.
      ELSE
         PUT STREAM strout UNFORMATTED
            "Error in creating Invoice full dump event !" SKIP.
   END.

   OUTPUT STREAM strout CLOSE.
   
   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcLogFile,"").

   IF llgError THEN LEAVE.
  
   lcContent = "Display permit was set to " + STRING(liCount) + " Invoices." + "  " + 
               "Published total Invoice full dump event count : " + STRING(liDumped).

   fReqStatus(2,lcContent). /* request handled succesfully */

