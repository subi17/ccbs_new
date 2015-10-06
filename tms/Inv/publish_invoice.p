/* ----------------------------------------------------------------------
  MODULE .......: publish_invoice.p
  TASK .........: Publish invoices to Newton AND HPD
  APPLICATION ..: tms
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 06.08.2015
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{fmakemsreq.i}
{email.i}

DEFINE INPUT PARAMETER iiMsRequest AS INT NO-UNDO. 

DEF VAR ldaDateFrom    AS DATE NO-UNDO. 
DEF VAR liCount        AS INT  NO-UNDO. 
DEF VAR lcDumpFileName AS CHAR NO-UNDO. 
DEF VAR oiEvents       AS INT  NO-UNDO. 
DEF VAR olInterrupted  AS LOG  NO-UNDO. 
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
          oiEvents       = 0
          lcContent      = ""
          llgError       = NO
          olInterrupted  = NO
          ldaDateFrom    = DATE(MONTH(TODAY),1,YEAR(TODAY))
          lcDumpFileName = fCParam("HPD","DumpOutDir")
          lcAddrConfDir  = fCParamC("RepConfDir")
          lcContLogDir   = fCParam("PublishInvoice","ContentLogDir")
          lcToday        = STRING(YEAR(TODAY),"9999") + 
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99") 
          lcDumpFileName = lcDumpFileName + "invoice_Full_" + lcToday + STRING(TIME) + ".txt.gz"
          lcLogFile      = lcContLogDir + "publishinvoice_" + lcToday + STRING(TIME) + ".log".

   OUTPUT STREAM strout TO VALUE(lcLogFile) APPEND.

   IF lcAddrConfDir > "" THEN 
      lcAddrConfDir = lcAddrConfDir + "publishinvoice.email".

   IF ldaDateFrom EQ ? THEN DO:
      fReqStatus(3,"Invalid Invoice Date").   
      LEAVE.   
   END.

   /* Publish invoices to Newton */
   RUN invoice_webdisp(ldaDateFrom,
                       1,     /* inv.type */
                       "",
                       0,
                       99999999,
                       "",
                       "ZZZZZZZZZZZZ",
                       YES,
                       OUTPUT liCount) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      fReqStatus(3,"ERROR: Display permit has not been set.").      
      
      PUT STREAM strout UNFORMATTED 
         "ERROR: Display permit has not been set." SKIP.

      llgError   = YES.
   END.
   ELSE  
      PUT STREAM strout UNFORMATTED 
         "Display permit was set to " + STRING(liCount) + " Invoices." SKIP.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcLogFile,"").

   IF llgError THEN LEAVE.

   /* Publish invoices to HPD */
   RUN invoicedump_pupu(87,              /* Dump ID */
                        lcDumpFileName,  
                        "Full",
                        0,
                        "",
                        "",
                        OUTPUT oiEvents,
                        OUTPUT olInterrupted). 

   IF olInterrupted THEN DO: 
      fReqStatus(3,"Publish Invoice Full dump to HPD Interrupted!.").
      
      PUT STREAM strout UNFORMATTED 
         "Publish Invoice Full dump to HPD Interrupted!." SKIP.
      
      llgError   = YES.
   END.      
   ELSE 
      PUT STREAM strout UNFORMATTED
         "Published total Invoice full dump event count : " + STRING(oiEvents) SKIP.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcLogFile,"").

   IF llgError THEN LEAVE.
  
   lcContent = "Display permit was set to " + STRING(liCount) + " Invoices." + "  " + 
               "Published total Invoice full dump event count : " + STRING(oiEvents).

   fReqStatus(2,lcContent). /* request handled succesfully */


