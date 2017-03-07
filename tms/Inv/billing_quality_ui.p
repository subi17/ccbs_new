/* ----------------------------------------------------------------------
  MODULE .......: billing_quality_ui.p
  TASK .........: Print a combined report of various billing statistics
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.08.09
  Version ......: yoigo
---------------------------------------------------------------------- */
&GLOBAL-DEFINE TraceLog NO

{Syst/commali.i}
{Func/cparam2.i}

&IF "{&TraceLog}" = "YES" 
&THEN
{Func/log.i}
fSetLogFileName("/tmp/billing_quality.log").
fSetLogEntryTypes("4GLTrace:4").
fClearLog().
&ENDIF

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey          AS LOG  NO-UNDO.
DEF VAR liCount        AS INT  NO-UNDO. 
DEF VAR lcFile         AS CHAR NO-UNDO.
DEF VAR liPeriod       AS INT  NO-UNDO.
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcError        AS CHAR NO-UNDO. 
DEF VAR lcTransDir     AS CHAR NO-UNDO. 
DEF VAR liExtent       AS INT  NO-UNDO.
DEF VAR llDetails      AS LOG  NO-UNDO.
DEF VAR ldaInvDate     AS DATE NO-UNDO EXTENT 2.
DEF VAR liInvType      AS INT  NO-UNDO.
DEF VAR ldaBillInvDate AS DATE NO-UNDO.
DEF VAR liBillCount    AS INT  NO-UNDO.
DEF VAR lcBillError    AS CHAR NO-UNDO.
DEF VAR llBillDetails  AS LOG  NO-UNDO.

FORM 
   SKIP(1)
   "Print a billing quality check report." AT 10 
   SKIP(1)

   "BILLING REPORT" AT 2 SKIP 
   ldaBillInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date"
      SKIP
      
   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "Invoice type"
      FORMAT ">9"
      SKIP

   llBillDetails COLON 20
      LABEL "Details"
      HELP "Print details"
      FORMAT "Yes/No"
      SKIP(1)

   "UNBILLED SUBSCRIPTIONS AND CUSTOMERS" AT 2 SKIP                   
   liPeriod COLON 20 
      LABEL "Billing Period" 
      FORMAT "999999"
      HELP "Billing period YYYYMM"
      SKIP

   ldaInvDate[1] COLON 20
      LABEL "Invoice Dates"
      FORMAT "99-99-99"
      HELP "Invoice should be found on these dates"
      "-"
   ldaInvDate[2] 
      NO-LABEL 
      FORMAT "99-99-99"
      HELP "Invoice should be found on these dates"
      VALIDATE(INPUT ldaInvDate[2] >= INPUT ldaInvDate[1],
               "Upper limit cannot be earlier than lower limit")
      SKIP
      
   llDetails COLON 20
      LABEL "Details"
      HELP "Print details"
      FORMAT "Yes/No"
      SKIP(2)

   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(55)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(55)" 
   SKIP(1)

WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + " BILLING QUALITY " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey          = FALSE
   ldaBillInvDate = TODAY
   liInvType      = 1 
   liPeriod       = IF MONTH(TODAY) = 1
                    THEN (YEAR(TODAY) - 1) * 100 + 12
                    ELSE YEAR(TODAY) * 100 + (MONTH(TODAY) - 1)
   llBillDetails  = FALSE
   llDetails      = TRUE
   ldaInvDate[1]  = DATE(MONTH(TODAY),1,YEAR(TODAY))
   ldaInvDate[2]  = IF MONTH(TODAY) = 12
                    THEN DATE(12,31,YEAR(TODAY))
                    ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1
   lcFile         = fCParamC("BillQualityFileName")
   lcTransDir     = fCParamC("BillQualityTransDir").

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY 
      ldaBillInvDate
      llBillDetails
      liInvType
      liPeriod
      ldaInvDate
      llDetails
      lcFile
      lcTransDir
   WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[3] = 1069
         ufk[4] = 1069
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   ELSE ASSIGN 
      toimi = 1
      ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE 
            ldaBillInvDate
            liInvType
            llBillDetails
            liPeriod
            ldaInvDate
            llDetails
            lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"liInvType") > 0 THEN DO:

               IF FRAME-FIELD = "liInvType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice", 
                                       "InvType",  
                                       "Report",   
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) @ liInvType.
                  END.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "ldaBillInvDate" THEN DO:
                  
                  ASSIGN FRAME fCrit ldaBillInvDate.
                  
                  IF ldaBillInvDate ENTERED THEN DO:
                     ASSIGN
                        liPeriod = YEAR(ldaBillInvDate) * 100 +
                                   MONTH(ldaBillInvDate)
                        ldaInvDate[1] = DATE(MONTH(ldaBillInvDate),1,
                                             YEAR(ldaBillInvDate))
                        ldaInvDate[2] = IF MONTH(ldaBillInvDate) = 12
                                        THEN DATE(12,31,YEAR(ldaBillInvDate))
                                        ELSE DATE(MONTH(ldaBillInvDate) + 1,1,
                                                  YEAR(ldaBillInvDate)) - 1.
                                                  
                     IF MONTH(ldaBillInvDate) > 1 THEN liPeriod = liPeriod - 1.
                     ELSE liPeriod = (YEAR(ldaBillInvDate) - 1) * 100 + 12.
                                                   
                     DISPLAY liPeriod ldaInvDate WITH FRAME fCrit.
                  END.   
               END.
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   ELSE IF toimi = 3 THEN  
      RUN Syst/report_config.p ("BillingReport").

   ELSE IF toimi = 4 THEN 
      RUN Syst/report_config.p ("UnbilledSubsQty").
 
   ELSE IF toimi = 5 THEN DO:
      
      IF lcFile = "" OR lcFile = ? THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      IF liPeriod < 200101 THEN DO:
         MESSAGE "Invalid billing period"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF ldaInvDate[1] = ? OR ldaInvDate[2] = ? THEN DO:
         MESSAGE "Invoice dates have not been defined"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
           
      RUN Inv/billing_report.p (ldaBillInvDate,
                            liInvType,
                            "no*no**" + lcFile,
                            llBillDetails,
                            0,
                            0,
                            "",
                            OUTPUT liBillCount).
      lcBillError = RETURN-VALUE.                      
 
      RUN Inv/unbilled_subsqty.p (liPeriod,
                              ldaInvDate[1],
                              ldaInvDate[2],
                              "append*trans*" + lcTransDir + "*" + lcFile,
                              llDetails,
                              0,
                              0,
                              "",
                              OUTPUT liCount).
      lcError = RETURN-VALUE.                        
                                                   
      MESSAGE 
         liBillCount "invoices were picked to the 1. part"
                    (IF lcBillError > "" 
                     THEN ". Error occurred: " + lcBillError
                     ELSE "") SKIP(1)
         liCount "subscriptions were picked to the 2. part"
                    (IF lcError > "" 
                     THEN ". Error occurred: " + lcError
                     ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

&IF "{&TraceLog}" = "YES" 
&THEN
fCloseLog().
&ENDIF

