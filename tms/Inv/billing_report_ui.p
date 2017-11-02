/* ----------------------------------------------------------------------
  MODULE .......: billing_report_ui.p
  TASK .........: Print a report from billing run
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 11.03.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liExtent      AS INT  NO-UNDO.
DEF VAR llDetails     AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Print a billing report from invoices." AT 10 
   SKIP(2)
                   
   ldaInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date"
      SKIP
      
   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "Invoice type"
      FORMAT ">9"
      SKIP

   llDetails COLON 20
      LABEL "Invoice Details"
      HELP "Print invoice details"
      FORMAT "Yes/No"
      SKIP(1)

   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(55)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(55)" 
   SKIP(6)

WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + " BILLING REPORT " + 
           STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey      = FALSE
   ldaInvDate = TODAY
   liInvType  = 1 
   llDetails  = FALSE
   lcFile     = fCParamC("BillRepFileName")
   lcTransDir = fCParamC("BillRepTransDir").
       
CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY liInvType
           ldaInvDate
           llDetails
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         Syst.CUICommon:ufk    = 0
         Syst.CUICommon:ufk[1] = 132 
         Syst.CUICommon:ufk[3] = 1069
         Syst.CUICommon:ufk[5] = 795
         Syst.CUICommon:ufk[8] = 8 
         Syst.CUICommon:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   ELSE ASSIGN 
      Syst.CUICommon:toimi = 1
      ufkey = TRUE.

   IF Syst.CUICommon:toimi = 1 THEN DO:

      Syst.CUICommon:ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE ldaInvDate
                liInvType
                llDetails
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.CUICommon:nap = KEYLABEL(LASTKEY).

            IF Syst.CUICommon:nap = "F9" AND 
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
               
               Syst.CUICommon:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(Syst.CUICommon:nap,Syst.CUICommon:poisnap) > 0 THEN DO:

            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   ELSE IF Syst.CUICommon:toimi = 3 THEN 
      RUN Syst/report_config.p ("BillingReport").
   
   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF lcFile = "" OR lcFile = ? THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
           
      RUN Inv/billing_report.p (ldaInvDate,
                            liInvType,
                            lcFile,
                            llDetails,
                            0,
                            0,
                            "",
                            OUTPUT liCount).
                                                   
      MESSAGE liCount "invoices were picked to the report" SKIP
              RETURN-VALUE
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

