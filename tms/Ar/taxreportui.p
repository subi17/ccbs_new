/* ----------------------------------------------------------------------
  MODULE .......: taxreportui.p
  TASK .........: Print a tax report from invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.03.07
  CHANGED ......: 25.04.07/aam also upper limit for InvType
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcTaxZone     AS CHAR NO-UNDO.
DEF VAR lcTZName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO EXTENT 2.
DEF VAR lcInvType     AS CHAR NO-UNDO. 
DEF VAR lcCustID      AS CHAR NO-UNDO.
DEF VAR ldtInvDate    AS DATE NO-UNDO EXTENT 2.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liFiles       AS INT  NO-UNDO.
DEF VAR ldtDate       AS DATE NO-UNDO EXTENT 2. 
DEF VAR liExtent      AS INT  NO-UNDO.

FORM 
   SKIP(2)
   "Print a tax report from invoices." AT 10 
   SKIP(2)
                   
   lcTaxZone COLON 20
      LABEL "Tax Zone"
      HELP  "Tax zone, EMPTY = all"
   lcTZName
      NO-LABEL
      FORMAT "X(30)" 
   SKIP

   lcCustID COLON 20
      LABEL "Customer ID"
      HELP  "Customer ID"
      FORMAT "X(12)"

   liInvType[1] COLON 20
      LABEL "Invoice Type"
      HELP "Invoice types"
      FORMAT ">9"
   "-" 
   liInvType[2] 
      NO-LABEL 
      HELP "Invoice types"
      FORMAT ">9"
      VALIDATE(INPUT liInvType[2] >= INPUT liInvType[1],
               "Upper limit cannot be less than lower limit")
      SKIP

   ldtInvDate[1] COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice dates"
   "-"
   ldtInvDate[2] 
      NO-LABEL
      FORMAT "99-99-9999"
      HELP "Invoice dates"
      VALIDATE(INPUT ldtInvDate[2] >= INPUT ldtInvDate[1],
               "End date cannot be earlier than begin date")
      SKIP(1)                 

   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(55)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(55)" 
   SKIP(5)

WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + " INVOICE TAX REPORT " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


FUNCTION fTaxName RETURNS LOGIC
   (INPUT icTaxZone AS CHAR):
   
   lcTZName = "".
   
   IF icTaxZone = "" THEN lcTZName = "ALL".
               
   ELSE DO:
      FIND TaxZone WHERE 
           TaxZone.TaxZone = icTaxZone
      NO-LOCK NO-ERROR.
      IF AVAILABLE TaxZone THEN lcTZName = TaxZone.TZName.
   END.
   
   DISPLAY lcTZName WITH FRAME fCrit.
   
END FUNCTION.

ASSIGN ufkey           = FALSE
       ldtInvDate[2]   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtInvDate[1]   = DATE(MONTH(ldtInvDate[2]),1,YEAR(ldtInvDate[2]))
       liInvType[1]    = 0 
       liInvType[2]    = 98
       lcFile          = fCParamC("TaxRepFileName")
       lcTransDir      = fCParamC("TaxRepTransDir").
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcTaxZone 
           lcCustID
           liInvType
           ldtInvDate
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fTaxName(lcTaxZone).

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.
   END.
   ELSE ASSIGN toimi = 1
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcTaxZone
                lcCustID
                liInvType
                ldtInvDate
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"liInvType") > 0 THEN DO:

               IF FRAME-FIELD = "liInvType" THEN DO:

                  liExtent = FRAME-INDEX.
                  
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "InvType",  /* FieldName */
                                       "Report",   /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) @ liInvType[liExtent].
                  END.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcTaxZone" THEN DO:

                  IF INPUT lcTaxZone > "" AND
                     NOT CAN-FIND(TaxZone WHERE 
                                  TaxZone.TaxZone = INPUT lcTaxZone)
                  THEN DO:
                     MESSAGE "Unknown tax zone."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  fTaxName(INPUT INPUT lcTaxZone).
               END.

            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   ELSE IF toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      /* must be whole month */
      ASSIGN ldtDate[1] = ldtInvDate[1] - 1
             ldtDate[2] = ldtInvDate[2] + 1.
      IF MONTH(ldtDate[1]) = MONTH(ldtInvDate[1]) OR
         MONTH(ldtDate[2]) = MONTH(ldtInvDate[2]) THEN DO:
         MESSAGE "Period must be a whole month"
         VIEW-AS ALERT-BOX ERROR. 
         NEXT. 
      END.
      
      RUN Ar/taxreport (lcTaxZone,
                     lcCustID,
                     liInvType[1],
                     liInvType[2],
                     ldtInvDate[1],
                     ldtInvDate[2],
                     lcFile,
                     OUTPUT liCount,
                     OUTPUT liFiles,
                     OUTPUT lcError).

           
      MESSAGE liCount "invoices were written to"
              liFiles "tax file" + (IF liFiles > 1 THEN "s" ELSE "") SKIP ""
              (IF lcError > "" 
               THEN "Error occurred: " + lcError
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

