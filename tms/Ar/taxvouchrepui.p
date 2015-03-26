/* ----------------------------------------------------------------------
  MODULE .......: taxvouchrepui.p
  TASK .........: Print a tax report from payments
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.03.07
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}

{lib/tokenlib.i}
{lib/tokenchk.i 'Payment'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcTaxZone     AS CHAR NO-UNDO.
DEF VAR lcTZName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liPaymType    AS INT  NO-UNDO.
DEF VAR lcPaymType     AS CHAR NO-UNDO. 
DEF VAR lcCustID      AS CHAR NO-UNDO.
DEF VAR ldtAccDate    AS DATE NO-UNDO EXTENT 2.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liFiles       AS INT  NO-UNDO.
DEF VAR ldtDate       AS DATE NO-UNDO EXTENT 2. 

FORM 
   SKIP(2)
   "Print a tax report from TopUp payments." AT 10 
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

   liPaymType COLON 20
      LABEL "Payment Type"
      HELP "One payment type or 0 = ALL"
      FORMAT ">9"
   lcPaymType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP

   ldtAccDate[1] COLON 20 
      LABEL "Accounting Date" 
      FORMAT "99-99-9999"
      HELP "Accounting date, empty = ALL"
   "-"
   ldtAccDate[2] 
      NO-LABEL
      FORMAT "99-99-9999"
      HELP "Accounting date"
      VALIDATE(INPUT ldtAccDate[2] >= INPUT ldtAccDate[1],
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
     TITLE " " + ynimi + " TOUP TAX REPORT " + 
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

FUNCTION fTypeName RETURNS LOGIC
   (iiPaymType AS INT):
   
   IF iiPaymType = 0 
   THEN lcPaymType = "ALL".
   ELSE lcPaymType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "Payment",
                                      "PaymType",
                                      STRING(iiPaymType)).
      
   DISPLAY lcPaymType WITH FRAME fCrit.
   
END FUNCTION.


ASSIGN ufkey           = FALSE
       ldtAccDate[2]   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtAccDate[1]   = DATE(MONTH(ldtAccDate[2]),1,YEAR(ldtAccDate[2]))
       liPaymType      = 7 
       lcFile          = fCParamC("TaxVouchFileName")
       lcTransDir      = fCParamC("TaxRepTransDir").
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcTaxZone 
           lcCustID
           liPaymType
           ldtAccDate
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fTaxName(lcTaxZone).
   fTypeName(liPaymType).

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN ufkey.
   END.
   ELSE ASSIGN toimi = 1
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN ufkey.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcTaxZone
                lcCustID
                /*    
                liPaymType
                */
                ldtAccDate
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"liPaymType") > 0 THEN DO:

               IF FRAME-FIELD = "liPaymType" THEN DO:
                  RUN h-tmscodes(INPUT "Payment",  /* TableName*/
                                       "PaymType",  /* FieldName */
                                       "AccRec",   /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) @ liPaymType.
                  END.
               END.
               
               ehto = 9.
               RUN ufkey.
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

               ELSE IF FRAME-FIELD = "liPaymType" THEN DO:
                  fTypeName(INPUT INPUT liPaymType).
                  IF lcPaymType = "" THEN DO:
                     MESSAGE "Unknown payment type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
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
      ASSIGN ldtDate[1] = ldtAccDate[1] - 1
             ldtDate[2] = ldtAccDate[2] + 1.
      IF MONTH(ldtDate[1]) = MONTH(ldtAccDate[1]) OR
         MONTH(ldtDate[2]) = MONTH(ldtAccDate[2]) THEN DO:
         MESSAGE "Period must be a whole month"
         VIEW-AS ALERT-BOX ERROR. 
         NEXT. 
      END.
      
      RUN taxvouchrep (lcTaxZone,
                       lcCustID,
                       liPaymType,
                       ldtAccDate[1],
                       ldtAccDate[2],
                       lcFile,
                       OUTPUT liCount,
                       OUTPUT liFiles,
                       OUTPUT lcError).
           
      MESSAGE liCount "payments were written to"
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

