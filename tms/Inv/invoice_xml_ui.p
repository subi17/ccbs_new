/* ----------------------------------------------------------------------
  MODULE .......: invoice_xml_ui.p
  TASK .........: Print invoices to an xml file (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.08.09
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR lcIgName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR llOnlyNew     AS LOG  NO-UNDO.
DEF VAR llCredit      AS LOG  NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR lcInvType     AS CHAR NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO EXTENT 2.
DEF VAR lcInvID       AS CHAR NO-UNDO EXTENT 2.
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liPreQty      AS INT  NO-UNDO.
DEF VAR llSeparate    AS LOG  NO-UNDO.
DEF VAR liDelType     AS INT  NO-UNDO.
DEF VAR lcDelType     AS CHAR NO-UNDO.
DEF VAR llTarFile     AS LOG  NO-UNDO.

FORM 
   SKIP(1)
   "Print invoices to an XML file." AT 10 
   SKIP(1)
                   
   lcInvGroup COLON 20
      LABEL "Invoice Group"
      HELP  "Invoice group, EMPTY = all"
   lcIgName
      NO-LABEL
      FORMAT "X(30)" 
   SKIP

   liCustNum[1] COLON 20
      LABEL  "Customers"
      HELP   "Customers"
      FORMAT ">>>>>>>9"
   "-"
   liCustNum[2]
      NO-LABEL
      HELP "Customers"
      VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
               "Invalid definition")
      FORMAT ">>>>>>>9"
      SKIP(1)                 

   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "One invoice type or 0 = ALL"
      FORMAT ">9"
   lcInvType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP

   liDelType COLON 20
      LABEL "Delivery Type"
      HELP "Invoice delivery type"
      FORMAT ">9"
   lcDelType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP
 
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date, empty = ALL"
      SKIP

   lcInvID[1] COLON 20
      LABEL "Invoices"
      HELP  "Invoices"
      FORMAT "X(14)"
   "-"
   lcInvID[2]
      NO-LABEL
      HELP "Invoices"
      VALIDATE(INPUT lcInvID[2] >= INPUT lcInvID[1],
               "Invalid definition")
      FORMAT "X(14)"
      SKIP           
            
   llOnlyNew COLON 20
      LABEL "Printing Status" 
      FORMAT "New/All"
      HELP "Print (A)ll or only (N)ew unprinted invoices" SKIP
        
   llCredit COLON 20
      LABEL "Credit Notes"
      HELP "Print credited and credit notes" 
      FORMAT "Yes/No"
      SKIP(1)
   
   llSeparate COLON 20
      LABEL "Separate Files"
      HELP "Print each invoice to a separate file"
      FORMAT "Yes/No"
      SKIP
   llTarFile COLON 20
      LABEL "Tar File"
      HELP "Create tar file for printing house"
      FORMAT "Yes/No"
      SKIP
   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(50)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(50)" 
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  INVOICES AS XML " + 
        STRING(pvm,"99-99-99") + " " FRAME fCrit.


FUNCTION fIGName RETURNS LOGIC
   (INPUT icInvGroup AS CHAR):
   
   lcIGName = "".
   
   IF icInvGroup = "" THEN lcIgName = "ALL".
               
   ELSE DO:
      FIND InvGroup WHERE 
           InvGroup.Brand    = gcBrand AND
           InvGroup.InvGroup = icInvGroup
      NO-LOCK NO-ERROR.
      IF AVAILABLE InvGroup THEN lcIgName = InvGroup.IGName.
   END.
   
   DISPLAY lcIgName WITH FRAME fCrit.
   
END FUNCTION.

FUNCTION fTypeName RETURNS LOGIC
   (iiInvType AS INT):
   
   IF iiInvType = 0 
   THEN lcInvType = "ALL".
   ELSE lcInvType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                     "Invoice",
                                     "InvType",
                                     STRING(iiInvType)).
      
   DISPLAY lcInvType WITH FRAME fCrit.
   
END FUNCTION.

FUNCTION fDelTypeName RETURNS LOGIC
   (iiDelType AS INT):
   
   IF iiDelType = 0 
   THEN lcDelType = "ALL".
   ELSE lcDelType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                     "Invoice",
                                     "DelType",
                                     STRING(iiDelType)).
      
   DISPLAY lcDelType WITH FRAME fCrit.
   
END FUNCTION.



ASSIGN ufkey         = FALSE
       liCustNum[2]  = 99999999
       lcInvID[2]    = FILL("Z",12)
       ldtInvDate    = TODAY
       llOnlyNew     = TRUE
       llCredit      = FALSE
       llSeparate    = TRUE
       llTarFile     = FALSE
       liInvType     = 1 
       liDelType     = 1
       lcFile        = fCParamC("InvXMLFile").
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   IF liInvType = 99 
   THEN lcTransDir = fCParamC("InvXMLTestTransDir").
   ELSE lcTransDir = fCParamC("InvXMLTransDir").

   PAUSE 0.
   DISPLAY lcInvGroup 
           lcInvID
           liCustNum
           ldtInvDate
           llOnlyNew
           llCredit
           liInvType
           liDelType
           llSeparate
           llTarFile
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fIGName(lcInvGroup).
   fTypeName(liInvType).
   fDelTypeName(liDelType).

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[3] = 1128
         ufk[5] = 63  
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN toimi = 1
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.p.
      
      liPreQty = 0.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcInvGroup
                liCustNum
                liInvType
                liDelType
                ldtInvDate
                lcInvID
                llOnlyNew
                llCredit
                llSeparate
                llTarFile
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND LOOKUP(FRAME-FIELD,"liInvType,liDelType") > 0
            THEN DO:

               IF FRAME-FIELD = "liInvType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "InvType", /* FieldName */
                                       "Report", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) ;& liInvType.
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "liDelType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "DelType", /* FieldName */
                                       "Billing", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) ;& liDelType.
                  END.
               END.
                
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcInvGroup" THEN DO:

                  IF INPUT lcInvGroup > "" AND
                     NOT CAN-FIND(InvGroup WHERE 
                                  InvGroup.Brand    = gcBrand AND
                                  InvGroup.InvGroup = INPUT lcInvGroup)
                  THEN DO:
                     MESSAGE "Unknown invoicing group."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  fIGName(INPUT INPUT lcInvGroup).
               END.

               ELSE IF FRAME-FIELD = "liInvType" THEN DO:
                  fTypeName(INPUT INPUT liInvType).
                  IF lcInvType = "" THEN DO:
                     MESSAGE "Unknown invoice type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "liDelType" THEN DO:
                  fDelTypeName(INPUT INPUT liDelType).
                  IF lcDelType = "" THEN DO:
                     MESSAGE "Unknown invoice delivery type"
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

   /* quantity of invoices */ 
   ELSE IF toimi = 3 THEN DO:
      
      RUN Inv/printdoc1co.p (lcInvGroup,
                       liCustNum[1],
                       liCustNum[2],
                       lcInvID[1],
                       lcInvID[2],
                       ldtInvDate,
                       llOnlyNew,
                       llCredit,
                       liInvType,
                       liDelType,
                       "",
                       "Qty",
                       FALSE,
                       OUTPUT liPreQty,
                       OUTPUT lcError).
      
      MESSAGE liPreQty "invoices will be printed with given criteria."
      VIEW-AS ALERT-BOX TITLE " QUANTITY ".
   END.


   /* printing */
   ELSE IF toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF liInvType = 99 THEN 
         lcFile = lcTransDir + "*" + lcFile.
         
      RUN Inv/printdoc1co.p (lcInvGroup,
                       liCustNum[1],
                       liCustNum[2],
                       lcInvID[1],
                       lcInvID[2],
                       ldtInvDate,
                       llOnlyNew,
                       llCredit,
                       liInvType,
                       liDelType,
                       "XML" + STRING(llSeparate,"SEP/"),
                       lcFile,
                       llTarFile,
                       OUTPUT liCount,
                       OUTPUT lcError).
      
      MESSAGE liCount "invoices were printed" SKIP ""
              (IF liPreQty > 0 
               THEN "Precounted quantity was " + STRING(liPreQty) + CHR(10)
               ELSE "") 
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

