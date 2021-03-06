/* ----------------------------------------------------------------------
  MODULE .......: ddoutfileui.p
  TASK .........: Print invoices to a csb19 file for direct debiting (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.01.07
  CHANGED ......: 16.04.07/aam billrun to ddoutfileco
                  25.05.07/aam emptyfile to ddoutfileco  
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "anttis".

{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR lcIgName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR lcInvType     AS CHAR NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO EXTENT 2.
DEF VAR lcInvID       AS CHAR NO-UNDO EXTENT 2.
DEF VAR liPrintState  AS INT  NO-UNDO EXTENT 2. 
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liFramePos    AS INT  NO-UNDO. 
DEF VAR liFiles       AS INT  NO-UNDO.
DEF VAR llSplit       AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Print invoices to a direct debit file using CSB19 format." AT 10 
   SKIP(2)
                   
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

   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date, empty = ALL"
      SKIP

   lcInvID[1] COLON 20
      LABEL "Invoices"
      HELP  "Invoices"
      FORMAT "X(12)"
   "-"
   lcInvID[2]
      NO-LABEL
      HELP "Invoices"
      VALIDATE(INPUT lcInvID[2] >= INPUT lcInvID[1],
               "Invalid definition")
      FORMAT "X(12)"
      SKIP           
            
   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "One invoice type or 0 = ALL"
      FORMAT ">9"
   lcInvType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP

   liPrintState[1] COLON 20
      LABEL  "Printing Status"
      HELP   "Printing status"
      FORMAT ">9"
   "-"
   liPrintState[2]
      NO-LABEL
      HELP "Printing status"
      VALIDATE(INPUT liPrintState[2] >= INPUT liPrintState[1],
               "Invalid definition")
      FORMAT ">9"
      SKIP(1)                 

   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(50)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(50)" 
      SKIP
   llSplit COLON 20
      LABEL "Split the File"
      HELP "Split DD file into two according to customers' banks"
      FORMAT "Yes/No"
   SKIP(1)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.Var:ynimi + "  CSB19 SPLIT TEST " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


FUNCTION fIGName RETURNS LOGIC
   (INPUT icInvGroup AS CHAR):
   
   lcIGName = "".
   
   IF icInvGroup = "" THEN lcIgName = "ALL".
               
   ELSE DO:
      FIND InvGroup WHERE 
           InvGroup.Brand    = Syst.Var:gcBrand AND
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
   ELSE lcInvType = Func.Common:mTMSCodeName("Invoice",
                                     "InvType",
                                     STRING(iiInvType)).
      
   DISPLAY lcInvType WITH FRAME fCrit.
   
END FUNCTION.


ASSIGN ufkey           = FALSE
       liCustNum[2]    = 99999999
       lcInvID[2]      = FILL("Z",12)
       ldtInvDate      = TODAY
       liPrintState[1] = 1
       liPrintState[2] = 1
       liInvType       = 1 
       lcFile          = "/apps/yoigo/tms_support/testing/YOIGO_CSB19_#BANK_#DDATE.txt"
       lcTransDir      = ""
       llSplit         = TRUE.
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcInvGroup 
           lcInvID
           liCustNum
           ldtInvDate
           liInvType
           liPrintState
           lcFile
           lcTransDir
           llSplit
   WITH FRAME fCrit.

   fIGName(lcInvGroup).
   fTypeName(liInvType).

   IF ufkey THEN DO:
      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 132 
         Syst.Var:ufk[5] = 795
         Syst.Var:ufk[8] = 8 
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN Syst.Var:toimi = 1
               ufkey = TRUE.

   IF Syst.Var:toimi = 1 THEN DO:

      Syst.Var:ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcInvGroup
                liCustNum
                ldtInvDate
                lcInvID
                liInvType
                liPrintState
                llSplit
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.Var:nap = KEYLABEL(LASTKEY).

            IF Syst.Var:nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"liInvType,liPrintState") > 0 THEN DO:

               IF FRAME-FIELD = "liInvType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "InvType",  /* FieldName */
                                       "Report",   /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) @ liInvType.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "liPrintState" THEN DO:

                  liFramePos = FRAME-INDEX.
                  
                  RUN Help/h-tmscodes.p(INPUT "Invoice",    /* TableName*/
                                       "PrintState", /* FieldName */
                                       "Report",     /* GroupCode */
                                 OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME fCrit:
                     CASE liFramePos:
                     WHEN 1 THEN DISPLAY INTEGER(lcCode) @ liPrintState[1].
                     WHEN 2 THEN DISPLAY INTEGER(lcCode) @ liPrintState[2].
                     END CASE.
                  END.    
               END.   
 
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcInvGroup" THEN DO:

                  IF INPUT lcInvGroup > "" AND
                     NOT CAN-FIND(InvGroup WHERE 
                                  InvGroup.Brand    = Syst.Var:gcBrand AND
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
               
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   ELSE IF Syst.Var:toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN /apps/yoigo/tms_support/testing/ddoutfileco_test.p (lcInvGroup,
                       liCustNum[1],
                       liCustNum[2],
                       lcInvID[1],
                       lcInvID[2],
                       ldtInvDate,
                       liInvType,
                       "",
                       liPrintState[1],
                       liPrintState[2],
                       lcFile,
                       FALSE,         /* no empty file */
                       llSplit,
                       0,
                       0,
                       OUTPUT liCount,
                       OUTPUT liFiles,
                       OUTPUT lcError).
      
      MESSAGE liCount "invoices were written to"
              liFiles "DD file" + (IF liFiles > 1 THEN "s" ELSE "") SKIP ""
              (IF lcError > "" 
               THEN "Error occurred: " + lcError
               ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.Var:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

