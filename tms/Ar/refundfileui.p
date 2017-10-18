/* ----------------------------------------------------------------------
  MODULE .......: refundfileui.p
  TASK .........: Print payments to a csb34 file for refund (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.09.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fbankday.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Payment'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR lcIgName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO EXTENT 2.
DEF VAR ldtAccDate    AS DATE NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liFramePos    AS INT  NO-UNDO. 
DEF VAR liFiles       AS INT  NO-UNDO.

FORM 
   SKIP(3)
   "Print payments to a refund file using CSB34.1 format." AT 10 
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
      FORMAT ">>>>>>>>9"
   "-"
   liCustNum[2]
      NO-LABEL
      HELP "Customers"
      VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
               "Invalid definition")
      FORMAT ">>>>>>>>9"
      SKIP(1)

   ldtAccDate COLON 20 
      LABEL "Posting Date" 
      FORMAT "99-99-9999"
      HELP "Posting date, empty = ALL"
      SKIP(1)                 

   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(50)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(50)" 
   SKIP(4)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  PAYMENTS TO CSB34  " + STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


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


ASSIGN ufkey           = FALSE
       liCustNum[2]    = 999999999
       ldtAccDate      = TODAY + 3
       lcFile          = fCParamC("RefundFile")
       lcTransDir      = fCParamC("RefundTransDir").
       
REPEAT:
  IF fBankDays(ldtAccDate) >= 3 THEN LEAVE.
  ldtAccDate = ldtAccDate + 1.
END.


CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcInvGroup 
           liCustNum
           ldtAccDate
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fIGName(lcInvGroup).

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN toimi = 1
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcInvGroup
                liCustNum
                ldtAccDate
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

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
      
      IF fBankDays(ldtAccDate) < 3 THEN DO:
         MESSAGE "There are less than 3 banking days before payment date."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Ar/refundfileco.p (lcInvGroup,
                        liCustNum[1],
                        liCustNum[2],
                        ldtAccDate,
                        lcFile,
                        FALSE,         /* no empty file */
                        OUTPUT liCount,
                        OUTPUT liFiles,
                        OUTPUT lcError).
      
      MESSAGE liCount "payments were written to"
              liFiles "refund file" + (IF liFiles > 1 THEN "s" ELSE "") 
              SKIP ""
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
   
   

