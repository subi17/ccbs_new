/* ---------------------------------------------------------------------------
  MODULE .......: balrel.p
  FUNCTION .....: ui FOR Balance report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 23.06.04/aam 
  MODIFIED .....: 
                  16.06.06/aam ClaimState instead of ClaimQty
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i "new"}

assign tuni1 = "balrel"
       tuni2 = "".

DEF VAR ufkey        AS LOG  NO-UNDO.
DEF VAR lcInvGroup   AS CHAR NO-UNDO EXTENT 2.
DEF VAR liCustNum    AS INT  NO-UNDO EXTENT 2.
DEF VAR ldClaimQty   AS DEC  NO-UNDO EXTENT 2.
DEF VAR liInvType    AS INT  NO-UNDO EXTENT 2.
DEF VAR liPaymPlan   AS INT  NO-UNDO. 
DEF VAR ldtPaymDate  AS DATE NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR liField      AS INT  NO-UNDO. 
DEF VAR lcFile       AS CHAR NO-UNDO.

form
   SKIP(2)
   "Current balances for selected customers are printed,"     AT 10 SKIP
   "including deposits, overpayments and advance payments."   AT 10 SKIP
   "Also sum of payments from given date onwards is printed." AT 10 SKIP(2)

   lcInvGroup[1] AT 10
      LABEL "Invoicing groups .."
      HELP "Invoicing group"
      FORMAT "X(8)"
   "-"
   lcInvGroup[2] 
      NO-LABEL
      HELP "Invoicing group"
      VALIDATE(INPUT lcInvGroup[2] >= INPUT lcInvGroup[1],
               "Upper limit cannot be less than lower limit")
      FORMAT "X(8)"          
   SKIP
      
   liCustNum[1] AT 10
      LABEL "Customers ........."
      HELP "Customer number"
      FORMAT ">>>>>>>9"
   "-"
   liCustNum[2]
      NO-LABEL 
      HELP "Customer number"
      VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
               "Upper limit cannot be less than lower limit")
      FORMAT ">>>>>>>9"
   SKIP

   liInvType[1] AT 10
      LABEL "Invoice Types ....."
      HELP "Invoice type of invoices"
      FORMAT "9"
   "-"
   liInvType[2]
      NO-LABEL 
      HELP "Invoice type of invoices"
      VALIDATE(INPUT liInvType[2] >= INPUT liInvType[1],
               "Upper limit cannot be less than lower limit")
      FORMAT "9"
   SKIP

   ldClaimQty[1] AT 10
      LABEL "Claiming State ...."
      HELP "Claiming state (qty) of invoices"
      FORMAT ">9.9<"
   "-"
   ldClaimQty[2]
      NO-LABEL 
      HELP "Claiming state (qty) of invoices"
      VALIDATE(INPUT ldClaimQty[2] >= INPUT ldClaimQty[1],
               "Upper limit cannot be less than lower limit")
      FORMAT ">9.9<"
   SKIP

   liPaymPlan AT 10
      LABEL "Active Payment Plan" 
      HELP "0=All invoices, 1=Invoices on a PP, 2=Invoices not on a PP"
      VALIDATE(INPUT liPaymPlan < 3, "Valid choices are 0-2")
      FORMAT "9"
   SKIP
   
   ldtPaymDate  AT 10
      LABEL "Payments From ....."
      HELP "Payments from this date onwards till today are calculated"
      VALIDATE(INPUT ldtPaymDate NE ?,
               "Date is mandatory")
      FORMAT "99-99-9999"
   SKIP(1)   
   lcFile AT 10
      LABEL "Excel-file ........"
      HELP "If name is given, then a tab-separated file is made"
      FORMAT "X(47)"
   SKIP(2)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " BALANCE REPORT " + STRING(pvm,"99-99-99") + " "
FRAME valinta.

VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

FIND LAST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN lcInvGroup[2] = InvGroup.InvGroup.
ASSIGN liCustNum[2]  = 99999999  
       ldClaimQty[2] = 8
       liInvType     = 1
       liPaymPlan    = 0
       ldtPaymDate   = IF MONTH(TODAY) = 1
                       THEN DATE(12,1,YEAR(TODAY) - 1)
                       ELSE DATE(MONTH(TODAY) - 1,1,YEAR(TODAY))
       ufkey         = false
       nap           = "1".

toimi:
REPEAT WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

   PAUSE 0.
   DISPLAY lcInvGroup liCustNum ldtPaymDate 
           liInvType ldClaimQty liPaymPlan lcFile
   WITH frame valinta.
      
   IF ufkey THEN DO:
      ASSIGN
      ufk[1]= 132  
      ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
      ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
      ufk[9]= 1
      ehto = 3 ufkey = FALSE.
      RUN Syst/ufkey.p.

      READKEY.
      nap = keylabel(lastkey).
   END.
   ELSE ufkey = TRUE.

   IF LOOKUP(nap,"1,f1") > 0 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH frame valinta ON ENDKEY UNDO, LEAVE:
         UPDATE 
         lcInvGroup
         liCustNum 
         liInvType
         ldClaimQty
         liPaymPlan
         ldtPaymDate
         lcFIle
         WITH FRAME valinta EDITING:
         
            READKEY. 
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND INDEX(FRAME-FIELD,"liInvType") > 0 
            THEN DO:

               liField = FRAME-INDEX.
                
               RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                    "InvType", /* FieldName */
                                    "Report", /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ?
               THEN DO WITH FRAME rajat:
                  DISPLAY INTEGER(lcCode) ;& liInvType[liField].
               END.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE.
               IF FRAME-FIELD = "" THEN DO:
               END.
            END.
            
            APPLY LASTKEY.
         END.
         
         LEAVE.
      END.
   END.

   ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
      
      IF lcFile = "" THEN DO:  
         tila = TRUE.
         {Syst/tmsreport.i "return"}
      END.
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.p.
      END. 
      
      RUN Ar/balrep.p  (lcInvGroup[1],
                   lcInvGroup[2],
                   liCustNum[1],
                   liCustNum[2],
                   liInvType[1],
                   liInvType[2],
                   ldClaimQty[1],
                   ldClaimQty[2],
                   liPaymPlan,
                   ldtPaymDate,
                   lcFile).

      IF lcFile = "" THEN DO:
         tila = FALSE.
         {Syst/tmsreport.i}        
      END.
      
      MESSAGE "Balance report finished" 
      VIEW-AS ALERT-BOX 
      TITLE " DONE ".

      LEAVE toimi.
   END.

   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.

END. /* toimi */

HIDE MESSAGE       NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.

