/* ------------------------------------------------------
  MODULE .......: cocalcui.p
  FUNCTION .....: commission calculation
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 08.01.03
  MODIFIED .....: 
                  13.10.03/aam tokens
                  21.11.03/aam DelDate
                  29.06.04/aam RuleID,
                               EventCust
                  30.08.04/aam brand             
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'coevent'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR liCustNum1    AS INT  NO-UNDO.
DEF VAR liCustNum2    AS INT  NO-UNDO.
DEF VAR liCustNum3    AS INT  NO-UNDO.
DEF VAR liCustNum4    AS INT  NO-UNDO.
DEF VAR liCoRuleID    AS INT  NO-UNDO. 
DEF VAR ldtDate1      AS DATE NO-UNDO. 
DEF VAR ldtDate2      AS DATE NO-UNDO. 
DEF VAR lcReseller    AS CHAR NO-UNDO.
DEF VAR lcSalesman    AS CHAR NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR llRecalc      AS LOG  NO-UNDO.
DEF VAR ldtDelete     AS DATE NO-UNDO. 

FORM
   skip(1)
   "Commission events will be created according to defined rules" AT 10 SKIP
   "and criteria given below. " AT 10 SKIP(2)

   lcInvGroup AT 10
      LABEL "Invoicing group"
      HELP  "Invoicing group, EMPTY = ALL"
      FORMAT "X(8)"
      VALIDATE(INPUT lcInvGroup = "" OR
               CAN-FIND(InvGroup WHERE 
                        InvGroup.Brand    = gcBrand AND
                        InvGroup.InvGroup = INPUT lcInvGroup),
              "Unknown invoicing group")

      InvGroup.IGName 
         NO-LABEL
         FORMAT "X(30)" 
      SKIP

   liCustNum1 AT 10
      LABEL "Rule Customers "
      HELP  "Minimum limit for rule customer numbers"
      FORMAT ">>>>>>>9"
   "-" AT 38
   liCustNum2 
      NO-LABEL 
      HELP  "Maximum limit for rule customer numbers"
      FORMAT ">>>>>>>9"
      VALIDATE(INPUT liCustNum2 >= INPUT liCustNum1,
               "Upper limit cannot be less than lower limit")
      SKIP

   liCustNum3 AT 10
      LABEL "Event Customers"
      HELP  "Minimum limit for event customer numbers"
      FORMAT ">>>>>>>9"
   "-" AT 38
   liCustNum4 
      NO-LABEL 
      HELP  "Maximum limit for event customer numbers"
      FORMAT ">>>>>>>9"
      VALIDATE(INPUT liCustNum4 >= INPUT liCustNum3,
               "Upper limit cannot be less than lower limit")
      SKIP

   liCoRuleID AT 10 
      LABEL "Rule ID ......."
      HELP "Rule that is used, 0 (empty) = ALL"
      VALIDATE(INPUT liCoRuleID = 0 OR 
               CAN-FIND(CoRule WHERE 
                        CoRule.Brand    = gcBrand AND
                        CORule.CoRuleID = INPUT liCoRuleID),
               "Unknown rule")
      FORMAT ">>>>>>>>"
      SKIP

   ldtDate1 AT 10
      LABEL "Time period ..."
      HELP  "Invoice dates, activation dates etc."
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate1 NE ?,
               "Date is mandatory")
   "-" AT 38
   ldtDate2  
      NO-LABEL 
      HELP  "Invoice dates, activation dates etc."
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate2 NE ? AND 
               INPUT ldtDate2 >= INPUT ldtDate1,
               "Upper limit cannot be less than lower limit")
      SKIP

   lcReseller AT 10
      LABEL "Reseller ......"
      HELP "EMPTY = all resellers"
      VALIDATE(INPUT lcReseller = "" OR
               CAN-FIND(Reseller WHERE
                        Reseller.Brand    = gcBrand AND
                        Reseller.Reseller = INPUT lcReseller),
               "Unknown reseller")
      FORMAT "X(8)"
   Reseller.RsName 
      NO-LABEL 
      FORMAT "X(30)" 
      SKIP

   lcSalesman AT 10
      LABEL "Salesman ......"
      HELP "EMPTY = all salesmen"
      VALIDATE(INPUT lcSalesman = "" OR
               CAN-FIND(Salesman WHERE 
                        Salesman.Brand    = gcBrand AND
                        Salesman.Salesman = INPUT lcSalesman),
               "Unknown salesman")
      FORMAT "X(8)"
   Salesman.SmName 
      NO-LABEL 
      FORMAT "X(30)" 
      SKIP(1)

   llRecalc AT 10
      LABEL "Recalculation ."
      HELP "Recalculate unpaid events that match the given criteria"
      FORMAT "Yes/No"
      SKIP
   
   ldtDelete AT 10
      LABEL "Deletion ......"
      HELP "Delete all on this date calculated (unpaid) events before recalc."
      FORMAT "99-99-9999"
      SKIP(2)

   WITH ROW 1 side-labels width 80
        title " " + ynimi + " COMMISSION CALCULATION " +
        string(pvm,"99-99-99") + " "
        FRAME fCriter.

FUNCTION fReseller RETURNS LOGICAL
   (icReseller AS CHAR).

   IF icReseller = "" 
   THEN DISPLAY "ALL" ;& Reseller.RSName WITH FRAME fCriter.

   ELSE DO:
      FIND Reseller WHERE 
           Reseller.Brand    = gcBrand AND
           Reseller.Reseller = icReseller
      NO-LOCK NO-ERROR.
      IF AVAILABLE Reseller THEN DISPLAY Reseller.RsName WITH FRAME fCriter. 
   END.

END FUNCTION.

FUNCTION fSalesman RETURNS LOGICAL
   (icSalesman AS CHAR).

   IF icSalesman = "" 
   THEN DISPLAY "ALL" ;& Salesman.SMName WITH FRAME fCriter.

   ELSE DO:
      FIND Salesman WHERE   
           Salesman.Brand    = gcBrand AND
           Salesman.Salesman = icSalesman
      NO-LOCK NO-ERROR.
      IF AVAILABLE Salesman THEN DISPLAY Salesman.SmName WITH FRAME fCriter.
   END.

END FUNCTION.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

ASSIGN liCustNum2    = 99999999
       liCustNum4    = 99999999
       ldtDate2      = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtDate1      = DATE(MONTH(ldtDate2),1,YEAR(ldtDate2))

       ufkey         = FALSE
       nap           = "1".

toimi:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO toimi, NEXT toimi:

      DISPLAY
      "ALL" ;& InvGroup.IGName
      liCustNum1
      liCustNum2
      liCustNum3
      liCustNum4
      liCoRuleID
      ldtDate1
      ldtDate2
      llRecalc
      ldtDelete
      WITH FRAME fCriter.

      fReseller("").
      fSalesman("").

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 7    ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 879 ELSE 0)
         ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 .
         RUN ufkey.p.

         READKEY.
         nap = keylabel(LASTKEY).
      END.
      ELSE ufkey = TRUE.
      
      IF LOOKUP(nap,"1,f1") > 0 THEN DO:

         repeat WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
             ehto = 9. RUN ufkey.p.
             UPDATE 
                lcInvGroup
                liCustNum1
                liCustNum2
                liCustNum3
                liCustNum4
                liCoRuleID
                ldtDate1
                ldtDate2
                lcReseller
                lcSalesman
                llRecalc
                ldtDelete
             WITH FRAME fCriter EDITING:
                READKEY.

                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:

                   IF FRAME-FIELD = "lcInvGroup" THEN DO:
                      IF INPUT lcInvGroup = "" 
                      THEN DISPLAY "ALL" ;& InvGroup.IGName. 
                      ELSE DO:
                         FIND InvGroup WHERE  
                              InvGroup.Brand    = gcBrand AND
                              InvGroup.InvGroup = INPUT lcInvGroup
                         NO-LOCK NO-ERROR.
                         IF AVAILABLE InvGroup THEN DISPLAY InvGroup.IGName.
                      END.
                   END. 

                   ELSE IF FRAME-FIELD = "lcReseller" THEN DO:
                      fReseller(INPUT FRAME fCriter lcReseller). 
                   END. 

                   ELSE IF FRAME-FIELD = "lcSalesman" THEN DO:
                      fSalesman(INPUT FRAME fCriter lcSalesman). 

                      IF INPUT lcSalesman NE "" AND 
                         AVAILABLE Salesman     AND
                         INPUT lcReseller NE "" AND 
                         Salesman.Reseller NE lcReseller
                      THEN DO: 
                         MESSAGE "Salesman belongs to another reseller"
                         VIEW-AS ALERT-BOX
                         ERROR.
                         NEXT.
                      END. 
                   END. 

                END.

                APPLY LASTKEY.                              
             END. 

             LEAVE.
         END.

         NEXT toimi.
      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
      THEN DO:

         llOk = TRUE. 
         MESSAGE "Start calculating commissions ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.

         ehto = 5.
         RUN ufkey.

         RUN cocalc(lcInvGroup,
                    liCustNum1,
                    liCustNum2,
                    liCustNum3,
                    liCustNum4,
                    liCoRuleID,
                    ldtDate1,
                    ldtDate2,
                    lcReseller,
                    lcSalesman,
                    llRecalc,
                    ldtDelete,
                    OUTPUT liCount).

         MESSAGE liCount "commission events were created."
         VIEW-AS ALERT-BOX
         INFORMATION.

         LEAVE toimi.

      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         LEAVE toimi.
      END.

END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

