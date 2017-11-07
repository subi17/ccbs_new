/* ------------------------------------------------------
  MODULE .......: cobalui.p
  FUNCTION .....: ui for adding balances from commission events
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.02.04
  MODIFIED .....: 19.04.04/aam create fat instead of balances
                  30.08.04/aam Brand, tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'coevent'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCoRule      AS INT  NO-UNDO.
DEF VAR lcReseller1   AS CHAR NO-UNDO.
DEF VAR lcReseller2   AS CHAR NO-UNDO.
DEF VAR ldtDate1      AS DATE NO-UNDO. 
DEF VAR ldtDate2      AS DATE NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liCust        AS INT  NO-UNDO. 
DEF VAR llOk          AS LOG  NO-UNDO.

FORM
   skip(2)
   "FAT events will be created from commission events." AT 10 SKIP(2)

   liCoRule AT 10 
      LABEL "Commission Rule"
      FORMAT ">>>>>>>>"
      HELP  "Rule from which events will be used (0/EMPTY = all)"
      VALIDATE(INPUT liCoRule = 0 OR
               CAN-FIND(CoRule WHERE 
                        CoRule.Brand    = Syst.Var:gcBrand AND
                        CoRule.CoRuleID = INPUT liCoRule),
              "Unknown rule")

      CoRule.RuleDesc 
         NO-LABEL
         FORMAT "X(30)" 
      SKIP

   lcReseller1 AT 10
      LABEL "Resellers ....."
      HELP  "Minimum limit for resellers"
      FORMAT "X(8)"
   "-" AT 38
   lcReseller2 
      NO-LABEL 
      HELP  "Maximum limit for resellers"
      FORMAT "X(8)"
      VALIDATE(INPUT lcReseller2 >= INPUT lcReseller1,
               "Upper limit cannot be less than lower limit")
      SKIP

   ldtDate1 AT 10
      LABEL "Time period ..."
      HELP  "Event calculation dates"
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate1 NE ?,
               "Date is mandatory")
   "-" AT 38
   ldtDate2  
      NO-LABEL 
      HELP  "Event calculation dates"
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate2 NE ? AND 
               INPUT ldtDate2 >= INPUT ldtDate1,
               "Upper limit cannot be less than lower limit")
      SKIP(9)

   WITH ROW 1 side-labels width 80
        title " " + Syst.Var:ynimi + " FAT FROM COMMISSION " +
        string(TODAY,"99-99-99") + " "
        FRAME fCriter.


FUNCTION fCoRule RETURNS LOGICAL
   (iiRuleID AS INT).

   IF iiRuleID = 0 
   THEN DISPLAY "ALL" ;& CoRule.RuleDesc WITH FRAME fCriter.

   ELSE DO:
      FIND CoRule WHERE 
           CoRule.Brand    = Syst.Var:gcBrand AND
           CoRule.CoRuleID = iiRuleID
      NO-LOCK NO-ERROR.
      IF AVAILABLE CoRule THEN DISPLAY CoRule.RuleDesc WITH FRAME fCriter.
   END.

END FUNCTION.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

FIND LAST Reseller WHERE Reseller.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.
ASSIGN lcReseller2   = IF AVAILABLE Reseller THEN Reseller.Reseller ELSE ""
       ldtDate2      = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtDate1      = DATE(MONTH(ldtDate2),1,YEAR(ldtDate2))

       ufkey = FALSE.

toimi:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO toimi, NEXT toimi:

      DISPLAY
      liCoRule
      lcReseller1
      lcReseller2
      ldtDate1
      ldtDate2
      WITH FRAME fCriter.

      fCoRule(liCoRule).
      
      IF ufkey THEN DO:
         ASSIGN
         Syst.Var:ufk[1]= 7    Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
         Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 795 ELSE 0)
         Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 
         Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 .
         RUN Syst/ufkey.p.

         READKEY.
         ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      END.
      ELSE ASSIGN Syst.Var:nap  = "1"
                  ufkey = TRUE.

      IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO:

         repeat WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
             Syst.Var:ehto = 9. RUN Syst/ufkey.p.
             UPDATE 
                liCoRule
                lcReseller1
                lcReseller2
                ldtDate1
                ldtDate2
             WITH FRAME fCriter EDITING:
                READKEY.

                IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:

                   IF FRAME-FIELD = "liCoRule" THEN DO:
                      fCoRule(INPUT INPUT liCoRule).
                   END. 

                END.

                APPLY LASTKEY.                              
             END. 

             LEAVE.
         END.

      END.

      ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW"
      THEN DO:

         llOk = TRUE. 
         MESSAGE "Start creating FAT events ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.

         Syst.Var:ehto = 5.
         RUN Syst/ufkey.p.

         RUN Ar/cobal.p (liCoRule,
                    lcReseller1,
                    lcReseller2,
                    ldtDate1,
                    ldtDate2,
                    OUTPUT liCount).

         MESSAGE liCount "FAT events were created."
         VIEW-AS ALERT-BOX
         INFORMATION.

         LEAVE toimi.

      END.

      ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN DO:
         LEAVE toimi.
      END.


END. /* Syst.Var:toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

