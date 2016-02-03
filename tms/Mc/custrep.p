/* ------------------------------------------------------
  MODULE .......: custrep.p
  FUNCTION .....: Choose criteria for customer report
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 13.03.03
  MODIFIED .....: 27.03.03 tk message when complete, email
                  07.04.03 tk RateCust added
                  12.09.03/aam brand
                  08.03.05/tk Category
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/email.i}

DEF VAR CustNum1  LIKE Customer.CustNum   NO-UNDO.
DEF VAR CustNum2  LIKE Customer.CustNum   NO-UNDO.
DEF VAR ZipCode   LIKE Customer.ZipCode   NO-UNDO FORMAT "99999".
DEF VAR Salesman  LIKE Customer.Salesman  NO-UNDO.
DEF VAR SMName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR RateCust  LIKE Customer.RateCust  NO-UNDO.
DEF VAR RCName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR lCG       AS   LOG                NO-UNDO FORMAT "Yes/No".
DEF VAR lGroups   AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR InvGroup  LIKE Customer.InvGroup  NO-UNDO.
DEF VAR IGName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR Reseller  LIKE Customer.Reseller  NO-UNDO.
DEF VAR RSName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR RatePlan  LIKE RatePlan.RatePlan  NO-UNDO.
DEF VAR RPName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR DiscPlan  LIKE DiscPlan.DiscPlan  NO-UNDO.
DEF VAR DPName    AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR Category  LIKE Customer.Category  NO-UNDO.
DEF VAR CatName   AS   CHAR               NO-UNDO FORMAT "x(30)".
DEF VAR lcCustName AS CHAR                NO-UNDO.

DEF TEMP-TABLE tCustNums 
   FIELD CustNum LIKE Customer.CustNum
   INDEX CustNum CustNum.

FORM
   SKIP(1)
   "          This program will generate a tab separated text file of"
   "          customers matching the criteria given below. "
   SKIP(1)

   CustNum1  AT 15 LABEL "Customer num .." HELP "Customers from number"
   "-"
   CustNum2        NO-LABEL                HELP "Customers to number"   SKIP
   ZipCode   AT 15 LABEL "Zipcode ......." 
      HELP "Zip code, empty for all"                                    SKIP
   Salesman  AT 15 LABEL "Salesman ......" 
      HELP "Salesman code, empty for all"
   "   " SMName          NO-LABEL                                       SKIP
   RateCust  AT 15 LABEL "Rate Cust ....."
      HELP "Rate Cust number, 0 for all"
   "  " RCName     NO-LABEL                                       SKIP
   lCG       AT 15 LABEL "CustGroups ...." 
   HELP "Do you want to select customer groups ?"
   "        " lGroups         NO-LABEL                                  SKIP
   InvGroup  AT 15 LABEL "Invgroup ......" 
      HELP "Invoice group code, empty for all"
   "   " IGName          NO-LABEL                                       SKIP
   Reseller  AT 15 LABEL "Reseller ......" 
      HELP "Reseller code, empty for all"
   "   " RSName          NO-LABEL                                       SKIP
   RatePlan  AT 15 LABEL "Rateplan ......"
      HELP "Rating plan code, empty for all"
   RPName          NO-LABEL                                             SKIP
   DiscPlan  AT 15 LABEL "Discplan ......"
      HELP "Discount plan code, empty for all"
   DPName          NO-LABEL                                             SKIP
   Category  AT 15 LABEL "Category ......"
   "       " CatName     NO-LABEL
   SKIP(3)   
   WITH ROW 1 SIDE-LABELS WIDTH 79
        TITLE " " + ynimi + " CUSTOMER REPORT " +
        STRING(TODAY,"99-99-99") + " "                   
        FRAME crit.

ASSIGN
   CustNum1 = 0
   CustNum2 = 999999999
   lCG      = FALSE.

PAUSE 0.

DISPLAY 
   CustNum1 
   CustNum2 
   ZipCode 
   Salesman 
   "ALL" @ SMName
   RateCust
   "ALL" @ RCName
   lCG
   "ALL" @ lGroups
   InvGroup
   "ALL" @ IGName 
   Reseller
   "ALL" @ RSName
   RatePlan
   "ALL" @ RPName
   DiscPlan
   "ALL" @ DPName
   "ALL" @ CatName
WITH FRAME crit.

limits:
REPEAT WITH FRAME crit:

   ehto = 9. RUN ufkey.

   REPEAT WITH FRAME crit ON ENDKEY UNDO, LEAVE limits:
      UPDATE 
         CustNum1
         CustNum2 
         ZipCode
         Salesman
         RateCust
         lCG
         InvGroup
         Reseller
         RatePlan
         DiscPlan
         Category
      WITH FRAME crit EDITING:
         READKEY.

         IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         HIDE MESSAGE.

            IF FRAME-FIELD = "CustNum1" THEN DO:
               IF INPUT CustNum1 NE 0 AND CustNum2 = 999999999 THEN
                  DISP INPUT CustNum1 @ CustNum2.
            END.

            ELSE IF FRAME-FIELD = "CustNum2" THEN DO: 
               IF INPUT CustNum2 < INPUT CustNum1 THEN DO:
                  MESSAGE "Wrong order !".
                  NEXT.
               END.   
            END. 

            ELSE IF FRAME-FIELD = "Salesman" THEN DO:
               IF INPUT Salesman NE "" THEN DO:
                  FIND FIRST Salesman WHERE    
                             Salesman.Brand    = gcBrand AND
                             Salesman.Salesman = INPUT Salesman
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL Salesman THEN DO:
                     MESSAGE "Unknown salesman !".
                     NEXT.
                  END.
                  ELSE DISP Salesman.SMName @ SMName.
               END.   
               ELSE DISP "ALL" @ SMName.   
            END.

            ELSE IF FRAME-FIELD = "RateCust" THEN DO:
               IF INPUT RateCust NE 0 THEN DO:
                  FIND FIRST Customer WHERE    
                             Customer.Brand   = gcBrand AND
                             Customer.CustNum = INPUT RateCust
                  NO-LOCK NO-ERROR.
                 
                  IF NOT AVAIL Customer THEN DO:
                     MESSAGE "Unknown Customer !".
                     NEXT.
                  END.
                  ELSE DO:
                     lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                    BUFFER Customer).                               DISP lcCustName @ RCName.
                  END.
               END.   
               ELSE DISP "ALL" @ RCName.   
            END.

            ELSE IF FRAME-FIELD = "lCG" THEN DO: 
               IF INPUT lCG = TRUE THEN DO:
                  RUN cgchoose.p(INPUT-OUTPUT lGroups).
                  DISP lGroups.
                  IF lGroups = "" THEN DO:
                     DISP 
                        "No"  @ lCG
                        "ALL" @ lGroups.
                  END.
                  APPLY 13.
                  ehto = 9.
                  RUN ufkey.
                  NEXT.
               END.
               ELSE DISP "ALL" @ lGroups.
            END. 

            ELSE IF FRAME-FIELD = "InvGroup" THEN DO:
               IF INPUT InvGroup NE "" THEN DO:
                  FIND FIRST InvGroup WHERE    
                             InvGroup.Brand    = gcBrand AND
                             InvGroup.InvGroup = INPUT InvGroup
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL InvGroup THEN DO:
                     MESSAGE "Unknown InvGroup !".
                     NEXT.
                  END.
                  ELSE DISP InvGroup.IGName @ IGName.
               END.   
               ELSE DISP "ALL" @ IGName.   
            END.

            ELSE IF FRAME-FIELD = "Reseller" THEN DO:
               IF INPUT Reseller NE "" THEN DO:
                  FIND FIRST Reseller WHERE    
                             Reseller.Brand    = gcBrand AND
                             Reseller.Reseller = INPUT Reseller
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL Reseller THEN DO:
                     MESSAGE "Unknown Reseller !".
                     NEXT.
                  END.
                  ELSE DISP Reseller.RSName @ RSName.
               END.   
               ELSE DISP "ALL" @ RSName.   
            END.

            ELSE IF FRAME-FIELD = "RatePlan" THEN DO:
               IF INPUT RatePlan NE "" THEN DO:
                  FIND FIRST RatePlan WHERE    
                             RatePlan.Brand    = gcBrand AND
                             RatePlan.RatePlan = INPUT RatePlan
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL RatePlan THEN DO:
                     MESSAGE "Unknown RatePlan !".
                     NEXT.
                  END.
                  ELSE DISP RatePlan.RPName @ RPName.
               END.   
               ELSE DISP "ALL" @ RPName.   
            END.

            ELSE IF FRAME-FIELD = "DiscPlan" THEN DO:
               IF INPUT DiscPlan NE "" THEN DO:
                  FIND FIRST DiscPlan WHERE    
                             DiscPlan.Brand    = gcBrand AND
                             DiscPlan.DiscPlan = INPUT DiscPlan
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL DiscPlan THEN DO:
                     MESSAGE "Unknown DiscPlan !".
                     NEXT.
                  END.
                  ELSE DISP DiscPlan.DPName @ DPName.
               END.   
               ELSE DISP "ALL" @ DPName.   
            END.
            ELSE IF FRAME-FIELD = "Category" THEN DO:
               IF INPUT Category NE "" THEN DO:
                  FIND FIRST CustCat WHERE    
                             CustCat.Brand    = gcBrand AND
                             CustCat.Category = INPUT Category
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL CustCat THEN DO:
                     MESSAGE "Unknown Category !".
                     NEXT.
                  END.
                  ELSE DISP CustCat.CatName @ CatName.
               END.   
               ELSE DISP "ALL" @ CatName.   
            END.

         END.
         APPLY LASTKEY.     
      END. 
      LEAVE.
   END.

   DISPLAY
      CustNum1
      CustNum2 
      ZipCode
      Salesman
      InvGroup
      Reseller
      RatePlan
      DiscPlan.

   task:
   REPEAT WITH FRAME crit:

      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.

      IF toimi = 1 THEN NEXT  limits.

      IF toimi = 8 THEN LEAVE limits.

      IF toimi = 5 THEN LEAVE task.

   END. /* task */

   IF(lCG) THEN DO:

      FOR EACH CGMember NO-LOCK WHERE
              CGMember.Brand = gcBrand AND
              LOOKUP(CGMember.CustGroup,lGroups) > 0.

         CREATE tCustNums.
         tCustNums.CustNum = CGMember.CustNum.

      END.

   END.

   RUN custrep1.p(INPUT CustNum1,
                INPUT CustNum2,
                INPUT ZipCode,
                INPUT Salesman,
                INPUT RateCust,
                INPUT lCG,
                INPUT TABLE tCustNums,
                INPUT InvGroup,
                INPUT Reseller,
                INPUT RatePlan,
                INPUT DiscPlan,
                INPUT Category
               ).


   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE limits.

END.   /* limits */

HIDE MESSAGE    NO-PAUSE.
HIDE FRAME crit NO-PAUSE.

