/* -----------------------------------------------------
  MODULE .......: Cussubrep.p
  FUNCTION .....: Customer & Subscription report
  APPLICATION ..: MASTER
  AUTHOR .......: JP
  CREATED ......: 20.08.2002
                  17.09.02/aam RatePlan instead of PriceList on BillTarget
                  15.09.03 jp  Brand 
                  02.02.06/aam brand instead of custnum in FixedFee-find
  VERSION ......: M15
  MODIFIED .....: 
-------------------------------------------------------------------------- */

{commali.i}
{excel.i}
{mobsub1.i}

DEF VAR stname    AS  CHAR                       NO-undo.
DEF VAR invno      AS int  FORMAT "zzzzzzz9"     NO-UNDO.
DEF VAR ok         AS log  FORMAT "Yes/No"       NO-UNDO.
DEF VAR InvGroup    like invgroup.InvGroup         NO-UNDO.
DEF VAR CustNum1      AS int  FORMAT "zzzzzz9".
DEF VAR CustNum2      AS int  FORMAT "zzzzzz9".
DEF VAR path       AS CHAR                       NO-UNDO.
DEF VAR priceplan  AS Char no-undo.
DEF VAR discplan   AS Char no-undo.
DEF VAR ffnumber   AS CHAR No-UNDO.
DEF VAR addserv    AS CHAR NO-UNDO.
DEF VAR filename   AS CHAR NO-UNDO INIT "cust_subs_report.txt".

Find First TMSUser WHERE TMSUser.UserCode = katun no-lock no-error.
IF AVAIL TMSUser THEN 
   path = TMSUser.repdir + "/" + filename.
ELSE 
   Path = "/tmp/" + filename. 

FORm SKIP(1)
"This program will produce a report containing following "        SKIP
"information:" SKIP(1)
"From Each Customer:   "       "From Each MSISDN: "        AT 37  SKIP
" - Customer Number    "       " - MSISDN number  "        AT 37  SKIP
" - Organization number"       " - Activation date"        AT 37  SKIP
" - Price Plan         "       " - List of Addtl services" AT 37  SKIP
" - Discount Plan      "       " - Status in TMS  "        AT 37  SKIP
" - List of own group B-nos" SKIP(1)

   InvGroup LABEL  " Invoice group .........."
           HELP "Invoice group's code (* = ALL)"
   invgroup.IGName NO-LABEL FORMAT  "x(16)"  skip
   CustNum1   LABEL  " Customer number ........"       FORMAT "zzzzzzz9"
           HELP "Customers FROM number"
   "-"
   CustNum2   NO-LABEL HELP "Customers TO number"      FORMAT "zzzzzzz9" skip
   path    LABEL  " Path and Filename ......"  FORMAT "x(40)"

WITH title color value(ctc) " CRITERIA FOR CREATING CUSTOMER & SUBSCRIPTION REPORT " side-LABELs
   color value(cfc) row 4 centered overlay FRAME rajat.
ASSIGN
   CustNum1  = 1001
   CustNum2  = 99999999.

PAUSE 0.

toimi:
   REPEAT WITH FRAME valinta on ENDkey UNDO toimi, return:
         /* We ASk the limits */
         ehto = 9. RUN ufkey.
         UPDATE
             InvGroup
             CustNum1 CustNum2 VALIDATE(input CustNum2 >= input CustNum1,
             "Impossible !")
              path
         WITH FRAME rajat editing :
            readkey. nap = keyLABEL(lastkey).
            IF lookup(nap,poisnap) > 0 THEN do:
               HIDE MESSAGE NO-PAUSE.
               IF FRAME-field = "InvGroup" THEN do:
                  ASSIGN FRAME rajat InvGroup.
                  IF InvGroup = "" THEN do:
                     HIDE FRAME rajat NO-PAUSE.
                     HIDE FRAME invno NO-PAUSE.
                     HIDE FRAME main  NO-PAUSE.
                     return.
                  END.
                  ELSE IF InvGroup = "*" THEN DO:
                     DISP "ALL" @ invgroup.IGName WITH FRAME rajat.

                  END.
                  ELSE do: 
                     FIND invgroup WHERE 
                          InvGroup.Brand    = gcBrand AND 
                          invgroup.InvGroup = InvGroup
                     NO-LOCK NO-ERROR.
                     IF not avail invgroup THEN do:
                        bell.  MESSAGE "Unknown invoice group !".
                        NEXT.
                     END.
                     DISP invgroup.IGName WITH FRAME rajat.
                     PAUSE 0.
                  END.
               END.
            END.
            apply lastkey.
      END.


      ASSIGN ufk = 0 ufk[1] = 0 ufk[2] = 0   ufk[4] = 0 ufk[5] = 795
                     ufk[8] = 8 ehto = 0.
      RUN ufkey.

      IF toimi = 5 THEN do:
         leave toimi.
      END.

      IF toimi = 8 THEN do:
         HIDE MESSAGE NO-PAUSE.
         HIDE FRAME rajat NO-PAUSE.
         HIDE FRAME main NO-PAUSE.
         return.
      END.
   END. /* toimi */

   /* REPORT FILE INFORMATION */
   OUTPUT STREAM EXCEL TO VALUE(PATH).

   PUT STREAM EXCEL UNFORMATTED
   "REPORT FILE INFORMATION"                          MY-nl         my-nl
   "DATE OF CREATION:       " + STRING(today,"99-99-9999")            MY-NL
   "CUSTOMER RANGE DEFINED: " + STRING(CustNum1) + " - " + 
                                String(CustNum2) MY-NL
   "INVOICE GROUP DEFINED:  " + IF   InvGroup = "*" THEN "ALL" 
                              ELSE invgroup.IGName                 MY-NL. 

   /***** CUSTOMER SECTION ****/

   FOR EACH Customer WHERE
            Customer.Brand     = gcBrand  AND 
            Customer.CustNum  >= CustNum1 AND
            Customer.CustNum  <= CustNum2 AND
            (IF   InvGroup NE "*" THEN 
             Customer.InvGroup = InvGroup ELSE TRUE) NO-LOCK.

      ASSIGN
      priceplan = ""
      discplan  = ""
      ffnumber  = "".

      For each BillTarg of Customer no-lock
      break 
      by BillTarg.RatePlan.
         IF first-of(BillTarg.RatePlan) THEN 
         priceplan = priceplan + BillTarg.RatePlan + " ". 
      end.

      For each BillTarg of Customer no-lock
      break 
      by BillTarg.DiscPlan.
         IF first-of(BillTarg.DiscPlan) THEN 
         discplan = discplan + BillTarg.DiscPlan + " ". 
      end.

      PUT STREAM EXCEL UNFORMATTED
      "*****"                              my-nl
      "Customer:"            TAB  Customer.CustNum TAB Customer.CustName My-nl
      "Price Plan:"          TAB  priceplan      MY-NL
      "Discount Plan:"       TAB  discplan       MY-NL.

      /* MOBILE SUBSCRIPTION SECTION */
      FOR EACH mobsub WHERE
               mobsub.CustNum = Customer.CustNum NO-LOCK.

         Addserv = "".      

         FOR EACH FixedFee  where 
                  Fixedfee.Brand     = gcBrand       AND
                  FixedFee.Hosttable = "mobsub"      AND 
                  FixedFee.Keyvalue  = STRING(mobsub.msseq) NO-LOCK.

            Find first BillItem where 
                       BillItem.Brand    = gcBrand AND 
                       BillItem.BillCode = FixedFee.BillCode 
            no-lock no-error.
            addserv = addserv + BillItem.BIName + ",".
         END.         

         PUT STREAM excel UNFORMATTED
         "*"                                       MY-NL
         "MSISDN:"            TAB mobsub.CLI   TAB 
         DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer)
         MY-NL
         
         "Activation date"    TAB mobsub.ActivationDate 
             FORMAT "99-99-9999"  MY-NL
         "Additional Service" TAB Addserv          MY-NL
         "Status:"            TAB  entry(mobsub.msstatus + 1, stnames) MY-nl.
      END.
   END.
   PUT STREAM EXCEL UNFORMATTED
   "*** END OF REPORT ***".
   OUTPUT STREAM excel CLOSE.
   MESSAGE 
   "File for customer and subscription report created." SKIP
   "File located: " path
   VIEW-AS ALERT-BOX TITLE "".

HIDE MESSAGE NO-PAUSE.
HIDE FRAME rajat NO-PAUSE.

