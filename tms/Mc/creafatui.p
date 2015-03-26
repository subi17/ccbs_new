 /* ------------------------------------------------------
  MODULE .......: creafatui.p
  FUNCTION .....: ui for adding customer into a fat group
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.02.04
  MODIFIED .....: 27.04.06/aam FatGroup.Amount
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{fcustdata.i}
{fixedfee.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 
DEF INPUT PARAMETER iiMsSeq   AS INT NO-UNDO. 

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR lcFatGroup    AS CHAR NO-UNDO. 
DEF VAR liFromPer     AS INT  NO-UNDO. 
DEF VAR ldAmt         AS DEC  NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCLI         AS CHAR NO-UNDO. 
DEF VAR liSeq         AS INT  NO-UNDO. 
DEF VAR lcRoles       AS CHAR NO-UNDO.
DEF VAR ldtChkDate    AS DATE NO-UNDO.

FORM
   SKIP(2)
   "Customer / subscription will be granted the given amount's worth"
      AT 5 SKIP
   "of FAT on the chosen FAT group."
      AT 5 SKIP(1)

   liCustNum  AT 5
      LABEL "Customer .."
      HELP  "Customer number" 
      FORMAT ">>>>>>>>>9"
   Customer.CustName
      NO-LABEL
   SKIP

   lcCLI AT 5
      LABEL "MSISDN ...."
      HELP "Subscription number"
      FORMAT "X(16)"
   SKIP
   
   lcFatGroup AT 5
      LABEL "FAT Group ."
      HELP  "FAT group"
      FORMAT "X(16)"
   FatGroup.FtgName 
      NO-LABEL
      SKIP

   ldAmt AT 5
      LABEL "Amount ...."
      HELP  "FAT Amount"
      FORMAT "->>>>>9.99"
      SKIP
      
   liFromPer AT 5
      LABEL "From Period"
      HELP  "First period when FAT can be used (YYYYMM)"
      FORMAT "999999"
      SKIP
   SKIP(1)
   WITH ROW 4 OVERLAY SIDE-LABELS CENTERED 
        TITLE " " + ynimi + "  GRANT FAT  " + STRING(pvm,"99-99-99") + " "
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

ASSIGN liCustNum = iiCustNum
       liFromPer = YEAR(TODAY) * 100 + MONTH(TODAY)
       ufkey     = FALSE.

IF iiMsSeq > 0 THEN DO:

   FOR FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
             MsOwner.MsSeq   = iiMsSeq AND
             MsOwner.CustNum = iiCustNum:
      ASSIGN lcCLI = MsOwner.CLI
             liSeq = iiMsSeq.
   END.
END.

toimi:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO toimi, NEXT toimi:

      FIND Customer NO-LOCK WHERE
           Customer.Brand   = gcBrand AND
           Customer.CustNum = liCustNum NO-ERROR.

      FIND FatGroup NO-LOCK WHERE
           FatGroup.Brand = gcBrand AND
           FatGroup.FtGrp = lcFatGroup NO-ERROR.
 
      /* if amount is defined on the group, it cannot be overriden here */
      IF AVAILABLE FatGroup THEN DO:
         IF FatGroup.Amount > 0 THEN ldAmt = FatGroup.Amount.
         IF FatGroup.FatPerc > 0 THEN ldAmt = 0.
      END. 
         
      
      DISPLAY
      liCustNum
      Customer.CustName WHEN AVAILABLE Customer
      lcCLI
      lcFatGroup
      FatGroup.FtgName WHEN AVAILABLE FatGroup
      ldAmt
      liFromPer
      WITH FRAME fCriter.

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 7   ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 795 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3.
         RUN ufkey.

         READKEY.
         nap = keylabel(LASTKEY).
      END.

      ELSE ASSIGN nap   = "1"  
                  ufkey = TRUE.

      IF LOOKUP(nap,"1,f1") > 0 THEN DO:

         repeat WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
             ehto = 9. RUN ufkey.
             UPDATE 
                liCustNum WHEN iiCustNum = 0
                lcCLI     WHEN iiMsSeq   = 0
                lcFatGroup
                ldAmt
                liFromPer
             WITH FRAME fCriter EDITING:
                READKEY.
                
                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
                  
                  IF FRAME-FIELD = "liCustNum" THEN DO:
                     IF INPUT liCustNum > 0 THEN DO:
                     
                        FIND Customer WHERE 
                             Customer.CustNum = INPUT liCustNum
                        NO-LOCK NO-ERROR.
                        IF NOT AVAIL Customer OR Customer.Brand NE gcBrand
                        THEN DO:
                           MESSAGE "Unknown Customer"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.
                        ELSE DISP Customer.CustName WITH FRAME fCriter.
                     END.
                  END.

                  ELSE IF FRAME-FIELD = "lcCLI" THEN DO:
                     IF INPUT lcCLI > "" THEN DO:
                      
                        /* check if cli is defined to chosen customer */
                        liSeq = 0.
                        FOR FIRST MsOwner NO-LOCK WHERE
                                  MsOwner.Brand   = gcBrand         AND
                                  MsOwner.CustNum = INPUT liCustNum AND
                                  MsOwner.CLI     = INPUT lcCLI:
                           liSeq = MsOwner.MsSeq.
                        END.
                        
                        IF liSeq = 0 THEN DO:
                           MESSAGE "MSISDN doesn't belong to this customer"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.
                        
                     END.
                  END.
                  
                  ELSE IF FRAME-FIELD = "lcFatGroup" THEN DO:
                     IF INPUT lcFatGroup > "" THEN DO:
                        FIND FatGroup WHERE
                             FatGroup.Brand = gcBrand AND
                             FatGroup.FtGrp = INPUT lcFatGroup 
                        NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE FatGroup THEN DO:
                           MESSAGE "Unknown FAT group"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.
                        DISPLAY FatGroup.FtgName WITH FRAME fCriter.
                        
                        IF FatGroup.Amount > 0 THEN DO:
                           ldAmt = FatGroup.Amount.
                           DISPLAY ldAmt WITH FRAME fCriter.
                        END.
                        IF FatGroup.FatPerc > 0 THEN DO:
                           ldAmt = 0.
                           DISPLAY ldAmt WITH FRAME fCriter.
                           NEXT-PROMPT liFromPer.
                           NEXT. 
                        END.
                     END.
                     ELSE DISPLAY "" @ FatGroup.FtgName WITH FRAME fCriter.
                  END.

                END. 

                APPLY LASTKEY.                              
             END. 

             LEAVE.
         END.

      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:

         IF lcFatGroup = ""        OR 
            liFromPer = 0          OR
            NOT AVAILABLE FatGroup OR
            NOT AVAILABLE Customer
         THEN DO:
            MESSAGE "All parameters for creation have not been defined"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         /* if type is "calls" or target is subscr. then cli is mandatory */
         IF lcCLI = "" AND 
            (FatGroup.FatType = 0 OR FatGroup.FatTarget = "0") 
         THEN DO: 
            MESSAGE "MSISDN is mandatory with this FAT group"
            VIEW-AS ALERT-BOX ERROR.
            NEXT toimi.
         END.

         ldtChkDate = fInt2Date(liFromPer,1).
         IF ldtChkDate = ? THEN DO:
            MESSAGE "Invalid period"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
   
         IF FatGroup.FATTarget > "0" THEN DO:
   
            /* if target is a customer then cli is not allowed */
            IF lcCLI > "" THEN DO:
               MESSAGE "This FAT is defined to be used on customer level"
               VIEW-AS ALERT-BOX ERROR.
               NEXT toimi.
            END.

            /* is customer in correct role */
            lcRoles = fCustRoles(BUFFER Customer).
            
            IF FatGroup.FATTarget = "1" AND SUBSTRING(lcRoles,1,1) NE "1"
            THEN DO:
               MESSAGE "This FAT is defined to be used for agreement customers"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            IF FatGroup.FATTarget = "2" AND SUBSTRING(lcRoles,2,1) NE "1"
            THEN DO:
               MESSAGE "This FAT is defined to be used for invoice customers"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
         
            IF FatGroup.FATTarget = "3" AND SUBSTRING(lcRoles,3,1) NE "1"
            THEN DO:
               MESSAGE "This FAT is defined to be used for user customers"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
         END.
         
         llOk = FALSE. 

         MESSAGE "Add FATime event(s)?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.

         ehto = 5.   
         RUN ufkey.

         RUN creafat (liCustNum,
                      liSeq,
                      lcFatGroup,
                      ldAmt,
                      0,
                      ?,
                      liFromPer,
                      999999,
                      OUTPUT lcError).

         IF lcError > "" THEN
         MESSAGE "FATime event(s) could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.
         
         ELSE 
         MESSAGE "FATime event(s) created" 
         VIEW-AS ALERT-BOX 
         TITLE " DONE ".

         LEAVE toimi.

      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         LEAVE toimi.
      END.

END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

