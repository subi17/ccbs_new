 /* ------------------------------------------------------
  MODULE .......: camprunui.p
  FUNCTION .....: ui for campaign run
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.02.04
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR lcCampaign1   AS CHAR NO-UNDO. 
DEF VAR lcCampaign2   AS CHAR NO-UNDO. 
DEF VAR liCustNum1    AS INT  NO-UNDO.
DEF VAR liCustNum2    AS INT  NO-UNDO.
DEF VAR ldtDate1      AS DATE NO-UNDO.
DEF VAR ldtDate2      AS DATE NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liErrors      AS INT  NO-UNDO.
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR llOk          AS LOG  NO-UNDO.

{camprundf.i}

FORM
   SKIP(4)
   "Campaign events will be set to customers / CLIs which"
      AT 10 SKIP
   "meet the campaigns' criteria."
      AT 10 
   SKIP(1)

   lcCampaign1 AT 10
      LABEL "Campaigns ......"
      HELP  "Minimum limit for campaign IDs"
      FORMAT "X(12)"
   "-" AT 41
   lcCampaign2 
      NO-LABEL 
      HELP  "Maximum limit for campaign IDs"
      FORMAT "X(12)"
      VALIDATE(INPUT lcCampaign2 >= INPUT lcCampaign1,
               "Upper limit cannot be less than lower limit")
      SKIP


   liCustNum1 AT 10
      LABEL "Customers ......"
      HELP  "Minimum limit for customer numbers"
      FORMAT ">>>>>>>9"
   "-" AT 41
   liCustNum2 
      NO-LABEL 
      HELP  "Maximum limit for customer numbers"
      FORMAT ">>>>>>>9"
      VALIDATE(INPUT liCustNum2 >= INPUT liCustNum1,
               "Upper limit cannot be less than lower limit")
      SKIP

   ldtDate1 AT 10
      LABEL "Activation dates"
      HELP "Activation dates of new CLIs"
      FORMAT "99-99-9999"
   "-" AT 41
   ldtDate2 
      NO-LABEL 
      HELP "Activation dates of new CLIs"
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate2 >= INPUT ldtDate1,
               "Upper limit cannot be less than lower limit")

   SKIP(7)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " CAMPAIGN RUN " +
              STRING(pvm,"99-99-99") + " "
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

ASSIGN liCustNum2    = 99999999
       ldtDate1      = TODAY
       ldtDate2      = TODAY.

FIND LAST Campaign NO-LOCK WHERE Campaign.Brand = gcBrand NO-ERROR.
IF AVAILABLE Campaign THEN lcCampaign2 = Campaign.Campaign.


ufkey = FALSE.

toimi:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO toimi, NEXT toimi:

      DISPLAY
      lcCampaign1 
      lcCampaign2
      liCustNum1
      liCustNum2
      ldtDate1
      ldtDate2
      WITH FRAME fCriter.

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 7   ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 795 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3.
         RUN ufkey.p.

         READKEY.
         nap = keylabel(LASTKEY).
      END.

      ELSE ASSIGN nap   = "1"  
                  ufkey = TRUE.

      IF LOOKUP(nap,"1,f1") > 0 THEN DO:

         repeat WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
             ehto = 9. RUN ufkey.p.
             UPDATE 
                lcCampaign1 
                lcCampaign2
                liCustNum1
                liCustNum2
                ldtDate1
                ldtDate2
             WITH FRAME fCriter EDITING:
                READKEY.

                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:

                END. 

                APPLY LASTKEY.                              
             END. 

             LEAVE.
         END.

      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:

         llOk = FALSE. 

         MESSAGE "Start setting campaign events ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.

         ehto = 5.
         RUN ufkey.

         /* collect customers */
         RUN camprunco (liCustNum1,
                        liCustNum2,
                        ldtDate1,
                        ldtDate2,
                        OUTPUT TABLE ttCust).
                          
         RUN camprun (INPUT TABLE ttCust,
                      lcCampaign1,
                      lcCampaign2,
                      0,       /* use cli periods */
                      TRUE,    /* disp messages */
                      OUTPUT liCount).

         MESSAGE liCount "campaign events were set." SKIP
         VIEW-AS ALERT-BOX 
         TITLE " Campaign Run Finished ".

         LEAVE toimi.

      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         LEAVE toimi.
      END.

END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

