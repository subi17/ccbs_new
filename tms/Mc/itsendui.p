 /* ------------------------------------------------------
  MODULE .......: itsendui.p
  FUNCTION .....: ui for sending information texts via eMail
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.05.03
  MODIFIED .....: 12.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/tmsparam2.i}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR liCustNum1    AS INT  NO-UNDO.
DEF VAR liCustNum2    AS INT  NO-UNDO.
DEF VAR ldtDate1      AS DATE NO-UNDO.
DEF VAR ldtDate2      AS DATE NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liErrors      AS INT  NO-UNDO.
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR lcMemo        AS CHAR NO-UNDO.
DEF VAR llFirst       AS LOG  NO-UNDO.

DEF VAR llExtCustGrp  AS LOG  NO-UNDO FORMAT "Yes/No".
DEF VAR lcExtCustGrp  AS CHAR NO-UNDO FORMAT "x(40)".

DEF TEMP-TABLE ttCustGroup NO-UNDO
   FIELD CustGroup AS CHAR
   INDEX CustGroup CustGroup.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT. 

FORM
   skip(2)
   "Information texts will be sent via eMail to customers that meet"
      AT 10 SKIP
   "the criteria given below."
      AT 10 SKIP(1)

   lcInvGroup AT 10
      LABEL "Invoicing group ...."
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

   llExtCustGrp AT 10
      LABEL "Ext. customer groups" 
      HELP "Select external customer groups "

      lcExtCustGrp 
         NO-LABEL 
      SKIP

   liCustNum1 AT 10
      LABEL "Customers .........."
      HELP  "Minimum limit for customer numbers"
      FORMAT ">>>>>>>9"
   "-" AT 43
   liCustNum2 
      NO-LABEL 
      HELP  "Maximum limit for customer numbers"
      FORMAT ">>>>>>>>9"
      VALIDATE(INPUT liCustNum2 >= INPUT liCustNum1,
               "Upper limit cannot be less than lower limit")
      SKIP(1)

   ldtDate1 AT 10
      LABEL "Effective dates ...."
      HELP "Effective dates of texts to be sent"
      FORMAT "99-99-9999"
   "-" AT 43
   ldtDate2 
      NO-LABEL 
      HELP "Effective dates of texts to be sent"
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDate2 >= INPUT ldtDate1,
               "Upper limit cannot be less than lower limit")

   SKIP(7)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " SENDING OF INFORMATION TEXTS " +
              STRING(pvm,"99-99-99") + " "
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

ASSIGN liCustNum2    = 999999999
       ldtDate1      = TODAY
       ldtDate2      = TODAY
       llFirst       = TRUE.

DISPLAY
   "ALL" ;& InvGroup.IGName
   llExtCustGrp
   liCustNum1
   liCustNum2
   ldtDate1
   ldtDate2
WITH FRAME fCriter.


ufkey = FALSE.

toimi:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO toimi, NEXT toimi:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 7   ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 795 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 
         ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      IF llFirst THEN ASSIGN nap     = "1"  
                             llFirst = FALSE.
      ELSE DO:                             
         READKEY.
         ASSIGN nap = keylabel(LASTKEY).
      END. 

      IF LOOKUP(nap,"1,f1") > 0 THEN DO:

         repeat WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
             ehto = 9. RUN Syst/ufkey.p.
             UPDATE 
                lcInvGroup
                llExtCustGrp
                liCustNum1
                liCustNum2
                ldtDate1
                ldtDate2
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

                   ELSE IF FRAME-FIELD = "llextcustgrp" 
                   THEN DO WITH FRAME fCriter:

                      EMPTY TEMP-TABLE ttCustGroup.

                      lcExtCustGrp = "".

                      IF INPUT llExtCustGrp = TRUE THEN DO:

                         RUN Mc/gathecg.p(INPUT-OUTPUT table ttCustGroup).

                         ehto = 9.
                         RUN Syst/ufkey.p.

                         FOR EACH ttCustGroup:
                            lcExtCustGrp = lcExtCustGrp + 
                                           (IF lcExtCustGrp NE "" 
                                            THEN ","
                                            ELSE "") + 
                                           ttCustGroup.CustGroup.
                         END.

                      END.

                      IF lcExtCustGrp = "" 
                      THEN lcExtCustGrp = "NOT SELECTED".
                      DISPLAY lcExtCustGrp.                     

                      IF INPUT llExtCustGrp THEN APPLY 13.
                   END.

                END. 

                APPLY LASTKEY.                              
             END. 

             LEAVE.
         END.

         DISPLAY lcInvGroup
                 llExtCustGrp
                 lcExtCustGrp
                 liCustNum1
                 liCustNum2
                 ldtDate1
                 ldtDate2.

         ufkey = TRUE.
         NEXT toimi.
      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:

         llOk = FALSE. 

         MESSAGE "Start sending information texts ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.

         ehto = 5.
         RUN Syst/ufkey.p.

         /* collect customers */
         RUN Mc/itsendco.p (lcInvGroup,
                       INPUT TABLE ttCustGroup,
                       liCustNum1,
                       liCustNum2,
                       OUTPUT TABLE ttCust).

         /* send texts */
         RUN Mc/itsend.p   (INPUT TABLE ttCust,
                       ldtDate1,
                       ldtDate2,
                       OUTPUT liCount,
                       OUTPUT liErrors,
                       OUTPUT lcErrFile).

         MESSAGE liCount "information texts were sent to customers." SKIP
                 liErrors "errors occurred during process." 
                 (IF liErrors > 0 
                  THEN "Log of errors is in file " + lcErrFile + "."
                  ELSE "") 
         VIEW-AS ALERT-BOX 
         TITLE " Sending Status ".

         LEAVE toimi.

      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         LEAVE toimi.
      END.

END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

