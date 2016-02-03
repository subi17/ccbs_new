/* ------------------------------------------------------
  MODULE .......: CONREPUI
  FUNCTION .....: UI for contact report
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 03.05.04/aam 
  VERSION ......: M15
  ------------------------------------------------------ */


{Syst/commali.i}
{Syst/utumaa.i "new"}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'contact'}

assign tuni1 = "conrep"
       tuni2 = "".

DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO.
DEF INPUT PARAMETER idtConDate AS DATE NO-UNDO. 

DEF VAR llUfkey      AS LOG                     NO-UNDO.
DEF VAR lcFile       AS CHAR  FORMAT "X(40)"    NO-UNDO.
DEF VAR liCustNum    AS INT   FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR liState      AS INT   FORMAT "9"        NO-UNDO. 
DEF VAR liCount      AS INT                     NO-UNDO. 


FORM 
   SKIP(2)
   "This program prints out a report of contact list." AT 10 
   SKIP(2)

   icUserCode AT 10
      LABEL "User ......."
      HELP "User whose contacts are printed" 
      FORMAT "X(12)"
   TMSUser.UserName 
      NO-LABEL 
      SKIP
   
   idtConDate AT 10
      LABEL "Contact Date"
      HELP "Date when contact was planned to be taken or was taken"
      FORMAT "99-99-99"
      SKIP   
         
   liCustNum AT 10
      LABEL "Customer ..."
      HELP  "Customer whose contacts are printed"
      FORMAT ">>>>>>>>"
   Customer.CustName 
      NO-LABEL 
      SKIP

   liState AT 10
      LABEL "Status ....."
      HELP "0=unhandled, 1=handled, 2=all"
      VALIDATE(INPUT liState <= 2,
               "Valid values are 0-2")
      FORMAT "9"

   SKIP(1)
   lcFile AT 10
      LABEL "File Name .."
      HELP "If file name is given, a tab separated file is printed"
      FORMAT "X(50)"
   SKIP(6)

   WITH ROW 1 SIDE-LABELS WIDTH 80 OVERLAY
        TITLE " " + ynimi + " CONTACT REPORT " +
        STRING(pvm,"99-99-99") + " "
        FRAME valinta.

PAUSE 0.
VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

ASSIGN llUfkey = FALSE
       nap     = "1".
       
IF icUserCode = "" THEN icUserCode = katun.
IF idtConDate = ? THEN idtConDate = TODAY.

toimi:
REPEAT WITH FRAME valinta on ENDkey undo toimi, NEXT toimi:

   PAUSE 0. 
   DISPLAY icUserCode
           idtConDate
           liCustNum
           liState
           lcFile.
           
   FIND TMSUser WHERE TMSUser.UserCode = icUserCode NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser THEN DISPLAY TMSUser.UserName.
   
   IF liCustNum > 0 THEN DO:
      FIND Customer WHERE 
           Customer.Brand   = gcBrand AND
           Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN DISPLAY Customer.CustName.
   END.
   ELSE DISPLAY "All" @ Customer.CustName.

   if llUfkey THEN DO:

      ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0 
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3.

      RUN ufkey.

      READKEY.
      nap = KEYLABEL(LASTKEY).

   END.
   ELSE llUfkey = TRUE.

   
   IF LOOKUP(nap,"1,f1") > 0 THEN DO:

      ehto = 9. 
      RUN ufkey.p.

      REPEAT WITH FRAME valinta ON ENDKEY UNDO, LEAVE:

         UPDATE icUserCode
                idtConDate
                liCustNum
                liState
                lcFile
         WITH FRAME valinta EDITING:

            READKEY.

            IF LOOKUP(keylabel(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME valinta:

               HIDE MESSAGE.

               IF FRAME-FIELD = "icUserCode" THEN DO:
                  FIND TMSUser WHERE TMSUser.UserCode = INPUT icUserCode
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TMSUser THEN DO:
                     BELL.
                     MESSAGE "Unknown user".
                     NEXT.
                  END.
                  DISPLAY TMSUser.UserName WITH FRAME valinta.
               END.
               
               IF FRAME-FIELD = "liCustNum" THEN DO:
                  IF INPUT liCustNum > 0 THEN DO:
                     FIND Customer WHERE 
                          Customer.Brand   = gcBrand AND
                          Customer.CustNum = INPUT liCustNum 
                     NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE Customer THEN DO:
                        BELL.
                        MESSAGE "Unknown customer".
                        NEXT.
                     END.
                     DISPLAY Customer.CustName WITH FRAME valinta.
                  END.
                  ELSE DISPLAY "All" @ Customer.CustName.
               END.

            END.

            APPLY LASTKEY.

         END.

         LEAVE. 

      END.

   END.

   ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:

      ehto = 5.
      RUN ufkey.

      IF lcFile = "" THEN DO:
         assign tila = true.
         {Syst/utuloste.i "return"}
      END.

      RUN conrep (icUserCode,
                  idtConDate,
                  liCustNum,
                  liState,
                  lcFile,
                  OUTPUT liCount).

      IF lcFile = "" THEN DO:
         assign tila = false.
         {Syst/utuloste.i}
      END.

      MESSAGE liCount "events were printed on contact report." 
      VIEW-AS ALERT-BOX
      TITLE " Done ".
      
      LEAVE toimi.
   END.

   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.

END. /* toimi */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.    

