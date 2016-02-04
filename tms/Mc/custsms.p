/* ===========================================================================
 MODULE ........: custsms.p
 APPLICATION ...: TMS
 TASK ..........: Send sms to customer
 CREATED .......: 19.01.06/aam (from mobsubsms.p)
 CHANGED .......: 
 Version .......: M15
 ============================================================================*/

{Syst/commali.i}
{Func/timestamp.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF SHARED variable siirto AS CHAR .

DEF VAR ok         AS LOG  NO-UNDO FORMAT "Yes/No".
DEF VAR Name       AS CHAR NO-UNDO.
DEF VAR lcText     AS CHAR NO-UNDO FORMAT "x(160)".
DEF VAR lcUpdText  AS CHAR NO-UNDO EXTENT 4.
DEF VAR liQty      AS INT  NO-UNDO.
DEF VAR lcKeyvalue AS CHAR NO-UNDO.
DEF VAR lcCustName AS CHAR NO-UNDO.
DEF VAR lcCLI      AS CHAR NO-UNDO.

FORM 
   SKIP(1)

   "With this program you can send an SMS message (160 characters)" 
      AT 5 SKIP
   "to customer" AT 5
      lcCustName FORMAT "X(40)"                         
      SKIP(1)
   
   lcCLI  AT 5 
      LABEL "Send To" 
      HELP "Send SMS to this MSISDN" 
      SKIP(1)
      
"  Choose a pre-defined SMS text?" lcKeyValue                SKIP
"          " lcUpdText[1]  AUTO-RETURN                         SKIP
"          " lcUpdText[2]  AUTO-RETURN                         SKIP 
"          " lcUpdText[3]  AUTO-RETURN                         SKIP 
"          " lcUpdText[4]  AUTO-RETURN                         SKIP(2) 

WITH  OVERLAY ROW 1 centered width 80
    COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) " SEND SMS "   
    NO-LABELS FRAME main.

PAUSE 0.

FIND Customer WHERE 
     Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Unknown customer"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, 
                              BUFFER Customer).

MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN Syst/ufkey.

               
UPDATE
lckeyvalue WHEN lcUpdText[1] = ""
lcUpdText[1]
lcUpdText[2]
lcUpdText[3]
lcUpdText[4]

WITH FRAME main EDITING:
             READKEY.

               IF FRAME-FIELD = "lckeyvalue" AND  
                  LOOKUP(KEYLABEL(LASTKEY),"F9") > 0 THEN DO:
                  RUN Help/h-invotxt(INPUT "SMS","", 0).
                  If siirto ne ? THEN ASSIGN
                     
                     lcUpdText[1] = substring(siirto,  1,40)
                     lcUpdText[2] = substring(siirto, 41,40)
                     lcUpdText[3] = substring(siirto, 81,40)
                     lcUpdText[4] = substring(siirto,121,40).

                  DISP lcUpdText WITH FRAME main.
                  liqty = 160.
                  LEAVE.
               END.
               ELSE IF FRAME-FIELD = "lckeyvalue" THEN DO:
                   ASSIGN  liqty = liqty - 40.
               END.
               ELSE IF LOOKUP(KEYLABEL(LASTKEY),"RETURN,CURSOR-DOWN") > 0 
               THEN liqty = liqty + 40.
               ELSE IF LOOKUP(KEYLABEL(LASTKEY),"CURSOR-UP") > 0
               THEN liqty = liqty - 40.
               ELSE IF LOOKUP(KEYLABEL(LASTKEY),"CURSOR-LEFT,BACKSPACE") > 0
               THEN liqty = liqty - 1.
               ELSE IF LOOKUP(KEYLABEL(LASTKEY),"DELETE") > 0
               THEN liqty = liqty - 0.
               ELSE liqty = liQty + 1.

               If liqty < 0 then liqty  = 0.
               if liqty > 160 THEN liqty = 160.

               PUT SCREEN ROW 1 COL 2 STRING(liqty) + "/160          ".

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
                PAUSE 0.
                  IF FRAME-FIELD = "lckeyvalue" THEN DO:
                  END.
               END.
               APPLY LASTKEY.
        END. /* EDITING */


ACTION:
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 2355
      ufk[8] = 8.
      RUN Syst/ufkey.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:

         ok = FALSE.
         
         lcText = lcUpdText[1] + lcUpdText[2] + lcUpdText[3] + lcUpdText[4].
          
         IF LENGTH(lctext) = 0 THEN DO:
            MESSAGE
            "Content of message was empty, message will not be sent!"
            VIEW-AS ALERT-BOX.
            LEAVE.
         END.
         
         MESSAGE 
         "Do you really want to send following message?"     SKIP(1)
         lcUpdText[1]                                             SKIP
         lcUpdText[2]                                             SKIP
         lcUpdText[3]                                             SKIP
         lcUpdText[4]
         VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

         IF NOT ok THEN NEXT Action.

         CREATE CallAlarm.
         CallAlarm.ActStamp = fmakets().
         ASSIGN
            CallAlarm.CLSeq    = 0
            CallAlarm.CASeq    = NEXT-VALUE(CallAlarm)
            CallAlarm.CustNo   = Customer.CustNum 
            CallAlarm.CLI      = lcCLI
            CallAlarm.DeliStat = 1            
            CallAlarm.Delitype = 1  /* info */
            CallAlarm.DeliPara = "1"
            CallAlarm.DeliMsg  = lcText
            CallAlarm.Limit    = 0
            CallAlarm.CreditType = 9
            CallAlarm.Brand      = gcBrand .

         LEAVE Action.
      END.
   END. /* Action */      

   PUT SCREEN ROW 1 "                                                  ".

   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

