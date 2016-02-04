/* ===========================================================================
 MODULE ........: Send infor sms to subscriptions
 APPLICATION ...: 
 TASK ..........:
 CREATED .......: 23.01.04 jp
 CHANGED .......: 
 Version .......: M15
 ============================================================================*/

{Syst/commali.i}
{Func/timestamp.i}

DEF INPUT PARAMETER  iiMSSeq AS INT    NO-UNDO.

DEF  SHARED variable siirto as c .

DEF VAR ok        AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR Name      AS C                NO-UNDO.
DEF VAR Stock   LIKE Stock.Stock      NO-UNDO.
DEF VAR lctext    AS C                NO-UNDO FORMAT "x(160)".
DEF VAR updText   AS C  EXTENT 4      NO-UNDO FORMAT "X(40)".
DEF VAR username  as c                no-undo format "x(40)".
DEF VAR liQty     AS INT              NO-UNDO.
DEF VAR lcKeyvalue AS c               NO-UNDO.
DEF VAR lcUserName AS C               NO-UNDO.

FIND FIRST mobsub WHERE 
           mobsub.msseq = iiMSSeq no-lock no-error.

FIND Customer WHERE 
     Customer.CustNum = mobsub.CustNum NO-LOCK NO-ERROR.

ASSIGN 
   lcUserName = DYNAMIC-FUNCTION("fDispCustName" IN
                ghFunc1, BUFFER Customer).
form 
skip(1)

"  NOTE: With this program you can send SMS message (160 character)" SKIP
"  to subscription number " mobsub.cli  username             SKIP(1)
   liqty AT 50 "/160" SKIP
"  Choose a pre-defined SMS text?" lckeyvalue                SKIP
"          " updText[1]  Auto-RETURN                         SKIP
"          " updText[2]  AUTO-RETURN                         SKIP 
"          " updText[3]  AUTO-RETURN                         SKIP 
"          " updText[4]  AUTO-RETURN                         SKIP(2) 

WITH  OVERLAY ROW 1 centered width 80
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " SMS INFORMATION FOR  " + mobsub.cli  
    NO-LABELS FRAME main.

PAUSE 0.

ASSIGN
updText[1] = substring(lctext,1,40)
updText[2] = substring(lctext,41,40)
updText[3] = substring(lctext,81,40)
updText[4] = substring(lctext,121,40)
liqty = 1.
DISP liqty WITH FRAME main.

MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN Syst/ufkey.
   username = "(" + lcUserName  + ").".

disp mobsub.cli   username .
               
UPDATE
lckeyvalue WHEN updText[1] = ""
updText[1]
updText[2]
updText[3]
updText[4]

WITH FRAME main EDITING:
             READKEY.

               IF FRAME-FIELD = "lckeyvalue" AND  
                  LOOKUP(KEYLABEL(LASTKEY),"F9") > 0 THEN DO:
                  RUN Help/h-invotxt(INPUT "SMS","", mobsub.msseq).
                  If siirto ne ? THEN ASSIGN
                     
                     updtext[1] = substring(siirto,  1,40)
                     updtext[2] = substring(siirto, 41,40)
                     updtext[3] = substring(siirto, 81,40)
                     updtext[4] = substring(siirto,121,40).

                  DISP updtext WITH FRAME main.
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

               DISP liqty WITH FRAME main.

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
                PAUSE 0.
                  IF FRAME-FIELD = "lckeyvalue" THEN DO:
                  END.
               END.
               APPLY LASTKEY.
        END. /* EDITING */

         lcText = updText[1] + updText[2] + updText[3] + updText[4].
         liqty = LENGTH(lcText).
         DISP liqty WITH FRAME main.           

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
         
         lcText = updText[1] + updText[2] + updText[3] + updText[4].
          
         IF LENGTH(lctext) = 0 THEN DO:
            MESSAGE
            "Content of message was empty, message will not be sent!"
            VIEW-AS ALERT-BOX.
            LEAVE.
         END.
         
         MESSAGE 
         "Do you really want to send following message?"     SKIP(1)
         updText[1]                                             SKIP
         updText[2]                                             SKIP
         updText[3]                                             SKIP
         updText[4]
         VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

         IF NOT ok THEN NEXT Action.

         CREATE CallAlarm.
         CallAlarm.ActStamp = fmakets().
         ASSIGN
            CallAlarm.CLSeq    = 0
            CallAlarm.CASeq    = NEXT-VALUE(CallAlarm)
            CallAlarm.CustNo   = Customer.CustNum 
            CallAlarm.CLI      = Mobsub.cli
            CallAlarm.DeliStat = 1            
            CallAlarm.Delitype = 1  /* info */
            CallAlarm.DeliPara = "1"
            CallAlarm.DeliMsg  = lcText
            CallAlarm.Limit    = 0
            CallAlarm.CreditType = 9
            CallAlarm.Orig       = "800622800"
            CallAlarm.Brand      = gcBrand .

         LEAVE Action.
      END.
   END. /* Action */      


   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.
