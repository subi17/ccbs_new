{commpaa.i}
gcBrand = "1".

{fmakesms.i}

DEFINE VARIABLE liCount       AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liSubCount    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcPostPaidMsg AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSender      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llgTime       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liMobSubCount AS INTEGER   NO-UNDO. 

DEFINE STREAM strout. 

lcPostPaidMsg = "Hola, hay novedades en Las Condiciones Generales a partir del 1 octubre. Si quieres puedes consultarlas en yoigo.com/informacion-legal." + "|" + 
             "Como sabes, en Yoigo si hay algo que no te convence puedes pedir la baja sin penalizacion si no tienes permanencia por la compra del movil. Saludos!".

ASSIGN lcSender = "Yoigo info"
       llgTime  = FALSE.

output stream strout to "ydr_2297_22_08_2016_log.txt".

put stream strout unformatted 
    "MSISDN"     "|"
    "SMS Text"   "|"
    "Time Stamp" SKIP.

RUN pSendSMS no-error.

if error-status:error then 
   put stream strout unformatted 
       "Error in sending sms " SKIP.

output stream strout close.

PROCEDURE pSendSMS:
DEFINE VARIABLE ldTimeStamp AS DECIMAL NO-UNDO.

   FOR EACH MobSub NO-LOCK WHERE 
            MobSub.Brand   = gcBrand AND 
            MobSub.PayType = FALSE:

      FIND FIRST Customer NO-LOCK where 
                 Customer.Brand   = gcBrand        AND 
                 Customer.CustNum = MobSub.CustNum NO-ERROR.

      IF NOT AVAIL Customer THEN NEXT.

      IF Customer.DelType NE 11 AND 
         Customer.DelType NE 12 AND 
         Customer.DelType NE 13 THEN NEXT. 

      IF TIME <= 32400 OR 
         TIME >= 79200 THEN
         llgTime = TRUE. 

      IF llgTime THEN DO:
         REPEAT:
            PAUSE 5. 

            IF TIME >= 32400 AND 
               TIME <= 79200 THEN DO:
               llgTime = FALSE.
               LEAVE.
            END.

         END.
      END.

      IF liMobSubCount MOD 13 EQ 0 THEN
         PAUSE 1.

      IF NOT llgTime THEN DO:
         DO liCount = 1 to NUM-ENTRIES(lcPostPaidMsg,"|"):
             
            assign 
               ldTimeStamp = 0
               ldTimeStamp = fmakets(). 

            fMakeSchedSMS2(MobSub.CustNum,
                           MobSub.CLI,
                           9,
                           ENTRY(liCount,lcPostPaidMsg,"|"),
                           ldTimeStamp,
                           lcSender,
                           "").  

            put stream strout unformatted 
               MobSub.CLI                        "|"
               ENTRY(liCount,lcPostPaidMsg,"|")  "|"
               ldTimeStamp                       SKIP. 
         END.                 

         liMobSubCount = liMobSubCount + 1.

         IF liMobSubCount MOD 10 EQ 0 THEN DO:
            DISPLAY liMobSubCount. 
            PAUSE 0.
         END.

      END.
   END.          

END PROCEDURE.

