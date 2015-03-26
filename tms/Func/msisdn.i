/* msisdn.i  - sets the default MSISDN prefix 

   changes:  24.02.05/aam check number range for dcf and change it when full
             02.03.05/ jp previous finalized  

*/

{commali.i}
{timestamp.i}

DEF VAR m_pref  AS C NO-UNDO FORMAT "x(3)".

DEF VAR lcGSVoice   AS   CHAR NO-UNDO INIT "0468". 
DEF VAR lcGSData    AS   CHAR NO-UNDO INIT "0406".
DEF VAR lcGSFax     AS   CHAR NO-UNDO INIT "0406".
DEF VAR lcGSDcf     AS   CHAR NO-UNDO.

{tmsparam.i DefMSISDNPr return}.  m_pref  = TMSParam.CharVal.
{tmsparam.i DCFPrefix   return}.  lcGSDcf = TMSParam.CharVal.

FUNCTION fSearchGenServNumber RETURNS CHAR
   (INPUT   VoiceCli   AS CHAR ,
    INPUT   crit       AS CHAR). 

   DEF VAR occli   as CHAR NO-UNDO.
   DEF VAR liCLI   AS I    NO-UNDO .

   IF  crit = "DATA" THEN  DO:

      Find first tmsparam where
                 TmsParam.Brand      = gcBrand        AND 
                 tmsparam.paramgroup = "subser"       AND
                 tmsparam.paramcode  = "DataNumber" 
      NO-LOCK NO-ERROR.

      IF  Not Avail tmsparam THEN occli = "ERROR".
      ELSE IF avail tmsparam THEN DO:
         
         LOOP:
         REPEAT:
         FIND CURRENT tmsparam EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF locked(tmsparam) THEN DO:
               MESSAGE 
               "Data number in use.... -PRESS ENTER".
               pause 6 no-message.
            
               NEXT LOOP.
            END.
            ELSE LEAVE LOOP.
         ENd.
         
         ASSIGN 
            liCLI = INT(tmsparam.intval).
         
         MSISDN:
         REPEAT:
            liCLI = liCLI + 1.

            /* check IF msisdn number is already in use */
            IF NOT can-find(FIRST msisdn where
                                  msisdn.Brand = gcBrand AND 
                                  msisdn.cli = lcGSData + string(liCLI))        
            THEN LEAVE MSISDN.
         END.

         ASSIGN
            occli           = lcGSData + STRING(liCLI)
            tmsparam.intval = liCLI.
      END.    
      
   END.

   ELSE IF crit = "FAX"  THEN DO:
      Find first tmsparam where
                 TmsParam.Brand      = gcBrand        AND 
                 tmsparam.paramgroup = "subser"       AND
                 tmsparam.paramcode  = "FaxNumber" EXCLUSIVE-LOCK NO-ERROR.

      IF    Not Avail tmsparam THEN occli = "ERROR".
      ELSE IF avail tmsparam THEN DO:
         ASSIGN 
            liCLI = tmsparam.intval.

         MSISDN:
         REPEAT:

            liCLI = liCLI + 1.

            /* check IF msisdn number is already in use */
            IF NOT can-find(FIRST msisdn where
                                  msisdn.Brand = gcBrand AND 
                                  msisdn.cli = lcGSFAX + string(liCLI))        
            THEN LEAVE MSISDN.
         END.

         ASSIGN
            occli           = lcGSData + STRING(liCLI)
            tmsparam.intval = liCLI.
          RELEASE tmsparam.
      END.    


   END.
   
   ELSE IF crit = "DCF"  THEN DO:

      /* is range full */ 
      IF CURRENT-VALUE(chpar-seq) > 4399999 AND 
         CURRENT-VALUE(chpar-seq) < 99000000 THEN DO:
         
         CURRENT-VALUE(chpar-seq) = 99000000.
      
         find tmsparam where
              tmsparam.brand = "1" and
              tmsparam.paramcode = "DCFPrefix" no-error.
                  
          ASSIGN 
             tmsparam.charval = "042"
             lcGSDcf          = "042".
      END. 
      
      liCLI = NEXT-VALUE(chpar-seq). 

      MSISDN:
      REPEAT:
         /* check IF msisdn number is already in use */
         IF NOT can-find(FIRST msisdn where
                               msisdn.Brand = gcBrand AND 
                               msisdn.cli = lcGSDcf + string(liCLI))    
         THEN LEAVE MSISDN.

         liCLI = NEXT-VALUE(chpar-seq).                      

      END.
      occli = lcGSDcf + STRING(liCLI).
   
   END.
   RETURN occli.

END. 

DEF BUFFER HistMSISDN FOR MSISDN.


FUNCTION fMakeMsidnHistoryTS RETURNS LOG
   (INPUT irRecID     AS RECID,
    INPUT idTimeStamp AS DEC):
   
   DEF VAR ldNewFrom  AS DEC  NO-UNDO.
   DEF VAR ldtNewDate AS DATE NO-UNDO.
   DEF VAR liNewTime  AS INT  NO-UNDO.
   
   FIND FIRST HistMSISDN WHERE 
              RECID(HistMSISDN) = irREcID EXCLUSIVE-LOCK.
          
   IF idTimeStamp = 0 OR idTimeStamp = ? THEN idTimeStamp = fMakeTS().
   
   ASSIGN HistMSISDN.ValidTo = idTimeStamp.

   ldNewFrom = HistMSISDN.ValidTo.
   
   /* make sure that there is atleast 1 second gap between rows */
   REPEAT:
      /* do this first so that ldtnewdate is available for ActionDate */
      fSplitTS(ldNewFrom,
               OUTPUT ldtNewDate,
               OUTPUT liNewTime).

      IF NOT CAN-FIND(FIRST MSISDN WHERE
                      MSISDN.Brand     = HistMSISDN.Brand AND
                      MSISDN.CLI       = HistMSISDN.CLI   AND
                      MSISDN.ValidFrom = ldNewFrom)
      THEN LEAVE.

      IF liNewTime >= 86400 THEN ASSIGN
         ldtNewDate = ldtNewDate + 1
         liNewTime  = 1.
      ELSE liNewTime = liNewTime + 1.
      
      ldNewFrom = fMake2Dt(ldtNewDate,liNewTime).
   END.
   
   CREATE MSISDN.
   BUFFER-COPY HistMSISDN EXCEPT ValidFrom validTo TO MSISDN.
   ASSIGN
      MSISDN.ValidFrom  = ldNewFrom
      MSISDN.ValidTo    = 99999999.99999
      MSISDN.ActionDate = ldtNewDate.

END FUNCTION. 

FUNCTION fMakeMsidnHistory RETURNS CHAR
   (INPUT   irRecID AS RECID):
   
   fMakeMsidnHistoryTS(irRecid,
                       fMakeTS()). 
END FUNCTION.


