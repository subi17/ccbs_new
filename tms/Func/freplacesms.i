/* freplacesms.i       2005/jp

   changes:            02.06.06/aam don't use 'case', it doesn't recognize
                                    dots (or other chars) in the end of the tag 

*/

&IF "{&fReplaceSMS}" NE "YES"
&THEN

&GLOBAL-DEFINE fReplaceSMS YES


DEF BUFFER bRepSubSer     FOR SubSer.
DEF BUFFER bRepSubserPara FOR SubserPara.

FUNCTION fReplaceSMS RETURNS LOG
  (INPUT icText    AS CHAR,       /* text        */
   INPUT iimsseq   AS int,
   INPUT idate     AS DATE,
   OUTPUT ocText   AS CHAR).      /* output Text */

   DEF VAR loop AS i NO-UNDO.

   ASSIGN octext = icText.

   ASSIGN ocText = REPLACE(ocText,"#OWNER",Customer.custname)
          ocText = REPLACE(ocText,"#CLI",Mobsub.cli)
          ocText = REPLACE(ocText,"#MSISDN",mobsub.cli)
          ocText = REPLACE(ocText,"#MASINS",Customer.CustName)
          ocText = REPLACE(ocText,"#NUMBER","number")
          ocText = REPLACE(ocText,"#PVM",STRING(idate,"99-99-9999")).

   IF INDEX(ocText,"#TERMDATE") > 0  THEN DO:
      FIND KillMS  WHERE 
           Killms.msseq = iimsseq no-lock no-error.
             
      IF avail Killms THEN ASSIGN
         ocText = REPLACE(ocText,"#TERMDATE",
                                 STRING(killms.killdate,"99-99-9999")).
   END.

   IF INDEX(ocText,"#PORTDAYANDTIME") > 0 THEN DO:
      IF NOT AVAILABLE MobSub OR MobSub.MsSeq NE iiMsSeq THEN
      FIND FIRST mobsub WHERE 
                 Mobsub.msseq = iimsseq no-lock no-error.
      IF avail mobsub then 
      FIND FIRST msisdn WHERE 
                 msisdn.cli = mobsub.cli NO-LOCK NO-ERROR.
      IF avail msisdn AND msisdn.portingdate ne ? THEN ASSIGN
         ocText = REPLACE(ocText,"#PORTDAYANDTIME",
                          STRING(msisdn.portingdate,"99.99.9999") + " " + 
                          REPLACE(STRING(msisdn.portingtime,"99.99"),",",":")).
      ELSE  ocText = REPLACE(ocText,"#PORTDAYANDTIME","").
   END.
      
   IF INDEX(ocText,"#FAX") > 0 THEN DO:
      FIND First bRepSubSer where 
                 bRepSubSer.msseq = iimsseq  and 
                 bRepSubSer.servcom = "T62" no-lock no-error.
      if avail bRepSubSer then 
      ocText = REPLACE(ocText,"#FAX",bRepSubSer.ssparam).
   END.   

   IF INDEX(ocText,"#DATA") > 0 THEN DO:
      FIND First bRepSubSer where 
                 bRepSubSer.msseq = iimsseq  and 
                 bRepSubSer.servcom = "B16" no-lock no-error.
      if avail bRepSubSer then 
         ocText = REPLACE(ocText,"#DATA",bRepSubSer.ssparam) .
      ELSE 
         octext = REPLACE(ocText,"#DATA","tuntematon").
   END.

   IF INDEX(ocText,"#DCF") > 0 THEN DO:
      FIND First bRepSubSerPara where
                 bRepSubSerPara.msseq    = iimsseq   and
                 bRepSubSerPara.servcom  = "PP2"     AND 
                 bRepSubserPara.Paraname = "MSISDN2" 
      no-lock no-error.

      IF avail bRepSubSerPara THEN 
         ocText = REPLACE(ocText,"#DCF",bRepSubSerPara.ParaValue).
      ELSE ocText = REPLACE(ocText,"#DCF","").                         
   END.

   IF INDEX(ocText,"#NUMSMSBUNDLE") > 0 THEN DO:
      FIND First bRepSubSer where 
                 bRepSubSer.msseq = iimsseq  and 
                 bRepSubSer.servcom = "SMSBundle" no-lock no-error.
      if avail bRepSubSer then 
         ocText = REPLACE(ocText,"#NUMSMSBUNDLE",bRepSubSer.ssparam) .
   END.

   IF INDEX(ocText,"#EXPDATESMSBUNDLE") > 0 THEN DO:
      ocText = REPLACE(ocText,"#EXPDATESMSBUNDLE",STRING((idate + 30 ),"99-99-9999")) .
   END.
end.


&ENDIF
