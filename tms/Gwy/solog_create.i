/* ----------------------------------------------------------------------
  MODULE .......: solog_create.i 
  TASK .........: Create Solog command functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 25.06.12
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */
&IF "{&SOLOG_CREATE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE SOLOG_CREATE_I YES

{Syst/commali.i}
{Func/date.i}

FUNCTION fCreateSolog RETURNS INT
   (  INPUT iiMsSeq AS INT,
      INPUT icCLI AS CHAR,
      INPUT icCommLine AS CHAR):

   DEF VAR liSolog AS INT NO-UNDO. 
   DEF VAR ldeTime AS DEC NO-UNDO. 

   ASSIGN
      ldeTime = fMakeTS()
      liSolog = NEXT-VALUE(Solog).

   CREATE Solog.
   ASSIGN
      Solog.Solog        = liSolog
      Solog.CreatedTS    = ldeTime
      Solog.ActivationTS = ldeTime
      Solog.MsSeq        = iiMsSeq
      Solog.CLI          = icCLI
      Solog.Stat         = 0
      Solog.Brand        = gcBrand 
      Solog.Users        = katun    
      Solog.MSrequest    = 0
      Solog.CommLine    = STRING(liSolog) + " " + icCommLine 
      Solog.TimeSlotTMS = ldeTime.

   RELEASE Solog.

   RETURN liSolog.

END.

FUNCTION fCreateDisplaySolog RETURNS INT
   (INPUT iiMsSeq AS INT):

   DEF BUFFER Mobsub FOR Mobsub.
   DEF VAR lcCommLine AS CHAR NO-UNDO. 

   FIND MobSub WHERE
        MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN 0.
             
   lcCommline = "DISPLAY,PL,MSISDN=34" +
                MobSub.Cli + ",OPERATOR=YOIGO".

   RETURN fCreateSolog(MobSub.MsSeq,
                       MobSub.CLi,
                       lcCommline).
END.

FUNCTION fCreateDisplayDSSSolog RETURNS INT
   (INPUT iiMsSeq AS INT):
   
   DEF BUFFER Mobsub FOR Mobsub.
   DEF VAR lcCommLine AS CHAR NO-UNDO. 

   FIND MobSub WHERE
        MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN 0.
             
   lcCommline = "DISPLAY,DSS-ACCOUNT=" +
                STRING(MobSub.Custnum) + ",OPERATOR=YOIGO".

   RETURN fCreateSolog(MobSub.MsSeq,
                       MobSub.CLi,
                       lcCommline).

END.

&ENDIF
