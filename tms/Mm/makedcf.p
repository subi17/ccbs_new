/* ----------------------------------------------------------------------
  MODULE .......: MAKEDCF.P
  TASK .........: Creates DCF
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 05-07-99
  CHANGED ......: 12.11.99 arboga-nr removed
                  03.11.03 jp dcf-handling
                  23.06.04 tk do not find order
                  13.12.04/aam idtDate, SSDate, fServComValue()
                  24.08.05/aam ilSolog
  Version ......: M15
  ---------------------------------------------------------------------- */


{Syst/commali.i}

{Func/msisdn.i}
{Func/fctserval.i}

DEF INPUT PARAMETER iiMsSeq LIKE MobSub.MsSeq NO-UNDO.
DEF INPUT PARAMETER idtDate AS DATE           NO-UNDO.
DEF INPUT PARAMETER ilSolog AS LOG            NO-UNDO.

DEF VAR lcCLI        AS CHAR NO-UNDO.
DEF VAR llOk         AS LOG  NO-UNDO.
DEF VAR def-ccode    AS C    NO-UNDO.
DEF VAR DCFPref      AS CHAR NO-UNDO.
DEF VAR liFrom       AS INT  NO-UNDO.
DEF VAR lcmi-no      AS CHAR NO-UNDO.
DEF VAR occli        AS CHAR No-UNDO.
DEF VAR liValue      AS INT  NO-UNDO.

{Func/cparam.i "DefCCode"         return}.  def-ccode = TMSparam.CharVal.
{Func/cparam.i DCFVoiceMailPref   RETURN}.  DCFPref   = TMSparam.CharVal.


FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK.


IF NOT mobsub.cli begins "04041" THEN DO:         
   lccli =  fSearchGenServNumber(mobsub.cli ,"DCF").
END. 
ELSE  lccli = "04042" + substring(mobsub.cli,6).     

/* allowed for clitype */
liValue = fServComValue(MobSub.CLIType,
                        "PP2",
                        OUTPUT llOk).
IF liValue = ? OR
   (liValue = 0 AND NOT llOk) THEN DO:

   IF NOT SESSION:BATCH THEN 
   MESSAGE "Service PP2 is not allowed for CLI type" MobSub.CLIType
   VIEW-AS ALERT-BOX ERROR.

   RETURN.
END.
                        
FIND ServCom WHERE 
     ServCom.Brand   = gcBrand AND 
     ServCom.ServCom = "PP2" no-lock.

FIND FIRST subser WHERE 
           Subser.MsSeq   = iiMsSeq AND 
           SubSer.ServCom = ServCom.ServCom AND
           SubSer.SSDate  = idtDate EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL subser THEN DO:
   ASSIGN
      SubSer.MsSeq   = iiMsSeq
      SubSer.ServCom = ServCom.ServCom
      SubSer.SSDate  = idtDate.
END.

ASSIGN SubSer.SSStat    = 1
       /* send to HLR */ 
       SubSer.SologStat = INTEGER(ilSolog AND ServCom.ActType = 0).

/* default attributes from clitype definitions */
FOR FIRST CTServEl NO-LOCK WHERE
          CtServEl.Brand     = gcBrand AND
          CtServEl.ServCom   = SubSer.ServCom AND
          CTServEl.CLIType   = MobSub.CLIType AND
          CTServEl.FromDate <= idtDate,
     EACH CTServAttr OF CTServEl NO-LOCK
BREAK BY CTServAttr.ServAttr
      BY CTServAttr.FromDate DESC:
      
   IF FIRST-OF(CTServAttr.ServAttr) THEN DO:

      FIND FIRST SubSerPara WHERE 
                 SubSerPara.MsSeq    = MobSub.MsSeq         AND 
                 SubSerPara.ServCom  = SubSer.ServCom       AND 
                 SubSerPara.ParaName = CTServAttr.ServAttr  AND
                 SubSerPara.SSDate   = SubSer.SSDate NO-ERROR.

      IF NOT AVAIL SubSerPara THEN DO:
         CREATE SubSerPara.
         ASSIGN SubSerPara.MsSeq     = MobSub.MsSeq
                SubSerPara.ServCom   = SubSer.ServCom
                SubSerPara.ParaName  = CTServAttr.ServAttr
                SubSerPara.SSDate    = SubSer.SSDate.
      END.   

      ASSIGN SubSerPara.ParaValue = CTServAttr.DefValue
             SubSerPara.SologStat = INTEGER(ilSolog AND ServCom.ActType = 0).

      IF SubSerPara.ParaName = "MSISDN2"
      THEN SubSerPara.ParaValue = lcCLI.
   END.   
      
END.

FIND FIRST MSISDN where 
           MSISDN.cli = lccli no-error.
           
IF not avail MSISDN then do:
   create MSISDN.
   ASSIGN 
      MSISDN.cli        = lccli
      MSISDN.brand      = gcbrand 
      MSISDN.custnum    = mobsub.custnum
      MSISDN.statuscode = 4.
ENd.           
ELSE DO:
   fMakeMsidnHistory(INPUT RECID(MSISDN)).
   ASSIGN 
      MSISDN.statuscode = 4
      MSISDN.custnum    = mobsub.custnum.
END.           

