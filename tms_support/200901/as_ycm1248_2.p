
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04.02.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to /apps/snet/200901/as_yts1248_3.log append. 

{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/fmakemsreq.i}
DEFINE VARIABLE liReq AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcInfo AS CHAR NO-UNDO. 
looppi:
FOR EACH mobsub where mobsub.brand = "1" NO-LOCK:
   
   FOR EACH msrequest where
      msrequest.brand = "1" and
      msrequest.reqtype = 1 and
      msrequest.reqstatus = 2 and
      msrequest.reqcparam1 = "obr" and
      msrequest.msseq = mobsub.msseq NO-LOCK break by msrequest.msseq 
         by msrequest.actstamp desc:
      
      if first-of(msrequest.msseq) and 
         msrequest.reqiparam1 = 0 and
         length(msrequest.reqcparam1) = 4 then do:

         i = i + 1.
         if i <= 2 then next looppi.
         
         FIND FIRST subser WHERE
            subser.msseq = mobsub.msseq and
            subser.servcom = "obr" NO-LOCK use-index servcom NO-ERROR.

         IF AVAIL subser and 
            subser.ssstat = 0 then do:

            liReq = fServiceRequest(MobSub.MsSeq,
                                    TRIM(SubSer.ServCom),
                                    SubSer.SSStat,
                                    SubSer.SSParam,
                                    fMakeTS(),
                                    "",
                                    FALSE,      /* fees */
                                    FALSE,      /* sms */
                                    "",
                                    OUTPUT lcInfo).

            put stream sout unformatted mobsub.msseq "|" mobsub.cli "|" liReq "|" lcInfo skip.
        end.
      end.
   end.
end.

output stream sout close.
