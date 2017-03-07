{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/fmakemsreq.i}

DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200903/as_ycm1368_2.log append.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

FOR EACH MobSub WHERE
         MobSub.Brand   = "1" AND
         MobSub.CliType = "CONTRD1" and
         MobSub.Creationdate = 3/17/2009 NO-LOCK: 

   find first dccli where
      dccli.brand = "1" and
      dccli.dcevent = "yoigoyoigo" and
      dccli.msseq = mobsub.msseq and
      dccli.validto > 3/17/2009 NO-LOCK NO-ERROR.
   
   if not avail dccli then do:
      next.
   end.

   FIND FIRST MsRequest WHERE
      MsRequest.MsSeq = mobsub.msseq AND
      MsRequest.Reqtype = 9 and
      MsRequest.reqcparam3 = "yoigoyoigo" AND
      MsRequest.reqstatus ne 2 NO-LOCK NO-ERROR.

   IF AVAIL msrequest then do:
      put stream slog unformatted mobsub.msseq "|0|ongoing request: " msrequest.msrequest " status: " msrequest.reqstatus skip.
      next.
   end.
   
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                "YOIGOYOIGO",
                                "term",
                                20090317.86399,
                                FALSE,
                                "5",
                                "",
                                OUTPUT lcResult).
   
   put stream slog unformatted mobsub.msseq "|" liRequest "|" lcResult skip.
   
END.
output stream slog close.
