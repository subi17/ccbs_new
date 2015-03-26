def var lcdcevent as char no-undo.
def var ldaBegin    as date no-undo.
def var ldaEnd      as date no-undo.
def var lcfile    as char no-undo.
def var j         as int no-undo.
def var ldtBegin  as decimal no-undo.
def var ldtEnd    as decimal no-undo.
def var gcBrand AS CHARACTER NO-UNDO initial "1". 

def stream slog.

{tmsconst.i}
{timestamp.i}

pause 0.
update
   lcdcevent format "x(12)"      label "Contract ID "
      help "DCEvent" skip(1)
   ldaBegin    format "99-99-9999" label "Activation From" skip(1)
   ldaEnd    format "99-99-9999" label "Activation To" skip(1)
   lcfile    format "x(50)"      label "File"
      help "Output file"
with overlay side-labels 1 column row 4 centered title " LIST PerContract Requests "
   frame fpercontr.
hide frame fpercontr no-pause.
   
if lcdcevent = "" or ldaBegin = ? or ldaEnd = ? or lcfile = "" 
then return.

output stream slog to value(lcfile).

put stream slog unformatted
   "MSISDN"  chr(9)
   "ContractID" chr(9)
   "Status" chr(9)
   "Activation" chr(9) 
   "Handled" skip.
   
pause 0.
disp j format ">>>>>>9" label "Found"
with overlay row 10 centered side-labels frame fqty.
   
ldtBegin = fHMS2TS(ldaBegin,"00:00:00").
ldtEnd = fHMS2TS(ldaEnd,"23:59:59").


FOR EACH MsRequest NO-LOCK WHERE
         Msrequest.Brand = gcBrand  AND 
         ( MsRequest.ReqType =  {&REQTYPE_CONTRACT_ACTIVATION} OR
           MsRequest.ReqType =  {&REQTYPE_CONTRACT_TERMINATION} ) AND
         MsRequest.ActStamp >= ldtBegin AND 
         MsRequest.ActStamp <= ldtEnd AND
         MsRequest.ReqCParam3 = lcdcevent :
   j = j + 1.
   pause 0.
   disp j with frame fqty.
   put stream slog unformatted
       MsRequest.cli   chr(9)
       MsRequest.ReqCParam3 chr(9)
       MSRequest.ReqStatus chr(9) 
       fTS2HMS(MsRequest.ActStamp) chr(9) 
       fTS2HMS(MsRequest.DoneStamp) skip.
END. 


hide frame fqty no-pause.

output stream slog close.
   

