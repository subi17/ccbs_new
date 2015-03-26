{/home/harrim/utilities/ttinpmobsub.i}

FUNCTION fGetNames RETURN CHARACTER 
  (INPUT pcBegin AS CHARACTER, INPUT piCount AS INTEGER):
   DEFINE VARIABLE cRet AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   cRet = pcBegin + "1".
   REPEAT i = 2 TO piCount:
      cRet = cRet + "," + pcBegin + STRING(i).
   END.
   RETURN cRet.
END.

fAddOption("ValueField", "MSISDN.CLI").
RUN /home/harrim/utilities/fixdumpexport.p(
    "/home/harrim/dumps/fixtures/freemsisdn/",
    0,
    0,
    0,
    "/home/harrim/dumps/fixtures/freemsisdn/free.log",
    "/home/harrim/msisdnlist.txt",
    0, 
    fGetNames("MSISDN", 1000),
    TABLE ttinpmobsub).





