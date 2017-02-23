{../tms_support/utilities/ttinpmobsub.i}
fAddOption("ValueField", "Mobsub.MsSeq").
fAddOption("DefaultExport", "FALSE").
fAddOption("ExportCOTarg", "TRUE").

RUN tms_support/utilities/expmobsubnew.p("/home/harrim/dumps/hierarch/", 0, 0, 1,
                   "/home/harrim/dumps/hierarch/logfile7.txt",
                   "/home/harrim/dumps/hierarch/msseq2.txt",
                   0, TABLE ttinpmobsub). 


