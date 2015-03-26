{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}

DEFINE VARIABLE clis AS CHARACTER NO-UNDO. 

clis = "606280691 607851157 608136141 610374626 615407977 616977041 616997311 619343485 620090082 622003062 622023000 622042467 622042988 622044696 622048299 622050415 622052779 622053256 622053936 622054663 622055868 622056099 622056557 622057695 622057908 622059361 622059482 622063779 622067633 622079985 622154591 622185746 622213860 622215531 622219321 622219735 622222526 622222703 622223895 622234392 622237023 622240576 622244261 622272140 622275051 622277495 622285505 622334665 622399142 622400981 622401591 622409828 622449866 622495933 622501074 622505438 622518093 622543128 622545936 622554620 622556028 622600362 622600737 622601837 622602843 622604705 622606550 622606985 622608559 622611562 622623033 622633657 622680306 622685598 622690104 622698326 622713314 622721305 622721888 622722411 622730356 622737474 622831520 622848724 622869994 622880752 625163234 625168464 625181052 625644382 625854143 625970174 626076053 626539936 627445389 628342576 628767886 629219654 630203731 630943660 635538401 635872586 635950346 638015758 638333871 645193337 647014349 647608414 648815923 649394533 650707388 650717989 650790507 650880552 651510718 651518370 651805416 651830401 652635883 654442212 655214078 655661471 656633458 656697645 657765077 659158460 659323484 659339439 659515001 659741631 662263053 663539001 663788631 664794966 665265922 665962845 666049409 667346569 671684081 675351800 676572692 677225120 677350743 679409809 679982602 686593914 690213839 690392824 691725556 692231994 696035608 696189887 699646188".

clis = "606280691".

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def buffer subreq for msrequest.

def stream slog.
output stream slog to /apps/snet/200901/as_yts1250.log append.

do i = 1 to num-entries(clis," ") with frame a:
   
   lcCli = entry(i, clis, " ").
   
   find mobsub where
      mobsub.cli = entry(i, clis, " ") NO-LOCK NO-ERROR.
   IF AVAIL mobsub then do:
      
      FIND FIRST msrequest where
         msrequest.msseq   = mobsub.msseq and
         msrequest.reqtype = 0 and
         msrequest.reqstatus = 3 use-index msseq NO-LOCK NO-ERROR.
      
      IF AVAIL msrequest then do:
         IF msrequest.reqcparam2 ne mobsub.clitype then do:
           
           find current msrequest EXCLUSIVE-LOCK NO-ERROR.
           put stream slog unformatted msrequest.msrequest " " msrequest.reqsource skip.
          assign msrequest.reqsource = "5".
          find current msrequest NO-LOCK.
          fReqstatus(8,"").

           /*
           /* 
            IF msrequest.reqcparam2 begins "contr" and
                mobsub.clitype begins "contr" then 
          */
            FOR EACH subreq where
               subreq.origrequest = msrequest.msrequest NO-LOCK:
               /*disp msrequest.actstamp mobsub.clitype msrequest.reqcparam2.*/
               if subreq.reqstat ne 2 then
               disp subreq.reqtype subreq.reqstatus.
            end.

            */
            end.
         else
            disp mobsub.cli "is ok".
      END.

   END.
   else do:
      disp mobsub.cli "NOT FOUND".
   end.

end.

output stream slog close.
