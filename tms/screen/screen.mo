echo "MCDR OnLine Reader"
sleep 3
screen -t prepaid  1  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/prepaid.log -param 2220 tenant=yoigo umask=0022
screen -t prepaid  2  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/prepaid.log -param 2221 tenant=yoigo umask=0022
screen -t prepaid  3  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/prepaid.log -param 2222 tenant=yoigo umask=0022
screen -t prepaid  4  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/prepaid.log -param 2223 tenant=yoigo umask=0022
screen -t prepaid  5  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/prepaid.log -param 2224 tenant=yoigo umask=0022
screen -t postpaid 6  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2210 tenant=yoigo umask=0022
screen -t postpaid 7  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2211 tenant=yoigo umask=0022
screen -t postpaid 8  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2212 tenant=yoigo umask=0022
screen -t postpaid 9  pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2213 tenant=yoigo umask=0022
screen -t postpaid 10 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2214 tenant=yoigo umask=0022
screen -t postpaid 11 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2215 tenant=yoigo umask=0022
screen -t postpaid 12 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2216 tenant=yoigo umask=0022
screen -t postpaid 13 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2217 tenant=yoigo umask=0022
screen -t postpaid 14 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2218 tenant=yoigo umask=0022
screen -t postpaid 15 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/postpaid.log -param 2219 tenant=yoigo umask=0022
screen -t tap3     16 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/tap3.log -param 2240 tenant=yoigo umask=0022
screen -t vas      17 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/vas.log -param 2250 tenant=yoigo umask=0022
screen -t front    18 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/front.log -param 2270 tenant=yoigo umask=0022
screen -t fixed    19 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/fixed.log -param 2225 tenant=yoigo umask=0022
screen -t fixed    20 pike -C /apps/yoigo/tms terminal -- Rate/onlinereader_start common ordercanal mobile star fraudcdr counter -clientlog /scratch/log/mo/fixed.log -param 2226 tenant=yoigo umask=0022
#screen -t roamcdr  19 xfear -mo_roam
