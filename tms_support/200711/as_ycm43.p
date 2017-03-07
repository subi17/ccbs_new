/*

Description 
 
 We need in DW the Daily_Calls from 200612xx to 200703xx, included.  
  
  And the next days:  
  2007.05.04 
  2007.05.05 
  2007.05.06 
  2007.09.30 
  2007.10.07 

*/

{Syst/testpaa.i}
katun = "anttis".
{Func/date.i}

RUN /apps/snet/200711/calldump_ycm43.p(0,date(5,5,2007)).
RUN /apps/snet/200711/calldump_ycm43.p(0,date(5,6,2007)).
RUN /apps/snet/200711/calldump_ycm43.p(0,date(5,7,2007)).
RUN /apps/snet/200711/calldump_ycm43.p(0,date(10,1,2007)).
RUN /apps/snet/200711/calldump_ycm43.p(0,date(10,8,2007)).

RUN /apps/snet/200711/calldump_ycm43.p(200612,today).
RUN /apps/snet/200711/calldump_ycm43.p(200701,today).
RUN /apps/snet/200711/calldump_ycm43.p(200702,today).
RUN /apps/snet/200711/calldump_ycm43.p(200703,today).

/* yts-306 */
RUN /apps/snet/200711/calldump_ycm43.p(0,date(11,18,2007)).
