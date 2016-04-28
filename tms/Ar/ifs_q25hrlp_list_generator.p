/* ----------------------------------------------------------------------
  MODULE .......: ifs_q25hrlp_list_generator.p
  TASK .........: Create a Q25M24 dump file for IFS 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & kariaika
  CREATED ......: 29.3.2016
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
gcbrand = "1".
{Syst/tmsconst.i}
{Func/q25functions.i}
fInitHRLPParameters().
fGenerateQ25List({&Q25_MONTH_24}).

