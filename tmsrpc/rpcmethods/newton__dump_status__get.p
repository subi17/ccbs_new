/**
 * Get dump file status information 
 *
 * @input dump;struct;mandatory;dump_search_criteria
 * @dump  starttime;datetime;optional; dump start time
          endtime;datetime;optional; dump end time
          dumpid;int;optional; dump id
          filename;string;optional; dump filename
 * @output array of dump structs
   @dumpa  dumps;struct
   @dumps  id;int;dump id
           filename;string;dump file name
           active;boolean;yes or no
           status;int;process status
           mode;string;full or modified
           create_start_time;timestamp;creation start time
           create_finish_time;timestamp;creation finish time
           file_size;string;file size
           avedur;string;average duration of dump creation time
           actual;string;actual duration time of dump
           host;string;Master/Replica dump creation database
           compress_size;string;compressed file size
           transfer_star_time;timestamp;transfer start time
           transfer_finish_time;timestamp;transfer finish time
*/

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
{timestamp.i}

&SCOPED-DEFINE SEARCH_LIMIT 1000

ASSIGN
   gcBrand = "1".

DEF VAR ldeStartTime AS DECIMAL NO-UNDO. 
DEF VAR ldeEndTime AS DECIMAL NO-UNDO.  
DEF VAR liDumpId AS INT NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR resp_array AS CHAR NO-UNDO.
DEF VAR dump_struct AS CHAR NO-UNDO.
DEF VAR llWildSearch AS LOG NO-UNDO.
DEF VAR liCases AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_struct(pcStruct,
   "starttime,endtime,dumpid,filename").

IF LOOKUP("starttime", lcStruct) > 0 THEN
   ldeStartTime = get_timestamp(pcStruct, "starttime").

IF LOOKUP("endtime", lcStruct) > 0 THEN
   ldeEndTime = get_timestamp(pcStruct, "endtime").

IF LOOKUP("dumpid", lcStruct) > 0 THEN
   liDumpId = get_int(pcStruct, "dumpid").

IF LOOKUP("filename", lcStruct) > 0 THEN
   lcFileName = get_string(pcStruct, "filename").

IF gi_xmlrpc_error NE 0 THEN RETURN.


resp_array = add_array(response_toplevel_id, "").

IF R-INDEX(lcFileName,"*") EQ LENGTH(lcFileName) THEN ASSIGN 
   lcFileName = RIGHT-TRIM(lcFileName,"*")
   llWildSearch = TRUE.

/* Search by Filename */
IF lcFileName > "" THEN DO:

   IF llWildSearch THEN DO:

      IF NOT CAN-FIND(FIRST DumpLog WHERE 
                            DumpLog.FileName BEGINS lcFileName) THEN RETURN.

      FOR EACH DumpLog NO-LOCK WHERE
               DumpLog.FileName BEGINS lcFileName BY CreateStart DESC:
         RUN pAddResultsArray.
         liCases = liCases + 1.
         IF liCases > {&SEARCH_LIMIT} THEN
            RETURN appl_err("Too many search results").
      END.

   END.
   ELSE DO:

      IF NOT CAN-FIND(FIRST DumpLog WHERE 
                            DumpLog.FileName = lcFileName) THEN RETURN.

      FOR EACH DumpLog NO-LOCK WHERE
               DumpLog.FileName = lcFileName BY CreateStart DESC:
         RUN pAddResultsArray.
      END.
   END.
END. /* IF lcFileName > "" THEN DO: */

/* Search by DATE and ID options */
ELSE DO:
   IF liDumpId > 0 THEN DO: /* DATE and ID */

      FOR EACH DumpLog NO-LOCK WHERE
               DumpLog.CreateStart >= ldeStartTime AND
               DumpLog.CreateStart <= ldeEndTime AND
               DumpLog.DumpId       = liDumpId:
         RUN pAddResultsArray.
       END. /* FOR EACH DumpLog NO-LOCK WHERE */
   END.

   ELSE /* DATE only */
      FOR EACH DumpLog NO-LOCK WHERE
               DumpLog.CreateStart >= ldeStartTime AND
               DumpLog.CreateStart <= ldeEndTime:
         
         liCases = liCases + 1.
         IF liCases > {&SEARCH_LIMIT} THEN
            RETURN appl_err("Too many search results").

         RUN pAddResultsArray.
      END. /* FOR EACH DumpLog NO-LOCK WHERE */
END.

/* Struct builder */
PROCEDURE pAddResultsArray:
   DEF VAR ldtStarted AS DATETIME NO-UNDO. 
   DEF VAR ldtEnded AS DATETIME NO-UNDO. 
   DEF VAR ldtNow AS DATETIME NO-UNDO.
   DEF VAR liElapsed AS INT NO-UNDO. 
   DEF VAR liDiffer AS INT NO-UNDO. 
   DEF VAR lcDiffer AS CHAR NO-UNDO. 
   DEF VAR lcEndTime AS CHAR NO-UNDO.
   DEF VAR lcStartTime AS CHAR NO-UNDO. 
   DEF VAR lcOmittedDumps AS CHAR NO-UNDO INIT "TMS,TRACK,Cassandra".

   FIND FIRST DumpFile NO-LOCK WHERE
              DumpFile.DumpID = DumpLog.DumpId NO-ERROR.

   IF NOT AVAIL DumpFile THEN RETURN.
   
   IF LOOKUP(DumpFile.FileCategory,lcOmittedDumps,",") > 0 THEN RETURN.

   /* Updated average for ongoing dumps: YOT-2766
      EndTime contain elapsed time, if ongoing dump
      StartTime changed to get equal implementation */
   IF DumpLog.DumpLogStatus = 0 THEN
      ASSIGN
         ldtStarted = fTimeStamp2DateTime(Dumplog.CreateStart)
         ldtNow = fTimeStamp2DateTime(fMakeTS())
         liElapsed = INT(ldtNow - ldtStarted) / 1000
         lcEndTime = STRING(liElapsed,"HH:MM:SS").
   ELSE DO:
      IF DumpLog.CreateEnd NE 00000000.00000 THEN DO:
         ASSIGN
            ldtStarted = fTimeStamp2DateTime(Dumplog.CreateStart)
            ldtEnded = fTimeStamp2DateTime(Dumplog.CreateEnd)
            liDiffer = INT(ldtEnded - ldtStarted) / 1000
            lcEndTime = fTS2HMS(DumpLog.CreateEnd)
            lcEndTime = REPLACE(lcEndTime,".","-").
         IF liDiffer > 0 THEN
            lcDiffer = STRING(liDiffer,"HH:MM:SS").
         ELSE
            lcDiffer = "".
      END.
      ELSE DO:
         ASSIGN
            ldtStarted = fTimeStamp2DateTime(Dumplog.CreateStart)
            lcEndTime = ""
            lcDiffer = "".
      END.

   END.

   ASSIGN
      lcStartTime = fTS2HMS(DumpLog.CreateStart)
      lcStartTime = REPLACE(lcStartTime,".","-").

   dump_struct = add_struct(resp_array, "").
   add_int(dump_struct, "id", DumpLog.DumpId).
   add_string(dump_struct,"filename", DumpLog.FileName).
   add_boolean(dump_struct,"active", DumpFile.Active).
   add_int(dump_struct,"status", DumpLog.DumpLogStatus).
   add_string(dump_struct,"mode", DumpLog.DumpType).
   add_string(dump_struct,"create_start_time",lcStartTime).
   add_string(dump_struct,"create_finish_time",lcEndTime).
   add_double(dump_struct,"file_size",DumpLog.Filesize).
   IF DumpLog.DumpType = "Full" THEN
      add_string(dump_struct,"avedur",STRING(DumpFile.AveDurFull,"HH:MM:SS")).
   ELSE
      add_string(dump_struct,"avedur",STRING(DumpFile.AveDurMod,"HH:MM:SS")).
   add_string(dump_struct,"actual",lcDiffer).
   add_string(dump_struct,"host", DumpLog.CreationDB).
      
   /* NOT YET AVAILABLE
   add_double(dump_struct,"compress_size", 0).
   add_timestamp(dump_struct,"transfer_start_time",0).
   add_timestamp(dump_struct,"transfer_finish_time",0).
   */
END PROCEDURE. /* PROCEDURE pAddResultsArray: */

FINALLY:
  IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
