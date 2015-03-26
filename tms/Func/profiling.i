&IF "{&PROFILING_I}" NE "YES" &THEN
&GLOBAL-DEFINE PROFILING_I YES

/** profiling.i  
 * 
 * contains functions to start and stop profiling sessions
 */

/** Use this function to start profiling a session.
 * 
 * @param icFilename where to output the profiling data
 * @param icListingsPath specify a directory where you 
 *          want profiler to attemp to generate the debug 
 *          listings. 
 *            
 *          - disable listings with "?" as directory
 */
FUNCTION fStartProfiling RETURNS LOGICAL 
(icFileName AS CHAR,
 icListingsPath AS CHAR) :
   IF PROFILER:PROFILING THEN RETURN FALSE.
   PROFILER:ENABLED = FALSE.

   PROFILER:FILE-NAME = icFileName.
   IF icListingsPath NE ? THEN DO:
      PROFILER:DIRECTORY = icListingsPath.
      PROFILER:LISTINGS = YES.
   END.

   PROFILER:ENABLED = TRUE.
   PROFILER:PROFILING = TRUE.
   RETURN PROFILER:PROFILING.
END FUNCTION.


/** Use this function to stop a profiling session
 *  and write the collected data to the outfile 
 *
 */
FUNCTION fStopProfiling RETURNS LOGICAL ():
   IF NOT PROFILER:PROFILING THEN RETURN FALSE.
   PROFILER:PROFILING = FALSE.
   PROFILER:ENABLED = FALSE.
   PROFILER:WRITE-DATA().
   RETURN PROFILER:PROFILING.
END FUNCTION.



&ENDIF 
