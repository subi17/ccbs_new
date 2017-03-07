/*------------------------------------------------------------
  MODULE .......: cliinlog.i
  FUNCTION .....: Initiates the logmodule. A flag decides
                  logoutput should be made OR NOT.

                  Usage: {Mf/cliinlog.i "NEW" "xxxxx"} 
                 ("NEW" only for the "top" program in a chain of procedures.
                  "xxxxx" IF this second argument is set, the logfile will have
                   the prefix "xxxxx". IF left a default FileName
                   will be used (Location of the File is set in TMSParam).

  APPLICATION ..: 
  AUTHOR .......: DW Orbil
  CREATED ......: 22.09.1999 kl
  MODIFIED .....: 
  Version ......: M15
--------------------------------------------------------------*/

DEFINE {1} SHARED STREAM   ilsLog.
DEFINE {1} SHARED VARIABLE ilLogType     AS CHARACTER NO-UNDO.
DEFINE {1} SHARED VARIABLE ilDestination AS CHARACTER NO-UNDO.
DEFINE {1} SHARED VARIABLE ilMessage     AS CHARACTER NO-UNDO FORMAT "X(80)".

DO:

   IF "{1}" = "NEW" THEN DO:

      /* Look in TMSParam which way status is going TO be Sent */
      /* via EMail OR TO a logfile.                          */
      {Func/tmsparam.i LogType RETURN}
      ilLogType = TMSParam.CharVal.

      /* Get the log-destination */
      {Func/tmsparam.i LogDest RETURN}
      ilDestination = TMSParam.CharVal.

      IF substr(ilDestination,length(ilDestination),1) NE "/" THEN
         ilDestination = ilDestination + "/".

      /* Rebuild dateformat TO get correct */
      /* sortorder. (Chronological)        */        
      ilDestination = ilDestination + "{2}" + 
                      STRING(YEAR(TODAY),"9999")  + 
                      STRING(MONTH(TODAY),"99")   +
                      STRING(DAY(TODAY),"99")     + ".log".

      IF ilLogType BEGINS "File" THEN DO:
         OUTPUT STREAM ilsLog TO VALUE(ilDestination) APPEND.
         PUT STREAM ilsLog SKIP(1).
      END.

   END.      

END.  

