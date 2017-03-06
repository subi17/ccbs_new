/* ----------------------------------------------------------------------
MODULE .......: mnpserverin.p
TASK .........: Handles incoming MNP contingency buzon messages
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 06.10.09
Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/log.i}
{xmlrpc/xmlrpc_client.i}
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}

DEFINE VARIABLE lcResponseFile AS CHAR NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcXMLFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llcXML AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcUrl AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcUnix AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcResponse AS LONGCHAR NO-UNDO.
DEFINE VARIABLE liTimeOut AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLogDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTmpDirIn AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTmpDirOut AS CHARACTER NO-UNDO. 

ASSIGN
   lcRootDir =  fCParam("MNP", "ContingencyServerDir")
   lcIncDir = lcRootDir + "/incoming/incoming/" 
   lcIncProcDir = lcRootDir + "/incoming/processed/" 
   lcOutSpoolDir = lcRootDir + "/outgoing/spool/"
   lcOutDir = lcRootDir + "/outgoing/outgoing/" 
   lcTmpDirIn = lcRootDir + "/incoming/tmp/"
   lcTmpDirOut = lcRootDir + "/outgoing/tmp/"
   lcLogDir = fCParam("MNP","MNPLogDir")
   liTimeOut = 120
   lcUrl = fCParam("MNP","MNPTMSURL").

initialize(lcURL, liTimeOut).

DEF STREAM sFile.
DEF STREAM sXMLFile.

fSetLogFileName(lcLogDir + "contingency_buzon.log").

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcIncDir + lcFileName.
   
   lcUnix = "cd " + lcIncDir + ";tar -xzPf " + 
      lcInputFile + " -C " + lcTmpDirIn. 
   UNIX SILENT VALUE(lcUnix). 
   
   /* handle all buzon xml files from extracted zip file */
   INPUT STREAM sXMLFile THROUGH VALUE("ls -1tr " + lcTmpDirIn).
   REPEAT TRANS:
      
      IMPORT STREAM sXMLFile UNFORMATTED lcXMLFileName.
      COPY-LOB FROM FILE lcTmpDirIn + lcXmlFileName TO llcXML. 
      
      /* call local rpc */
      xmlrpc_cleanup().
      run_rpc_method_without_serializing(TRUE,
                                         llcXML,
                                         FALSE,
                                         OUTPUT lcResponse). 
      
      lcResponseFile = lcTmpDirOut + lcXMLFileName.
      
      IF gi_xmlrpc_error NE ? AND gi_xmlrpc_error NE 0 THEN DO:
         lcResponse = gc_xmlrpc_error.
         COPY-LOB lcResponse TO FILE lcResponseFile.
         fLogError(gc_xmlrpc_error).
         xmlrpc_cleanup().
         NEXT.
      END.
      ELSE DO:
         COPY-LOB lcResponse TO FILE lcResponseFile.
      END.
   END.
   INPUT STREAM sXMLFile CLOSE.
   
   /* pack response xml files to same filename as input file */
   lcUnix = "cd " + lcTmpDirOut + ";tar --remove-files -czPf " + 
      lcTmpDirOut + lcFileName + " *". 

   UNIX SILENT VALUE(lcUnix). 
   UNIX SILENT VALUE("mv " + lcTmpDirOut + lcFileName + " " + lcOutDir). 
   UNIX SILENT VALUE("mv " + lcIncDir + lcFileName + " " + lcIncProcDir). 
   UNIX SILENT VALUE("rm -f " + lcTmpDirIn + "*").

   xmlrpc_cleanup().
END.

FINALLY:
   INPUT STREAM sXMLFile CLOSE.
   INPUT STREAM sFile CLOSE.
   xmlrpc_finalize().
END.
