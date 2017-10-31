/* ----------------------------------------------------------------------
  MODULE .......: simread_cui.p
  TASK .........: TMS CUI to select SIM File.
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 01-02-13
  CHANGED ......: 

  Version ......: 
------------------------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}

DEFINE VARIABLE lcSIMfile          AS CHARACTER NO-UNDO FORMAT "X(40)".
DEFINE VARIABLE lcProcessedDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedSIMFile AS CHARACTER NO-UNDO.

form /* SIM PaymFile */
   skip(1)
   " Read from file:" lcSIMfile NO-LABEL
   help "Path and Name of a SIM specification PaymFile"
   skip(1)
   WITH centered ROW 8 OVERLAY NO-LABELS 
   title " Read SIM/IMSI Data From SimFile "
   FRAME SIMfile.

DO WITH FRAME SIMfile :
   RUN Syst/filebrowser.p(fCParam("SIM","ReadInSimFile")).

   lcSIMfile = RETURN-VALUE.

   Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
   PAUSE 0.

   IF lcSIMfile = "" OR lcSIMfile = ? THEN LEAVE.

   lcSIMfile = fCParam("SIM","ReadInSimFile") + lcSIMfile.

   IF search(lcSIMfile) = ? THEN 
   DO:
      MESSAGE "File " lcSIMfile " does not exist !"
         VIEW-AS ALERT-BOX error.
         NEXT.
   END.

   lcProcessedDir = fCParam("SIM","ProcessedSimFile").
   
   RUN Mm/simread.p(INPUT lcSIMfile,
               INPUT lcProcessedDir,
               OUTPUT lcProcessedSIMFile).

   HIDE FRAME SIMfile.

END.
