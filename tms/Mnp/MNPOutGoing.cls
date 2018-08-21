 
 /*------------------------------------------------------------------------
    File        : MNPOutGoing
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Apr 26 16:37:03 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Mnp.MNPOutGoing FINAL: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	CONSTRUCTOR PRIVATE MNPOutGoing (  ):
	END CONSTRUCTOR.

   METHOD PUBLIC STATIC CHARACTER mGetMNPOutOngoing
      (icCLI AS CHARACTER):

      DEFINE BUFFER MNPSub     FOR MNPSub.
      DEFINE BUFFER MNPProcess FOR MNPProcess.

      FOR EACH MNPSub WHERE
         MNPSub.CLI = icCLI NO-LOCK,
         EACH MNPProcess WHERE
              MNPProcess.MNPSeq = MNPSub.MNPSeq AND
              MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
              LOOKUP(STRING(MNPProcess.StatusCode),"2,5") > 0 NO-LOCK:
         RETURN MNPProcess.portrequest.
      END.

      RETURN "".

   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mIsMNPOutOngoing
      (icCLI AS CHARACTER):
      RETURN mGetMNPOutOngoing(icCLI) > "".
   END METHOD.

END CLASS.