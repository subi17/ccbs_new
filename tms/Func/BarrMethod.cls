 
 /*------------------------------------------------------------------------
    File        : BarrMethod
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Apr 26 17:34:08 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Func.BarrMethod FINAL: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PRIVATE BarrMethod (  ):
	END CONSTRUCTOR.

   METHOD PUBLIC STATIC CHARACTER mGetActiveBarrings
      (iiMsSeq AS INTEGER):
   
      /*groups of checked barrings*/
      DEFINE VARIABLE lcBList AS CHARACTER NO-UNDO.
   
      DEFINE BUFFER Barring FOR Barring.
   
      FOR EACH Barring NO-LOCK WHERE
               Barring.MsSeq EQ iiMsSeq
               USE-INDEX MsSeq BREAK BY Barring.BarringCode:
   
         IF FIRST-OF(Barring.BarringCode) AND
            Barring.BarringStatus NE {&BARR_STATUS_INACTIVE}
         THEN lcBList = lcBList + "," + Barring.BarringCode.
     END.

     RETURN TRIM(lcBList, ",").

   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mSubsHaveActiveBarring
      (iiMsSeq        AS INTEGER,
       icBarringTypes AS CHARACTER):
   
      /*groups of checked barrings*/
      DEFINE VARIABLE lcBList AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lii     AS INTEGER NO-UNDO.

      ASSIGN
         lcBList = mGetActiveBarrings(iiMsSeq)
         lcBList = REPLACE(lcBList, "=0","")
         lcBList = REPLACE(lcBList, "=1","")
         icBarringTypes = REPLACE(icBarringTypes, "=0","") 
         icBarringTypes = REPLACE(icBarringTypes, "=1","").
  
      DO lii = 1 TO NUM-ENTRIES(lcBList):
         IF LOOKUP(ENTRY(lii, lcBList), icBarringTypes) > 0
         THEN RETURN TRUE.
      END.
      
      RETURN FALSE.

   END METHOD.

END CLASS.