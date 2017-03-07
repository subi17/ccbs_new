/* shaper.i    09.01.09/aam 

   add / remove subscription from 'shaper' (controls speed for data subscr.)
*/

{Func/timestamp.i}
{Func/fmakemsreq.i}

FUNCTION fTerminateShaper RETURNS LOGIC
   (INPUT  iiMsSeq       AS INT,
    INPUT  iiMainRequest AS INT,
    INPUT  idtEventDate  AS DATE,
    INPUT  icCreator     AS CHAR,
    OUTPUT ocParameter   AS CHAR,
    OUTPUT ocError       AS CHAR):
   
   DEF VAR llRemoved   AS LOG  NO-UNDO.
   DEF VAR ldActStamp  AS DEC  NO-UNDO.
   DEF VAR liCreated   AS INT  NO-UNDO.
   
   llRemoved = FALSE.
   
   FOR FIRST SubSer NO-LOCK WHERE
             SubSer.MsSeq   = iiMsSeq  AND
             SubSer.ServCom = "SHAPER" AND
             SubSer.SSDate <= idtEventDate:

      IF Subser.SSStat > 0 THEN DO: 

         IF idtEventDate = TODAY 
         THEN ldActStamp = fSecOffSet(fMakeTS(),-1). /* YTS-2473 */
         ELSE ldActStamp = fMake2DT(idtEventDate,0).
         
         ocParameter = SubSer.SSParam.
         
         liCreated = fServiceRequest (iiMsSeq ,     
                                      SubSer.ServCom,
                                      0,
                                      "",
                                      ldActStamp,
                                      "",                /* SalesMan */ 
                                      FALSE,             /* fees */
                                      FALSE,             /* SMS */ 
                                      icCreator,
                                      "",
                                      iiMainRequest,
                                      (iiMainRequest > 0),
                                      OUTPUT ocError).
      
         IF liCreated > 0 THEN DO:
            llRemoved = TRUE.
         END.
      END.          
   END.

   RETURN llRemoved.
   
END FUNCTION.

