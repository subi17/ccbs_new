/* KooAa 27.04.2001
   convert seconds into clock time, very large values allowed 
   
   07.12.2001/aam format for hours one shorter 
   31.10.2003/aam use >>>
   11.12.2003/aam iiLength
   21.12.2006/aam fMin2C, fSec2MinC
*/

function fSec2C returns char
  (input tme      as dec,
   input iiLength as int).
    
   def var hh    as de   no-undo.
   def var mm    as i    no-undo.
   def var ss    as i    no-undo.
   def var lcDur as char no-undo.
   
   if iiLength = 0 then return "". 

   assign 
      hh       = truncate(tme / 3600,0)
      tme      = tme - (hh * 3600)
      mm       = truncate(tme / 60,0)
      ss       = tme modulo 60
      lcDur    = string(hh,"->>>>>>99") + ":" +
                 string(mm,"99")        + ":" +
                 string(ss,"99")
      iiLength = min(iiLength,15)
      lcDur    = substring(lcDur,16 - iiLength).
                    
   return lcDur.                 

end.

FUNCTION fMin2C RETURNS CHARACTER
   (idTime   AS INT,
    iiLength AS INT):

   DEF VAR liHours AS INT  NO-UNDO.
   DEF VAR liMin   AS INT  NO-UNDO.
   DEF VAR lcDur   AS CHAR NO-UNDO. 
   
   IF iiLength = 0 THEN RETURN "".

   ASSIGN liHours  = TRUNCATE(idTime / 60,0)
          liMin    = idTime MOD 60
          lcDur    = STRING(liHours,"->>>>>>99") + ":" +
                     STRING(liMin,"99")
          iiLength = MIN(iiLength,12)
          lcDur    = SUBSTRING(lcDur,13 - iiLength).
          
   RETURN lcDur.
   
END FUNCTION.

FUNCTION fSec2MinC RETURNS CHARACTER
   (idTime   AS INT,
    iiLength AS INT):

   DEF VAR liMin   AS INT  NO-UNDO.
   DEF VAR liSec   AS INT  NO-UNDO.
   DEF VAR lcDur   AS CHAR NO-UNDO. 
   
   IF iiLength = 0 THEN RETURN "".

   ASSIGN liMin    = TRUNCATE(idTime / 60,0)
          liSec    = idTime MOD 60
          lcDur    = STRING(liMin,"->>>>>>99") + ":" +
                     STRING(liSec,"99")
          iiLength = MIN(iiLength,12)
          lcDur    = SUBSTRING(lcDur,13 - iiLength).
          
   RETURN lcDur.
   
END FUNCTION.
     
