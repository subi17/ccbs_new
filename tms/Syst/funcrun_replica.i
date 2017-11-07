/* funcrun_replica.i    20.12.12/aam 
*/

{Syst/commali.i}
{Func/cparam2.i}

DEF VAR liReplPortion AS INT  NO-UNDO.
DEF VAR liReplVolume  AS INT  NO-UNDO.

   
FUNCTION fInitReplicaSettings RETURNS CHAR
   (iiUseReplica AS INT,
    INPUT-OUTPUT ioBatchQty AS INT):

   DEF VAR lcReplica AS CHAR NO-UNDO. 

   IF iiUseReplica = 0 THEN RETURN "".
   
   /* if both production and replica are used then double the qty of batches */
   IF iiUseReplica = 1 THEN ioBatchQty = ioBatchQty * 2. 
      
   lcReplica = fCParamC("ReplicaHost").
   IF lcReplica = ? OR lcReplica = "" THEN 
      RETURN "ERROR:Replica host has not been defined".
         
   ASSIGN 
      liReplPortion = fCParamI("ReplicaPortion")
      liReplVolume  = fCParamI("ReplicaVolume").
   IF liReplPortion = ? OR liReplPortion = 0 THEN 
      liReplPortion = 50. 
   IF liReplVolume = ? OR liReplVolume = 0 THEN 
      liReplVolume = 100. 

   RETURN lcReplica.
   
END FUNCTION.

FUNCTION fCalculateReplicaQty RETURNS LOGIC
   (iiUseReplica AS INT,
    iiBatchQty   AS INT,
    iiTotalQty   AS INT,
    INPUT-OUTPUT ioMainLimit AS INT,
    OUTPUT oiReplQty   AS INT,
    OUTPUT oiReplLimit AS INT):

   DEF VAR liReplTotal AS INT  NO-UNDO.
   
   /* 1 = use both production and replica */
   IF iiUseReplica = 1 THEN DO:

      IF liReplPortion > 0 AND liReplPortion < 100 THEN 
         oiReplQty = iiBatchQty * liReplPortion / 100.
      ELSE RETURN FALSE.   

      IF liReplVolume = 50 THEN oiReplLimit = ioMainLimit.
      ELSE IF liReplVolume > 0 AND liReplVolume < 100 THEN ASSIGN
         liReplTotal = iiTotalQty * liReplVolume / 100
         oiReplLimit = liReplTotal / oiReplQty
         ioMainLimit = (iiTotalQty - liReplTotal) / (iiBatchQty - oiReplQty).
   END.

END FUNCTION.
 