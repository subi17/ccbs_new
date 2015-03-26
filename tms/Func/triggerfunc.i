FUNCTION fTriggerRateQueue RETURN INTEGER.


   DEF VAR liCounter AS INT NO-UNDO.

   FOR EACH TriggerConf NO-LOCK  BREAK BY TriggerConf.Prior.

      FOR  EACH TriggerEvent NO-LOCK WHERE
                TriggerEvent.TriggerConfID   = TriggerConf.TriggerConfID AND
                TriggerEvent.StatusCode      = 4.
                             
         FOR EACH TriggerItem WHERE
                  TriggerItem.TriggerConfID  = TriggerConf.TriggerConfID    AND
                  TriggerItem.TriggerEventID = TriggerEvent.TriggerEventID  AND
                  TriggerItem.StatusCode     = 0 NO-LOCK.
                              
            liCounter = liCounter + 1.
         END.
                              
      END.
                            
   ENd.

   RETURN liCounter.                                    
                                       
END FUNCTION.
                                       


