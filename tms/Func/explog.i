/* --------------------------------------------------
  MODULE .......: EXPLOG.I
  FUNCTION .....: Create - update export log PaymFile
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 22.02.00 kl
  MODIFIED .....: 
  Version ......: M15
------------------------------------------------------ */

{Mf/errors.i}
{Func/timestamp.i}

/* create - update time stamp */
function fPutExpLog returns logical
  (input xid as int, input xRepType as char, input xexp as char).

   find first ExpLog where
              ExpLog.ExpNum   = xid   AND
              ExpLog.DataType = xRepType AND
              ExpLog.ExpType  = xexp
   exclusive-lock no-error no-wait.

   /* not locked */
   if NOT fIsErr(errLocked) then do:
      /* doesn't exists */
      if fIsErr(errMissing) then do:
         create ExpLog.
         assign
            ExpLog.ExpNum   = xid
            ExpLog.DataType = xRepType
            ExpLog.ExpType  = xexp.
      end.
      assign ExpLog.ExpStamp = fMakeTS().

   end.

end.

/* return time stamp, zero if NOT exists */
function fGetExpLog returns decimal
  (input xid as int, input xRepType as char, input xexp as char).

   def var ret as de no-undo init 0 format "99999999.99999".

   find first ExpLog where
              ExpLog.ExpNum   = xid   AND
              ExpLog.DataType = xRepType AND
              ExpLog.ExpType  = xexp
   no-lock no-error.

   if avail ExpLog then ret = ExpLog.ExpStamp.

   return ret.

end.
