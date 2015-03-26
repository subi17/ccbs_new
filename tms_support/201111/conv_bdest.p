
def var liid as int no-undo.

for each bdest no-lock:
   liid = max(liid,bdest.bdestid).
end.   

for each bdest 
by bdest.bdestid desc:

   if bdest.bdestid = 0 then assign
      liid = liid + 1
      bdest.bdestid = liid.

   if bdest.fromdate = ? then bdest.fromdate = 12/1/6. 
   
   for each reptext exclusive-lock where
      reptext.brand = bdest.brand and
      reptext.texttype = 2 and
      reptext.linkcode = bdest.bdest and
      reptext.linknum = 0:
      
      assign
         reptext.linkcode = string(bdest.bdestid)
         reptext.linknum = bdest.bdestid.
   end.

   for each rateccn exclusive-lock where
        rateccn.brand = bdest.brand and
        rateccn.bdest = bdest.bdest and
        rateccn.bdestid = 0:
      assign
         rateccn.bdestid = bdest.bdestid
         rateccn.desttype = bdest.desttype.
   end.
end.