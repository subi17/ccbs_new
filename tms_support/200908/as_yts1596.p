for first invoice NO-LOCK  where
   invoice.brand = "1" and
/*   invoice.extinvid = "191D02674023":*/
   invoice.extinvid = "191D02250720": 
  
  disp invoice.extinvid invoice.invdate invoice.duedate.
  invoice.duedate = 8/7/2009.
end.

