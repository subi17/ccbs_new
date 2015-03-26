/* subsrep.i        21.08.03/aam

   print amounts to subscription report
*/


    PUT STREAM tul 
       ({1} ttBal.Qty)          FORMAT "->>>>>9"       TO 48
       ({1} ttBal.Pulse)        FORMAT "->>>>>9"       TO 56
       ({1} ttBal.Sec) / 60     FORMAT "->>>>>9"       TO 64
       ({1} ttBal.Amt)          FORMAT "->>>>>>>9.99"  TO 77
       ({1} ttBal.VatAmt)       FORMAT "->>>>>>>9.99"  TO 90
       ({1} ttBal.CCost)        FORMAT "->>>>>>>9.999" TO 104
       (IF ({1} ttBal.Amt) NE 0
        THEN 100 * (({1} ttBal.Amt) - ({1} ttBal.CCost)) / ({1} ttBal.Amt)
        ELSE 0)                 FORMAT "->>9.99"      TO 112
       SKIP.

