find order where
   order.brand = "1" and
   order.orderid = 2711842 NO-LOCK.
find sim where
   sim.icc = order.icc and
   sim.simstat = 21 EXCLUSIVE-LOCK.

sim.simstat = 20.
disp order.crstamp order.cli order.icc sim.simstat.
pause.

release sim.

find order where
   order.brand = "1" and
   order.orderid = 2711986 NO-LOCK.
find sim where
   sim.icc = order.icc and
   sim.simstat = 21 EXCLUSIVE-LOCK.

sim.simstat = 20.
disp order.crstamp order.cli order.icc sim.simstat.
