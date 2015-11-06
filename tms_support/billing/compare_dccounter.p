DEF INPUT PARAMETER icCli        LIKE Mobsub.CLI          NO-UNDO.
DEF INPUT PARAMETER icEvent      LIKE Daycampaign.DCEvent NO-UNDO.
DEF INPUT PARAMETER idtDate      LIKE Dccounter.dcdate    NO-UNDO.
DEF INPUt PARAMETER llFullPacket AS LOG                   NO-UNDO.

DEF VAR ldAmount   AS DEC NO-UNDO.
DEF VAR liKpl      AS INT NO-UNDO.
DEF VAR liduration AS INT NO-UNDO.
DEF VAR ldedata    as dec no-undo.

FOR EACH dccli NO-LOCK where 
         dccli.Brand   = "1"      AND 
         dccli.dcevent = icEvent  .
   FOR EACH dccounter where
            dccounter.dcdate >= idtdate      AND 
            dccounter.msseq   = dccli.msseq  and 
            dccounter.dcevent = icevent NO-LOCK.

      IF llFullPacket AND 
          dccounter.amount ne dccounter.MaxCharge THEN NEXT.
 
      IF NOT llFullPacket AND
         dccounter.amount = dccounter.MaxCharge THEN NEXT.

      FIND FIRST msowner where 
                 msowner.msseq = dccounter.msseq no-lock no-error.
      DISP
      "Counter:"
      msowner.cli
      dccounter.dcdate
      dccounter.dcevent
      dccounter.amount FORMAT ">>>>>>9.999999"
      dccounter.dctarget
      with frame ab.
                           
      FOR EACH mobcdr NO-LOCK where 
               mobcdr.cli       = dccli.cli       AND 
               Mobcdr.Datest    = dccounter.dcdate  AND 
               mobcdr.billcode  = dccounter.dctarget        
      BREAK
      BY Mobcdr.Rateccn
      BY Mobcdr.BillCode
      BY Mobcdr.Bdest .
               
         ASSIGN
         ldamount   = ldamount + mobcdr.amount
         likpl      = likpl + 1
         liduration = liduration + mobcdr.billdur
         ldeDAta    = ldedata + mobcdr.datain + mobcdr.dataout.

         IF LAST-OF(mobcdr.bdest) THEN DO:

            disp 
               mobcdr.datest 
               mobcdr.billcode
               MObcdr.bdest
               mobcdr.rateccn  FORMAT ">>9" column-label "CCN"
               ldamount 
               likpl FORMAT ">>>9" column-label "qty"
               liduration  FORMAT ">>>>9" column-label "sek"
               ldedata  / 1024 / 1024 "Mb" no-label
               WITH 5 down  FRAME AA. 
               down.   
               MESSAGE 
               "PRESS ENTER TO CONTINUE" . PAUSE no-message.
            ASSIGN
               ldamount  = 0
               liduration = 0
               likpl      = 0
               liduration = 0
               ldedata    = 0.

         END.
      END.
   END.
END.
