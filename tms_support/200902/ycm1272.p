{rate_roamzone.i}

DEF VAR ldDate AS DATE NO-UNDO FORMAT "99-99-9999".
DEF VAR liCCN  AS INT  NO-UNDO.

lddate = today.

update lddate LABEL "Date" liccn LABEL "Type".
DEF VAR kpl     as i no-undo.
DEF VAR llleave as log no-undo.
DEF VAR lcimsi like mobcdr.imsi.
llleave = TRUE.
LOOP:
for each mobcdr no-lock where 
         mobcdr.datest = ldDAte .

   IF MObcdr.SPOCMT = liccn  THEN  DO:
   

      IF (fGetMcdrDtlValue(mobcdr.datest,mobcdr.dtlseq,"destination address") = "" AND liccn = 7) OR      
          fGetMcdrDtlValue(mobcdr.datest,mobcdr.dtlseq,"originating address") = ""  THEN DO:

         kpl = kpl + 1.
         if liccn = 7 THEN lcimsi = mobcdr.imsi2.
         else              lcimsi = mobcdr.imsi.
         
         disp Mobcdr.datest STRING(mobcdr.timest,"hh:mm:ss") Mobcdr.cli lcimsi  amount   WITH 15 down OVERLAY FRAME aa
         
         . down.
       

          if kpl mod 15 = 0 THEN DO:
              MESSAGE "conitnue?"  update llleave.
              
              if llleave = false THEN LEAVE loop.

          END.
      END.
   ENd.

END.