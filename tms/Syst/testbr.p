/* testbr.p     03.10.03/aam 
*/

&GLOBAL-DEFINE BrTable Brand 
&GLOBAL-DEFINE BrFrame tstfram
&GLOBAL-DEFINE GenBrowseVariables NO

{Syst/commali.i}

FORM 
   "" AT 2 WITH NO-LABELS FRAME tstfram.

{Func/brand.i}

assign lcBrand = gcBrand
       gcBrand = "".

/* run this in order to change propath etc. */
fChgBrand(lcBrand,  
          ?).

