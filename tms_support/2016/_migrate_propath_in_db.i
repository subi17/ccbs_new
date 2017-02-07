   if {2} ne "" then do:
      lcModule = search({2}).
      if lcModule eq ? then
         lcModule = search({2} + ".p").
   end.
   else
      lcModule = search({1} + ".p").

   if lcModule eq ? then do:
      put stream serr unformatted
         "{1}" "|"
         {1} skip.
      next.
   end.
   lcModuleWithPath = replace(lcModule,lcTMSRoot,"").

   lcModuleWithPath = replace({1},
                              (if {2} > "" then {2} else {1}),
                              replace(lcModule,lcTMSRoot,"")).
   if "{1}" eq "menutree.module" then
      lcModuleWithPath = substring(lcModuleWithPath,1,
                                  length(lcModuleWithPath) - 2).

   put stream sout unformatted
      "{1}" "|"
      {1} "|"
      lcModuleWithPath skip.

   if not llSimulate then
      {1} = lcModuleWithPath.

