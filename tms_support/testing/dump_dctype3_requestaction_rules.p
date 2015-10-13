def var lcdel as char no-undo init "|".

output to "/apps/yoigo/tms_support/testing/dump_dctype3_requestaction_rules.txt".

put unformatted "ReqType" lcdel "New ClIType" lcdel "PayType" lcdel "ParamField"lcdel "IncludeParam" lcdel "ValidFrom" lcdel "ValidTo" skip.

for each requestaction where
         requestaction.Brand = "1" and
         lookup(string(requestaction.reqtype),"0,81") > 0 and
         requestaction.ActionType = "DCType" and
         requestaction.ActionKey = "3" and
         requestaction.Action = 2 and
         requestaction.ValidTo >= today no-lock,
    each requestactionrule where
         requestactionrule.RequestActionID = requestaction.RequestActionID and
         requestactionrule.ToDate >= today no-lock:
    put unformatted string(requestaction.reqtype) lcdel
                    requestaction.clitype lcdel
                    string(requestaction.PayType) lcdel
                    requestactionrule.ParamField lcdel
                    requestactionrule.ParamValue lcdel
                    string(requestactionrule.FromDate) lcdel
                    string(requestactionrule.ToDate) skip.
end.
output close.
