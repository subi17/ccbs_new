let s:path = $PWD
while s:path != "/"
    if glob(s:path . "/tools/provim/ccbs.vim") != ""
        let ccbspath = s:path
        let s:path = "/"
        execute "source" . ccbspath . "/tools/provim/ccbs.vim"
"        execute "source" . ccbspath . "/tools/provim/abbrev_maps.vim"
    endi
    let s:path = fnamemodify(s:path, ":h")
endw

