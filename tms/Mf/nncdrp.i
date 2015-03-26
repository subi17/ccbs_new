
FUNCTION fComment RETURNS CHAR
   (INPUT prm AS INT).

case prm:
when 1 THEN RETURN 
"# CDR config file"                                + nlx +
"# Sections:"                                      + nlx +
"#  IDENT       name of this switch"               + nlx +
"#  VALID       used from this  date on"           + nlx +
"#  INDIRECT    indirect prefixes"                 + nlx +
"#  OPTIONS     option flags"                      + nlx +
"#  SECTIONS    prefix_section to call_type pairs" + nlx +
"#  CGRANGE     circuit groups"                    + nlx +
"#  AREACODE    arecodes (if used)"                + nlx +
"#  FREECALL    free call (020, 800, ...)"         + nlx + nlx +
"# Self defined prefix_section_to_call_type"       + nlx +
"# sections in SECTIONS (example)"                 + nlx +
"#  ENQUIRY     number enquire etc..."             + nlx +
"#  MOBILE      mobile prefixes"                   + nlx +
"#  + some country related special cases"          + nlx + nlx +
"#  STOCK       stockholm switch"                  + nlx +
"#  GOTH        gothenburg switch"                 + nlx +
"#  OSLO        oslo switch"                       + nlx +
"#  MALMO       malmo switch"                      + nlx +
"#  COPEN       Copenhagen switch"                 + nlx +
"#  HELSI       Helsinki switch"                   + nlx.

when 2 THEN RETURN 
"# This will set the date from which on dated cdr will be handled" + nlx +
"# by this configuration file. If there is an other file with"     + nlx +
"# different extension (other wice the same name) and with later"  + nlx +
"# valid date, this file is used till that date, after which that" + nlx +
"# file will be used and so on."                                   + nlx +
"#"                                                                + nlx +
"# The format of the date is (presicely):"                         + nlx +
"# dd.mo.year hh:mm:ss"                                            + nlx +
"# eg."                                                            + nlx +
"# 12.03.1999 21:35:22"                                            + nlx.

when 3 THEN RETURN
"# Here are the prefixes that Tele1 indirect customers use" + nlx +
"# PREFIX   TYPE    ABS.TYPE"                               + nlx.
when 4 THEN RETURN
"# Here is the list of operators taken from the database" + nlx +
"# Name     Code  Ported Prefix"                          + nlx.

when 5 THEN RETURN
"# Here are the prefixes that the ported numbers have" + nlx.

when 6 THEN RETURN
"# Here are option flags (one per line)"         + nlx +
"# The only one recognized at the moment is ORE" + nlx +
"# ORE = special handling for Orebro connection" + nlx.

when 7 THEN RETURN 
"# Here we define our own sections."                                   + nlx +
"# Any names (max 20 char) can be given"                               + nlx +
"# to these sections."                                                 + nlx +
"#   call types  0, 1 and 2 are reserved"                              + nlx +
"#   0  = ordinary, 1 = international, 2 = freecall (020, 0800, ...)"  + nlx +
"#   A  = A-number type"                                               + nlx +
"#   B  = B-number type"                                               + nlx +
"#   AB = both A- and B-number type"                                   + nlx +
"#section_name        A/B-num        call_type       info"             + nlx.

when 8 THEN RETURN 
"# circuit group definitions"                                   + nlx +
"# ctaregory: S = tele1 switch, O = other operator,"            + nlx +
"#            D = other direcly connected, I = internal (0);"   + nlx.

when 9 THEN RETURN 
"#categ  name              cgr range       id  type " + nlx.

when 10 THEN RETURN
"# Defaults" + nlx.

otherwise return "".

END.

END.


