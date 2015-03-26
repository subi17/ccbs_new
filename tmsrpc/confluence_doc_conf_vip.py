
import re
def replace_from(old_text, new_text, replace_start):
# temp solution, replace always the whole page
   return new_text
#    match = re.search(replace_start, old_text, re.M)
#    if match is None:
#        raise StandardError("Didn't find replace anchor in current page")
#    return old_text[:match.end() + 1] + new_text

# applications = {
#      dirname:  [ (confluence_url, space|page or pageId,
#                   edit_function), ... ]
# def edit_function(old_text, new_text):
#    return new_text

applications = { 
        '.':  [('https://luna.qvantel.net',
                'Yoigo|VIP and IVR TOOL External API Documentation',
                 lambda ot, nt: replace_from(ot, nt, 'Generated documentation starts here:'))],
#                 None)],
        }
