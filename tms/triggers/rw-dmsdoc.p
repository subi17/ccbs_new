{triggers/hpdwrite_generic.i DMSDoc DMSDOC Ordercanal DMSID DocTypeID DocStatusTS}

DMSDoc.DocStatusTS = (YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + (TIME / 100000).