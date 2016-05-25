from gearbox.migrations import Migration

class AddTableMNPCancelProposal(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPCancelProposal', area="Sta_Data_128", dump_name="mnpcancel")
        t.column('ReferenceCode', 'character', format="x(5)", initial="", max_width=10, label="ReferenceCode", column_label="ReferenceCode", position=2, order=10)
        t.column('StatusCode', 'integer', format=">9", initial="0", max_width=4, label="StatusCode", column_label="StatusCode", position=3, order=20)
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=4, order=30)
        t.column('MNPSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="MNPSeq", column_label="MNPSeq", position=5, order=40)
        t.column('AttachmentFile', 'character', format="x(45)", initial="", max_width=90, label="AttachmentFile", column_label="AttachmentFile", position=6, order=50)
        t.column('Pdf', 'blob', format="x(8)", lob_size="1M", lob_bytes=1048576, lob_area="Lob_Data", position=7, order=60)
        t.index('MNPSeq', [['MNPSeq'], ['CreatedTS', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('StatusCode', [['StatusCode'], ['CreatedTS', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPCancelProposal')
