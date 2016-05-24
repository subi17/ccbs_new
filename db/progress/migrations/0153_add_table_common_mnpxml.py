from gearbox.migrations import Migration

class AddTableMNPXml(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPXml', area="Sta_Data_256", dump_name="mnpxml")
        t.column('XMLSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="XMLSeq", column_label="XMLSeq", position=2, order=10)
        t.column('RawMessage', 'clob', clob_collation="basic", format="x(8)", lob_size=1M, lob_bytes=1048576, lob_area="Lob_Data", position=3, clob_type=1, order=20, clob_codepage="utf-8", description="Raw mnp message")
        t.index('XMLSeq', [['XMLSeq']], area="Dyn_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('MNPXml')
