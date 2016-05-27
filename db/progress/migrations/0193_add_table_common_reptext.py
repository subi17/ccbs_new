from gearbox.migrations import Migration

class AddTableRepText(Migration):

    database = "common"

    def up(self):
        t = self.table('RepText', area="Sta_Data_128", label="Report Texts", table_trigger=[{'crc': '?', 'procedure': 'rd-reptext.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-reptext.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="reptext", desc="Report texts")
        t.column('TextType', 'integer', format=">9", initial="0", max_width=4, label="InvType", column_label="InvType", position=2, order=10, help="Invoice Language Type")
        t.column('LinkValue', 'character', format="x(8)", initial="", max_width=16, label="Key", column_label="Key", position=3, order=20, help="Key Value")
        t.column('Language', 'integer', format=">9", initial="0", max_width=4, label="Language", column_label="Language", position=4, order=30, help="Code of Language")
        t.column('RepText', 'character', format="x(35)", initial="", max_width=70, label="Text", column_label="Text", position=5, order=40, help="Text")
        t.column('LinkNum', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Int", column_label="Int", position=6, order=50, help="Integer code value")
        t.column('LinkCode', 'character', format="x(30)", initial="", max_width=60, label="Char", column_label="Char", position=7, order=60, help="Character code value")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=9, order=80, help="Date when text becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=10, order=90, help="Date when text expires")
        t.index('LinkCode', [['Brand'], ['TextType'], ['LinkCode'], ['Language'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Language', [['Brand'], ['TextType'], ['Language'], ['LinkCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('RepText')
