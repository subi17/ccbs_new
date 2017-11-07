from gearbox.migrations import Migration

class AddTableDFField(Migration):

    database = "common"

    def up(self):
        t = self.table('DFField', area="Sta_Data_128", label="Dump File Field", dump_name="dffield", desc='''Fields in a dump file
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('DumpID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump ID", column_label="ID", position=3, order=30, help="Unique ID")
        t.column('DFTable', 'character', format="x(20)", initial="", max_width=40, label="Table Name", column_label="Table", position=4, order=40, help="Table name")
        t.column('DFField', 'character', format="x(20)", initial="", max_width=40, label="Field Name", column_label="Field", position=5, order=50, help="Field name")
        t.column('DFLabel', 'character', format="x(30)", initial="", max_width=60, label="Label", position=6, order=60, help="Label")
        t.column('OrderNbr', 'integer', format=">>>9", initial="0", max_width=4, label="Order Number", column_label="Order", position=7, order=70, help="Order number in file")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=8, order=80, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=9, order=90, help="Valid to")
        t.index('DumpID', [['DumpID'], ['OrderNbr']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('DFField')
