from gearbox.migrations import Migration

class AddTableCDRCounter(Migration):

    database = "common"

    def up(self):
        t = self.table('CDRCounter', area="Dyn_Data_128", label="CDR Counter", dump_name="cdrcount", desc="Counters for CDR import")
        t.column('CDRType', 'integer', format=">>>>9", initial="0", help="CDR type", max_width=4, label="CDR Type", column_label="Type", position=2, order=10, description='''
''')
        t.column('ImportDate', 'date', format="99-99-99", max_width=4, label="Import Date", column_label="Import", position=3, order=20, help="Import date")
        t.column('Qty', 'integer', format=">>>>>>>9", initial="0", help="Quantity", max_width=4, label="Quantity", column_label="Qty", position=4, order=30, description='''



''')
        t.column('Duration', 'integer', format=">>>>>>>>9", initial="0", help="Duration", max_width=4, label="Duration", column_label="Durat", position=5, order=40, description='''

''')
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", help="Amount", max_width=17, label="Amount", column_label="Amt", position=6, order=50, description='''

''')
        t.index('ImportDate', [['ImportDate', 'DESC'], ['CDRType']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CDRType', [['CDRType'], ['ImportDate', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('CDRCounter')
