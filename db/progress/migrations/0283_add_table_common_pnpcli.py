from gearbox.migrations import Migration

class AddTablePNPCLI(Migration):

    database = "common"

    def up(self):
        t = self.table('PNPCLI', area="Sta_Data_64", label="PNP CLI", dump_name="pnpcli", desc='''PNP CLI
''')
        t.column('PNPGroup', 'character', format="x(10)", initial="", max_width=20, label="PNP Group", column_label="Group", position=2, order=10, help="PNP group")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", help="Subscription ID", max_width=4, label="Subscription ID", column_label="Subscr.", position=3, order=20, description='''

''')
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, help="Valid from", max_width=4, label="Begin Date", column_label="From", position=4, order=30, description='''


''')
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, help="Valid to", max_width=4, label="End Date", column_label="To", position=5, order=40, description='''
''')
        t.column('PNPSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Sequence", column_label="Seq", position=6, order=50, help="PNP Sequence")
        t.index('MsSeq', [['MSSeq'], ['PNPGroup'], ['ValidFrom', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PNPGroup', [['PNPGroup'], ['MSSeq'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('PNPSeq', [['PNPSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PNPCLI')
