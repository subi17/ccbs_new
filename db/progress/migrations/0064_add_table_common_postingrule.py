from gearbox.migrations import Migration

class AddTablePostingRule(Migration):

    database = "common"

    def up(self):
        t = self.table('PostingRule', area="Sta_Data_256", label='''Posting Rules
''', dump_name="postingr", desc='''Posting rules
''')
        t.column('BIGroup', 'character', format="x(8)", initial="", max_width=16, label="Product group", column_label="Prod.grp", position=2, order=10, help="Product group")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Effective date", column_label="Eff.date", position=3, order=20, help="Date when rule becomes effective")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Account", column_label="Acc", position=4, order=30, help="Account number")
        t.column('UnbAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Acc. for unb.", column_label="Unb.Acc", position=5, order=40, help="Account number for unbilled events")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('Pgcode', [['Brand'], ['BIGroup'], ['FromDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Account', [['Brand'], ['AccNum']], area="Sta_Index_2")
        t.index('FromDate', [['Brand'], ['FromDate', 'DESC'], ['BIGroup']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PostingRule')
