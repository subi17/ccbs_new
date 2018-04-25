from gearbox.migrations import Migration

class AddTableCCRule(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('CCRule')
        t.column('CCRuleID', 'character', format="X(8)", initial="", max_width=16, label="Rule ID", column_label="Rule ID", position=7, order=60, help="ID of the CCRULE Record")
        t.column('BillCode', 'character', format="X(16)", initial="", max_width=32, label="BillItem", column_label="BillItem", position=8, order=70, help="BillCode for the CCRule record")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="InEff. Date", column_label="InEff. Date", position=9, order=80, help="Date when rule becomes ineffective")
        t.column('AccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Account", column_label="Account", position=10, order=90, help="Account Number")
        t.column('EUAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="EU Sales Account", column_label="EU", position=11, order=100, help="Account number for EU sales")
        t.column('EUConAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="EU Cons. Sales", column_label="EUCon", position=12, order=110, help="Account number for EU sales with VAT")
        t.column('FSAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Foreign Sales Account", column_label="Foreign", position=13, order=120, help="Account number for sales outside EU")
        t.column('ReportingID', 'character', format="X(8)", initial="", max_width=16, label="Reporting ID", column_label="Reporting ID", position=14, order=130, help="Reporting ID")
        t.column('CLIType', 'character', format="X(8)", initial="", max_width=26, label="CLIType", column_label="CLIType", position=15, order=140, help="CLIType")
        t.column('AccountingID', 'character', format="X(12)", initial="", max_width=24, label="AccountingID", column_label="AccountingID", position=16, order=150, help="AccountingID")
        t.index('CCRuleID', [['CCRuleID']], area="Dyn_Index_1", unique=True)
        t.index('BillCode', [['Brand'], ['BillCode'], ['ValidTo']], area="Dyn_Index_1")
        t.index('CLIType', [['Brand'], ['CLIType'], ['Category'], ['ValidTo']], area="Dyn_Index_1")
        t.alter_index('Category', [['Brand'], ['Category'], ['BillCode'], ['CLIType'], ['ValidTo']], area="Dyn_Index_1", primary=True, unique=True)

    def down(self):
        t = self.alter_table('CCRule')
        t.alter_index('Category', [['Brand'], ['Category'], ['BIGroup'], ['ValidFrom']], area="Dyn_Index_1", primary=True, unique=True)
        t.drop_index('Pgcode')
        t.drop_column('BIGroup')
        