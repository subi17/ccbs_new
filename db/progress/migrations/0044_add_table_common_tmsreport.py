from gearbox.migrations import Migration

class AddTableTMSReport(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSReport', area="Sta_Data_128", label="Reports", dump_name="tmsrepor", desc="Reports")
        t.column('RepName', 'character', format="x(8)", initial="", max_width=16, label="Printname", column_label="Printname", position=2, order=10, help="Name of a printout")
        t.column('Memo', 'character', format="x(32)", initial="", max_width=64, label="PrtExplanation", column_label="PrtExplanation", position=3, order=20, help="Name of a printout")
        t.column('PageWidth', 'integer', format="ZZ9", initial="0", max_width=4, label="Width", column_label="Width", position=4, order=80, help="Maximum number of characters per line on printer")
        t.column('UpdPerm', 'logical', format="Yes/No", initial="yes", max_width=1, label="Ask", column_label="Ask", position=5, order=90, help="Ask printer's setup when starting to print (Y/N)")
        t.column('PrintQty', 'integer', format="zzzzz9", initial="0", max_width=4, label="MaxNo", column_label="MaxNo", position=6, order=100, help="Amount of data when normal user's print will be disrupted")
        t.column('EMail', 'character', format="x(40)", initial="", max_width=80, label="E-mail", column_label="E-mail", position=7, order=110, help="Customer's e-mail address")
        t.column('ChEMail', 'logical', format="yes/no", initial="no", max_width=1, position=8, order=120, help="Can user change the eMail address")
        t.index('RepName', [['RepName']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TMSReport')
