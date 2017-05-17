from gearbox.migrations import Migration

class AddTableCLISeries(Migration):

    database = "star"

    def up(self):
        t = self.table('CLISeries', area="Sta_Data_256", dump_name="cliserie")
        t.column('CustNum', 'integer', format=">>>>>>9", initial="0", max_width=4, label="CustNumber", column_label="CustNumber", position=2, order=10, help="Customer who owns CLI Series")
        t.column('Series', 'character', format="x(10)", initial="", max_width=20, label="Series", column_label="Series", position=3, order=20, help="Number Series first digits")
        t.column('Name', 'character', format="x(20)", initial="", max_width=40, label="Name", column_label="Name", position=4, order=30, help="Name of number Series")
        t.index('Series', [['Series']], area="Sta_Index_2", primary=True)
        t.index('CustNum', [['CustNum'], ['Series']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CLISeries')
