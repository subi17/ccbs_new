from gearbox.migrations import Migration

class AddTablestarParam(Migration):

    database = "star"

    def up(self):
        t = self.table('starParam', area="Sta_Data_256", dump_name="starpara")
        t.column('groupCode', 'character', format="X(20)", initial="", max_width=40, label="Group", position=2, order=10, help="Group code")
        t.column('paramCode', 'character', format="X(20)", initial="", max_width=16, label="Code", position=3, order=20, help="Code")
        t.column('c_value', 'character', format="X(50)", initial="", max_width=1020, label="Character", extent=10, position=4, order=30, help="Character values")
        t.column('i_value', 'integer', format=">>>>>>>>9", initial="0", max_width=200, label="Integer", extent=10, position=5, order=40, help="Intger values")
        t.column('de_value', 'decimal', format="->>>>>>>>>>>9.9<<<<<<<<", decimals=9, initial="0", max_width=320, label="Decimal", extent=10, position=6, order=50)
        t.column('da_value', 'date', format="99.99.99", initial=self.unknown, max_width=180, label="Date", extent=10, position=7, order=60, help="Date values")
        t.column('l_value', 'logical', format="yes/no", initial="no", max_width=80, label="Logical", extent=10, position=8, order=70, help="Locigal values")
        t.column('memo', 'character', format="X(50)", initial="", max_width=100, label="Memo", position=9, order=80, help="Memo of parameter")
        t.index('paramCode', [['groupCode'], ['paramCode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('starParam')
