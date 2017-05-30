from gearbox.migrations import Migration

class AddTableFinBank(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('FinBank', area="Sta_Data_256", dump_name="finbank", desc="Parameter data for the bank interfaces (eTupas, eMaksu)")
        t.column('bankname', 'character', format="x(11)", initial="", max_width=22, position=2, order=10, help="Name of the bank company")
        t.column('interface', 'character', format="x(6)", initial="", max_width=12, position=3, order=20, help="etupas or emaksu")
        t.column('paramname', 'character', format="x(9)", initial="", max_width=18, position=4, order=30, help="Name of the parameter")
        t.column('paramvalue', 'character', format="x(20)", initial="", max_width=40, position=5, order=40, help="Value of the parameter")
        t.column('postform', 'logical', format="yes/no", initial="yes", max_width=1, position=6, order=50, help="Whether the entry must be a A01Y_ form-value")
        t.column('macposition', 'integer', format="->,>>>,>>9", initial="-1", max_width=4, position=7, order=60, help="Position of this value in the premac-string")
        t.index('bank', [['bankname'], ['interface'], ['paramname']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('FinBank')
