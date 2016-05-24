from gearbox.migrations import Migration

class AddTableTerminalConf(Migration):

    database = "common"

    def up(self):
        t = self.table('TerminalConf', area="Sta_Data_128", label="TerminalConf", dump_name="terminalconf", desc="Terminal Configurations")
        t.column('TerminalCode', 'character', format="x(10)", initial="", max_width=20, label="Terminal Code", column_label="TerminalCode", position=2, order=10, help="Terminal product code")
        t.column('DextraPrice', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="Dextra Price", column_label="DextraPrice", position=3, order=20, help="Terminal price agree between Yoigo and Dextra")
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="ValidFrom", position=4, order=30, help="First effective day")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="ValidTo", position=5, order=40, help="Last effective day")
        t.index('TerminalCode', [['TerminalCode'], ['ValidTo', 'DESC']], area="Sta_Index_3", primary=True)
        t.index('ValidTo', [['ValidTo', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('TerminalConf')
