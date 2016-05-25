from gearbox.migrations import Migration

class AddTableTFConf(Migration):

    database = "common"

    def up(self):
        t = self.table('TFConf', area="Sta_Data_128", label="TFConf", dump_name="tfconf", desc="Terminal financial configuration values")
        t.column('TFConfId', 'integer', format=">>>>9", initial="0", max_width=4, label="TFConfId", position=2, order=10, help="Unique Id")
        t.column('RVPercentage', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="RVPercentage", position=3, order=20, help="Terminal Residual value percentage to")
        t.column('CommFeePerc', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="CommFeePerc", position=4, order=30, help="Commission fee percentage")
        t.column('TAE', 'decimal', format=">>9.99", decimals=2, initial="0", max_width=17, label="TAE", position=5, order=40)
        t.column('PaytermCode', 'character', format="X(5)", initial="", max_width=10, label="PaytermCode", position=6, order=50)
        t.column('ResidualCode', 'character', format="X(5)", initial="", max_width=10, label="ResidualCode", position=7, order=60)
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="ValidFrom", position=8, order=70, help="Date when configuration becomes active")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="ValidTo", position=9, order=80, help="Date after configuration becomes inactive")
        t.index('TFConfID', [['TFConfId']], area="Sta_Index_2", primary=True, unique=True)
        t.index('RVPercentage', [['RVPercentage'], ['ValidTo', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TFConf')
