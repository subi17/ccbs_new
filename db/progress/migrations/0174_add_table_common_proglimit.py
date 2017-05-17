from gearbox.migrations import Migration

class AddTableProgLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('ProgLimit', area="Sta_Data_128", dump_name="proglimit")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=7, order=20, help="Sequence for Servicelimit")
        t.column('GroupCode', 'character', format="x(16)", initial="", max_width=32, label="GroupCode", column_label="Group Code", position=10, order=10, help="Group Code of Servicelimit")
        t.column('LimitFrom', 'decimal', format=">>>>>>>>9.999999", decimals=6, initial="0", max_width=20, label="LimitFrom", column_label="LimitFrom", position=11, order=120, help="Limit value from")
        t.column('LimitTo', 'decimal', format=">>>>>>>>9.999999", decimals=6, initial="0", max_width=20, label="LimitTo", column_label="LimitTo", position=12, order=140, help="Limit value to")
        t.column('BDest', 'character', format="x(16)", initial="", max_width=32, label="BDest", column_label="Bdest", position=15, order=160, help="Billing Destination")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", position=18, order=60, help="Valid from this date on")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", position=19, order=70, help="Valid until this date")
        t.index('ValidTo', [['GroupCode'], ['SLSeq'], ['ValidTo', 'DESC'], ['LimitTo']], area="Sta_Index_2", primary=True)
        t.index('LimitTo', [['GroupCode'], ['SLSeq'], ['LimitTo'], ['ValidFrom']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ProgLimit')
