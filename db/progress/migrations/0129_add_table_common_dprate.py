from gearbox.migrations import Migration

class AddTableDPRate(Migration):

    database = "common"

    def up(self):
        t = self.table('DPRate', area="Sta_Data_128", label="Discount Plan Rate", dump_name="dprate", desc="This table will contain actual amount/percentage")
        t.column('DPId', 'integer', mandatory=True, format="zzzzzzz9", initial="0", max_width=4, label="Discount Plan Id", column_label="PlanId", position=2, order=10, help="Discount Plan Id")
        t.column('DiscValue', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Discount Value", column_label="Value", position=3, order=20, help="Discount amount")
        t.column('ValidFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Effective from date")
        t.column('ValidTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=5, order=40, help="Effective to date")
        t.index('DPId', [['DPId']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('DPRate')
