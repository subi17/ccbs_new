from gearbox.migrations import Migration

class AddTableDPSubject(Migration):

    database = "common"

    def up(self):
        t = self.table('DPSubject', area="Sta_Data_128", label="Discount Plan Subject", dump_name="dpsubject")
        t.column('DPId', 'integer', mandatory=True, format="zzzzzzz9", initial="0", max_width=4, label="Discount Plan Id", column_label="PlanId", position=2, order=10, help="Discount Plan Id")
        t.column('DPSubject', 'character', format="x(16)", initial="", max_width=32, label="Subject", position=3, order=20, help="Discount plan subject")
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Effective from date")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="To", position=5, order=40, help="Effective to date")
        t.index('DPSubject', [['DPId'], ['DPSubject'], ['ValidTo', 'DESC']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('DPSubject')
