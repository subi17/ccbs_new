from gearbox.migrations import Migration

class AddTableDiscountPlan(Migration):

    database = "common"

    def up(self):
        t = self.table('DiscountPlan', area="Sta_Data_64", label="Discount Plans", table_trigger=[{'crc': '?', 'procedure': 'rd-discountplan.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-discountplan.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="discountplan")
        t.column('Brand', 'character', format="x(12)", initial="", max_width=24, label="Brand", column_label="Brand", position=2, order=10, help="Brand")
        t.column('DPId', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Discount Plan Id", column_label="PlanId", position=3, order=20, help="Discount plan Id")
        t.column('Subject', 'character', format="x(32)", initial="", max_width=64, label="Determinative Subject", column_label="Subject", position=4, order=30, help="Discount plan subject")
        t.column('TargetType', 'character', format="x(16)", initial="", max_width=32, label="Targeted To", column_label="Target", position=5, order=40, help="Targeted to All or List of targets")
        t.column('DPUnit', 'character', format="x(8)", initial="", max_width=16, label="Discount Unit", column_label="Unit", position=6, order=50, help="Discount unit")
        t.column('Priority', 'integer', format=">>>9", initial="0", max_width=4, label="Priority", column_label="Prior.", position=7, order=60, help="Priority between discount plans")
        t.column('ProcessStopper', 'logical', format="Yes/No", initial="no", max_width=1, label="Process Stopper", column_label="Stopper", position=8, order=70, help="Process stopper for a lower priority plan")
        t.column('DPCurrency', 'character', format="x(16)", initial="", max_width=32, label="Currency", column_label="Curr.", position=9, order=80, help="Currency in which discount units are given")
        t.column('BillCode', 'character', format="x(32)", initial="", max_width=64, label="Billling Item", column_label="Bill.Item", position=10, order=90, help="Billing item for invoice rows")
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="From", position=11, order=100, help="Effective from date")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="To", position=12, order=110, help="Effective to date")
        t.column('DPRuleId', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="Discount Rule Id", column_label="Rule Id", position=13, order=120, help="Discount Rule Id")
        t.column('DPName', 'character', format="x(64)", initial="", max_width=128, label="Name", position=14, order=130, help="Plan name")
        t.column('DPMemo', 'character', format="x(40)", initial="", max_width=80, label="Memo", position=15, order=140, help="Memo")
        t.column('SubjectType', 'character', format="x(8)", initial="", max_width=16, label="Subject Type", column_label="Subj.Type", position=16, order=150, help="Used for All or for a List")
        t.column('MaxAmount', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Max Amount", column_label="Max", position=17, order=160, help="Maximum amount per period")
        t.column('MinBaseAmount', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Min Base Amount", column_label="Min Base", position=18, order=170, help="Minimum base amount for triggering discount")
        t.column('CCDisplay', 'integer', format="9", initial="0", max_width=4, label="CC Display", column_label="CC", position=19, order=180, help="CC tool visibility")
        t.column('ValidPeriods', 'integer', format=">>9", initial="0", max_width=4, label="Valid Periods", column_label="Periods", position=20, order=190, help="How many periods discount is valid for a member")
        t.index('DPRuleID', [['Brand'], ['DPRuleId'], ['ValidTo', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('DPId', [['DPId']], area="Sta_Index_1", unique=True)

    def down(self):
        self.drop_table('DiscountPlan')
