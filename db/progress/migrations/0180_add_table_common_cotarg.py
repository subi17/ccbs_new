from gearbox.migrations import Migration

class AddTableCoTarg(Migration):

    database = "common"

    def up(self):
        t = self.table('CoTarg', area="Sta_Data_128", dump_name="cotarg")
        t.column('CoRuleID', 'integer', format=">>>>>9", initial="0", max_width=4, label="Rule ID", column_label="RuleID", position=2, order=10, help="Commissiong rule ID")
        t.column('TargType', 'character', format="X(1)", initial="", help="Commission Target Type. Valid values are C, S or R", max_width=2, label="Target Type", column_label="Target Type", position=3, order=20, description="Valid values are (C)ustomer, (S)alesman or (R)eseller")
        t.column('CoTarg', 'character', format="X(10)", initial="", max_width=20, label="Commission Target", column_label="Commission Target", position=4, order=30, help="Commission Target Code based on Target type")
        t.column('RsLevel', 'integer', format=">9", initial="0", max_width=4, label="Reseller Level", column_label="RSLevel", position=7, order=60, help="Salesman's level in reseller's organization")
        t.column('CoTargId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Target ID", column_label="TargID", position=8, order=70, help="Commission target ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=101, help="Code Of Brand")
        t.column('PromotedID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Promoted ID", column_label="Prom.ID", position=13, order=111, help="Promoted ID")
        t.column('PromotedCLI', 'character', format="x(12)", initial="", max_width=24, label="Promoted MSISDN", column_label="Prom.MSISDN", position=14, order=121, help="Promoted MSISDN")
        t.column('CommStatus', 'integer', format=">9", initial="0", max_width=4, label="Commission Status", column_label="Status", position=15, order=131, help="Commission status")
        t.column('StatusReason', 'integer', format=">9", initial="0", max_width=4, label="Status Reason", column_label="Reason", position=16, order=141, help="Reason for current status")
        t.column('HandledTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Handled", position=17, order=151, help="Latest handling time")
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", help="Order sequence number", max_width=4, label="OrderId", column_label="OrderId", position=18, order=161, description="Order sequence number")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Created", position=19, order=170, help="Added to commission queue")
        t.index('CoRuleID', [['Brand'], ['CoRuleID'], ['TargType'], ['CoTarg']], area="Sta_Index_2", primary=True)
        t.index('CommStatus', [['Brand'], ['CommStatus'], ['CoTarg']], area="Sta_Index_2")
        t.index('CoTargID', [['CoTargId']], area="Sta_Index_2", unique=True)
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_2")
        t.index('PromotedID', [['Brand'], ['PromotedID']], area="Sta_Index_2")
        t.index('TargType', [['Brand'], ['TargType'], ['CoTarg']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CoTarg')
