from gearbox.migrations import Migration

class AddTableMNPRetentionRule(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPRetentionRule', area="Sta_Data_128", label="MNPRetentionRule", dump_name="mnpretentionrule", desc="MNP retention rules")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of Brand")
        t.column('RetentionRuleID', 'integer', format=">>>9", initial="0", max_width=4, label="ID", position=3, order=20)
        t.column('SegmentCode', 'character', format="x(8)", initial="", max_width=16, label="SegmentCode", position=4, order=30, help="Subscription segmentation code")
        t.column('CLIType', 'character', format="x(30)", initial="", max_width=60, label="CLIType", position=5, order=40, help="Subscription type")
        t.column('PenaltyMonthsLeft', 'integer', format=">>9", initial="0", max_width=4, label="PenaltyMonthsLeft", position=6, order=50, help="Penalty months remaining")
        t.column('PenaltyLeft', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=20, label="PenaltyLeft", position=7, order=60, help="Penalty amount in euros remaining")
        t.column('ConsumptionAverage', 'decimal', format="->>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Amount", position=8, order=70, help="Average amount of last X invoices")
        t.column('SMSText', 'character', format="x(60)", initial="", max_width=120, label="SMSText", position=9, order=80, help="SMS template text")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=10, order=90, help="Date when rule expires")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=11, order=100, help="Date when rule begins")
        t.index('RetentionRuleID', [['RetentionRuleID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPRetentionRule')
