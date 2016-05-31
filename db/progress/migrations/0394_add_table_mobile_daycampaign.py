from gearbox.migrations import Migration

class AddTableDayCampaign(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DayCampaign', area="Sta_Data_256", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-daycampaign.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-daycampaign.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="daycampa")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Periodical Term", column_label="ID", position=2, order=10, help="ID of periodical term")
        t.column('ValidFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Valid from")
        t.column('ValidTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=5, order=41, help="Valid to")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillCode", position=6, order=50, help="Billing item code")
        t.column('MaxChargeIncl', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Max. Charge Incl. VAT", column_label="MaxIncl", position=7, order=60, help="Maximum charge including VAT")
        t.column('DCTarget', 'character', format="x(12)", initial="", max_width=24, label="Target", column_label="Target", position=8, order=70, help="Target (allowed billing item)")
        t.column('Weekday', 'character', format="x(7)", initial="", max_width=14, label="Weekday", column_label="Weekday", position=9, order=80, help="Weekday (1=Sunday, 2=monday...7=Saturday)")
        t.column('DCName', 'character', format="x(60)", initial="", max_width=120, label="Name", column_label="Name", position=10, order=90, help="Name of periodical term")
        t.column('MaxChargeExcl', 'decimal', format="->>>>>>>9.999", decimals=3, initial="0", help="Maximum charge excluding VAT", max_width=18, label="Max. Charge Excl.VAT", column_label="MaxExcl", position=11, order=110, description='''

''')
        t.column('CCN', 'integer', format=">>>9", initial="0", max_width=4, label="CCN", position=12, order=100, help="Report CCN that is marked to CDRs")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=13, order=120, help="Code Of Brand")
        t.column('DCType', 'character', format="x(8)", initial="", max_width=16, label="Campaign Type", column_label="Type", position=14, order=130, help="Campaign type")
        t.column('CalcMethod', 'integer', format="9", initial="0", max_width=4, label="Calculation Method", column_label="Method", position=15, order=200, help="Calculation method")
        t.column('InclUnit', 'integer', format=">9", initial="0", max_width=4, label="Included Unit", column_label="Incl.Unit", position=16, order=40, help="Unit of included material")
        t.column('DurMonths', 'integer', format=">>9", initial="0", max_width=4, label="Duration In Months", column_label="Duration", position=17, order=210, help="Duration in months")
        t.column('DurType', 'integer', format="9", initial="0", max_width=4, label="Type Of Duration", column_label="Dur.Type", position=18, order=205, help="Type of duration")
        t.column('Renewal', 'integer', format="9", initial="0", max_width=4, label="Renewal Method", column_label="Renewal", position=19, order=215, help="Renewal method")
        t.column('TermFeeModel', 'character', format="x(12)", initial="", max_width=24, label="Termination Fee Model", column_label="Term.FM", position=20, order=230, help="Fee model for terminating contract")
        t.column('FeeModel', 'character', format="x(12)", initial="", max_width=24, label="Creation Fee Model", column_label="Creation FM", position=21, order=220, help="Fee model for creating contract")
        t.column('ModifyFeeModel', 'character', format="x(12)", initial="", max_width=24, label="Modification Fee Model", column_label="Mod.FM", position=22, order=225, help="Fee model for modifying contract")
        t.column('DurUnit', 'integer', format=">9", initial="0", max_width=4, label="Duration Unit", column_label="Dur.Unit", position=23, order=240, help="Duration unit")
        t.column('Effective', 'integer', format=">9", initial="0", max_width=4, label="Effective", column_label="Eff.", position=24, order=250, help="When contract becomes effective")
        t.column('TermFeeCalc', 'integer', format=">9", initial="0", max_width=4, label="Term. Fee Calculation", column_label="TFee Calc", position=25, order=260, help="Termination fee calculation method")
        t.column('InclStartCharge', 'logical', format="yes/no", initial="yes", max_width=1, label="Include Start Charge", column_label="InclStartFee", position=26, order=270, help="Include start charge to final price")
        t.column('InstanceLimit', 'integer', format=">>9", initial="1", max_width=4, label="Instance Limit", column_label="InstanceLimit", position=27, order=280, help="How many contracts can be active at the same time")
        t.column('LastMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="Last Month Calculation", column_label="Last Month", position=28, order=300, help="Last month calculation method")
        t.column('FirstMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="First Month Calculation", column_label="1.Month", position=29, order=290, help="First month calculation method")
        t.column('BundleUpsell', 'character', format="x(12)", initial="", max_width=24, label="Bundle Upsell", column_label="Upsell", position=30, order=320)
        t.column('StatusCode', 'integer', format="9", initial="1", max_width=4, label="Status", column_label="Status", position=31, order=310)
        t.column('DSSPriority', 'integer', format=">9", initial="0", max_width=4, label="DSSPriority", column_label="DSSPriority", position=32, order=330, help="DSS first month fee calculation priority")
        t.index('DCEvent', [['Brand'], ['DCEvent']], area="Sta_Index_3", primary=True, unique=True)
        t.index('DCType', [['Brand'], ['DCType']], area="Sta_Index_3")

    def down(self):
        self.drop_table('DayCampaign')
