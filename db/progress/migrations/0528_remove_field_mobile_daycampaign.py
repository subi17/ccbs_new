from gearbox.migrations import Migration

class RemoveFieldsDayCampaign(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('DayCampaign')
		t.drop_column('LastMonthCalc')   
        t.drop_column('FirstMonthCalc')	
        t.drop_column('BundleUpsell')		
		t.drop_column('StatusCode')
	    t.drop_column('DSSPriority')		

    def down(self):
        t = self.alter_table('DayCampaign')
        t.column('LastMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="Last Month Calculation", column_label="Last Month", position=28, order=300, help="Last month calculation method")
        t.column('FirstMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="First Month Calculation", column_label="1.Month", position=29, order=290, help="First month calculation method")
        t.column('BundleUpsell', 'character', format="x(12)", initial="", max_width=24, label="Bundle Upsell", column_label="Upsell", position=30, order=320)
        t.column('StatusCode', 'integer', format="9", initial="1", max_width=4, label="Status", column_label="Status", position=31, order=310)
        t.column('DSSPriority', 'integer', format=">9", initial="0", max_width=4, label="DSSPriority", column_label="DSSPriority", position=32, order=330, help="DSS first month fee calculation priority")
        