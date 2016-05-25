from gearbox.migrations import Migration

class AddTableDCCounter(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DCCounter', area="Sta_Data_128", dump_name="dccounte")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Periodical Term", column_label="Term", position=2, order=10, help="ID of periodical term")
        t.column('DCDate', 'date', format="99-99-9999", max_width=4, label="Date", column_label="Date", position=3, order=20, help="Day of the daily campaign")
        t.column('MSSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Sub.ID", position=4, order=30, help="Sequence for a subscription")
        t.column('Amount', 'decimal', format=">>9.99999", decimals=5, initial="0", max_width=20, label="Amount", column_label="Amount", position=5, order=41, help="Used amount")
        t.column('MaxCharge', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Max. Charge", column_label="MaxCharge", position=6, order=50, help="Max. charge")
        t.column('DCTarget', 'character', format="x(12)", initial="", max_width=24, label="Target", column_label="Target", position=7, order=60, help="Target (allowed billing item)")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillCode", position=8, order=70, help="Billing item code")
        t.column('CCN', 'integer', format=">>>9", initial="0", max_width=4, label="CCN", position=9, order=80, help="Report CCN that is marked to CDRs")
        t.column('CalcMethod', 'integer', format="9", initial="0", max_width=4, label="Calculation Method", column_label="Method", position=12, order=200, help="Calculation method")
        t.column('DCType', 'character', format="x(8)", initial="", max_width=16, label="Campaign Type", column_label="Type", position=14, order=130, help="Campaign type")
        t.column('InclUnit', 'integer', format=">9", initial="0", max_width=4, label="Included Unit", column_label="Incl.Unit", position=15, order=40, help="Unit of included material")
        t.column('Latest', 'decimal', format="99999999.999999", decimals=5, initial="0", max_width=20, label="Latest", column_label="Latest", position=16, order=250, help="Latest packet EDR")
        t.index('MSSeq', [['MSSeq'], ['DCDate'], ['DCTarget']], area="Sta_Index_2", primary=True)
        t.index('DCEvent', [['DCEvent'], ['DCDate']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DCCounter')
