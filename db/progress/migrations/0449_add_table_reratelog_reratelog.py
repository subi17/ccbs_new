from gearbox.migrations import Migration

class AddTableRerateLog(Migration):

    database = "reratelog"

    def up(self):
        t = self.table('RerateLog', area="Sta_Data_128", label="Rerate Log", dump_name="reratelog", desc="Rerate action log")
        t.column('InvCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Customer", position=2, order=10, help="Invoice customer")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=3, order=20, help="Subscription ID")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=22, label="MSISDN", column_label="MSISDN", position=4, order=30, help="MSISDN")
        t.column('EventSource', 'character', format="x(12)", initial="", max_width=24, label="Source", column_label="Source", position=5, order=40, help="Source of the rerate")
        t.column('PeriodBegin', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Period Begin", column_label="Begin", position=6, order=50, help="Begin date for period")
        t.column('PeriodEnd', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Period End", column_label="End", position=7, order=60, help="End date for period")
        t.column('Started', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Started", position=8, order=70, help="Start time for rerate")
        t.column('Ended', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Ended", column_label="Ended", position=9, order=80, help="End time for rerate")
        t.column('ChangedQty', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Changed Qty", column_label="Changed", position=10, order=90, help="Quantity of changed records")
        t.column('StartDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Start Date", column_label="Started", position=11, order=100, help="Starting date")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=12, order=110, help="Brand")
        t.index('InvCust', [['InvCust'], ['StartDate', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('CLI', [['Brand'], ['CLI'], ['StartDate', 'DESC']], area="Sta_Index_2")
        t.index('MsSeq', [['MsSeq'], ['StartDate', 'DESC']], area="Sta_Index_2")
        t.index('StartDate', [['Brand'], ['StartDate', 'DESC'], ['Started', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('RerateLog')
