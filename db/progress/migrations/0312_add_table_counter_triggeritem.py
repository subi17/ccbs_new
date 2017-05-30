from gearbox.migrations import Migration

class AddTableTriggerItem(Migration):

    database = "counter"

    def up(self):
        t = self.table('TriggerItem', area="Sta_Data_64", dump_name="triggeritem")
        t.column('TriggerEventID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="TriggerEventID", column_label="TriggerEventID", position=2, order=10, help="Trigger EventID")
        t.column('TriggerConfID', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=3, order=20, help="Trigger Configuration code")
        t.column('InvCust', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="InvCust", column_label="InvCust", position=4, order=30, help="Number of Customer Invoice")
        t.column('Period', 'integer', format=">>>>>9", initial="0", max_width=4, label="Period", column_label="Period", position=5, order=40, help="Rating Period")
        t.column('Created', 'datetime', format="99-99-9999 HH:MM:SS.SSS", initial=self.unknown, max_width=8, label="Created", column_label="Created", position=6, order=50, help="Event creation date and time")
        t.column('Handled', 'datetime', format="99-99-9999 HH:MM:SS.SSS", initial=self.unknown, max_width=8, label="Handled", column_label="Handled", position=7, order=60, help="Date and Time when trigger item was handled")
        t.column('Activated', 'datetime', format="99-99-9999 HH:MM:SS.SSS", initial=self.unknown, max_width=8, label="Activated", column_label="Activated", position=8, order=70, help="Date and time when trigger item was activated")
        t.column('StatusCode', 'integer', format=">9", initial="0", max_width=4, label="StatusCode", column_label="StatusCode", position=9, order=80, help="Status of Trigger Item")
        t.column('CLI', 'character', format="x(14)", initial="", max_width=28, label="CLI", column_label="CLI", position=10, order=35, help="Rating CLI")
        t.index('InvCust', [['InvCust'], ['StatusCode']], area="Sta_Index_2", primary=True)
        t.index('Created', [['Created', 'DESC']], area="Sta_Index_2")
        t.index('TriggerConfID', [['TriggerConfID'], ['TriggerEventID'], ['StatusCode'], ['Period', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TriggerItem')
