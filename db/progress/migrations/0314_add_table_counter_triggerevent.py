from gearbox.migrations import Migration

class AddTableTriggerEvent(Migration):

    database = "counter"

    def up(self):
        t = self.table('TriggerEvent', area="Dyn_Data_64", dump_name="triggerevent", desc="Auto-generated sequence number for TriggerEvent ID")
        t.column('TriggerEventID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="TriggerEventID", column_label="TriggerEventID", position=2, order=10, help="Trigger EventID")
        t.column('TriggerConfID', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=3, order=20, help="Trigger Configuration code")
        t.column('EventSource', 'character', format="x(12)", initial="", max_width=24, label="Activated", column_label="Activated", position=4, order=30, help="Name of the activation module")
        t.column('StatusCode', 'integer', format=">9", initial="0", max_width=4, label="StatusCode", column_label="StatusCode", position=5, order=40, help="Status of Trigger Event")
        t.column('Created', 'datetime', format="99-99-9999 HH:MM:SS.SSS", max_width=8, label="Created", column_label="Created", position=6, order=50, help="Event creation date and time")
        t.column('Activated', 'datetime', format="99-99-9999 HH:MM:SS.SSS", max_width=8, label="Activated", column_label="Activated", position=7, order=60, help="Event activated Date and Time")
        t.column('Handled', 'datetime', format="99-99-9999 HH:MM:SS.SSS", max_width=8, label="Handled", column_label="Handled", position=8, order=70, help="Event handled Date and Time")
        t.column('Duration', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Duration", column_label="Duration", position=9, order=80, help="Time(sec) taken to complete the activity")
        t.column('Qty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Qty", column_label="Qty", position=10, order=90, help="Generated items (qty)")
        t.column('ChangedFields', 'character', format="x(40)", initial="", max_width=80, label="ChangedFields", column_label="ChangedField", position=11, order=100, help="All changed fields")
        t.column('TableID', 'recid', format=">>>>>>>>>>>9", max_width=8, label="TableID", column_label="TableID", position=12, order=110, help="RecordID")
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Table Name", column_label="Table", position=13, order=120, help="Table related to the action")
        t.column('ChangedValues', 'character', format="x(20)", initial="", max_width=40, label="ChangedValues", column_label="ChangedValues", position=14, order=130, help="Changed Values")
        t.column('KeyValue', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Key Value", column_label="Key", position=15, order=140, help="Key value of the record related to the action")
        t.column('Reason', 'character', format="x(16)", initial="", max_width=32, label="Reason", column_label="Reason", position=16, order=150, help="Reason")
        t.index('TriggerConf', [['TriggerConfID'], ['StatusCode']], area="Dyn_Index_2", primary=True)
        t.index('TriggerEventID', [['TriggerEventID'], ['Created', 'DESC']], area="Dyn_Index_2")

    def down(self):
        self.drop_table('TriggerEvent')
