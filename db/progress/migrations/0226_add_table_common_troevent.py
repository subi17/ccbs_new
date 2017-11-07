from gearbox.migrations import Migration

class AddTableTroEvent(Migration):

    database = "common"

    def up(self):
        t = self.table('TroEvent', area="Sta_Data_256", label="Trouble Ticket Events", dump_name="troevent", desc="Trouble Ticket events")
        t.column('TTNum', 'integer', format=">,>>>,>>9", initial="0", help="Reference no. for Customer", max_width=4, label="Reference", column_label="Reference", position=2, order=10, description="Auto-generated sequence number for Trouble Ticket")
        t.column('PlanStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Planned Date and time of Activity", max_width=20, label="Planned", column_label="Planned", position=3, order=20, description="Time Stamp yyyymmdd.time (sec)")
        t.column('StartStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Actual Start Date and time of Activity", max_width=20, label="Started", column_label="Started", position=4, order=30, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Handler', 'character', format="x(8)", initial="", help="Code of the responsible person", max_width=16, label="Responsible Person", column_label="Responsible Person", position=5, order=40, description="Responsible Person")
        t.column('Active', 'character', format="x(30)", initial="", max_width=60, label="Activity", column_label="Activity", position=6, order=50, help="Action to be performed")
        t.column('ActType', 'character', format="x(1)", initial="", max_width=2, label="Activity Type", column_label="Activity Type", position=7, order=60, help="Type of Activity to be performed")
        t.column('EndStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Actual End Date and time of Activity", max_width=20, label="Ended", column_label="Ended", position=8, order=70, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Duration', 'decimal', format=">>9.99", initial="0.00", max_width=15, label="Duration", column_label="Duration", position=9, order=80, help="Time Taken to complete the activity")
        t.column('Priority', 'character', format="X(8)", initial="", max_width=16, label="Priority", column_label="Priority", position=10, order=90, help="Priority of activity - High,Medium or Low")
        t.column('Remarks', 'character', format="x(60)", initial="", max_width=120, label="Remarks", column_label="Remarks", position=11, order=100, help="Activity Remarks - if any")
        t.index('TTNum', [['TTNum'], ['PlanStamp']], area="Sta_Index_2", primary=True)
        t.index('handend', [['Handler'], ['EndStamp']], area="Sta_Index_2")
        t.index('handler', [['Handler'], ['PlanStamp']], area="Sta_Index_2")
        t.index('seqend', [['Handler'], ['EndStamp']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TroEvent')
