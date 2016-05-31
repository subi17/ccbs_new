from gearbox.migrations import Migration

class AddTableBarringConf(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('BarringConf', area="Sta_Data_128", label="Barring Configuration", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-barringconf.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-barringconf.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="barringconf", desc="Barring Configuration")
        t.column('BarringGroup', 'character', format="X(25)", initial="", max_width=50, label="Barring group", column_label="BarringGroup", position=2, order=10, help="Barring group")
        t.column('BarringCode', 'character', format="X(25)", initial="", max_width=50, label="Barring Name", column_label="BarringCode", position=3, order=20, help="Barring Name")
        t.column('Mask', 'character', format="X(15)", initial="", max_width=30, label="Limited Services", column_label="Mask", position=4, order=30, help="Bit mask of limited services")
        t.column('AllowedAppIDs', 'character', format="X(15)", initial="", max_width=30, label="Allowed Applications", column_label="AllowedAppIDs", position=6, order=50, help="User permissions for Barring Mask")
        t.column('BarringStatus', 'character', format="x(10)", initial="", max_width=20, label="Barring Status", column_label="BarringStatus", position=7, order=60, help="Barring status")
        t.column('AllowedPaymentType', 'character', format="X(15)", initial="", max_width=30, label="Allowed Payment Type", column_label="AllowedPaymentType", position=8, order=70, help="Allowed Payment Type")
        t.column('NWComponent', 'character', format="X(12)", initial="", max_width=24, label="NWComponent", column_label="NWComponent", position=9, order=80, help="Network component")
        t.column('NWActParam', 'character', format="X(25)", initial="", max_width=50, label="NWActParam", column_label="NWActParam", position=10, order=90, help="Network activation parameter")
        t.column('NWDeactParam', 'character', format="X(25)", initial="", max_width=50, label="NWDeactParam", column_label="NWDeactParam", position=11, order=100, help="Network deactivation parameter")
        t.column('Priority', 'integer', format="zz9", initial="0", max_width=4, label="Priority", column_label="Priority", position=12, order=110, help="Priority")
        t.column('UIPriority', 'integer', format="zz9", initial="0", max_width=4, label="UIPriority", column_label="UIPriority", position=13, order=120, help="UI Priority")
        t.column('IFSPriority', 'integer', format="zz9", initial="0", max_width=4, label="IFSPriority", column_label="IFSPriority", position=14, order=130, help="IFS Priority")
        t.column('OldCode', 'character', format="X(25)", initial="", max_width=50, label="OldCode", column_label="OldCode", position=15, order=140, help="Old Code")
        t.index('BarringCode', [['BarringCode']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('OldCode', [['OldCode']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('BarringConf')
