from gearbox.migrations import Migration

class AddTableServCom(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ServCom', area="Sta_Data_64", label="Servcom", dump_name="servcom", desc="Service Component")
        t.column('Service', 'character', format="x(8)", initial="", max_width=16, label="Service Group", column_label="Service", position=2, order=10, help="Code of service group")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Component", position=3, order=20, help="Code of Service Component")
        t.column('ScName', 'character', format="x(60)", initial="", max_width=120, label="Component Name", column_label="Name", position=4, order=30, help="Name of Service Component")
        t.column('memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=5, order=40, help="Memo of Service Component")
        t.column('ScLocalName', 'character', format="x(60)", initial="", max_width=120, label="Local Name", column_label="Local Name", position=6, order=50, help="Name in Local Language")
        t.column('ScValueRange', 'integer', format=">>>9", initial="0", max_width=20, label="Range", column_label="Range", extent=2, position=7, order=80, help="Value Range for a Service Parameter in HLR")
        t.column('ScParameter', 'logical', format="yes/no", initial="no", max_width=1, label="Parameter?", column_label="Parameter?", position=8, order=70, help="Does this service contain a subscriber-specific parameter ?")
        t.column('ScChgable', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Changeable", column_label="ChgAble", position=9, order=90, help="Changeable on subscription level")
        t.column('ScPosition', 'integer', format="zz9", initial="0", max_width=4, label="Pos", column_label="Pos", position=10, order=100, help="Position")
        t.column('FeeModel', 'character', format="x(12)", initial="", max_width=24, label="Opening Fee Model", column_label="Open FM", position=12, order=120, help="Fee model for opening service")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=13, order=110, help="Code Of Brand")
        t.column('ActType', 'integer', format="9", initial="0", max_width=4, label="Activation Type", column_label="Activ.", position=14, order=130, help="Activation method type")
        t.column('Target', 'integer', format="9", initial="0", max_width=4, label="Target", column_label="Target", position=15, order=140, help="This Service belongs to")
        t.column('ServAttrL', 'logical', format="yes/no", initial="no", max_width=1, label="Service Attributes", column_label="Attributes", position=16, order=150, help="Does component contain attributes")
        t.column('ServType', 'integer', format="9", initial="0", max_width=4, label="Service Type", column_label="ServType", position=17, order=160, help="Service type; basic / additional")
        t.column('SepHLR', 'logical', format="Yes/No", initial="no", max_width=1, label="Separate Command", column_label="SepHLR", position=18, order=170, help="Separate command line to HLR")
        t.column('ParFeeModel', 'character', format="x(12)", initial="", max_width=24, label="Parameter Fee Model", column_label="Param FM", position=19, order=190, help="Fee model for changing parameter")
        t.column('ClFeeModel', 'character', format="x(12)", initial="", max_width=24, label="Closing Fee Model", column_label="Close FM", position=20, order=180, help="Fee model for closing service")
        t.column('ServiceLimit', 'character', format="x(16)", initial="", max_width=32, label="Service Limit", column_label="SLimit", position=21, order=60, help="Service limit group")
        t.column('ChgFeeModel', 'character', format="x(12)", initial="", max_width=24, label="Changing Fee Model", column_label="ChangeFM", position=22, order=200, help="Fee model for changing service's value")
        t.column('CloseTime', 'integer', format=">>>>9", initial="0", max_width=4, label="Closing Time", column_label="Close", position=23, order=210, help="Closing time rule")
        t.column('SMSTxt', 'character', format="x(12)", initial="", max_width=24, label="SMS Text", column_label="SMS", position=24, order=220, help="Key value of the information text for SMS")
        t.column('NWElement', 'character', format="x(8)", initial="", max_width=16, label="Network Element", column_label="NWE", position=25, order=230, help="Network element")
        t.column('ChgSMSTxt', 'character', format="x(12)", initial="", max_width=24, label="SMS Text For Changing", column_label="Change SMS", position=26, order=250, help="Key value of the information text for SMS for changing value")
        t.column('ClSMSTxt', 'character', format="x(12)", initial="", max_width=24, label="SMS Text For Closing", column_label="Close SMS", position=27, order=240, help="Key value of the information text for SMS for closing")
        t.index('ServCom', [['Brand'], ['ServCom']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SCName', [['Brand'], ['ScName']], area="Sta_Index_3")
        t.index('Service', [['Brand'], ['Service'], ['ServCom']], area="Sta_Index_3")

    def down(self):
        self.drop_table('ServCom')
