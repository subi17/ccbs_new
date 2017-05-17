from gearbox.migrations import Migration

class AddTableServAttr(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ServAttr', area="Sta_Data_64", label="ServAttr", dump_name="servattr", desc="Service Component")
        t.column('ServAttr', 'character', format="x(14)", initial="", max_width=28, label="Service Attribute", column_label="Attribute", position=2, order=10, help="Attribute of a service component")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Service Component", position=3, order=20, help="Code of Service Component")
        t.column('SAName', 'character', format="x(60)", initial="", max_width=120, label="SAName", column_label="SAttrName", position=4, order=30, help="Name of Service Component attribute")
        t.column('ScValueRange', 'integer', format=">>>9", initial="0", max_width=20, label="Range", column_label="Range", extent=2, position=7, order=80, help="Value Range for a Service Attribute in HLR")
        t.column('SaParameter', 'logical', format="yes/no", initial="no", max_width=1, label="Parameter?", column_label="Parameter?", position=8, order=70, help="Does this service contain a subscriber-specific parameter ?")
        t.column('ScChgable', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Changeable", column_label="ChgAble", position=9, order=90, help="Changeable on subscription level")
        t.column('FeeModel', 'character', format="x(12)", initial="", max_width=24, label="Fee Model", column_label="FeeModel", position=12, order=120, help="Fee model for changing attribute value")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=13, order=110, help="Code Of Brand")
        t.column('DefValue', 'character', format="x(8)", initial="", max_width=16, label="Default Value", column_label="Value", position=14, order=130, help="Default value")
        t.index('ServCom', [['Brand'], ['ServCom'], ['ServAttr']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SaName', [['Brand'], ['ServCom'], ['SAName']], area="Sta_Index_3")
        t.index('ServAttr', [['Brand'], ['ServAttr'], ['ServCom']], area="Sta_Index_3")

    def down(self):
        self.drop_table('ServAttr')
