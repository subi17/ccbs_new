from gearbox.migrations import Migration

class AddTableDCServiceAttribute(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DCServiceAttribute', area="Sta_Data_256", label="DC Service Attributes", dump_name="dcserviceattribute", desc="Default values for service attributes of a periodical contract ")
        t.column('DCServiceAttributeID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="DC Service Attribute ID", column_label="ID", position=2, order=70, help="Unique id for record")
        t.column('DefParam', 'character', format="x(20)", initial="", max_width=40, label="Default Parameter", column_label="Parameter", position=4, order=40, help="Default value for parameter")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=5, order=50, help="Date when becomes effective")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=7, order=60, help="Date when expires")
        t.column('ServAttr', 'character', format="x(14)", initial="", max_width=28, label="Service Attribute", column_label="Attribute", position=8, order=25, help="Attribute of a service component")
        t.column('DCServiceComponentID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="DC Service Component ID", column_label="Comp.ID", position=9, order=80, help="Unique id for component record")
        t.index('DCServiceAttributeID', [['DCServiceAttributeID']], area="Sta_Index_3", primary=True, unique=True)
        t.index('DCServiceComponentID', [['DCServiceComponentID'], ['ServAttr'], ['ToDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('DCServiceAttribute')
