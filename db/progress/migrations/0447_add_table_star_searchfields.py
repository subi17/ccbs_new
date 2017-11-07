from gearbox.migrations import Migration

class AddTablesearchfields(Migration):

    database = "star"

    def up(self):
        t = self.table('searchfields', area="Sta_Data_256", label="Search Fields", dump_name="searchfi", desc="Automatic search fields")
        t.column('fieldname', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Field", position=2, order=10, help="Field name of search value")
        t.column('program', 'character', format="X(50)", initial="", max_width=100, label="Program", position=3, order=30, help="Program with recursize directory, if needed")
        t.column('tablename', 'character', format="X(15)", initial="", max_width=30, label="Table", position=4, order=20, help="Table name if needed")
        t.column('sdo', 'character', mandatory=True, format="X(40)", initial="", max_width=80, label="SDO", position=5, order=40, help="Name of SDO. Recuisive directory name")
        t.column('appsrv', 'character', mandatory=True, format="X(20)", initial="", max_width=40, label="AppServer", position=7, order=50, help="Logical name of Application Server")
        t.column('searchprogram', 'character', mandatory=True, format="X(50)", initial="lib/wsearch.w", max_width=100, label="Search program", position=8, order=60, help="Search program name")
        t.column('valuefield', 'character', mandatory=True, format="X(20)", initial="", max_width=40, label="Value field", position=9, order=70, help="Field name of returned value")
        t.column('displayfields', 'character', mandatory=True, format="X(256)", initial="", max_width=512, label="Display fields", position=10, order=80, help="Field's what it shown to search browser")
        t.column('viewerprogram', 'character', format="X(50)", initial="", max_width=100, label="Viewer", position=11, order=90, help="Viewer program, if you needed")
        t.index('fieldname', [['fieldname'], ['tablename'], ['program']], area="Sta_Index_2", primary=True)
        t.index('program', [['program'], ['tablename'], ['fieldname']], area="Sta_Index_2")
        t.index('sdo', [['sdo'], ['tablename'], ['fieldname']], area="Sta_Index_2")
        t.index('tablename', [['tablename'], ['fieldname'], ['program']], area="Sta_Index_2")

    def down(self):
        self.drop_table('searchfields')
