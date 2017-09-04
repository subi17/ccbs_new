from gearbox.migrations import Migration

class AddTablemoreinfoitem(Migration):

    database = "star"

    def up(self):
        t = self.table('moreinfoitem', area="Sta_Data_256", label="More Info Item", dump_name="moreitem")
        t.column('number', 'integer', format=">9", initial="0", max_width=4, label="Number", position=2, order=20, help="Number for moreinfo")
        t.column('itemnumber', 'integer', format=">9", initial="0", max_width=4, label="Item", position=3, order=30, help="Item number")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=4, order=10, help="Programcode")
        t.column('startprogramcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=5, order=40, help="Programcode for starting program")
        t.column('msdo', 'character', format="X(50)", initial="", max_width=100, label="Main SDO", position=6, order=50, help="Program with recursize directory")
        t.column('msdofields', 'character', format="X(50)", initial="", max_width=100, label="Main SDO fields", position=7, order=60, help="List of main logicalfieldnames")
        t.column('ssdo', 'character', format="X(50)", initial="", max_width=100, label="Sub SDO", position=8, order=70, help="Program with recursize directory")
        t.column('ssdofields', 'character', format="X(50)", initial="", max_width=100, label="Sub SDO fields", position=9, order=80, help="List of main logicalfieldnames")
        t.column('paramterstr', 'character', format="X(50)", initial="", max_width=100, label="Parameter", position=10, order=90, help="Special parameter to subprogram..")
        t.column('accelerator', 'character', format="X(20)", initial="", max_width=40, label="Accelerator", position=11, order=100, help="Accelerator key")
        t.index('itemnumber', [['programcode'], ['number'], ['itemnumber']], area="Sta_Index_2", primary=True, unique=True)
        t.index('msdo', [['msdo']], area="Sta_Index_2")
        t.index('ssdo', [['ssdo']], area="Sta_Index_2")
        t.index('startprogramcode', [['startprogramcode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('moreinfoitem')
