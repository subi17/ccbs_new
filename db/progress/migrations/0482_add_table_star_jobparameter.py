from gearbox.migrations import Migration

class AddTablejobparameter(Migration):

    database = "star"

    def up(self):
        t = self.table('jobparameter', area="Sta_Data_256", dump_name="jobparam")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=2, order=10, help="Programcode")
        t.column('rowNumber', 'integer', format=">9", initial="0", max_width=4, label="Row", position=3, order=20, help="RowNumber")
        t.column('fieldNumber', 'integer', format="9", initial="1", max_width=4, label="Field", position=4, order=30, help="Field number")
        t.column('fieldLabel', 'character', format="X(15)", initial="", max_width=30, label="Label", position=5, order=50, help="Field label")
        t.column('fieldViewAs', 'character', format="X(12)", initial="FILL-IN", max_width=24, label="View As", position=6, order=60, help="Field view-as")
        t.column('fieldDataType', 'character', format="X(12)", initial="CHARACTER", max_width=24, label="Data type", position=7, order=70, help="Field data type")
        t.column('fieldFormat', 'character', format="X(12)", initial="X(8)", max_width=24, label="Format", position=8, order=80, help="Field format")
        t.column('ForEach', 'character', format="X(50)", initial="", max_width=100, label="FOR EACH", position=9, order=100, help="FOR EACH clause if needed")
        t.column('DataField', 'character', format="X(12)", initial="", max_width=24, label="DataField", position=10, order=110, help="Data field")
        t.column('LabelField', 'character', format="X(12)", initial="", max_width=24, label="Label field", position=11, order=120)
        t.column('searchfieldname', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Search", position=13, order=130, help="Field name of search value")
        t.column('initvalueProgram', 'character', format="X(40)", initial="", max_width=80, label="Init Program", position=14, order=140, help="Init value program")
        t.column('fieldName', 'character', format="X(15)", initial="", max_width=30, label="Name", position=15, order=40, help="Field name")
        t.column('fieldHelp', 'character', format="x(50)", initial="", max_width=100, label="Help", position=16, order=150, help="Help")
        t.column('biggerThan', 'character', format="X(12)", initial="", max_width=24, label="Bigger than", position=17, order=160, help="This equal or begger that given field")
        t.column('fieldItemValue', 'character', format="X(50)", initial="", max_width=100, label="ItemValues", position=18, order=90, help="Combo-box,Toggle-box,Radio-set item values")
        t.index('main', [['programcode'], ['rowNumber'], ['fieldNumber']], area="Sta_Index_2", primary=True, unique=True)
        t.index('fieldname', [['programcode'], ['fieldName']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('jobparameter')
