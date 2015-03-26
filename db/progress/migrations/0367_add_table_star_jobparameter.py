from gearbox.migrations import Migration

class Addjobparameter(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('jobparameter', area='Sta_Data_256',
                       dump_name='jobparam')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('rowNumber', 'integer', format='>9', initial='0',
                 label='Row',
                 help='RowNumber')
        t.column('fieldNumber', 'integer', format='9', initial='1',
                 label='Field',
                 help='Field number')
        t.column('fieldName', 'character', format='X(15)', initial='',
                 label='Name',
                 help='Field name')
        t.column('fieldLabel', 'character', format='X(15)', initial='',
                 label='Label',
                 help='Field label')
        t.column('fieldViewAs', 'character', format='X(12)', initial='FILL-IN',
                 label='View As',
                 help='Field view-as')
        t.column('fieldDataType', 'character', format='X(12)', initial='CHARACTER',
                 label='Data type',
                 help='Field data type')
        t.column('fieldFormat', 'character', format='X(12)', initial='X(8)',
                 label='Format',
                 help='Field format')
        t.column('fieldItemValue', 'character', format='X(50)', initial='',
                 label='ItemValues',
                 help='Combo-box,Toggle-box,Radio-set item values')
        t.column('ForEach', 'character', format='X(50)', initial='',
                 label='FOR EACH',
                 help='FOR EACH clause if needed')
        t.column('DataField', 'character', format='X(12)', initial='',
                 help='Data field')
        t.column('LabelField', 'character', format='X(12)', initial='',
                 label='Label field')
        t.column('searchfieldname', 'character', mandatory=True, format='X(15)', initial='',
                 label='Search',
                 help='Field name of search value')
        t.column('initvalueProgram', 'character', format='X(40)', initial='',
                 label='Init Program',
                 help='Init value program')
        t.column('fieldHelp', 'character', format='x(50)', initial='',
                 label='Help')
        t.column('biggerThan', 'character', format='X(12)', initial='',
                 label='Bigger than',
                 help='This equal or begger that given field')
        t.index('fieldname', ['programcode', 'fieldName'], area='Sta_Index_2',
                unique=True)
        t.index('main', ['programcode', 'rowNumber', 'fieldNumber'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('jobparameter')

