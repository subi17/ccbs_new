from gearbox.migrations import Migration

class AddRequestActionRule(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestActionRule', area='Sta_Data_64',
                       label='Request Action Rule',
                       dump_name='requestactionrule',
                       desc='Request action rule')
        t.column('RequestActionID', 'integer', format='>>>>>>>9', initial='0',
                 label='Request Action ID',
                 column_label='ID',
                 help='Request action ID')
        t.column('ParamField', 'character', format='x(12)', initial='',
                 label='Parameter',
                 help='Parameter field')
        t.column('ParamValue', 'character', format='x(20)', initial='',
                 label='Parameter Value',
                 column_label='Value',
                 help='Parameter value')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when usage of this rule ends')
        t.column('ExclParamValue', 'character', format='x(30)', initial='',
                 label='Excluded Parameter Value',
                 column_label='Excl.Value',
                 help='Excluded parameter value')
        t.index('ParamField', ['RequestActionID', 'ParamField', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('RequestActionRule')

