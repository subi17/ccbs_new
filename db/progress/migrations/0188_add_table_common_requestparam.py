from gearbox.migrations import Migration

class AddRequestParam(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestParam', area='Sta_Data_64',
                       label='Request Parameters',
                       dump_name='requestparam',
                       desc='Request parameters')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('ReqType', 'integer', format='>>9', initial='0',
                 label='Request Type',
                 column_label='Type',
                 description='Request type')
        t.column('ParamField', 'character', format='x(12)', initial='',
                 label='Parameter',
                 help='Parameter field')
        t.column('Usage', 'character', format='x(30)', initial='',
                 help='Usage, i.e. contents of parameter')
        t.column('IntConfig', 'integer', format='>>>>9', initial='0',
                 label='Configuration')
        t.column('CharConfig', 'character', format='x(30)', initial='',
                 label='Configuration')
        t.column('DispParam', 'logical', initial='yes',
                 label='Display Parameter',
                 column_label='Display',
                 help='Display parameter in UI')
        t.column('Description', 'character', format='x(50)', initial='')
        t.index('ParamField', ['Brand', 'ReqType', 'ParamField'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RequestParam')

