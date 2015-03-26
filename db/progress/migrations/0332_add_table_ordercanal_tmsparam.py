from gearbox.migrations import Migration

class AddTMSParam(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('TMSParam', area='Sta_Data_128',
                       label='Configuration Parameters',
                       dump_name='tmsparam',
                       desc='Company parameters')
        t.column('ParamGroup', 'character', mandatory=True, format='x(16)', initial='',
                 label='Group',
                 column_label='Group',
                 help='Parameter Group')
        t.column('ParamCode', 'character', format='x(24)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Parameter code (description)')
        t.column('ParamType', 'character', format='xx', initial='',
                 label='Type',
                 column_label='Type',
                 help='Type of character (I,C,DE,DA)',
                 valexp='lookup(ParamType,"I,C,DE,DA") > 0',
                 valmsg='Valid Type Codes are (I)nt (C)har (DE)cimal (DA)te')
        t.column('IntVal', 'integer', format='->>>>>>>>9', initial='0',
                 label='Integer',
                 column_label='Integer',
                 help='Integer value')
        t.column('DecVal', 'decimal', decimals=6, format='->>>>>>>9.99<<<<', initial='0',
                 label='Decimal',
                 column_label='Decimal',
                 help='Decimal value')
        t.column('CharVal', 'character', format='x(50)', initial='',
                 label='Char',
                 column_label='Char',
                 help='Character value')
        t.column('DateVal', 'date', format='99-99-9999',
                 label='Date',
                 column_label='Date')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo')
        t.column('ParamName', 'character', format='x(30)', initial='',
                 label='Parameter',
                 column_label='Parameter',
                 help='Name of parameter')
        t.column('Online', 'logical', format='Y/N', initial='no',
                 label='OnLine',
                 column_label='OnLine',
                 help='Is this parameter used with OnLine connection ?')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('ParamCode', ['Brand', 'ParamCode', 'ParamGroup'], area='Sta_Index_2',
                unique=True)
        t.index('ParamGroup', ['Brand', 'ParamGroup', 'ParamCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ParamName', ['Brand', 'ParamName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('TMSParam')

