from gearbox.migrations import Migration

class AddMedCTX(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('MedCTX', area='Sta_Data_256',
                       label='MedCTX',
                       dump_name='medctx',
                       desc='CDR preprosessor file CENTREX sections')
        t.column('CtxId', 'character', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of FreePhone Centrex Group')
        t.column('CtxFrom', 'character', format='x(12)', initial='',
                 label='From',
                 column_label='From',
                 help='Number series FROM')
        t.column('CtxTo', 'character', format='x(12)', initial='',
                 label='TO',
                 column_label='TO',
                 help='Number series TO')
        t.column('Ident', 'character', initial='',
                 column_label='Ident',
                 help='Identification name of the switch / fidex')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation / memory field for centrex number series')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.index('CtxId', ['CtxId', 'CtxFrom'], area='Sta_Index_2')
        t.index('CustNum', ['CustNum', 'CtxId', 'CtxFrom'], area='Sta_Index_2')
        t.index('Ident', ['Ident', 'CtxId', 'CtxFrom', 'CtxTo'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MedCTX')

