from gearbox.migrations import Migration

class AddMsReqStatFunc(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MsReqStatFunc', area='Sta_Data_256')
        t.column('ReqType', 'integer', format='zz9', initial='0',
                 label='Request type',
                 column_label='Type',
                 description='Request type')
        t.column('ReqStatus', 'integer', format='zz9', initial='0',
                 label='MsRquest Status',
                 column_label='RequestStatus',
                 description='Avaialble status of MsRequest')
        t.column('FuncGroup', 'character', format='x(40)', initial='',
                 column_label='FuncGrp',
                 description='Available functions for status')
        t.index('ReqType', ['ReqType', 'ReqStatus'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('MsReqStatFunc')

